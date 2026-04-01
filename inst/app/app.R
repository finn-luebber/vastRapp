# ============================================================================
# VAST Interactive Builder — Shiny App
# Version: 1.1
# Depends: vastR, vastRai, shiny, bslib, jsonlite, htmltools
# ============================================================================

library(shiny)
library(bslib)
library(jsonlite)
library(htmltools)
library(vastR)
library(vastRai)

# ============================================================================
# Constants & Helpers
# ============================================================================

app_version <- tryCatch(
    paste0("v", as.character(utils::packageVersion("vastRapp"))),
    error = function(e) ""
)
EDGE_TYPES <- c(
  "Causation" = "causation",
  "Conceptual implication" = "implication",
  "Prediction" = "prediction",
  "Reasoning" = "reasoning",
  "Transformation" = "transformation",
  "Unknown" = "unknown"
)

EDGE_TYPE_FNS <- c(
  causation = "vast_causation",
  implication = "vast_implication",
  prediction = "vast_prediction",
  reasoning = "vast_reasoning",
  transformation = "vast_transformation",
  unknown = "vast_unknown"
)

# vastR stores edge types as single-letter codes internally
EDGE_TYPE_CODES <- c(
  "c" = "causation",
  "i" = "implication",
  "p" = "prediction",
  "r" = "reasoning",
  "t" = "transformation",
  "u" = "unknown",
  "n" = "naming",
  "noise" = "noise"
)

edge_type_label <- function(type_code) {
  if (type_code %in% names(EDGE_TYPE_CODES)) return(EDGE_TYPE_CODES[[type_code]])
  if (type_code == "") return("structural")
  type_code
}

LAYOUT_DIRS <- c(
  "Top-to-bottom" = "TB",
  "Left-to-right" = "LR",
  "Bottom-to-top" = "BT",
  "Right-to-left" = "RL"
)

DIAMOND_OPTIONS <- c("AND", "OR", "XOR", "Custom")

RIGOR_COLORS <- c(
  clearly_specified = "#4CAF50",
  partially_specified = "#FF9800",
  unspecified = "#F44336"
)

RIGOR_ICONS <- c(
  clearly_specified = "\u2713",
  partially_specified = "~",
  unspecified = "\u2717"
)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ============================================================================
# viz.js Graphviz renderer (replaces DiagrammeR grViz)
# ============================================================================

vizOutput <- function(id, width = "100%", height = "500px") {
  tagList(
    tags$div(
      id = id,
      class = "viz-container",
      style = sprintf(
        "width: %s; height: %s; overflow: auto; border: 1px solid #ddd; border-radius: 8px; background: white; display: flex; align-items: center; justify-content: center;",
        width, height
      ),
      tags$div(
        id = paste0(id, "_placeholder"),
        style = "color: #999; text-align: center; padding: 2rem;",
        tags$p(style = "font-size: 1.1rem;", "No model to display."),
        tags$p(style = "font-size: 0.85rem;", "Use the Build tab or load a file to get started.")
      )
    ),
    tags$div(
      id = paste0(id, "_error"),
      style = "display: none; padding: 0.75rem; margin-top: 0.5rem; background: #FFF3E0; border: 1px solid #FFB74D; border-radius: 6px; color: #E65100; font-size: 0.85rem;",
      tags$strong("Rendering issue: "),
      tags$span(id = paste0(id, "_error_msg"), ""),
      tags$span(" Try ", tags$strong("Export PNG"), " for complex diagrams.")
    )
  )
}

vizHead <- function() {
  tagList(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/viz.js/2.1.2/viz.js"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/viz.js/2.1.2/full.render.js"),
    tags$script(HTML('
      var vizInstance = null;
      function getViz() {
        if (!vizInstance) vizInstance = new Viz();
        return vizInstance;
      }

      function renderDot(containerId, dotString) {
        var container = document.getElementById(containerId);
        var placeholder = document.getElementById(containerId + "_placeholder");
        var errorDiv = document.getElementById(containerId + "_error");
        var errorMsg = document.getElementById(containerId + "_error_msg");
        if (!container) return;
        errorDiv.style.display = "none";
        if (!dotString || dotString.trim() === "") {
          if (placeholder) placeholder.style.display = "block";
          var oldSvg = container.querySelector("svg");
          if (oldSvg) oldSvg.remove();
          return;
        }
        if (placeholder) placeholder.style.display = "none";
        getViz().renderSVGElement(dotString)
          .then(function(svg) {
            var oldSvg = container.querySelector("svg");
            if (oldSvg) oldSvg.remove();
            svg.style.maxWidth = "100%";
            svg.style.height = "auto";
            svg.style.maxHeight = (container.clientHeight - 20) + "px";
            container.appendChild(svg);
          })
          .catch(function(error) {
            vizInstance = new Viz();
            errorDiv.style.display = "block";
            errorMsg.textContent = error.message || "Unknown rendering error.";
          });
      }

      Shiny.addCustomMessageHandler("renderDot", function(msg) {
        renderDot(msg.id, msg.dot);
      });

      Shiny.addCustomMessageHandler("downloadSVG", function(msg) {
        var container = document.getElementById(msg.id);
        if (!container) return;
        var svg = container.querySelector("svg");
        if (!svg) { alert("No diagram to export."); return; }
        var svgData = new XMLSerializer().serializeToString(svg);
        var blob = new Blob([svgData], {type: "image/svg+xml"});
        var url = URL.createObjectURL(blob);
        var a = document.createElement("a");
        a.href = url; a.download = msg.filename || "vast_diagram.svg";
        document.body.appendChild(a); a.click(); document.body.removeChild(a);
        URL.revokeObjectURL(url);
      });

      Shiny.addCustomMessageHandler("downloadPNG", function(msg) {
        var container = document.getElementById(msg.id);
        if (!container) return;
        var svg = container.querySelector("svg");
        if (!svg) { alert("No diagram to export."); return; }
        var svgData = new XMLSerializer().serializeToString(svg);
        var canvas = document.createElement("canvas");
        var ctx = canvas.getContext("2d");
        var img = new Image();
        var svgBlob = new Blob([svgData], {type: "image/svg+xml;charset=utf-8"});
        var url = URL.createObjectURL(svgBlob);
        img.onload = function() {
          canvas.width = msg.width || 1200;
          canvas.height = msg.height || 800;
          ctx.fillStyle = "white";
          ctx.fillRect(0, 0, canvas.width, canvas.height);
          var scale = Math.min(canvas.width / img.width, canvas.height / img.height);
          var x = (canvas.width - img.width * scale) / 2;
          var y = (canvas.height - img.height * scale) / 2;
          ctx.drawImage(img, x, y, img.width * scale, img.height * scale);
          canvas.toBlob(function(blob) {
            var a = document.createElement("a");
            a.href = URL.createObjectURL(blob);
            a.download = msg.filename || "vast_diagram.png";
            document.body.appendChild(a); a.click(); document.body.removeChild(a);
          }, "image/png");
          URL.revokeObjectURL(url);
        };
        img.src = url;
      });

      // Clipboard helper with fallback for non-HTTPS
      Shiny.addCustomMessageHandler("copyToClipboard", function(msg) {
        if (!msg.text || msg.text === "") return;
        if (navigator.clipboard && window.isSecureContext) {
          navigator.clipboard.writeText(msg.text).then(function() {
            Shiny.setInputValue("clipboard_success", Math.random());
          }).catch(function() {
            fallbackCopy(msg.text);
          });
        } else {
          fallbackCopy(msg.text);
        }
      });

      function fallbackCopy(text) {
        var ta = document.createElement("textarea");
        ta.value = text;
        ta.style.position = "fixed";
        ta.style.left = "-9999px";
        document.body.appendChild(ta);
        ta.select();
        try {
          document.execCommand("copy");
          Shiny.setInputValue("clipboard_success", Math.random());
        } catch (e) {
          Shiny.setInputValue("clipboard_fail", Math.random());
        }
        document.body.removeChild(ta);
      }
    '))
  )
}

# Helper: build choices for concept dropdowns (concept + diamond IDs)
make_concept_choices <- function(model) {
  if (is.null(model)) return(character())
  choices <- c()
  for (nid in names(model$nodes)) {
    node <- model$nodes[[nid]]
    if (node$type %in% c("concept", "data", "diamond")) {
      lbl <- if (node$type == "diamond") {
        paste0(nid, ": [Diamond] ", node$label)
      } else {
        name_lbl <- nid
        for (e in model$edges) {
          if (identical(e$type, "n") && identical(e$from, nid)) {
            name_node <- model$nodes[[e$to]]
            if (!is.null(name_node)) {
              name_lbl <- gsub('[\u201C\u201D]', '', name_node$label)
            }
            break
          }
        }
        paste0(nid, ": ", name_lbl)
      }
      choices <- c(choices, setNames(nid, lbl))
    }
  }
  choices
}

make_group_choices <- function(model) {
  if (is.null(model) || length(model$groups) == 0) return(character())
  choices <- c()
  for (gid in names(model$groups)) {
    grp <- model$groups[[gid]]
    choices <- c(choices, setNames(gid, paste0("[Group] ", gid, ": ", grp$label)))
  }
  choices
}

groups_containing_node <- function(model, node_id) {
  if (is.null(model) || length(model$groups) == 0) return(character())
  result <- c()
  for (gid in names(model$groups)) {
    grp <- model$groups[[gid]]
    if (node_id %in% grp$node_ids) {
      result <- c(result, setNames(gid, paste0(gid, ": ", grp$label)))
    }
  }
  result
}

get_concept_name <- function(model, concept_id) {
  if (is.null(model)) return(concept_id)
  for (e in model$edges) {
    if (identical(e$type, "n") && identical(e$from, concept_id)) {
      name_node <- model$nodes[[e$to]]
      if (!is.null(name_node)) {
        return(gsub('[\u201C\u201D]', '', name_node$label))
      }
    }
  }
  concept_id
}

next_id <- function(model, prefix) {
  if (is.null(model)) return(paste0(prefix, "1"))
  existing <- names(model$nodes)
  i <- 1
  while (paste0(prefix, i) %in% existing) i <- i + 1
  paste0(prefix, i)
}

next_group_id <- function(model) {
  if (is.null(model) || length(model$groups) == 0) return("HOC1")
  existing <- names(model$groups)
  i <- 1
  while (paste0("HOC", i) %in% existing) i <- i + 1
  paste0("HOC", i)
}

count_connected_edges <- function(model, node_id) {
  if (is.null(model)) return(0)
  count <- 0
  for (e in model$edges) {
    if (e$from == node_id || e$to == node_id) count <- count + 1
  }
  count
}

remove_concept <- function(model, concept_id) {
  name_node_id <- NULL
  for (e in model$edges) {
    if (identical(e$type, "n") && identical(e$from, concept_id)) {
      name_node_id <- e$to
      break
    }
  }
  ids_to_remove <- c(concept_id, name_node_id)
  model$edges <- Filter(function(e) {
    !(e$from %in% ids_to_remove || e$to %in% ids_to_remove)
  }, model$edges)
  model$nodes[[concept_id]] <- NULL
  if (!is.null(name_node_id)) model$nodes[[name_node_id]] <- NULL
  for (gid in names(model$groups)) {
    model$groups[[gid]]$node_ids <- setdiff(model$groups[[gid]]$node_ids, ids_to_remove)
  }
  model
}

# ============================================================================
# Example Models — Quick-start options for new users
# ============================================================================

EXAMPLES <- list(
  sun = list(
    label = "Conceptual Implications",
    description = "Simple model with implication relationships (cf. Leising et al., 2023, Fig. 2)",
    build = function() {
      vast_model(title = "Conceptual Implications", naming_mode = "separated") |>
        add_nodes(
          vast_concept("S", "S"), vast_concept("H", "H"), vast_concept("B", "B"),
          vast_name("n_sun", "Sun"), vast_name("n_hot", "Hot"), vast_name("n_bright", "Bright")
        ) |>
        add_edges(
          vast_implication("S", "H"), vast_implication("S", "B"),
          vast_naming("S", "n_sun"), vast_naming("H", "n_hot"), vast_naming("B", "n_bright")
        )
    }
  ),
  rel_types = list(
    label = "Relationship Types",
    description = "All four relationship types: causation, transformation, prediction, reasoning (cf. Leising et al., 2023, Fig. 3)",
    build = function() {
      vast_model(title = "Relationship Types", naming_mode = "separated",
                 rankdir = "LR", nodesep = 1.0, ranksep = 1.5) |>
        add_nodes(
          vast_concept("C_smk", "C_smk"), vast_concept("C_lc", "C_lc"),
          vast_concept("C_tic", "C_tic"), vast_concept("C_tif", "C_tif"),
          vast_concept("C_ht", "C_ht"), vast_concept("C_ych", "C_ych"),
          vast_concept("C_eq1", "C_eq1"), vast_concept("C_eq2", "C_eq2"),
          vast_name("n_smk", "Smoking"), vast_name("n_lc", "Lung Cancer"),
          vast_name("n_tic", "Temperature\nin Celsius"), vast_name("n_tif", "Temperature\nin Fahrenheit"),
          vast_name("n_ht", "Height"), vast_name("n_ych", "Number of Y\nChromosomes"),
          vast_name("n_eq1", "X + 4 = 8"), vast_name("n_eq2", "X = 4")
        ) |>
        add_edges(
          vast_naming("C_smk", "n_smk"), vast_naming("C_lc", "n_lc"),
          vast_naming("C_tic", "n_tic"), vast_naming("C_tif", "n_tif"),
          vast_naming("C_ht", "n_ht"), vast_naming("C_ych", "n_ych"),
          vast_naming("C_eq1", "n_eq1"), vast_naming("C_eq2", "n_eq2"),
          vast_causation("C_smk", "C_lc"),
          vast_transformation("C_tic", "C_tif"),
          vast_prediction("C_ht", "C_ych"),
          vast_reasoning("C_eq1", "C_eq2")
        )
    }
  ),
  advanced = list(
    label = "Advanced Features",
    description = "Groups, IS/OUGHT statements, perspectives, diamonds, and compound edges",
    build = function() {
      vast_model(title = "IS and OUGHT with Perspectives", naming_mode = "separated", rankdir = "TB") |>
        add_nodes(
          vast_concept("INC", "INC"),
          vast_name("n_inc", "Expected increase of average\nglobal temperature in the\nnext 50 years (Kelvin)"),
          vast_is("is_inc", value = "2"),
          vast_ought("ought_inc", value = "0"),
          vast_perspective("p_daniel", holder = "Daniel", value = "1.0"),
          vast_perspective("p_marcos", holder = "Marcos", value = "0.8")
        ) |>
        add_edges(
          vast_naming("INC", "n_inc"),
          vast_relation("INC", "is_inc", type = "", color = "gray50"),
          vast_relation("INC", "ought_inc", type = "", color = "gray50"),
          vast_relation("p_daniel", "is_inc", type = "", color = "gray50", style = "dashed"),
          vast_relation("p_marcos", "is_inc", type = "", color = "gray50", style = "dashed")
        )
    }
  ),
  social_media = list(
    label = "Complete Analysis",
    description = "Full VAST analysis: Social media and depression, with data, mechanisms, groups, IS/OUGHT",
    build = function() {
      vast_model(
        title = "Does Social Media Use Cause Depression in Adolescents?",
        naming_mode = "separated", rankdir = "TB", nodesep = 0.7,
        ranksep = 1.0, fontsize_edges = 12
      ) |>
        add_nodes(
          vast_concept("SMU", "SMU"), vast_name("n_smu", "Social Media\nUse (hours/day)"),
          vast_concept("DEP", "DEP"), vast_name("n_dep", "Depressive\nSymptoms"),
          vast_concept("SC", "SC"), vast_name("n_sc", "Social\nComparison"),
          vast_concept("SLEEP", "SLP"), vast_name("n_sleep", "Sleep\nQuality"),
          vast_concept("LONE", "LONE"), vast_name("n_lone", "Perceived\nLoneliness"),
          vast_data("D1", "Cross-sectional\nSurvey (N=5000)"),
          vast_data("D2", "Longitudinal\nStudy (N=800)"),
          vast_diamond("med_or", "OR"),
          vast_noise_source("noise_dep"),
          vast_is("is_smu_dep", value = "?"),
          vast_ought("ought_smu", value = "low"),
          vast_perspective("p_res", holder = "Researcher A", value = "0.7"),
          vast_perspective("p_crit", holder = "Critic B", value = "0.3")
        ) |>
        add_edges(
          vast_naming("SMU", "n_smu"), vast_naming("DEP", "n_dep"),
          vast_naming("SC", "n_sc"), vast_naming("SLEEP", "n_sleep"),
          vast_naming("LONE", "n_lone"),
          vast_causation("SMU", "SC", strength = "0.4"),
          vast_causation("SMU", "SLEEP", strength = "-0.3"),
          vast_causation("SMU", "LONE", strength = "?"),
          vast_causation("SC", "med_or"), vast_causation("SLEEP", "med_or"),
          vast_causation("LONE", "med_or"), vast_causation("med_or", "DEP"),
          vast_noise("noise_dep", "DEP"),
          vast_prediction("D1", "SMU", strength = "0.35"),
          vast_prediction("D2", "SMU", strength = "0.20"),
          vast_reasoning("D1", "is_smu_dep", strength = "weak"),
          vast_reasoning("D2", "is_smu_dep", strength = "moderate"),
          vast_relation("SMU", "is_smu_dep", type = "", color = "gray60"),
          vast_relation("SMU", "ought_smu", type = "", color = "gray60"),
          vast_relation("p_res", "is_smu_dep", type = "", color = "gray50", style = "dashed"),
          vast_relation("p_crit", "is_smu_dep", type = "", color = "gray50", style = "dashed")
        ) |>
        add_groups(
          vast_group("mechanisms", label = "Proposed Mechanisms",
                     node_ids = c("SC", "n_sc", "SLEEP", "n_sleep", "LONE", "n_lone", "med_or"),
                     fillcolor = "#FFF8E1", bordercolor = "#F57F17", penwidth = 2.5),
          vast_group("evidence", label = "Empirical Evidence",
                     node_ids = c("D1", "D2"),
                     fillcolor = "#E8F5E9", bordercolor = "#2E7D32", penwidth = 2.5)
        )
    }
  )
)

# ============================================================================
# UI
# ============================================================================

ui <- page_fillable(
  theme = bs_theme(
    version = 5, bootswatch = "flatly",
    base_font = font_google("Source Sans Pro"),
    heading_font = font_google("Source Sans Pro"),
    font_scale = 0.92, "card-border-radius" = "8px"
  ),
  vizHead(),
  tags$head(tags$style(HTML('
    .viz-container svg { cursor: grab; }
    .viz-container svg:active { cursor: grabbing; }
    .slot-indicator {
      background: #e3f2fd; border: 1px solid #90caf9; border-radius: 6px;
      padding: 0.4rem 0.75rem; margin-bottom: 0.75rem; font-weight: 600;
      display: flex; align-items: center; justify-content: space-between;
    }
    .model-pill {
      display: inline-block; padding: 0.35rem 1rem; border-radius: 20px;
      cursor: pointer; font-weight: 600; font-size: 0.88rem;
      border: 2px solid #1976D2; transition: all 0.2s;
    }
    .model-pill.active { background: #1976D2; color: white; }
    .model-pill.inactive { background: white; color: #1976D2; }
    .model-pill.empty { opacity: 0.5; }
    .model-label-input {
      border: none; background: transparent; font-weight: 600;
      font-size: 0.88rem; width: auto; min-width: 60px; max-width: 180px;
      text-align: center; padding: 0;
    }
    .model-label-input:focus { outline: 1px solid #1976D2; border-radius: 4px; background: white; }
    .rigor-card {
      border-radius: 6px; padding: 0.5rem 0.75rem; margin: 0.25rem;
      color: white; font-weight: 600; cursor: pointer; font-size: 0.85rem;
    }
    .toast-msg {
      position: fixed; bottom: 20px; right: 20px; background: #333; color: white;
      padding: 0.5rem 1rem; border-radius: 6px; z-index: 9999;
      animation: fadeInOut 2s forwards; font-size: 0.88rem;
    }
    @keyframes fadeInOut {
      0% { opacity: 0; transform: translateY(10px); }
      15% { opacity: 1; transform: translateY(0); }
      85% { opacity: 1; }
      100% { opacity: 0; }
    }
    .diff-table { font-size: 0.82rem; }
    .diff-table td.differs { background: #FFF8E1; }
    .form-group { margin-bottom: 0.5rem; }
    .compact-input .form-control { padding: 0.3rem 0.5rem; font-size: 0.88rem; }
    .selector-row {
      display: flex; align-items: flex-end; gap: 0.35rem; margin-bottom: 0.35rem;
    }
    .selector-row .form-group { flex: 1; margin-bottom: 0; }
    #details_tabs { margin-top: 0.5rem; flex-shrink: 0; }
    #details_tabs .card-header { padding: 0.25rem 0.5rem; }
    #details_tabs .nav-link { padding: 0.3rem 0.75rem; font-size: 0.85rem; }
    #build_accordion .accordion-button { padding: 0.4rem 0.75rem; font-size: 0.88rem; font-weight: 600; }
    #build_accordion .accordion-body { padding: 0.5rem 0.75rem; }
    #build_accordion .accordion-item { border-radius: 8px; margin-bottom: 0.25rem; }
  '))),

  tags$div(id = "toast_container"),

  # --- Header ---
  div(
      style = "display: flex; align-items: center; justify-content: space-between; padding: 0.5rem 0; margin-bottom: 0.5rem; border-bottom: 1px solid #dee2e6;",
      div(
          tags$h5(
              "VAST Interactive Builder",
              tags$span(app_version, style = "font-weight: 300; opacity: 0.5; margin-left: 6px; font-size: 0.85em;"),
              style = "margin: 0; font-weight: 700;"
          ),
          tags$div(style = "font-size: 0.78rem; color: #666; margin-top: 0.1rem;",
                   "Finn Luebber, ",
                   tags$a(href = "https://social-neuroscience-luebeck.com/", target = "_blank", style = "color: #1976D2;", "Social Neuroscience Lab, University of Luebeck")
          ),
          tags$div(style = "font-size: 0.78rem; color: #666; margin-top: 0.1rem;",
          tags$a(href = "https://github.com/finn-luebber/vastRapp", target = "_blank", "github.com/finn-luebber/vastRapp"))
      
      ),
      div(
      style = "display: flex; gap: 0.5rem;",
      actionButton("about_btn", "\u2139 About", class = "btn-outline-info btn-sm"),
      actionButton("cite_btn", "\U0001F4D6 Cite", class = "btn-outline-secondary btn-sm"),
      actionButton("undo_btn", "\u21A9 Undo", class = "btn-outline-secondary btn-sm"),
      downloadButton("save_btn", "\U0001F4BE Save", class = "btn-outline-primary btn-sm"),
      tags$label(
        class = "btn btn-outline-primary btn-sm", style = "margin: 0; cursor: pointer;",
        "\U0001F4C2 Load",
        tags$input(type = "file", id = "load_file", accept = ".json", style = "display: none;",
          onchange = "Shiny.setInputValue('load_file', this.files[0] ? this.files[0].name : null); var reader = new FileReader(); reader.onload = function(e) { Shiny.setInputValue('load_file_content', e.target.result); }; if(this.files[0]) reader.readAsText(this.files[0]);")
      )
    )
  ),

  # --- Main layout ---
  layout_columns(
    col_widths = c(5, 7), fillable = TRUE,

    # ========== LEFT PANEL ==========
    navset_card_tab(
      id = "workspace_tabs", full_screen = FALSE,
      height = "calc(100vh - 180px)",

      # --- Tab 1: Build Model ---
      nav_panel(title = "Build Model", value = "build",
        div(style = "overflow-y: auto; height: 100%; padding-right: 0.25rem;",
          div(class = "slot-indicator",
            uiOutput("slot_indicator_ui"),
            div(style = "display: flex; gap: 0.25rem;",
              actionButton("switch_slot_build", "Switch slot", class = "btn-outline-primary btn-sm"),
              actionButton("clear_model_btn", "\u2716 Clear", class = "btn-outline-danger btn-sm")
            )
          ),
          card(card_header(class = "py-1", "Quick Start: Load an Example"),
            card_body(class = "py-2",
              tags$p(style = "font-size: 0.82rem; color: #555; margin-bottom: 0.5rem;",
                "Load a pre-built example into the active slot to explore VAST features:"),
              div(style = "display: flex; flex-wrap: wrap; gap: 0.35rem;",
                actionButton("ex_sun", "Conceptual Implications", class = "btn-outline-secondary btn-sm",
                  title = "Simple model with implication relationships"),
                actionButton("ex_rel_types", "Relationship Types", class = "btn-outline-secondary btn-sm",
                  title = "All four relationship types"),
                actionButton("ex_advanced", "IS/OUGHT & Perspectives", class = "btn-outline-secondary btn-sm",
                  title = "IS/OUGHT statements and perspectives"),
                actionButton("ex_social_media", "Complete Analysis", class = "btn-outline-info btn-sm",
                  title = "Full analysis with all VAST features")
              )
            )
          ),
          card(card_header(class = "py-1", "Model Settings"),
            card_body(class = "compact-input py-2",
              layout_columns(col_widths = c(5, 4, 3),
                textInput("model_title", "Title", value = "", width = "100%"),
                textInput("model_analyst", "Analyst", value = "", width = "100%"),
                selectInput("model_layout", "Layout", choices = LAYOUT_DIRS, selected = "TB", width = "100%")
              ),
              actionButton("apply_settings", "Apply", class = "btn-primary btn-sm")
            )
          ),
          accordion(
            id = "build_accordion", open = FALSE, multiple = TRUE,
            accordion_panel("Concepts", value = "concepts",
              div(class = "selector-row",
                selectInput("sel_concept", NULL, choices = character(), width = "100%"),
                actionButton("edit_concept_btn", "\u270E", class = "btn-outline-secondary btn-sm", title = "Edit"),
                actionButton("del_concept_btn", "\u2717", class = "btn-outline-danger btn-sm", title = "Delete")
              ),
              hr(style = "margin: 0.5rem 0;"),
              div(class = "compact-input",
                layout_columns(col_widths = c(3, 5, 4),
                  textInput("new_concept_id", "ID", value = "", width = "100%"),
                  textInput("new_concept_name", "Name", value = "", width = "100%"),
                  selectInput("new_concept_type", "Type", choices = c("Concept" = "concept", "Data" = "data"), width = "100%")
                ),
                actionButton("add_concept", "Add concept", class = "btn-primary btn-sm")
              )
            ),
            accordion_panel("Relationships", value = "relationships",
              div(class = "selector-row",
                selectInput("sel_edge", NULL, choices = character(), width = "100%"),
                actionButton("edit_edge_btn", "\u270E", class = "btn-outline-secondary btn-sm", title = "Edit"),
                actionButton("del_edge_btn", "\u2717", class = "btn-outline-danger btn-sm", title = "Delete")
              ),
              hr(style = "margin: 0.5rem 0;"),
              div(class = "compact-input",
                layout_columns(col_widths = c(6, 6),
                  selectInput("new_edge_from", "From", choices = character(), width = "100%"),
                  selectInput("new_edge_to", "To", choices = character(), width = "100%")
                ),
                layout_columns(col_widths = c(6, 6),
                  selectInput("new_edge_type", "Type", choices = EDGE_TYPES, width = "100%"),
                  textInput("new_edge_strength", "Strength", value = "", width = "100%")
                ),
                uiOutput("compound_edge_ui"),
                actionButton("add_edge", "Add relationship", class = "btn-primary btn-sm")
              )
            ),
            accordion_panel("Diamonds (Connectives)", value = "diamonds",
              div(class = "selector-row",
                selectInput("sel_diamond", NULL, choices = character(), width = "100%"),
                actionButton("edit_diamond_btn", "\u270E", class = "btn-outline-secondary btn-sm", title = "Edit"),
                actionButton("del_diamond_btn", "\u2717", class = "btn-outline-danger btn-sm", title = "Delete")
              ),
              hr(style = "margin: 0.5rem 0;"),
              div(class = "compact-input",
                layout_columns(col_widths = c(3, 5, 4),
                  textInput("new_diamond_id", "ID", value = "", width = "100%"),
                  selectInput("new_diamond_content", "Content", choices = DIAMOND_OPTIONS, width = "100%"),
                  checkboxInput("new_diamond_footnote", "Footnote", value = FALSE)
                ),
                conditionalPanel(condition = "input.new_diamond_content == 'Custom'",
                  textInput("new_diamond_custom", "Custom formula", value = "", width = "100%")
                ),
                actionButton("add_diamond", "Create diamond", class = "btn-primary btn-sm")
              )
            ),
            accordion_panel("IS / OUGHT Statements", value = "is_ought",
              div(class = "selector-row",
                selectInput("sel_is_ought", NULL, choices = character(), width = "100%"),
                actionButton("edit_io_btn", "\u270E", class = "btn-outline-secondary btn-sm", title = "Edit"),
                actionButton("add_persp_btn", "+P", class = "btn-outline-success btn-sm", title = "Add perspective"),
                actionButton("del_io_btn", "\u2717", class = "btn-outline-danger btn-sm", title = "Delete")
              ),
              uiOutput("perspective_form_ui"),
              hr(style = "margin: 0.5rem 0;"),
              div(class = "compact-input",
                layout_columns(col_widths = c(3, 6, 3),
                  radioButtons("new_io_type", "Type", choices = c("IS" = "is", "OUGHT" = "ought"), inline = TRUE),
                  uiOutput("io_applies_to_ui"),
                  textInput("new_io_value", "Value", value = "", width = "100%")
                ),
                actionButton("add_is_ought", "Add statement", class = "btn-primary btn-sm")
              )
            ),
            accordion_panel("Groups (Higher-Order Concepts)", value = "groups",
              div(class = "selector-row",
                selectInput("sel_group", NULL, choices = character(), width = "100%"),
                actionButton("edit_group_btn", "\u270E", class = "btn-outline-secondary btn-sm", title = "Edit"),
                actionButton("del_group_btn", "\u2717", class = "btn-outline-danger btn-sm", title = "Delete")
              ),
              hr(style = "margin: 0.5rem 0;"),
              div(class = "compact-input",
                layout_columns(col_widths = c(3, 5, 4),
                  textInput("new_group_id", "ID", value = "", width = "100%"),
                  textInput("new_group_label", "Label", value = "", width = "100%"),
                  uiOutput("group_parent_ui")
                ),
                uiOutput("group_members_ui"),
                actionButton("add_group", "Add group", class = "btn-primary btn-sm")
              )
            ),
            accordion_panel("Noise Influences", value = "noise",
              div(class = "selector-row",
                selectInput("sel_noise", NULL, choices = character(), width = "100%"),
                actionButton("del_noise_btn", "\u2717", class = "btn-outline-danger btn-sm", title = "Delete")
              ),
              hr(style = "margin: 0.5rem 0;"),
              div(class = "compact-input",
                layout_columns(col_widths = c(6, 6),
                  selectInput("new_noise_target", "Target concept", choices = character(), width = "100%"),
                  textInput("new_noise_strength", "Strength", value = "", width = "100%")
                ),
                actionButton("add_noise", "Add noise", class = "btn-primary btn-sm")
              )
            )
          )
        )
      ),

      # --- Tab 2: Narrative -> VAST ---
      nav_panel(title = "Narrative \u2192 VAST", value = "n2v",
        div(style = "overflow-y: auto; height: 100%; padding-right: 0.25rem;",
          card(card_header(class = "py-1", "Theory Input"),
            card_body(class = "py-2",
              textAreaInput("theory_text", "Theory paragraph", value = "", rows = 6, width = "100%",
                placeholder = "Paste your theory paragraph here, and generate a prompt to evaluate the theory..."),
              textAreaInput("theory_context", "Additional context (optional)", value = "", rows = 2, width = "100%",
                placeholder = "e.g., domain, terminology, specific instructions")
            )
          ),
          card(card_header(class = "py-1", "Generate Prompt"),
            card_body(class = "py-2",
              actionButton("gen_n2v_prompt", "Generate prompt", class = "btn-primary btn-sm"),
              uiOutput("n2v_prompt_ui")
            )
          ),
          card(card_header(class = "py-1", "Load LLM Response"),
            card_body(class = "py-2",
              textAreaInput("llm_json", "LLM JSON response", value = "", rows = 8, width = "100%",
                placeholder = "Paste the LLM's JSON output here..."),
              actionButton("load_json", "Load models", class = "btn-primary btn-sm"),
              uiOutput("rigor_display_ui"),
              tags$div(style = "margin-top: 0.75rem; padding: 0.5rem; background: #f8f9fa; border-radius: 6px; font-size: 0.78rem; color: #555;",
                tags$strong("About the rigor assessment: "),
                "The T1\u2013T4 indicators evaluate the theoretical rigor of the source text. ",
                "They are based on the ", tags$em("Indicators for the Assessment of Theoretical Rigor"),
                " (Musfeld et al., v3.0; ",
                tags$a(href = "https://www.researchgate.net/publication/400424233_Indicators_for_the_Assessment_of_Theoretical_Rigor", target = "_blank", style = "color: #1976D2;", "Preprint"),
                "): ",
                "T1 (Phenomenon) assesses whether the theory clearly specifies its target phenomenon, ",
                "T2 (Definitions) evaluates concept definitions, ",
                "T3 (Mechanism) examines the explanatory mechanism (completeness, quality, sufficiency), and ",
                "T4 (Scope) checks whether boundary conditions are stated. ",
                "vastRai extends the original binary scale to three levels: ",
                tags$em("clearly specified"), ", ", tags$em("partially specified"), ", and ", tags$em("unspecified"), "."
              )
            )
          )
        )
      ),

      # --- Tab 3: VAST -> Narrative ---
      nav_panel(title = "VAST \u2192 Narrative", value = "v2n",
        div(style = "overflow-y: auto; height: 100%; padding-right: 0.25rem;",
          card(card_header(class = "py-1", "Model Selection"),
            card_body(class = "py-2",
              uiOutput("v2n_model_info"),
              textAreaInput("v2n_context", "Additional context (optional)", value = "", rows = 2, width = "100%",
                placeholder = "e.g., Write for a psychology audience"),
              checkboxInput("v2n_fidelity", "Assess fidelity", value = FALSE),
              actionButton("gen_v2n_prompt", "Generate prompt", class = "btn-primary btn-sm"),
              uiOutput("v2n_prompt_ui")
            )
          ),
          card(card_header(class = "py-1", "Narrative Response (optional)"),
            card_body(class = "py-2",
              textAreaInput("v2n_response", "LLM narrative response", value = "", rows = 6, width = "100%",
                placeholder = "Paste the LLM's narrative here for your reference...")
            )
          )
        )
      )

    ),

    # ========== RIGHT PANEL: Diagram + Details ==========
    div(style = "display: flex; flex-direction: column; height: calc(100vh - 180px);",
      div(style = "display: flex; align-items: center; gap: 0.75rem; margin-bottom: 0.5rem;",
        div(style = "display: flex; gap: 0.25rem;", uiOutput("model_pills_ui")),
        div(style = "font-size: 0.82rem; color: #666;", uiOutput("model_stats_ui")),
        div(style = "margin-left: auto;",
          selectInput("naming_mode_sel", NULL,
            choices = c("Integrated" = "integrated", "Separated" = "separated", "FIMM" = "fimm"),
            selected = "separated", width = "140px")
        )
      ),
      div(style = "flex: 1; min-height: 0;",
        vizOutput("diagram", height = "100%")
      ),
      div(style = "display: flex; gap: 0.5rem; margin-top: 0.5rem; flex-wrap: wrap;",
        actionButton("export_png", "\U0001F4F7 Export PNG", class = "btn-outline-secondary btn-sm"),
        actionButton("export_svg", "\U0001F5BC Export SVG", class = "btn-outline-secondary btn-sm"),
        downloadButton("export_report", "\U0001F4C4 Export Report", class = "btn-outline-secondary btn-sm"),
        actionButton("goto_v2n", "\u2192 Narrative Prompt", class = "btn-outline-primary btn-sm")
      ),

      # Bottom area: Details tabs
      navset_card_tab(
        id = "details_tabs",
        height = "250px",

        nav_panel(title = "Notes", value = "notes",
          div(style = "overflow-y: auto; height: 100%; padding: 0.25rem;",
            uiOutput("translation_notes_ui")
          )
        ),

        nav_panel(title = "Comparison", value = "comparison",
          div(style = "overflow-y: auto; height: 100%; padding: 0.25rem;",
            uiOutput("model_comparison_ui")
          )
        ),

        nav_panel(title = "R Code", value = "rcode",
          div(style = "overflow-y: auto; height: 100%; padding: 0.25rem;",
            uiOutput("r_code_toggle_ui"),
            verbatimTextOutput("r_code_display"),
            actionButton("copy_r_code", "Copy code", class = "btn-outline-secondary btn-sm")
          )
        )
      )
    )
  ),

  # --- References footer ---
  tags$div(style = "padding: 0.75rem 0; margin-top: 0.5rem; border-top: 1px solid #dee2e6; font-size: 0.75rem; color: #666;",
    tags$strong("References"),
    tags$ul(style = "margin: 0.35rem 0 0 0; padding-left: 1.2rem; list-style-type: none;",
      tags$li(style = "margin-bottom: 0.25rem;", "Leising, D., Grenke, O., & Cramer, M. (2023). Visual Argument Structure Tool (VAST) Version 1.0. ",
        tags$em("Meta-Psychology"), ", 7. ",
        tags$a(href = "https://doi.org/10.15626/MP.2021.2911", target = "_blank", "doi:10.15626/MP.2021.2911")),
      tags$li(style = "margin-bottom: 0.25rem;", "Musfeld, P., Leising, D., Grenke, O., & Heene, M. (in preparation). ",
        "Indicators for the Assessment of Theoretical Rigor (v3.0). ",
        tags$a(href = "https://www.researchgate.net/publication/400424233_Indicators_for_the_Assessment_of_Theoretical_Rigor", target = "_blank", "Preprint")),
      tags$li(style = "margin-bottom: 0.25rem;", tags$strong("vastR"), " \u2014 R package for VAST model construction and Graphviz rendering. ",
        tags$a(href = "https://github.com/finn-luebber/vastR", target = "_blank", "github.com/finn-luebber/vastR")),
      tags$li(style = "margin-bottom: 0.25rem;", tags$strong("vastRai"), " \u2014 R package for AI-assisted translation between narrative theories and VAST models.",
              tags$a(href = "https://github.com/finn-luebber/vastRai", target = "_blank", "github.com/finn-luebber/vastRai"))
    )
  )
)


# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  state <- reactiveValues(
    model_a = NULL, model_b = NULL,
    model_a_label = "Model A", model_b_label = "Model B",
    model_a_spec = NULL, model_b_spec = NULL,
    active_slot = "A",
    translation_notes = NULL, rigor_assessment = NULL, source_text = NULL,
    r_code_a = NULL, r_code_b = NULL,
    undo_snapshot = NULL, r_code_slot = "A"
  )

  active_model <- reactive({
    if (state$active_slot == "A") state$model_a else state$model_b
  })

  set_active_model <- function(model) {
    if (state$active_slot == "A") state$model_a <- model
    else state$model_b <- model
  }

  save_undo <- function() {
    state$undo_snapshot <- list(
      model_a = state$model_a, model_b = state$model_b,
      model_a_label = state$model_a_label, model_b_label = state$model_b_label,
      model_a_spec = state$model_a_spec, model_b_spec = state$model_b_spec,
      translation_notes = state$translation_notes,
      rigor_assessment = state$rigor_assessment, source_text = state$source_text,
      r_code_a = state$r_code_a, r_code_b = state$r_code_b
    )
  }

  observeEvent(input$undo_btn, {
    snap <- state$undo_snapshot
    if (is.null(snap)) { show_toast("Nothing to undo"); return() }
    state$model_a <- snap$model_a; state$model_b <- snap$model_b
    state$model_a_label <- snap$model_a_label; state$model_b_label <- snap$model_b_label
    state$model_a_spec <- snap$model_a_spec; state$model_b_spec <- snap$model_b_spec
    state$translation_notes <- snap$translation_notes
    state$rigor_assessment <- snap$rigor_assessment; state$source_text <- snap$source_text
    state$r_code_a <- snap$r_code_a; state$r_code_b <- snap$r_code_b
    state$undo_snapshot <- NULL
    show_toast("Undo successful")
  })

  show_toast <- function(msg) {
    insertUI(selector = "#toast_container", where = "beforeEnd",
      ui = tags$div(class = "toast-msg", msg), immediate = TRUE)
  }

  # ---- About modal ----
  observeEvent(input$about_btn, {
    showModal(modalDialog(
      title = "About VAST Interactive Builder",
      tags$p("The VAST Interactive Builder is a visual interface for constructing, ",
        "comparing, and translating theories using the ",
        tags$strong("Visual Argument Structure Tool (VAST)"), " framework ",
        "(Leising, Grenke, & Cramer, 2023)."),
      tags$p("VAST diagrams make the structure of psychological (and other) theories explicit: ",
        "concepts, their names, the relationships between them, normative and descriptive claims, ",
        "and the degree of theoretical rigor."),
      tags$p("This app integrates three R packages:"),
      tags$ul(
        tags$li(tags$strong("vastR"), " \u2014 constructs VAST models and renders them as Graphviz diagrams."),
        tags$li(tags$strong("vastRai"), " \u2014 translates between narrative theory text and formal VAST structures using LLM-generated prompts. ",
          "It produces two alternative models from the same text; their structural divergence reveals exactly where the theory is underspecified."),
        tags$li(tags$strong("vastRapp"), " (this app) \u2014 provides the interactive interface.")
      ),
      tags$p("All LLM features work via a copy-paste prompt workflow \u2014 no API key or account is needed. ",
        "The app works entirely without an LLM for manual model construction."),
      easyClose = TRUE, size = "m",
      footer = modalButton("Close")
    ))
  })

  # ---- Cite modal ----
  observeEvent(input$cite_btn, {
    cite_text <- paste0(
      "Luebber, F. (2026). VAST Interactive Builder [Software]. ",
      "Built on vastR and vastRai. https://github.com/finn-luebber/vastR\n\n",
      "Leising, D., Grenke, O., & Cramer, M. (2023). Visual Argument Structure Tool ",
      "(VAST) Version 1.0. Meta-Psychology, 7. doi:10.15626/MP.2021.2911\n\n",
      "Musfeld, P., Leising, D., Grenke, O., & Heene, M. (in preparation). ",
      "Indicators for the Assessment of Theoretical Rigor (v3.0)."
    )
    showModal(modalDialog(
      title = "How to Cite",
      tags$p(style = "font-size: 0.85rem; color: #555;", "If you use the VAST Interactive Builder in your work, please cite:"),
      tags$pre(style = "font-size: 0.8rem; white-space: pre-wrap; background: #f8f9fa; padding: 0.75rem; border-radius: 6px;",
        cite_text),
      easyClose = TRUE, size = "m",
      footer = tagList(
        actionButton("copy_cite", "Copy to clipboard", class = "btn-outline-primary btn-sm"),
        modalButton("Close")
      )
    ))
    state$.cite_text <- cite_text
  })
  observeEvent(input$copy_cite, {
    session$sendCustomMessage("copyToClipboard", list(text = state$.cite_text))
    show_toast("Citation copied!")
  })

  # ---- Diagram rendering ----
  observe({
    model <- active_model()
    if (is.null(model)) {
      session$sendCustomMessage("renderDot", list(id = "diagram", dot = ""))
      return()
    }
    model$naming_mode <- input$naming_mode_sel %||% "separated"
    dot <- tryCatch(vast_to_dot(model), error = function(e) NULL)
    session$sendCustomMessage("renderDot", list(id = "diagram", dot = dot %||% ""))
  })

  # ---- Model pills ----
  output$model_pills_ui <- renderUI({
    a_empty <- is.null(state$model_a); b_empty <- is.null(state$model_b)
    a_class <- if (state$active_slot == "A") "model-pill active" else "model-pill inactive"
    b_class <- if (state$active_slot == "B") "model-pill active" else "model-pill inactive"
    if (a_empty) a_class <- paste(a_class, "empty")
    if (b_empty) b_class <- paste(b_class, "empty")
    tagList(
      tags$span(class = a_class, onclick = "Shiny.setInputValue('switch_to_a', Math.random());",
        tags$input(type = "text", class = "model-label-input", id = "label_a_input",
          value = state$model_a_label,
          style = if (state$active_slot == "A") "color: white;" else "color: #1976D2;",
          onchange = "Shiny.setInputValue('label_a_changed', this.value);"),
        if (a_empty) tags$span(style = "font-size: 0.75rem; opacity: 0.7;", " (empty)")
      ),
      tags$span(class = b_class, onclick = "Shiny.setInputValue('switch_to_b', Math.random());",
        tags$input(type = "text", class = "model-label-input", id = "label_b_input",
          value = state$model_b_label,
          style = if (state$active_slot == "B") "color: white;" else "color: #1976D2;",
          onchange = "Shiny.setInputValue('label_b_changed', this.value);"),
        if (b_empty) tags$span(style = "font-size: 0.75rem; opacity: 0.7;", " (empty)")
      )
    )
  })

  observeEvent(input$switch_to_a, { state$active_slot <- "A" })
  observeEvent(input$switch_to_b, { state$active_slot <- "B" })
  observeEvent(input$switch_slot_build, {
    state$active_slot <- if (state$active_slot == "A") "B" else "A"
  })
  observeEvent(input$label_a_changed, {
    if (nzchar(input$label_a_changed)) state$model_a_label <- input$label_a_changed
  })
  observeEvent(input$label_b_changed, {
    if (nzchar(input$label_b_changed)) state$model_b_label <- input$label_b_changed
  })

  output$model_stats_ui <- renderUI({
    model <- active_model()
    if (is.null(model)) return(NULL)
    n_nodes <- if (length(model$nodes) > 0) sum(vapply(model$nodes, function(n) n$type %in% c("concept", "data"), logical(1))) else 0L
    n_edges <- if (length(model$edges) > 0) sum(vapply(model$edges, function(e) !(e$type %in% c("n", "")), logical(1))) else 0L
    tags$span(paste0(n_nodes, " concepts \u00B7 ", n_edges, " edges"))
  })

  output$slot_indicator_ui <- renderUI({
    label <- if (state$active_slot == "A") state$model_a_label else state$model_b_label
    tags$span(paste0("Editing: ", label))
  })

  # ---- Clear model ----
  observeEvent(input$clear_model_btn, {
    if (is.null(active_model())) { show_toast("Model is already empty"); return() }
    label <- if (state$active_slot == "A") state$model_a_label else state$model_b_label
    showModal(modalDialog(title = "Clear model?",
      paste0("This will delete '", label, "'. This action can be undone."),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_clear_model", "Clear", class = "btn-danger"))
    ))
  })
  observeEvent(input$confirm_clear_model, {
    removeModal(); save_undo()
    if (state$active_slot == "A") { state$model_a <- NULL; state$model_a_spec <- NULL; state$r_code_a <- NULL }
    else { state$model_b <- NULL; state$model_b_spec <- NULL; state$r_code_b <- NULL }
    show_toast("Model cleared")
  })

  # ---- Load example models ----
  load_example <- function(example_key) {
    ex <- EXAMPLES[[example_key]]
    if (is.null(ex)) return()
    model <- active_model()
    if (!is.null(model)) {
      state$.pending_example <- example_key
      label <- if (state$active_slot == "A") state$model_a_label else state$model_b_label
      showModal(modalDialog(title = "Replace current model?",
        paste0("Loading the '", ex$label, "' example will replace '", label, "'. This action can be undone."),
        footer = tagList(modalButton("Cancel"), actionButton("confirm_load_example", "Replace", class = "btn-danger"))
      ))
    } else {
      do_load_example(example_key)
    }
  }
  do_load_example <- function(example_key) {
    ex <- EXAMPLES[[example_key]]
    if (is.null(ex)) return()
    save_undo()
    model <- ex$build()
    set_active_model(model)
    # Clear associated spec and code (user-built, not from JSON)
    if (state$active_slot == "A") { state$model_a_spec <- NULL; state$r_code_a <- NULL }
    else { state$model_b_spec <- NULL; state$r_code_b <- NULL }
    show_toast(paste0("Loaded: ", ex$label))
  }
  observeEvent(input$confirm_load_example, {
    removeModal()
    ex_key <- state$.pending_example
    state$.pending_example <- NULL
    if (!is.null(ex_key)) do_load_example(ex_key)
  })
  observeEvent(input$ex_sun, { load_example("sun") })
  observeEvent(input$ex_rel_types, { load_example("rel_types") })
  observeEvent(input$ex_advanced, { load_example("advanced") })
  observeEvent(input$ex_social_media, { load_example("social_media") })

  observeEvent(input$apply_settings, {
    save_undo()
    model <- active_model()
    if (is.null(model)) {
      model <- vast_model(title = input$model_title, analyst = input$model_analyst,
        rankdir = input$model_layout, naming_mode = input$naming_mode_sel %||% "separated")
    } else {
      model$title <- input$model_title; model$analyst <- input$model_analyst; model$rankdir <- input$model_layout
    }
    set_active_model(model)
  })

  observe({ updateTextInput(session, "new_concept_id", value = next_id(active_model(), "C")) })
  observe({ updateTextInput(session, "new_diamond_id", value = next_id(active_model(), "D")) })
  observe({ updateTextInput(session, "new_group_id", value = next_group_id(active_model())) })

  # ---- Update ALL selector dropdowns when model changes ----
  observe({
    model <- active_model()
    choices <- make_concept_choices(model)
    updateSelectInput(session, "new_edge_from", choices = choices)
    updateSelectInput(session, "new_edge_to", choices = choices)
    updateSelectInput(session, "new_noise_target", choices = choices)
    updateSelectInput(session, "sel_concept", choices = if (length(choices) > 0) choices else c("(none)" = ""))

    # Diamond selector
    dc <- character()
    if (!is.null(model)) for (nid in names(model$nodes)) {
      nd <- model$nodes[[nid]]
      if (nd$type == "diamond") dc <- c(dc, setNames(nid, paste0(nid, ": ", nd$label)))
    }
    updateSelectInput(session, "sel_diamond", choices = if (length(dc) > 0) dc else c("(none)" = ""))

    # IS/OUGHT selector
    ioc <- character()
    if (!is.null(model)) for (nid in names(model$nodes)) {
      nd <- model$nodes[[nid]]
      if (nd$type %in% c("is", "ought")) {
        tgt_name <- "?"
        for (e in model$edges) {
          if (identical(e$from, nid) && identical(e$type, "")) {
            tgt <- if (!is.null(e$lhead)) e$lhead else e$to
            tgt_name <- get_concept_name(model, tgt)
            if (tgt %in% names(model$groups)) tgt_name <- model$groups[[tgt]]$label
            break
          }
        }
        ioc <- c(ioc, setNames(nid, paste0(toupper(nd$type), " on \"", tgt_name, "\"")))
      }
    }
    updateSelectInput(session, "sel_is_ought", choices = if (length(ioc) > 0) ioc else c("(none)" = ""))

    # Group selector
    gc <- make_group_choices(model)
    updateSelectInput(session, "sel_group", choices = if (length(gc) > 0) gc else c("(none)" = ""))

    # Noise selector
    nc <- character()
    if (!is.null(model)) for (i in seq_along(model$edges)) {
      e <- model$edges[[i]]
      if (identical(e$type, "noise")) {
        s <- if (!is.null(e$strength)) paste0(" (", e$strength, ")") else ""
        nc <- c(nc, setNames(as.character(i), paste0("Noise \u2192 ", get_concept_name(model, e$to), s)))
      }
    }
    updateSelectInput(session, "sel_noise", choices = if (length(nc) > 0) nc else c("(none)" = ""))

    # Edge (relationship) selector
    ec <- character()
    if (!is.null(model)) for (i in seq_along(model$edges)) {
      e <- model$edges[[i]]
      if (!(e$type %in% c("n", "", "noise"))) {
        s <- if (!is.null(e$strength)) paste0(" (", e$strength, ")") else ""
        ec <- c(ec, setNames(as.character(i),
          paste0(get_concept_name(model, e$from), " \u2192[", edge_type_label(e$type), "]", s, "\u2192 ", get_concept_name(model, e$to))
        ))
      }
    }
    updateSelectInput(session, "sel_edge", choices = if (length(ec) > 0) ec else c("(none)" = ""))
  })

  observe({
    model <- active_model()
    if (!is.null(model)) {
      updateTextInput(session, "model_title", value = model$title)
      updateTextInput(session, "model_analyst", value = model$analyst)
      updateSelectInput(session, "model_layout", selected = model$rankdir)
      updateSelectInput(session, "naming_mode_sel", selected = model$naming_mode)
    } else {
      updateTextInput(session, "model_title", value = "")
      updateTextInput(session, "model_analyst", value = "")
      updateSelectInput(session, "model_layout", selected = "TB")
    }
  })

  ensure_model <- function() {
    model <- active_model()
    if (is.null(model)) model <- vast_model(title = input$model_title %||% "",
      analyst = input$model_analyst %||% "", rankdir = input$model_layout %||% "TB",
      naming_mode = input$naming_mode_sel %||% "separated")
    model
  }

  # ============================================================================
  # BUILD TAB: Add/Edit/Delete
  # ============================================================================

  observeEvent(input$add_concept, {
    id <- trimws(input$new_concept_id); name <- trimws(input$new_concept_name)
    ctype <- input$new_concept_type
    if (!nzchar(id) || !nzchar(name)) { show_toast("ID and Name are required"); return() }
    save_undo(); model <- ensure_model()
    if (id %in% names(model$nodes)) { show_toast(paste0("ID '", id, "' already exists")); return() }
    name_id <- paste0(id, "_name")
    if (ctype == "concept") {
      model <- model |> add_nodes(vast_concept(id, label = id), vast_name(name_id, label = name)) |> add_edges(vast_naming(id, name_id))
    } else {
      model <- model |> add_nodes(vast_data(id, label = name), vast_name(name_id, label = name)) |> add_edges(vast_naming(id, name_id))
    }
    set_active_model(model)
    updateTextInput(session, "new_concept_name", value = "")
  })

  observeEvent(input$del_concept_btn, {
    cid <- input$sel_concept; if (!nzchar(cid)) return()
    model <- active_model()
    if (is.null(model) || !(cid %in% names(model$nodes))) return()
    n <- count_connected_edges(model, cid)
    if (n > 0) {
      showModal(modalDialog(title = "Delete concept?",
        paste0("'", cid, "' has ", n, " connected edge(s). All will be removed."),
        footer = tagList(modalButton("Cancel"), actionButton("confirm_delete_concept", "Delete", class = "btn-danger"))
      ))
      state$.pending_delete_concept <- cid
    } else {
      save_undo(); set_active_model(remove_concept(model, cid)); show_toast(paste0("Removed ", cid))
    }
  })
  observeEvent(input$confirm_delete_concept, {
    removeModal(); cid <- state$.pending_delete_concept; if (is.null(cid)) return()
    model <- active_model(); if (is.null(model)) return()
    save_undo(); set_active_model(remove_concept(model, cid))
    state$.pending_delete_concept <- NULL; show_toast(paste0("Removed ", cid))
  })

  observeEvent(input$edit_concept_btn, {
    cid <- input$sel_concept; if (!nzchar(cid)) return()
    model <- active_model()
    if (is.null(model) || !(cid %in% names(model$nodes))) return()
    showModal(modalDialog(title = paste0("Edit concept: ", cid),
      textInput("edit_concept_name", "Name", value = get_concept_name(model, cid)),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_edit_concept", "Save", class = "btn-primary"))
    ))
    state$.editing_concept_id <- cid
  })
  observeEvent(input$confirm_edit_concept, {
    removeModal(); cid <- state$.editing_concept_id; nm <- trimws(input$edit_concept_name)
    if (is.null(cid) || !nzchar(nm)) return()
    model <- active_model(); if (is.null(model)) return()
    save_undo()
    for (e in model$edges) {
      if (identical(e$type, "n") && identical(e$from, cid)) {
        model$nodes[[e$to]]$label <- paste0("\u201C", nm, "\u201D"); break
      }
    }
    set_active_model(model); state$.editing_concept_id <- NULL; show_toast(paste0("Updated ", cid))
  })

  observeEvent(input$add_edge, {
    from <- input$new_edge_from; to <- input$new_edge_to; etype <- input$new_edge_type
    if (!nzchar(from) || !nzchar(to)) { show_toast("Select From and To nodes"); return() }
    save_undo(); model <- ensure_model()
    strength <- if (nzchar(input$new_edge_strength)) {
      val <- suppressWarnings(as.numeric(input$new_edge_strength))
      if (is.na(val)) input$new_edge_strength else val
    } else NULL
    lhead <- input$new_edge_lhead; ltail <- input$new_edge_ltail
    if (!is.null(lhead) && !nzchar(lhead)) lhead <- NULL
    if (!is.null(ltail) && !nzchar(ltail)) ltail <- NULL
    fn_name <- EDGE_TYPE_FNS[[etype]]; edge_fn <- getFromNamespace(fn_name, "vastR")
    args <- list(from = from, to = to)
    if (!is.null(strength)) args$strength <- strength
    if (!is.null(lhead)) args$lhead <- lhead
    if (!is.null(ltail)) args$ltail <- ltail
    model <- add_edges(model, do.call(edge_fn, args))
    set_active_model(model); updateTextInput(session, "new_edge_strength", value = "")
  })

  output$compound_edge_ui <- renderUI({
    model <- active_model()
    if (is.null(model) || length(model$groups) == 0) return(NULL)
    layout_columns(col_widths = c(6, 6),
      selectInput("new_edge_ltail", "Arrow starts at group",
        choices = c("(none)" = "", groups_containing_node(model, input$new_edge_from)), width = "100%"),
      selectInput("new_edge_lhead", "Arrow ends at group",
        choices = c("(none)" = "", groups_containing_node(model, input$new_edge_to)), width = "100%")
    )
  })

  observeEvent(input$del_edge_btn, {
    s <- input$sel_edge; if (!nzchar(s)) return()
    idx <- as.integer(s); model <- active_model()
    if (is.null(model) || idx < 1 || idx > length(model$edges)) return()
    save_undo(); model$edges[[idx]] <- NULL; set_active_model(model); show_toast("Relationship removed")
  })

  observeEvent(input$edit_edge_btn, {
    s <- input$sel_edge; if (!nzchar(s)) return()
    idx <- as.integer(s); model <- active_model()
    if (is.null(model) || idx < 1 || idx > length(model$edges)) return()
    e <- model$edges[[idx]]
    current_type <- EDGE_TYPE_CODES[[e$type]] %||% e$type
    showModal(modalDialog(
      title = paste0("Edit: ", get_concept_name(model, e$from), " \u2192 ", get_concept_name(model, e$to)),
      selectInput("edit_edge_type", "Type", choices = EDGE_TYPES, selected = current_type),
      textInput("edit_edge_strength", "Strength", value = if (!is.null(e$strength)) as.character(e$strength) else ""),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_edit_edge", "Save", class = "btn-primary"))
    ))
    state$.editing_edge_idx <- idx
  })
  observeEvent(input$confirm_edit_edge, {
    removeModal(); idx <- state$.editing_edge_idx; if (is.null(idx)) return()
    model <- active_model()
    if (is.null(model) || idx < 1 || idx > length(model$edges)) return()
    save_undo(); e <- model$edges[[idx]]
    strength <- if (nzchar(input$edit_edge_strength)) {
      val <- suppressWarnings(as.numeric(input$edit_edge_strength))
      if (is.na(val)) input$edit_edge_strength else val
    } else NULL
    fn_name <- EDGE_TYPE_FNS[[input$edit_edge_type]]
    edge_fn <- getFromNamespace(fn_name, "vastR")
    args <- list(from = e$from, to = e$to)
    if (!is.null(strength)) args$strength <- strength
    if (!is.null(e$lhead)) args$lhead <- e$lhead
    if (!is.null(e$ltail)) args$ltail <- e$ltail
    model$edges[[idx]] <- do.call(edge_fn, args)
    set_active_model(model); state$.editing_edge_idx <- NULL; show_toast("Relationship updated")
  })

  observeEvent(input$add_diamond, {
    id <- trimws(input$new_diamond_id)
    if (!nzchar(id)) { show_toast("ID required"); return() }
    save_undo(); model <- ensure_model()
    if (id %in% names(model$nodes)) { show_toast("ID already exists"); return() }
    content <- input$new_diamond_content
    if (content == "Custom") content <- input$new_diamond_custom %||% "AND"
    fn <- if (input$new_diamond_footnote) TRUE else NULL
    model <- model |> add_nodes(vast_diamond(id, label = content, footnote = fn))
    set_active_model(model)
  })

  observeEvent(input$del_diamond_btn, {
    did <- input$sel_diamond; if (!nzchar(did)) return()
    model <- active_model(); if (is.null(model)) return()
    save_undo(); set_active_model(remove_concept(model, did)); show_toast(paste0("Removed ", did))
  })

  observeEvent(input$edit_diamond_btn, {
    did <- input$sel_diamond; if (!nzchar(did)) return()
    model <- active_model()
    if (is.null(model) || !(did %in% names(model$nodes))) return()
    nd <- model$nodes[[did]]
    current_label <- nd$label
    current_footnote <- !is.null(nd$footnote)
    showModal(modalDialog(title = paste0("Edit diamond: ", did),
      selectInput("edit_diamond_content", "Content",
        choices = DIAMOND_OPTIONS,
        selected = if (current_label %in% c("AND", "OR", "XOR")) current_label else "Custom"),
      conditionalPanel(condition = "input.edit_diamond_content == 'Custom'",
        textInput("edit_diamond_custom", "Custom formula",
          value = if (!(current_label %in% c("AND", "OR", "XOR"))) current_label else "")
      ),
      checkboxInput("edit_diamond_footnote", "Footnote", value = current_footnote),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_edit_diamond", "Save", class = "btn-primary"))
    ))
    state$.editing_diamond_id <- did
  })
  observeEvent(input$confirm_edit_diamond, {
    removeModal(); did <- state$.editing_diamond_id; if (is.null(did)) return()
    model <- active_model(); if (is.null(model) || !(did %in% names(model$nodes))) return()
    save_undo()
    content <- input$edit_diamond_content
    if (content == "Custom") content <- input$edit_diamond_custom %||% "AND"
    model$nodes[[did]]$label <- content
    model$nodes[[did]]$footnote <- if (input$edit_diamond_footnote) TRUE else NULL
    set_active_model(model); state$.editing_diamond_id <- NULL; show_toast(paste0("Updated ", did))
  })

  output$io_applies_to_ui <- renderUI({
    model <- active_model()
    selectInput("new_io_target", "Applies to",
      choices = c(make_concept_choices(model), make_group_choices(model)), width = "100%")
  })

  observeEvent(input$add_is_ought, {
    target <- input$new_io_target; io_type <- input$new_io_type
    if (!nzchar(target)) { show_toast("Select a target"); return() }
    save_undo(); model <- ensure_model()
    value <- if (nzchar(input$new_io_value)) {
      val <- suppressWarnings(as.numeric(input$new_io_value))
      if (is.na(val)) input$new_io_value else val
    } else NULL
    io_id <- paste0(toupper(io_type), "_", target)
    while (io_id %in% names(model$nodes)) io_id <- paste0(io_id, "_")
    io_node <- if (io_type == "is") vast_is(io_id, value = value) else vast_ought(io_id, value = value)
    is_group <- target %in% names(model$groups)
    if (is_group) {
      members <- model$groups[[target]]$node_ids
      edge <- if (length(members) > 0) vast_relation(io_id, members[1], type = "", lhead = target)
              else vast_relation(io_id, target, type = "")
    } else {
      edge <- vast_relation(io_id, target, type = "")
    }
    model <- model |> add_nodes(io_node) |> add_edges(edge)
    set_active_model(model); updateTextInput(session, "new_io_value", value = "")
  })

  observeEvent(input$del_io_btn, {
    io_id <- input$sel_is_ought; if (!nzchar(io_id)) return()
    model <- active_model(); if (is.null(model)) return()
    save_undo()
    persp_ids <- c()
    for (e in model$edges) {
      if (identical(e$to, io_id) && identical(e$type, "")) {
        pn <- model$nodes[[e$from]]
        if (!is.null(pn) && pn$type == "perspective") persp_ids <- c(persp_ids, e$from)
      }
    }
    ids <- c(io_id, persp_ids)
    model$edges <- Filter(function(e) !(e$from %in% ids || e$to %in% ids), model$edges)
    for (rid in ids) model$nodes[[rid]] <- NULL
    set_active_model(model); show_toast("Statement removed")
  })

  observeEvent(input$edit_io_btn, {
    io_id <- input$sel_is_ought; if (!nzchar(io_id)) return()
    model <- active_model()
    if (is.null(model) || !(io_id %in% names(model$nodes))) return()
    nd <- model$nodes[[io_id]]
    current_value <- nd$value
    showModal(modalDialog(title = paste0("Edit ", toupper(nd$type), " statement: ", io_id),
      textInput("edit_io_value", "Value", value = if (!is.null(current_value)) as.character(current_value) else ""),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_edit_io", "Save", class = "btn-primary"))
    ))
    state$.editing_io_id <- io_id
  })
  observeEvent(input$confirm_edit_io, {
    removeModal(); io_id <- state$.editing_io_id; if (is.null(io_id)) return()
    model <- active_model(); if (is.null(model) || !(io_id %in% names(model$nodes))) return()
    save_undo()
    nd <- model$nodes[[io_id]]
    new_value <- if (nzchar(input$edit_io_value)) {
      val <- suppressWarnings(as.numeric(input$edit_io_value))
      if (is.na(val)) input$edit_io_value else val
    } else NULL
    nd$value <- new_value
    # Rebuild label
    io_type_label <- toupper(nd$type)
    nd$label <- if (!is.null(new_value)) paste0(io_type_label, "\n", new_value) else io_type_label
    model$nodes[[io_id]] <- nd
    set_active_model(model); state$.editing_io_id <- NULL; show_toast(paste0("Updated ", io_id))
  })

  perspective_target <- reactiveVal(NULL)
  observeEvent(input$add_persp_btn, {
    io_id <- input$sel_is_ought; if (nzchar(io_id)) perspective_target(io_id)
  })

  output$perspective_form_ui <- renderUI({
    target <- perspective_target(); if (is.null(target)) return(NULL)
    div(style = "margin-top: 0.5rem; padding: 0.5rem; background: #f0f7f0; border-radius: 6px;",
      tags$strong(paste0("Add perspective for ", target), style = "font-size: 0.85rem;"),
      div(class = "compact-input",
        layout_columns(col_widths = c(6, 6),
          textInput("persp_holder", "Holder name", value = "", width = "100%"),
          textInput("persp_agreement", "Agreement (0-1)", value = "", width = "100%")
        ),
        div(style = "display: flex; gap: 0.25rem;",
          actionButton("confirm_add_persp", "Add", class = "btn-success btn-sm"),
          actionButton("cancel_add_persp", "Cancel", class = "btn-outline-secondary btn-sm")
        )
      )
    )
  })
  observeEvent(input$cancel_add_persp, { perspective_target(NULL) })
  observeEvent(input$confirm_add_persp, {
    target <- perspective_target(); holder <- trimws(input$persp_holder)
    if (!nzchar(holder)) { show_toast("Holder name required"); return() }
    save_undo(); model <- active_model()
    agreement <- if (nzchar(input$persp_agreement)) suppressWarnings(as.numeric(input$persp_agreement)) else NULL
    if (!is.null(agreement) && is.na(agreement)) agreement <- NULL
    persp_id <- paste0("P_", gsub("[^A-Za-z0-9]", "_", holder), "_", target)
    while (persp_id %in% names(model$nodes)) persp_id <- paste0(persp_id, "_")
    model <- model |> add_nodes(vast_perspective(persp_id, holder = holder, value = agreement)) |>
      add_edges(vast_relation(persp_id, target, type = ""))
    set_active_model(model); perspective_target(NULL)
    updateTextInput(session, "persp_holder", value = ""); updateTextInput(session, "persp_agreement", value = "")
  })

  output$group_members_ui <- renderUI({
    choices <- make_concept_choices(active_model())
    if (length(choices) == 0) return(NULL)
    checkboxGroupInput("new_group_members", "Members", choices = choices, inline = TRUE)
  })
  output$group_parent_ui <- renderUI({
    selectInput("new_group_parent", "Parent group",
      choices = c("(none)" = "", make_group_choices(active_model())), width = "100%")
  })

  observeEvent(input$add_group, {
    id <- trimws(input$new_group_id); label <- trimws(input$new_group_label)
    if (!nzchar(id) || !nzchar(label)) { show_toast("ID and Label required"); return() }
    save_undo(); model <- ensure_model()
    members <- input$new_group_members %||% character(0)
    parent <- input$new_group_parent
    grp <- vast_group(id, label = label, node_ids = members)
    model <- model |> add_groups(grp)
    if (!is.null(parent) && nzchar(parent)) {
      model$groups[[parent]]$child_group_ids <- c(model$groups[[parent]]$child_group_ids, id)
    }
    set_active_model(model)
    updateTextInput(session, "new_group_label", value = "")
  })

  observeEvent(input$del_group_btn, {
    gid <- input$sel_group; if (!nzchar(gid)) return()
    model <- active_model(); if (is.null(model)) return()
    save_undo()
    for (pid in names(model$groups)) {
      model$groups[[pid]]$child_group_ids <- setdiff(model$groups[[pid]]$child_group_ids, gid)
    }
    model$groups[[gid]] <- NULL; set_active_model(model); show_toast(paste0("Removed group ", gid))
  })

  observeEvent(input$edit_group_btn, {
    gid <- input$sel_group; if (!nzchar(gid)) return()
    model <- active_model()
    if (is.null(model) || !(gid %in% names(model$groups))) return()
    grp <- model$groups[[gid]]
    all_concepts <- make_concept_choices(model)
    showModal(modalDialog(title = paste0("Edit group: ", gid),
      textInput("edit_group_label", "Label", value = grp$label),
      checkboxGroupInput("edit_group_members", "Members", choices = all_concepts,
        selected = grp$node_ids, inline = TRUE),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_edit_group", "Save", class = "btn-primary"))
    ))
    state$.editing_group_id <- gid
  })
  observeEvent(input$confirm_edit_group, {
    removeModal(); gid <- state$.editing_group_id; if (is.null(gid)) return()
    model <- active_model(); if (is.null(model) || !(gid %in% names(model$groups))) return()
    save_undo()
    new_label <- trimws(input$edit_group_label)
    if (nzchar(new_label)) model$groups[[gid]]$label <- new_label
    model$groups[[gid]]$node_ids <- input$edit_group_members %||% character(0)
    set_active_model(model); state$.editing_group_id <- NULL; show_toast(paste0("Updated group ", gid))
  })

  observeEvent(input$add_noise, {
    target <- input$new_noise_target; if (!nzchar(target)) { show_toast("Select a target"); return() }
    save_undo(); model <- ensure_model()
    ns_id <- paste0("ns_", target); i <- 1
    while (ns_id %in% names(model$nodes)) { ns_id <- paste0("ns_", target, "_", i); i <- i + 1 }
    strength <- if (nzchar(input$new_noise_strength)) {
      val <- suppressWarnings(as.numeric(input$new_noise_strength))
      if (is.na(val)) input$new_noise_strength else val
    } else NULL
    model <- model |> add_nodes(vast_noise_source(ns_id)) |> add_edges(vast_noise(ns_id, target, strength = strength))
    set_active_model(model); updateTextInput(session, "new_noise_strength", value = "")
  })

  observeEvent(input$del_noise_btn, {
    s <- input$sel_noise; if (!nzchar(s)) return()
    idx <- as.integer(s); model <- active_model()
    if (is.null(model) || idx < 1 || idx > length(model$edges)) return()
    save_undo()
    noise_edge <- model$edges[[idx]]
    if (!is.null(noise_edge)) model$nodes[[noise_edge$from]] <- NULL
    model$edges[[idx]] <- NULL; set_active_model(model); show_toast("Noise removed")
  })

  # ============================================================================
  # NARRATIVE -> VAST
  # ============================================================================

  observeEvent(input$gen_n2v_prompt, {
    theory <- input$theory_text
    if (!nzchar(theory)) { show_toast("Enter theory text first"); return() }
    ctx <- if (nzchar(input$theory_context)) input$theory_context else NULL
    prompt <- tryCatch(narrative_to_prompt(theory, context = ctx, copy_to_clipboard = FALSE),
      error = function(e) { show_toast(paste("Error:", e$message)); NULL })
    if (!is.null(prompt)) {
      output$n2v_prompt_ui <- renderUI({
        tagList(
          div(style = "max-height: 300px; overflow-y: auto; margin-top: 0.5rem;",
            tags$pre(style = "font-size: 0.78rem; white-space: pre-wrap;", prompt)),
          actionButton("copy_n2v_prompt", "Copy to clipboard", class = "btn-outline-primary btn-sm mt-1")
        )
      })
    }
  })
  observeEvent(input$copy_n2v_prompt, {
    ctx <- if (nzchar(input$theory_context)) input$theory_context else NULL
    prompt <- narrative_to_prompt(input$theory_text, context = ctx, copy_to_clipboard = FALSE)
    session$sendCustomMessage("copyToClipboard", list(text = prompt)); show_toast("Copied!")
  })

  observeEvent(input$load_json, {
    json_text <- input$llm_json
    if (!nzchar(json_text)) { show_toast("Paste JSON output first"); return() }
    if (!is.null(state$model_a) || !is.null(state$model_b)) {
      showModal(modalDialog(title = "Replace existing models?",
        paste0("Loading will replace both ", state$model_a_label, " and ", state$model_b_label, "."),
        footer = tagList(modalButton("Cancel"), actionButton("confirm_load_json", "Replace", class = "btn-danger"))
      ))
    } else do_load_json(json_text)
  })
  observeEvent(input$confirm_load_json, { removeModal(); do_load_json(input$llm_json) })

  do_load_json <- function(json_text) {
    save_undo()
    result <- tryCatch(vast_from_json(json_text),
      error = function(e) { show_toast(paste("JSON Error:", e$message)); NULL })
    if (is.null(result)) return()
    state$model_a <- result$closest_match; state$model_b <- result$distant_compatible
    state$model_a_label <- "Closest match"; state$model_b_label <- "Distant compatible"
    state$model_a_spec <- result$.parsed_specs$closest_match
    state$model_b_spec <- result$.parsed_specs$distant_compatible
    state$translation_notes <- result$translation_notes; state$rigor_assessment <- result$rigor_assessment
    state$source_text <- result$source_text
    state$r_code_a <- result$r_code$closest_match; state$r_code_b <- result$r_code$distant_compatible
    state$active_slot <- "A"; show_toast("Models loaded successfully!")
  }

  output$rigor_display_ui <- renderUI({
    ra <- state$rigor_assessment; if (is.null(ra)) return(NULL)
    make_card <- function(label, item) {
      if (is.null(item)) return(NULL)
      rating <- item$rating %||% "unspecified"
      color <- RIGOR_COLORS[[rating]] %||% "#999"; icon <- RIGOR_ICONS[[rating]] %||% "?"
      div(class = "rigor-card", style = sprintf("background: %s;", color),
        tags$span(paste0(label, " ", icon)),
        tags$div(style = "font-size: 0.75rem; font-weight: normal; margin-top: 0.25rem; display: none;",
          class = "rigor-detail", item$justification %||% ""),
        onclick = "$(this).find('.rigor-detail').slideToggle(200);")
    }
    div(style = "margin-top: 0.75rem;",
      tags$strong("Rigor Assessment", style = "font-size: 0.9rem;"),
      div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 0.35rem; margin-top: 0.35rem;",
        make_card("T1 Phenomenon", ra$T1_phenomenon),
        make_card("T2 Definitions", ra$T2_concept_definitions),
        make_card("T3 Mechanism", ra$T3_mechanism),
        make_card("T4 Scope", ra$T4_scope)
      ),
      if (!is.null(ra$overall_assessment))
        tags$p(style = "font-size: 0.82rem; margin-top: 0.5rem; color: #555;",
          tags$strong("Overall: "), ra$overall_assessment)
    )
  })

  # ============================================================================
  # VAST -> NARRATIVE
  # ============================================================================

  output$v2n_model_info <- renderUI({
    model <- active_model()
    label <- if (state$active_slot == "A") state$model_a_label else state$model_b_label
    if (is.null(model)) tags$p(class = "text-muted", "No model to generate from. Build or load a model first.")
    else tagList(tags$p("Generating prompt for: ", tags$strong(label), tags$br(),
      tags$small(class = "text-muted", "Switch models using the toggle in the diagram panel.")))
  })

  observeEvent(input$gen_v2n_prompt, {
    model <- active_model(); if (is.null(model)) { show_toast("No model available"); return() }
    ctx <- if (nzchar(input$v2n_context)) input$v2n_context else NULL
    prompt <- tryCatch(vast_to_prompt(model, context = ctx, assess_fidelity = input$v2n_fidelity, copy_to_clipboard = FALSE),
      error = function(e) { show_toast(paste("Error:", e$message)); NULL })
    if (!is.null(prompt)) {
      output$v2n_prompt_ui <- renderUI({
        tagList(
          div(style = "max-height: 300px; overflow-y: auto; margin-top: 0.5rem;",
            tags$pre(style = "font-size: 0.78rem; white-space: pre-wrap;", prompt)),
          actionButton("copy_v2n_prompt", "Copy to clipboard", class = "btn-outline-primary btn-sm mt-1"))
      })
    }
  })
  observeEvent(input$copy_v2n_prompt, {
    model <- active_model(); ctx <- if (nzchar(input$v2n_context)) input$v2n_context else NULL
    prompt <- vast_to_prompt(model, context = ctx, assess_fidelity = input$v2n_fidelity, copy_to_clipboard = FALSE)
    session$sendCustomMessage("copyToClipboard", list(text = prompt)); show_toast("Copied!")
  })
  observeEvent(input$goto_v2n, { nav_select("workspace_tabs", selected = "v2n", session = session) })

  # ============================================================================
  # TABS: Translation Notes, Model Comparison, R Code
  # ============================================================================

  output$translation_notes_ui <- renderUI({
    tn <- state$translation_notes
    if (is.null(tn)) return(tags$p(class = "text-muted", style = "font-size: 0.85rem;",
      "No translation notes available. Load an LLM response in the Narrative \u2192 VAST tab."))
    parts <- list()
    if (length(tn$auxiliary_assumptions) > 0) {
      parts <- c(parts, list(tags$h6("Auxiliary Assumptions")))
      for (aa in tn$auxiliary_assumptions) {
        parts <- c(parts, list(div(
          style = "background: #f8f9fa; border-radius: 6px; padding: 0.5rem; margin-bottom: 0.35rem; font-size: 0.82rem;",
          tags$strong(aa$id %||% ""), ": ", aa$description %||% "",
          if (!is.null(aa$reason)) tags$div(style = "color: #555;", tags$em("Reason: "), aa$reason),
          if (!is.null(aa$text_evidence)) tags$div(style = "color: #555;", tags$em("Evidence: "), aa$text_evidence)
        )))
      }
    }
    if (length(tn$underspecification_points) > 0) {
      parts <- c(parts, list(tags$h6("Underspecification Points")))
      for (up in tn$underspecification_points) {
        parts <- c(parts, list(div(
          style = "background: #fff8e1; border-radius: 6px; padding: 0.5rem; margin-bottom: 0.35rem; font-size: 0.82rem;",
          tags$strong(up$id %||% ""), ": ", up$description %||% "",
          if (!is.null(up$impact)) tags$div(style = "color: #555;", tags$em("Impact: "), up$impact)
        )))
      }
    }
    if (!is.null(tn$model_divergence_summary) && nzchar(tn$model_divergence_summary)) {
      parts <- c(parts, list(tags$h6("Model Divergence"),
        tags$p(style = "font-size: 0.82rem;", tn$model_divergence_summary)))
    }
    tagList(parts)
  })

  output$model_comparison_ui <- renderUI({
    ma <- state$model_a; mb <- state$model_b
    if (is.null(ma) || is.null(mb)) return(tags$p(class = "text-muted", style = "font-size: 0.85rem;",
      "Load or build a second model to compare."))
    spec_a <- state$model_a_spec %||% vastRai:::serialize_vast_model(ma)
    spec_b <- state$model_b_spec %||% vastRai:::serialize_vast_model(mb)
    diff_result <- tryCatch(vastRai:::diff_models(spec_a, spec_b, state$model_a_label, state$model_b_label), error = function(e) NULL)
    if (is.null(diff_result)) return(tags$p("Could not compute model comparison."))
    parts <- list()
    if (length(diff_result$only_a_nodes) > 0 || length(diff_result$only_b_nodes) > 0) {
      parts <- c(parts, list(tags$h6("Node Differences")))
      if (length(diff_result$only_a_nodes) > 0)
        parts <- c(parts, list(tags$p(style = "font-size: 0.82rem;",
          tags$strong(paste0("Only in ", diff_result$name_a, ": ")), paste(diff_result$only_a_nodes, collapse = ", "))))
      if (length(diff_result$only_b_nodes) > 0)
        parts <- c(parts, list(tags$p(style = "font-size: 0.82rem;",
          tags$strong(paste0("Only in ", diff_result$name_b, ": ")), paste(diff_result$only_b_nodes, collapse = ", "))))
    }
    shared <- diff_result$shared_edges
    if (nrow(shared) > 0) {
      identical_edges <- shared[!shared$differs, , drop = FALSE]
      differing_edges <- shared[shared$differs, , drop = FALSE]
      if (nrow(identical_edges) > 0) {
        parts <- c(parts, list(tags$h6("Shared Relationships (identical)"),
          tags$table(class = "table table-sm diff-table",
            tags$thead(tags$tr(tags$th("From"), tags$th("To"), tags$th("Type"), tags$th("Strength"))),
            tags$tbody(lapply(seq_len(nrow(identical_edges)), function(i) {
              r <- identical_edges[i, ]
              tags$tr(tags$td(r$from_name), tags$td(r$to_name), tags$td(r$type_a), tags$td(r$strength_a))
            })))))
      }
      if (nrow(differing_edges) > 0) {
        parts <- c(parts, list(tags$h6("Shared Relationships (differ)"),
          tags$table(class = "table table-sm diff-table",
            tags$thead(tags$tr(tags$th("From"), tags$th("To"),
              tags$th(paste0("Type (", diff_result$name_a, ")")), tags$th(paste0("Str. (", diff_result$name_a, ")")),
              tags$th(paste0("Type (", diff_result$name_b, ")")), tags$th(paste0("Str. (", diff_result$name_b, ")")))),
            tags$tbody(lapply(seq_len(nrow(differing_edges)), function(i) {
              r <- differing_edges[i, ]
              td <- r$type_a != r$type_b; sd <- r$strength_a != r$strength_b
              tags$tr(tags$td(r$from_name), tags$td(r$to_name),
                tags$td(class = if (td) "differs" else "", r$type_a),
                tags$td(class = if (sd) "differs" else "", r$strength_a),
                tags$td(class = if (td) "differs" else "", r$type_b),
                tags$td(class = if (sd) "differs" else "", r$strength_b))
            })))))
      }
    }
    make_edge_table <- function(df, title) {
      if (nrow(df) == 0) return(NULL)
      list(tags$h6(title), tags$table(class = "table table-sm diff-table",
        tags$thead(tags$tr(tags$th("From"), tags$th("To"), tags$th("Type"), tags$th("Strength"))),
        tags$tbody(lapply(seq_len(nrow(df)), function(i) {
          r <- df[i, ]; tags$tr(tags$td(r$from_name), tags$td(r$to_name), tags$td(r$type), tags$td(r$strength))
        }))))
    }
    parts <- c(parts, make_edge_table(diff_result$only_a, paste0("Only in ", diff_result$name_a)))
    parts <- c(parts, make_edge_table(diff_result$only_b, paste0("Only in ", diff_result$name_b)))
    tagList(parts)
  })

  output$r_code_toggle_ui <- renderUI({
    radioButtons("r_code_slot_sel", NULL,
      choices = setNames(c("A", "B"), c(state$model_a_label, state$model_b_label)),
      selected = state$r_code_slot, inline = TRUE)
  })
  observeEvent(input$r_code_slot_sel, { state$r_code_slot <- input$r_code_slot_sel })

  output$r_code_display <- renderText({
    slot <- state$r_code_slot
    code <- if (slot == "A") state$r_code_a else state$r_code_b
    if (is.null(code)) {
      model <- if (slot == "A") state$model_a else state$model_b
      if (is.null(model)) return("No model in this slot.")
      spec <- tryCatch(vastRai:::serialize_vast_model(model), error = function(e) NULL)
      if (is.null(spec)) return("Could not serialize model.")
      built <- tryCatch(vastRai:::build_vast_model(spec), error = function(e) NULL)
      if (is.null(built)) return("Could not generate code.")
      return(built$code)
    }
    code
  })

  observeEvent(input$copy_r_code, {
    slot <- state$r_code_slot
    code <- if (slot == "A") state$r_code_a else state$r_code_b
    if (is.null(code)) {
      model <- if (slot == "A") state$model_a else state$model_b
      if (is.null(model)) { show_toast("No code to copy"); return() }
      spec <- vastRai:::serialize_vast_model(model)
      built <- vastRai:::build_vast_model(spec); code <- built$code
    }
    session$sendCustomMessage("copyToClipboard", list(text = code)); show_toast("Code copied!")
  })

  # ============================================================================
  # EXPORT
  # ============================================================================

  observeEvent(input$export_png, {
    label <- if (state$active_slot == "A") state$model_a_label else state$model_b_label
    session$sendCustomMessage("downloadPNG", list(id = "diagram",
      filename = paste0(vastRai:::sanitize_filename(label), ".png"), width = 1600, height = 1000))
  })
  observeEvent(input$export_svg, {
    label <- if (state$active_slot == "A") state$model_a_label else state$model_b_label
    session$sendCustomMessage("downloadSVG", list(id = "diagram",
      filename = paste0(vastRai:::sanitize_filename(label), ".svg")))
  })

  output$save_btn <- downloadHandler(
    filename = function() {
      paste0(vastRai:::sanitize_filename(state$model_a$title %||% state$model_b$title %||% "vast_session"), ".json")
    },
    content = function(file) {
      spec_a <- if (!is.null(state$model_a)) tryCatch(vastRai:::serialize_vast_model(state$model_a), error = function(e) NULL) else NULL
      spec_b <- if (!is.null(state$model_b)) tryCatch(vastRai:::serialize_vast_model(state$model_b), error = function(e) NULL) else NULL
      save_data <- list(version = "1.0", model_a = spec_a, model_a_label = state$model_a_label,
        model_b = spec_b, model_b_label = state$model_b_label,
        translation_notes = state$translation_notes, rigor_assessment = state$rigor_assessment, source_text = state$source_text)
      writeLines(jsonlite::toJSON(save_data, auto_unbox = TRUE, pretty = TRUE, null = "null"), file)
    }
  )

  observeEvent(input$load_file_content, {
    req(input$load_file_content)
    parsed <- tryCatch(jsonlite::fromJSON(input$load_file_content, simplifyVector = FALSE),
      error = function(e) { show_toast("Invalid JSON file"); NULL })
    if (is.null(parsed)) return()
    if (!is.null(state$model_a) || !is.null(state$model_b)) {
      showModal(modalDialog(title = "Replace existing data?",
        "Loading will replace all current data. Any unsaved changes will be lost.",
        footer = tagList(modalButton("Cancel"), actionButton("confirm_load_file", "Replace", class = "btn-danger"))))
      state$.pending_load <- input$load_file_content
    } else do_load_file(input$load_file_content)
  })
  observeEvent(input$confirm_load_file, {
    removeModal(); do_load_file(state$.pending_load); state$.pending_load <- NULL
  })

  do_load_file <- function(json_text) {
    save_undo(); parsed <- jsonlite::fromJSON(json_text, simplifyVector = FALSE)
    if ("models" %in% names(parsed)) {
      result <- tryCatch(vast_from_json(json_text), error = function(e) { show_toast(paste("Error:", e$message)); NULL })
      if (is.null(result)) return()
      state$model_a <- result$closest_match; state$model_b <- result$distant_compatible
      state$model_a_label <- "Closest match"; state$model_b_label <- "Distant compatible"
      state$model_a_spec <- result$.parsed_specs$closest_match; state$model_b_spec <- result$.parsed_specs$distant_compatible
      state$translation_notes <- result$translation_notes; state$rigor_assessment <- result$rigor_assessment
      state$source_text <- result$source_text
      state$r_code_a <- result$r_code$closest_match; state$r_code_b <- result$r_code$distant_compatible
    } else if ("model_a" %in% names(parsed)) {
      if (!is.null(parsed$model_a)) {
        built <- tryCatch(vastRai:::build_vast_model(parsed$model_a), error = function(e) NULL)
        state$model_a <- if (!is.null(built)) built$model else NULL
        state$r_code_a <- if (!is.null(built)) built$code else NULL; state$model_a_spec <- parsed$model_a
      } else { state$model_a <- NULL; state$r_code_a <- NULL; state$model_a_spec <- NULL }
      if (!is.null(parsed$model_b)) {
        built <- tryCatch(vastRai:::build_vast_model(parsed$model_b), error = function(e) NULL)
        state$model_b <- if (!is.null(built)) built$model else NULL
        state$r_code_b <- if (!is.null(built)) built$code else NULL; state$model_b_spec <- parsed$model_b
      } else { state$model_b <- NULL; state$r_code_b <- NULL; state$model_b_spec <- NULL }
      state$model_a_label <- parsed$model_a_label %||% "Model A"; state$model_b_label <- parsed$model_b_label %||% "Model B"
      state$translation_notes <- parsed$translation_notes; state$rigor_assessment <- parsed$rigor_assessment
      state$source_text <- parsed$source_text
    } else { show_toast("Unknown file format"); return() }
    state$active_slot <- "A"; show_toast("File loaded successfully!")
  }

  output$export_report <- downloadHandler(
    filename = function() {
      paste0(vastRai:::sanitize_filename(state$model_a$title %||% state$model_b$title %||% "vast_report"), "_report.txt")
    },
    content = function(file) {
      lines <- c("VAST Analysis Report", paste(rep("=", 50), collapse = ""), "")
      if (!is.null(state$source_text) && nzchar(state$source_text))
        lines <- c(lines, "SOURCE TEXT", paste(rep("-", 30), collapse = ""), state$source_text, "")
      ra <- state$rigor_assessment
      if (!is.null(ra)) {
        lines <- c(lines, "RIGOR ASSESSMENT", paste(rep("-", 30), collapse = ""))
        for (item_name in c("T1_phenomenon", "T2_concept_definitions", "T3_mechanism", "T4_scope")) {
          item <- ra[[item_name]]
          if (!is.null(item)) lines <- c(lines, paste0(item_name, ": ", item$rating %||% "n/a"),
            paste0("  ", item$justification %||% ""))
        }
        if (!is.null(ra$overall_assessment)) lines <- c(lines, "", "Overall:", ra$overall_assessment)
        lines <- c(lines, "")
      }
      ma <- state$model_a; mb <- state$model_b
      if (!is.null(ma) && !is.null(mb)) {
        sa <- state$model_a_spec %||% vastRai:::serialize_vast_model(ma)
        sb <- state$model_b_spec %||% vastRai:::serialize_vast_model(mb)
        dr <- tryCatch(vastRai:::diff_models(sa, sb, state$model_a_label, state$model_b_label), error = function(e) NULL)
        if (!is.null(dr)) {
          dt <- tryCatch(vastRai:::format_diff_text(dr), error = function(e) NULL)
          if (!is.null(dt)) lines <- c(lines, "MODEL COMPARISON", paste(rep("-", 30), collapse = ""), dt, "")
        }
      }
      tn <- state$translation_notes
      if (!is.null(tn)) {
        lines <- c(lines, "TRANSLATION NOTES", paste(rep("-", 30), collapse = ""))
        if (length(tn$auxiliary_assumptions) > 0) {
          lines <- c(lines, "Auxiliary Assumptions:")
          for (aa in tn$auxiliary_assumptions) lines <- c(lines, paste0("  ", aa$id %||% "", ": ", aa$description %||% ""))
        }
        if (length(tn$underspecification_points) > 0) {
          lines <- c(lines, "", "Underspecification Points:")
          for (up in tn$underspecification_points) lines <- c(lines, paste0("  ", up$id %||% "", ": ", up$description %||% ""))
        }
        if (!is.null(tn$model_divergence_summary)) lines <- c(lines, "", "Model Divergence:", tn$model_divergence_summary)
        lines <- c(lines, "")
      }
      for (slot in c("A", "B")) {
        code <- if (slot == "A") state$r_code_a else state$r_code_b
        label <- if (slot == "A") state$model_a_label else state$model_b_label
        model <- if (slot == "A") state$model_a else state$model_b
        if (is.null(code) && !is.null(model)) {
          spec <- tryCatch(vastRai:::serialize_vast_model(model), error = function(e) NULL)
          if (!is.null(spec)) { built <- tryCatch(vastRai:::build_vast_model(spec), error = function(e) NULL); if (!is.null(built)) code <- built$code }
        }
        if (!is.null(code)) lines <- c(lines, paste0("R CODE: ", label), paste(rep("-", 30), collapse = ""), code, "")
      }
      writeLines(lines, file)
    }
  )

  observeEvent(input$clipboard_success, { })
  observeEvent(input$clipboard_fail, { show_toast("Clipboard access denied") })
}

shinyApp(ui = ui, server = server)
