# vastRapp

An interactive Shiny application for building, comparing, and translating [VAST](https://doi.org/10.15626/MP.2021.2911) (Visual Argument Structure Tool) models.

## What it does

The VAST Interactive Builder lets you:

- **Build VAST models visually** — add concepts, relationships, groups, IS/OUGHT statements, and noise through a form-based interface. Diagrams render live via viz.js.
- **Translate narrative → VAST** — paste a theory paragraph, generate a structured prompt for any LLM (Claude, ChatGPT, etc.), then load the LLM's JSON response to get two VAST models: a closest-match and a structurally distant alternative. The divergence between them reveals underspecification in the theory.
- **Translate VAST → narrative** — generate a prompt that asks an LLM to write a theory paragraph from your model.
- **Assess theoretical rigor** — view T1–T4 ratings (phenomenon, definitions, mechanism, scope) from the LLM analysis.
- **Compare models** — structural diff showing shared, differing, and unique elements across two models.
- **Export** — PNG, SVG, text reports, R code, and full session saves as JSON.

No LLM API key required. The app uses a prompt-bridge approach: it generates prompts you copy into any LLM, then parses the JSON response.

## Part of the VAST ecosystem

| Package | Role | Repository |
|---------|------|------------|
| [**vastR**](https://github.com/finn-luebber/vastR) | Core model construction and Graphviz rendering | `finn-luebber/vastR` |
| [**vastRai**](https://github.com/finn-luebber/vastRai) | AI bridge layer (prompts, JSON parsing, rigor assessment) | `finn-luebber/vastRai` |
| **vastRapp** (this package) | Interactive Shiny UI | `finn-luebber/vastRapp` |

## Installation

```r
# install.packages("remotes")
remotes::install_github("finn-luebber/vastRapp")
```

This automatically installs vastR and vastRai from GitHub as dependencies.

## Usage

```r
library(vastRapp)
run_app()
```

You can pass arguments through to `shiny::runApp()`:

```r
run_app(port = 3838, launch.browser = TRUE)
```

## App overview

The app has a two-panel layout:

- **Left panel** — three tabs: *Build model* (manual construction), *Narrative → VAST* (LLM-assisted translation), *VAST → Narrative* (reverse direction)
- **Right panel** — live diagram with model toggle (A/B), export buttons, and collapsible sections for translation notes, model comparison, and R code

### Quick start: narrative translation

1. Go to the **Narrative → VAST** tab
2. Paste a theory paragraph and click **Generate prompt**
3. Copy the prompt into Claude, ChatGPT, or any capable LLM
4. Paste the LLM's JSON response back and click **Load models**
5. Two models appear — switch between them with the pills above the diagram
6. Explore translation notes, rigor assessment, and structural comparison in the bottom panels

### Quick start: manual building

1. Go to the **Build model** tab
2. Set a title and analyst name under *Model settings*
3. Add concepts (each automatically gets a name node and naming edge)
4. Add relationships between concepts
5. Add groups, IS/OUGHT statements, diamonds, and noise as needed
6. Export via the buttons above the diagram

## Reference

Leising, D., Grenke, O., & Cramer, M. (2023). Visual Argument Structure Tool (VAST) Version 1.0. *Meta-Psychology*, 7. [doi:10.15626/MP.2021.2911](https://doi.org/10.15626/MP.2021.2911)
Musfeld, P., Leising, D., Grenke, O., & Heene, M. (in preparation). Indicators for the Assessment of Theoretical Rigor (v3.0).(https://www.researchgate.net/publication/400424233_Indicators_for_the_Assessment_of_Theoretical_Rigor)

## License

MIT
