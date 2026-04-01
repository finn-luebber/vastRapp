#' Launch the VAST Interactive Builder
#'
#' Starts the interactive Shiny application for building, comparing, and
#' translating VAST models.
#'
#' @param ... Additional arguments passed to [shiny::runApp()], such as
#'   `port`, `host`, or `launch.browser`.
#'
#' @return This function does not return; it runs the Shiny app until the
#'   session is ended.
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#'
#' @export
run_app <- function(...) {
  app_dir <- system.file("app", package = "vastRapp")
  if (app_dir == "") {
    stop("Could not find the app directory. Try reinstalling `vastRapp`.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
