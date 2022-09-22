gg2themes <- list(
  "Standard ggplot2 theme" = ggplot2::theme_grey(),
  "Black/white theme" = ggplot2::theme_bw(),
  "Light theme" = ggplot2::theme_light(),
  "Dark theme" = ggplot2::theme_dark(),
  "Minimal theme" = ggplot2::theme_minimal(),
  "Classic/R base theme" = ggplot2::theme_classic()
)

utils::globalVariables(".")
utils::globalVariables("where")

#' Get path to carrrotter example
#'
#' carrrotter comes bundled with a few correct bAXIL csv files inside the `inst/extdata`
#' directory. This function takes the name of a file or multiple files and returns the path.
#'
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' carrrotter_example_files()
#' carrrotter_example_files(c("MI18-L8-A_10kV_raw.csv", "MI18-L8-B_10kV_raw.csv"))
carrrotter_example_files <- function (file = NULL)
{
  if (is.null(file)) {
    dir(system.file("extdata", package = "carrrotter"))
  }
  else {
    system.file("extdata", file, package = "carrrotter", mustWork = TRUE)
  }
}
