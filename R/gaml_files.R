# NOTE: these 2 functions manipulate gaml files. The first one, get_gaml, lists
# all the gaml model files from a file hierarchy. the second one, get_experiment,
# lists all the experiments of a gaml model file.



# list_gaml --------------------------------------------------------------------

#' List gaml files
#'
#' List the gaml files in a file hierarchy.
#'
#' @param x path to the top of a file hierarchy.
#'
#' @export
#'
list_gaml <- function(x) {
  if (missing(x)) x <- "."
  gaml <- grep(".gaml$", dir(x, recursive = TRUE), value = TRUE)
  paste0(x, "/", gaml)
}



# list_experiment --------------------------------------------------------------

#' List a model's experiments
#'
#' List the experiments of a given model.
#'
#' @param x path to a gaml model file.
#'
#' @importFrom readtext readtext
#'
#' @export
#'
list_experiment <- function(x) {
  gaml <- readtext(x, verbosity = FALSE)
  gaml <- strsplit(gaml$text, "\n")[[1]]  # because strsplit returns a list
  gaml <- gaml[grepl("^ *experiment", gaml)]
  gaml <- sapply(gaml, function(x) strsplit(gsub("  *", " ", x), " "))
  sel <- unname(sapply(gaml, function(x) which(x == "experiment")) + 1)
  unname(unlist(Map(`[`, gaml, sel)))
}


