# NOTE:

# ------------------------------------------------------------------------------

#' @importFrom stats setNames
#'
#' @keywords internal
#'
#' @noRd
#'
get_parameters <- function(x) {
  setNames(data.frame(t(as.numeric(sapply(x$Parameters, `[`, "value")))),
           sapply(x$Parameters, `[`, "name"))
}



# ------------------------------------------------------------------------------

#' @importFrom stats setNames
#'
#' @keywords internal
#'
#' @noRd
#'
get_variables <- function(x) {
  setNames(as.integer(sapply(x$Outputs, `[`, "framerate")),
           sapply(x$Outputs, `[`, "name"))
}



# ------------------------------------------------------------------------------

#' @keywords internal
#'
#' @noRd
#'
get_steps <- function(x) {
  as.integer(x$.attrs["finalStep"])
}



# ------------------------------------------------------------------------------

#' @keywords internal
#'
#' @noRd
#'
get_seed <- function(x) {
  as.integer(x$.attrs["seed"])
}



# ------------------------------------------------------------------------------

#' @keywords internal
#'
#' @noRd
#'
get_model <- function(x) {
  unname(x$.attrs["sourcePath"])
}



# ------------------------------------------------------------------------------
#' Loads An Experiment
#'
#' Loads an experiment from a model specified in a gaml file and returns an
#' object of class \code{plan}.
#'
#' @param exprmnt a character string giving the name of the experiment to load
#' @param model a character string giving the name of the file from which to
#'              load the experiment.
#'
#' @importFrom XML xmlToList xmlParse
#' @export
load_experiment <- function(exprmnt, model) {
  message("Loading experiment '", exprmnt,
          "' from file '", basename(model), "'...")
  tmp <- tempfile(fileext = ".xml")
  system(paste0("java -jar ", getOption("gamar.startjar"), # creates the workspace directory
                " -Xms", getOption("gamar.Xms"),
                " -Xmx", getOption("gamar.Xmx"),
                " -Djava.awt.headless=true org.eclipse.core.launcher.Main",
                " -application msi.gama.headless.id4 -xml ",
                exprmnt, " ", model, " ", tmp, " > /dev/null"),
         ignore.stdout = TRUE, ignore.stderr = TRUE)
  unlink("workspace", TRUE, TRUE) # removing the "workspace" directory that is created by the command above
  out <- xmlToList(xmlParse(tmp))$Simulation
  do.call(plan, lapply(list(get_parameters,
                            get_seed,
                            get_variables,
                            get_steps,
                            get_model),
                       function(f) f(out)))
}

