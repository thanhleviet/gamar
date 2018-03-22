# definition of the 'plan' class:

# the help plan generic --------------------------------------------------------
#' @export
plan <- function(x, ...) {
  UseMethod("plan")
}



# constructor for the data frame method ----------------------------------------
#' @importFrom tools file_path_as_absolute
new_plan_df <- function(parameters, seeds, variables, nbsteps, model) {
  stopifnot(is.data.frame(parameters), all(sapply(parameters, is.numeric)))
  stopifnot(is.numeric(seeds))
  stopifnot(is.vector(variables), is.numeric(variables), !is.null(names(variables)))
  stopifnot(is.numeric(nbsteps), length(nbsteps) == 1)
  stopifnot(is.character(model), length(model) == 1)
  structure(list(parameters = parameters,
                 variables  = as_integer(variables),
                 nbsteps    = as_integer(nbsteps),
                 model      = file_path_as_absolute(model),
                 seeds      = seeds), class = "plan")
}



# common validator -------------------------------------------------------------
validate_plan <- function(x) {
  if (any(is.na(x$parameters))) stop("missing parameters values not allowed")
  if (any(x$variables < 1)) stop("the periodicities of monitoring should be strictly positive integers")
  if (any(x$nbsteps < 1)) stop("the duration of simulations should be a strictly positive integer")
  if (!file.exists(x$model)) stop("model file does not exist", call. = FALSE)
  x
}



# validator for the data frame method ------------------------------------------
validate_plan_df <- function(x) {
  if (any(is.na(x$seeds))) stop("missing seeds values not allowed")
  validate_plan(x)
}



# helper method for data frames ------------------------------------------------
plan.data.frame <- function(parameters, seeds, variables, nbsteps, model) {
  validate_plan_df(new_plan_df(parameters, seeds, variables, nbsteps, model))
}



# constructor for plan method --------------------------------------------------
new_plan_pl <- function(...) {
  arguments <- list(...)
  stopifnot(all(sapply(arguments, class) %in% "plan"))
  stopifnot(all(sapply(arguments, function(x) all(names(x) %in% c("parameters", "variables", "nbsteps", "model", "seeds")))))
  stopifnot(all(apply(sapply(arguments, function(x) names(x$parameters)), 1, function(x) length(unique(x))) == 1))
  stopifnot(all(apply(sapply(arguments, function(x) names(x$variables)), 1, function(x) length(unique(x))) == 1))
  stopifnot(length(unique(sapply(arguments, function(x) x$nbsteps))) == 1)
  stopifnot(length(unique(sapply(arguments, function(x) x$model))) == 1)
  argument1 <- arguments[[1]]
  plan.data.frame(do.call(rbind, lapply(arguments, function(x) x$parameters)),
                  unique(na.exclude(unlist(lapply(arguments, function(x) x$seed)))),
                  as_integer(argument1$variables),
                  as_integer(argument1$nbsteps),
                  file_path_as_absolute(argument1$model))
}



# validator for plan method ----------------------------------------------------
validate_plan_pl <- function(x) {
  if(length(x$seeds) < 1) stop("provide at least 1 non missing seed value")
  validate_plan(x)
}



# helper method for plans ------------------------------------------------------
plan.plan <- function(...) {
  validate_plan_pl(new_plan_pl(...))
}



# print method -----------------------------------------------------------------
#' @importFrom mcutils ovv
#' @export
print.plan <- function(x) {
  cat("\nA plan of experiments of the process model defined in:\n")
  cat(x$model, "\n")
  nb_set <- nrow(x$parameters)
  multiple_sets <- nb_set > 1
  if (multiple_sets)
    cat("\nParameters values of each of the", nb_set, "sets of experiments of the plan:\n")
  else cat("\nParameters values of each of the single set of experiments of the plan:\n")
  ovv(x$parameters, 3, 4, 2)
  cat("\n\nPeriods (in number of steps) at which variables are monitored:\n")
  print(x$variables) # 'cat' would remove the names of the vector
  cat("\nNumber of steps in the simulations:", x$nbsteps, "\n")
  seeds <- x$seeds
  nb <- length(seeds)
  if (nb > 1) {
    if (multiple_sets) cat("\nEach set of experiments corresponds to", nb,
                           "simulations performed with the following seed values:\n")
    else cat("\nThis set of experiments corresponds to",  nb,
             "simulations performed with the following seed values:\n")
    if (nb > 6) cat(paste(head(seeds, 3), collapse = ", "), ", ......, ",
                    paste(tail(seeds, 3), collapse = ", "), sep = "")
    else cat(paste0(seeds, collapse = ", "))
  } else {
    if (multiple_sets)
      cat("\nEach 'set of experiments' here actually corresponds to only 1 simulation performed with the seed", seeds)
    else cat("\nThis 'set of experiments' here actually corresponds to only 1 simulation performed with the seed", seeds)
  }
  cat("\n\n")
  invisible(x)
}


