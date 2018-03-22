# NOTE: depending of the values of its arguments, the function examples will
# call show_examples or copy_examples. copy_examples is a vectorized version of
# the copy_example function. show_examples calls make_white_space.



# make_white_space -------------------------------------------------------------
# Used by show_examples

#' Create colon with preceding white space of variable length
#'
#' Creates the pattern " : " where the white space before ":" is of length
#' length + 1.
#'
#' @param length the length of the white space before the colon.
#'
#' @noRd
#'
#' @keywords internal
#'
make_white_space <- function(length) {
  paste0(paste(rep(" ", length), collapse = ""), " : ")
}



# show_examples ----------------------------------------------------------------
# Used by examples and using make_white_space.

#' Show examples
#'
#' Formatted outpout of a 2-variables data frame, using the make_white_space
#' function.
#'
#' @param examples_file a 2-variables data frame.
#'
#' @noRd
#'
#' @keywords internal
#'
show_examples <- function(examples_files) {
  nc <- nchar(examples_files[, 1])
  ws <- sapply(max(nc) - nc, make_white_space)
  examples_files2 <- cbind(examples_files[, 1], ws, examples_files[, 2])
  cat(paste(apply(examples_files2, 1, paste, collapse = ""), collapse = "\n"))
  invisible(examples_files)
}



# copy_example -----------------------------------------------------------------
# Used by copy_examples.

#' Copy one built-in model
#'
#' Copy one built-in model to a given location
#'
#' @keywords internal
#'
#' @noRd
#'
copy_example <- function(what, to, examples_files) {
  examples_files <- subset(examples_files, model == what)
  what <- examples_files$copy
  is_dir <- examples_files$dir
  wd <- getwd()
# destination file / directory:
  missing_to <- is.na(to)
  if (missing_to) to <- wd else if(!grepl("^/|^./", to)) to <- paste0("./", to)
  to <- paste0(to, "/", what)
# optionally renaming the destination file / directory:
  if (file.exists(to)) {
    if (is_dir) ext <- NULL  # if "to" is a directory
    else {                   # if "to" is a file
      ext <- ".gaml"
      to <- gsub("\\.gaml$", "", to)
    }
    i <- 1; while (file.exists(paste0(to, i, ext))) i <- i + 1
    to <- paste0(to, i, ext)
    mssg <- gsub(wd, ".", to)
  } else {
    if (missing_to) mssg <- "working directory"
    else mssg <- gsub(wd, ".", to) # relative path
  }
  from <- system.file("examples", package = "gamar")
  if (is_dir) {
    dir.create(to, recursive = TRUE) # if it's a directory, it necessarily does not exist
    out <- file.copy(paste0(from, "/", what), to, recursive = TRUE)
  } else {
    dn <- dirname(to)
    if (!file.exists(dn)) dir.create(dn) # creates the directory if inexistant
    out <- file.copy(paste0(from, "/", what), to)
  }
  stopifnot(out)
  dir_file <- ifelse(examples_files$dir, "Directory ", "File ")
  message(paste0(dir_file, "'", what, "' copied to ", mssg, "."))
  invisible(out)
}



# copy_examples ----------------------------------------------------------------
# Used by examples and using copy_example.

#' A vectorized version of copy_examples
#'
#' Copies multiple built-in examples to multiple locations
#'
#' @keywords internal
#'
#' @noRd
#'
copy_examples <- function(what, to, examples_files) {
  lw <- length(what)
  lt <- length(to)
  if (lw < lt) what <- rep_len(what, lt)
  else if (lt < lw) to <- rep_len(to, lw)
  for(i in seq_along(what)) copy_example(what[i], to[i], examples_files)
}



# examples ---------------------------------------------------------------------

#' Manage built-in models
#'
#' This function can be used with 0, 1 or 2 arguments values. With no argument
#' values it lists the built-in models available in the system library. With 1
#' or 2 argument values it will copy (a) built-in model(s) to (a) specified
#' location(s).
#'
#' @param what character string, name of the built-in model to copy, as defined
#'             when calling the function without arguments.
#'
#' @param to character string, path of the location to copy to.
#'
#' @export
examples <- function(what, to) {
  if (missing(what) & missing(to)) show_examples(gamar:::examples_files)
  else {
    if (missing(to)) to <- NA
    copy_examples(what, to, gamar:::examples_files)
  }
}



