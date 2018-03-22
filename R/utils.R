# ------------------------------------------------------------------------------
as_integer <- function(x) {
  if (is.integer(x)) return(x)
  else if (any(x > as.integer(x))) # this depends on the machine precision
    warning("variables coerced to integers", call. = FALSE)
  as.integer(x)
}
