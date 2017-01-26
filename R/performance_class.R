#' @include generic_methods.R bin_class.R

Performance <- setRefClass("Performance", fields = c(
  y = "numeric",
  w = "numeric"),
  contains = "VIRTUAL")

Performance$methods(initialize = function(y, ..., w=rep(1, length(y))) {
  callSuper(y=y, w=w, ...)
  stopifnot(!any(is.na(y)))
  stopifnot(length(y) == length(w))
})

