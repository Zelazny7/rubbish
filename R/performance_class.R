#' @include bin_class.R

setGeneric("bin_", def = function(.self, b, ...) callGeneric("bin_"))

Performance <- setRefClass("Performance", fields = c(
  y = "numeric",
  w = "numeric"))

Performance$methods(initialize = function(y=numeric(0), ..., w=rep(1, length(y))) {
  callSuper(y=y, w=w, ...)
  stopifnot(!any(is.na(y)))
  stopifnot(length(y) == length(w))
})
