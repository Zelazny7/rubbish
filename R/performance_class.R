#' @include generic_methods.R bin_class.R

Performance <- setRefClass("Performance", fields = c(
  y = "numeric",
  w = "numeric"))

Performance$methods(initialize = function(y=numeric(0), ..., w=rep(1, length(y))) {
  callSuper(y=y, w=w, ...)
  stopifnot(!any(is.na(y)))
  stopifnot(length(y) == length(w))
})

## split up a perf variable based on a segment variable
Performance$methods(split = function(seg, ...) {
  s <- base::split(data.frame(y, w), seg, drop = TRUE)
  lapply(s, function(x) do.call(getRefClass()$new, x))
})

# Performance$methods(copy = function(...) {
#   Performance$new(y=y, w=w)
# })
