#' @include performance_class.R transform_class.R

setClassUnion("NumericOrFactor", members = c("numeric", "factor"))

## bin class ##
Bin <- setRefClass("Bin",
  fields = c(
    x = "NumericOrFactor",
    name = "character",
    perf = "Performance",
    tf = "Transform",
    history = "list"
    # cache = "list" # Need to add cache-ing ability
    ),
  contains = "VIRTUAL")

Bin$methods(initialize = function(name="Unknown", x, perf, ...) {
  ## perform bin checks here
  stopifnot(length(x) > 0)
  callSuper(name=name, x=x, perf=perf, ...)
  stopifnot(length(x) == length(perf$y))
})



Bin$methods(update = update_)

setMethod(
  "update_",
  signature = c(.self="Bin"),
  function(.self, ...) {
    callGeneric(.self$perf, b = .self)
  })

Bin$methods(bin = function(...) {
  history <<- c(history, list(tf))
  update()
})

Bin$methods(collapse = function(...) {
  history <<- c(history, list(tf))
  update()
})

Bin$methods(expand = function(...) {
  history <<- c(history, list(tf))
  update()
})

Bin$methods(factorize = function(...) {
  val_nas <- is.na(x)
  val_exc <- x %in% tf@exceptions$input
  val_nrm <- !(val_nas | val_exc)
  list(normal = val_nrm, missing = val_nas, exception = val_exc)
})
