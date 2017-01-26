#' @include performance_class.R transform_class.R

setClassUnion("NumericOrFactor", members = c("numeric", "factor"))

## bin class ##
Bin <- setRefClass("Bin",
  fields = c(
    x = "NumericOrFactor",
    name = "character",
    perf = "Performance",
    tf = "Transform",
    history = "list",
    cache = "list"
    ),
  contains = "VIRTUAL")

Bin$methods(initialize = function(name="Unknown", x, perf, ...) {
  ## perform bin checks here
  stopifnot(length(x) > 0)
  callSuper(name=name, x=x, perf=perf, ...)
  stopifnot(length(x) == length(perf$y))
})

setMethod(
  "update_",
  signature = c(.self="Bin"),
  function(.self, ...) {
    callGeneric(.self$perf, b = .self)
  })

Bin$methods(update = function(...) {
  result <- update_(.self)

  tf@subst <<- result$normal[,"Pred"]
  tf@nas <<- c(Missing=result$missing[,"Pred"])
  tf@exceptions$output <<- result$exception[,"Pred"]
  names(tf@exceptions$output) <<- tf@exceptions$input

  ## append to the history and the cache
  history <<- c(history, list(tf))
  cache <<- c(cache, list(result))
  show()
})

Bin$methods(bin = function(...) {
  update()
})

Bin$methods(collapse = function(...) {
  update()
})

Bin$methods(expand = function(...) {
  update()
})


Bin$methods(select = function(n) {
  n <- max(min(n, length(history)), 1)
  history <<- c(history, list(history[[n]]))
  cache <<- c(cache, list(cache[[n]]))
  tf <<- history[[n]]
})

Bin$methods(factorize = function(..., n) {
  if (!missing(n)) {
    select(n)
  }

  val_nas <- is.na(x)
  val_exc <- x %in% tf@exceptions$input
  val_nrm <- !(val_nas | val_exc)
  list(normal = val_nrm, exception = val_exc, missing = val_nas)
})

Bin$methods(show = function(...) {
  if (length(cache) == 0) bin()

  round(do.call(rbind, cache[[length(cache)]]), 3)
  #print(cache[[length(cache)]])
})

Bin$methods(undo = function(...) {
  if (length(history) <= 1) {
    return()
  } else {
    tf <<- history[[length(history)]]
    cache <<- head(cache, -1)
    history <<- head(history, -1)
  }
})

Bin$methods(reset = function(...) {
  print("Implement the reset function")
})

Bin$methods(subst = function(..., n) {
  idx <- as.character(factorize(n=n)$factor)
  c(tf@subst, tf@nas, tf@exceptions$output)[idx]
})
