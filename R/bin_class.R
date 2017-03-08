#' @include performance_class.R transform_class.R generic_methods.R

setClassUnion("NumericOrFactor", members = c("numeric", "factor"))

## bin class ##
Bin <- setRefClass("Bin",
  fields = c(
    x = "NumericOrFactor",
    name = "character",
    perf = "Performance",
    tf = "Transform", ## current transform
    history = "list", ## current + all previous transforms
    args = "list"
    ),
  contains = "VIRTUAL")

Bin$methods(initialize = function(name="Unknown", x, perf, ...) {
  ## perform bin checks here
  stopifnot(length(x) > 0)
  callSuper(name=name, x=x, perf=perf, ...)
  stopifnot(length(x) == length(perf$y))
})

Bin$methods(update = function(...) {

  result <- perf$update(b = .self)

  ## do it in the result!
  result <- lapply(result, function(v) {
    v[!is.finite(v)] <- 0
    v
  })

  tf <<- update_transform(tf, result)

  ## append to the history and the cache
  history <<- c(history, list(tf))

  # show()
})

Bin$methods(bin = function(...) {
  .self$perf$bin(b=.self, ...)
  args <<- modifyList(args, list(...))
  update()
})

Bin$methods(collapse = function(...) {
  update()
})

Bin$methods(expand = function(...) {
  update()
})

Bin$methods(factorize = function(newdata=.self$x, transform=.self$tf, ..., n) {
  val_nas <- is.na(newdata)
  val_exc <- newdata %in% as.numeric(names(transform@exceptions))
  val_nrm <- !(val_nas | val_exc)
  list(normal = val_nrm, exception = val_exc, missing = val_nas)
})

Bin$methods(as.matrix = function(transform=.self$tf, ...) {
  if (length(transform@repr) == 0) {
    stop("`bin` function not called yet.", call. = FALSE)
  }

  out <- round(do.call(rbind, transform@repr), 3)

  i <- match(transform@neutralized, row.names(out), 0)

  ## the ones that are no longer present get dropped
  tf@neutralized <<- tf@neutralized[i != 0]

  out[i, "Pred"] <- 0
  out
})

Bin$methods(show = function(...) {

  cat(.self$name, sep="\n")
  print(.self$as.matrix())

})


Bin$methods(undo = function(...) {
  if (length(history) > 0) {
    tf <<- history[[length(history)]]
    history <<- head(history, -1)
  }
  show()
})


Bin$methods(reset = function(...) {
  do.call(perf$bin, c(list(b=.self), args))
  tf@neutralized <<- character(0)
  update()
})


### move the definition to the transform class so it can do the unwrapping
Bin$methods(set_equal = function(v1, v2, ...) {
  print("Implement this in the Transform Class")
})


Bin$methods(set_cutpoints = function(cuts, ...) {
  cuts <- sort(unique(c(-Inf, cuts, Inf)))
  tf@tf <<- cuts
  update()
})


Bin$methods(neutralize = function(i, ...) {
  tf <<- neutralize_(tf, i)
  show()
})


Bin$methods(mono = function(m, ...) {
  args$mono <<- m
  do.call(perf$bin, c(list(b=.self), args))
  update()
})

Bin$methods(exceptions = function(e, ...) {
  args$exceptions <<- e
  tf@exceptions <<- setNames(rep(0, length(e)), e)
  update()
})

Bin$methods(plot = function(...) {
  perf$plot(b = .self)
})

Bin$methods(predict = function(newdata=.self$x, transform=.self$tf, ...) {
  idx <- as.character(.self$factorize(newdata=newdata, transform=transform, ...)$factor)
  out <- c(transform@subst, transform@nas, transform@exceptions)[idx]
  out[names(out) %in% transform@neutralized] <- 0
  unname(out)
})

## remove this later. just for testing purposes
Bin$methods(save_to_disk = function(f, ...) {
  saveRDS(.self, f)
})

Bin$methods(sort_value = function(...) {
  perf$sort_value(b=.self)
})

## summary should be tied to the performance
Bin$methods(summary = function(tf=.self$tf, ...) {
  perf$summary(tf = tf)
})
