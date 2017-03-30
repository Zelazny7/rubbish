#' @include performance_class.R transform_class.R

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

    if (nrow(v) > 0) {

      ## find witch levels are in the overrides
      i <- intersect(names(tf@overrides), row.names(v))
      v[i, "Pred"] <- tf@overrides[i]

    }
    v
  })

  tf <<- update_transform(tf, result)

  ## append to the history and the cache
  history <<- c(history, list(tf))
})

Bin$methods(bin = function(...) {
  "Call performance bin method and pass in .self"
  perf$bin(b=.self, ...)
  args <<- modifyList(args, list(...))
  update()
})

Bin$methods(collapse = function(...) {
  "Updated Bin after inherited collapse"
  update()
})

Bin$methods(expand = function(...) {
  "Updated Bin after inherited expand"
  update()
})

Bin$methods(factorize = function(newdata=.self$x, transform=.self$tf, ..., n) {
  "Return list of filters for exceptions, missing, and normal values"
  val_nas <- is.na(newdata)
  val_exc <- newdata %in% as.numeric(names(transform@exceptions))
  val_nrm <- !(val_nas | val_exc)
  list(normal = val_nrm, exception = val_exc, missing = val_nas)
})

Bin$methods(as.matrix = function(tf=.self$tf, ...) {
  "Matrix representation of Bin data"
  if (length(tf@repr) == 0) {
    stop("`bin` function not called yet.", call. = FALSE)
  }

  round(do.call(rbind, tf@repr), 3)
})

Bin$methods(show = function(...) {
  "String representation of Bin object"

  m <- .self$as.matrix()

  ## add row labels
  lbls <- sprintf("[%02d]  ", seq.int(nrow(m)))
  lbls[length(lbls)] <- ""

  row.names(m) <- paste0(lbls, row.names(m))

  cat(.self$name, sep="\n")
  print(m)

})


Bin$methods(undo = function(...) {
  "Undo the last operation that updated the history list. Pops the "
  if (length(history) > 1) {
    tf <<- history[[length(history) - 1]]
    history <<- head(history, -1)
  } else {
    tf <<- history[[1]]
    history <<- list()
  }
  show()
})


Bin$methods(reset = function(...) {
  do.call(perf$bin, c(list(b=.self), args))
  tf@overrides <<- numeric(0)
  update()
})


Bin$methods(set_equal = function(v1, v2, ...) {
  tf <<- set_equal_(tf, v1, v2)
  update()
})


Bin$methods(set_cutpoints = function(cuts, ...) {
  cuts <- sort(unique(c(-Inf, cuts, Inf)))
  tf@tf <<- cuts
  update()
})


Bin$methods(neutralize = function(i, ...) {
  tf <<- neutralize_(tf, i)
  update()
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
  idx <- as.character(factorize(newdata=newdata, transform=transform, ...)$factor)
  out <- c(transform@subst, transform@nas, transform@exceptions)[idx]

  i <- intersect(names(out), names(transform@overrides))
  out[i] <- transform@overrides[i]

  unname(out)
})

Bin$methods(sort_value = function(...) {
  perf$sort_value(b=.self)
})

## summary should be tied to the performance
Bin$methods(summary = function(tf=.self$tf, ...) {
  perf$summary(tf = tf)
})


Bin$methods(sas = function(coef=1, ...) {

})
