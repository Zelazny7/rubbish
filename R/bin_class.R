setClassUnion("NULLnumeric", c("NULL", "numeric"))

setGeneric("update_", def = function(.self, ..., b) callGeneric("update_"))

## create a transform object that applies the transoformation to a variable
setClass("Transform", slots = c(
  tf = "ANY",
  value = "numeric",
  subst = "numeric",
  nas = "numeric",
  exceptions = "list")
)

setMethod("initialize", "Transform", function(.Object, ...) {
  .Object@exceptions = list(input=numeric(), output=numeric())
  validObject(.Object)
  .Object
})

Performance <- setRefClass("Performance", fields = c(
  y = "numeric",
  w = "numeric"),
  contains = "VIRTUAL")

Performance$methods(initialize = function(y, ..., w=rep(1, length(y))) {
  callSuper(y=y, w=w, ...)
  stopifnot(!any(is.na(y)))
  stopifnot(length(y) == length(w))
})

Binary_Performance <- setRefClass("Binary_Performance", fields = c(
  ones = "numeric",
  zeros = "numeric"),
  contains = "Performance")

Binary_Performance$methods(initialize =  function(y, ...) {
  callSuper(y=y, ...)
  ones <<- sum((y == 1) * w)
  zeros <<- sum((y == 0) * w)
})

Binary_Performance$methods(summarize = function(x, f) {
  ## TODO: Implement
})


## exceptions is a list of two vectors each with the same number of elements

## bin class ##
Bin <- setRefClass("Bin",
  fields = c(
    name = "character",
    perf = "Performance",
    tf = "Transform",
    history = "list"),
  contains = "VIRTUAL")

Bin$methods(initialize = function(name="Unknown", x, perf, ...) {
  ## perform bin checks here
  stopifnot(length(x) > 0)
  callSuper(name=name, x=x, perf=perf, ...)
  stopifnot(length(x) == length(perf$y))
})

Bin$methods(bin = function(...) {
  history <<- c(history, list(tf))
})

Bin$methods(collapse = function(...) {
  history <<- c(history, list(tf))
})

Bin$methods(expand = function(...) {
  history <<- c(history, list(tf))
})

Bin$methods(factorize = function(...) {
  print("In factorize super?")
  l <- c("MISSING","EXCEPTION","NORMAL")

  factor(ifelse(is.na(x), "MISSING", ifelse(x %in% tf@exceptions$input,
    "EXCEPTION", "NORMAL")), levels = l)
  ## turn x into a factor based on the transform
})

Bin$methods(update = update_)

Continuous <- setRefClass("Continuous",
  fields = c(x="numeric"),
  contains = "Bin"
)

Discrete <- setRefClass("Discrete",
  fields = c(x="factor"),
  contains = "Bin"
)



fmt_numeric_cuts <- function(cuts) {
  l = format(round(cuts, 2), trim=TRUE, nsmall=2, digits=2, big.mark=",",
    scientific = FALSE)

  fmt = sprintf("(%%%1$ds - %%%1$ds]", max(nchar(l))) ## get width of largest value

  sprintf(fmt, head(l,-1), tail(l, -1))
}


Discrete$methods(factorize = function(...) {
  f <- callSuper(...)

  out <- x
  levels(out) <- unlist(tf@tf)[levels(out)]
  out <- addNA(out)
  levels(out)[is.na(levels(out))] <- "Missing"

  out
})

Continuous$methods(factorize = function(...) {
  f <- callSuper(...)

  lbls <- fmt_numeric_cuts(tf@tf)
  out <- factor(x, exclude=NULL, levels=c(lbls, tf@exceptions$input, NA))

  levels(out)[is.na(levels(out))] <- "Missing"
  out[f == "NORMAL"] <- cut(x[f == "NORMAL"], tf@tf, include.lowest = T,
    labels = lbls)

  out
})


setMethod("update_", signature = c(.self="Binary_Performance"),
  function(.self, ...) {

    print("Binary Performance update!")

  })

setMethod("update_", signature = c(.self="Binary_Performance", b="Bin"),
  function(.self, b) {
    print("Binary Performance update! with Bin info!")

    ## TODO: return an object that summarizes the bin and the binary performance


    ## discretize the x var based on the bin type and the transform
    f <- b$factorize()

    ## can now split x and y and w and calculate

    ## exceptions
    exceptions <- .self$summarize(x = x, f = (f == "EXCEPTIONS"))

    ## Calculate values for NAs, Exceptions, and Normal Values


  })

setMethod("update_", signature = c(.self="Bin"),
  function(.self, ...) {
    #browser()
    callGeneric(.self$perf, b = .self)
  })

Binary_Performance$methods(update = update_)

Continuous$methods(
  bin = function(min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0,
                 exceptions=numeric(0), ...) {

    f <- !is.na(x)
    tf@tf <<- .Call("bin", as.double(x[f]), as.double(perf$y[f]),
      as.double(perf$w[f]), as.double(min.iv), as.integer(min.cnt),
      as.integer(min.res), as.integer(max.bin), as.integer(mono),
      as.double(exceptions))

    tf@exceptions$input <<- exceptions

    callSuper()
  }
)

Continuous$methods(
  collapse = function(v) {
    d <- unique(pmax(pmin(tail(v, -1), length(tf@tf) - 1), 2))
    tf@tf <<- tf@tf[-d]
    callSuper()
  }
)

Continuous$methods(
  expand = function(v) {
    f <- ! (is.na(x) | x %in% exceptions$input)

    a <- min(max(1, v), length(tf$tf))  # can't be smaller than 1
    z <- max(min(v + 1, length(tf@tf)), a) # or larger than max els
    # return()

    vals <- x[x > tf@tf[a] & x <= tf@tf[z] & f]

    q <- unique(quantile(vals, seq(0, 1, 0.2))) # quintiles

    tf@tf <<- sort(c(tf@tf[-z], q))
    callSuper()
  }
)

Discrete$methods(initialize = function(x, ...) {
  if(any(levels(x) %in% "")) stop("Factor levels contain blanks")
  callSuper(x=x, ...)
})

Discrete$methods(bin = function() {
  tf@tf <<- as.list(levels(x))
  names(tf@tf) <<- levels(x)
  callSuper()
})

Discrete$methods(collapse = function(v) {
  f <- which(tf@tf %in% unique(tf@tf)[v]) ## which values were selected for collapse?
  tf@tf[f] <<- paste(names(tf@tf)[f], collapse=',') # collapse them with commas
  callSuper()
})
