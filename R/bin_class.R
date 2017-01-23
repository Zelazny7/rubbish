setClassUnion("NULLnumeric", c("NULL", "numeric"))

## create a transform object that applies the transoformation to a variable
setClass("Transform", slots = c(
  tf = "ANY",
  woe = "numeric",
  pred = "numeric",
  nas = "numeric",
  exceptions = "list")
)

setMethod("initialize", "Transform", function(.Object, ...) {
  .Object@exceptions = list(input=numeric(), output=numeric())
  validObject(.Object)
  .Object
})

## exceptions is a list of two vectors each with the same number of elements

## bin class ##
Bin <- setRefClass("Bin",
  fields = c(
    name = "character",
    y = "numeric",
    w = "NULLnumeric",
    tf = "Transform",
    history = "list"),
  contains = "VIRTUAL")

Bin$methods(initialize = function(name="Unknown", x, y, w=rep(1, length(x)), ...) {
  ## perform bin checks here
  stopifnot(length(x) > 0)
  stopifnot(length(x) == length(y))
  stopifnot(length(x) == length(w))
  stopifnot(!any(is.na(y)))
  callSuper(name=name, x=x, y=y, w=w, ...)
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

Continuous <- setRefClass("Continuous",
  fields = c(x="numeric"),
  contains = "Bin"
)

Discrete <- setRefClass("Discrete",
  fields = c(x="factor"),
  contains = "Bin"
)

# Continuous$methods(initialize = function(exceptions, ...) {
#   #exceptions <<- list(input = numeric(), output = numeric())
#   callSuper(...)
# })

Continuous$methods(
  bin = function(min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0,
                 exceptions=numeric(0), ...) {

    f <- !is.na(x)
    tf@tf <<- .Call("bin", as.double(x[f]), as.double(y[f]), as.double(w[f]),
                  as.double(min.iv), as.integer(min.cnt), as.integer(min.res),
                  as.integer(max.bin), as.integer(mono), as.double(exceptions))

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


### Discrete Class ###
Discrete <- setRefClass("Discrete",
  fields = c(x = "factor", map = "Transform"),
  contains = "Bin")

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


# continuous$methods("Bin" = callSuper())
data(titanic)
b <- Continuous$new(x=titanic$Fare, y=titanic$Survived)
b$collapse(2:7)
b$bin()

#levels(titanic$Embarked)[1] <- NA
d <- Discrete$new(x=titanic$Pclass, y=titanic$Survived)
# d$bin()
# d$collapse(2:3)


# d$Bin()
