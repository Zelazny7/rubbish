setClassUnion("ValidBinType", c("numeric", "factor"))

bin <- setRefClass("bin",
  fields = c(
    name = "character",
    x    = "ValidBinType",
    y    = "numeric",
    w    = "numeric"),
  contains = "VIRTUAL")

## TODO: remove the x, y, w from the function call and reference via .self!

## create a generic method here
setGeneric("Bin",
  function(.self, x, y, w, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0, exceptions=numeric(0)) {
    stopifnot(NROW(x) > 0)
    if (!missing(y)) stopifnot(NROW(x) == NROW(y))
    standardGeneric("Bin")
  }
)

bin$methods("Bin" = Bin)

continuous <- setRefClass("continuous",
  fields = c(cuts = "numeric"),
  contains = "bin")

discrete <- setRefClass("discrete",
  fields = c(map = "list"),
  contains = "bin")


setMethod("Bin",
  signature = c("bin", "ValidBinType", "numeric"),
  function(.self, x, y, w, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0, exceptions=numeric(0)) {
    w <- rep(1, length(x))
    callGeneric(.self=.self, x=x, y=y, w=w, name=name, min.iv=min.iv, min.cnt=min.cnt,
      min.res=min.res, max.bin=max.bin, mono=mono, exceptions=exceptions, ...)
  }
)

setMethod("Bin",
  signature = c("continuous", "numeric", "numeric", "numeric"),
  function(.self, x, y, w, min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0, exceptions=numeric(0)) {

    f <- !is.na(x)
    cuts <- .Call("bin", as.double(x[f]), as.double(y[f]), as.double(w[f]),
      as.double(min.iv), as.integer(min.cnt), as.integer(min.res),
      as.integer(max.bin), as.integer(mono), as.double(exceptions))

    .self$cuts <- cuts

  }
)




# continuous$methods("Bin" = callSuper())

b <- continuous$new()
# b$Bin()



