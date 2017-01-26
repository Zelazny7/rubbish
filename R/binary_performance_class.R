#' @include performance_class.R continuous_class.R discrete_class.R

Binary_Performance <- setRefClass("Binary_Performance", fields = c(
  ones = "numeric",
  zeros = "numeric"),
  contains = "Performance")

Binary_Performance$methods(initialize =  function(y, ...) {
  callSuper(y=y, ...)
  ones <<- sum((y == 1) * w)
  zeros <<- sum((y == 0) * w)
})

setMethod("bin_",
  signature = c(.self="Binary_Performance", b="Continuous"),
  function(.self, b, min.iv=0.01, min.cnt=10, min.res=0,
    max.bin=10, mono=0, exceptions=numeric(0)) {

    f <- !is.na(b$x)
    b$tf@tf <- .Call("bin", as.double(b$x[f]), as.double(.self$y[f]),
      as.double(.self$w[f]), as.double(min.iv), as.integer(min.cnt),
      as.integer(min.res), as.integer(max.bin), as.integer(mono),
      as.double(exceptions))

    b$tf@exceptions$input <- exceptions
  })

setMethod("bin_",
  signature = c(.self="Binary_Performance", b="Discrete"),
  function(.self, b) {

    b$tf@tf <- as.list(levels(b$x))
    names(b$tf@tf) <- levels(b$x)
  })

Binary_Performance$methods(bin = bin_)

Binary_Performance$methods(summarize = function(x, y, w) {
  N1 <- tapply((y == 1) * w, x, sum)
  N0 <- tapply((y == 0) * w, x, sum)

  N  <- tapply(w, x, sum)
  P1  <- N1 / ones
  P0  <- N0 / zeros
  WoE <- log(P1 / P0)
  IV  <- (P1 - P0) * WoE

  #c("N", "#1", "#0", "%N","%1","%0","P(1)","WoE","IV", "Pred")
  cbind(N = N, `#1` = N1, `#0` = N0, `%N` = N / sum(ones, zeros),
    `%1` = N1 / ones, `%0` = N0 / zeros, `P(1)` = N1 / N,
    WoE = WoE, IV = IV, Pred = WoE)

})

setMethod(
  "update_",
  signature = c(.self="Binary_Performance", b="Bin"),
  function(.self, b) {
    ## discretize the x var based on the bin type and the transform
    info <- b$factorize()

    ## can now split x and y and w and calculate
    lapply(info$types, function(f) {
      .self$summarize(factor(info$factor[f]), .self$y[f], .self$w[f])
    })

  })

Binary_Performance$methods(update = update_)



