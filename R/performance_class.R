#' @include generic_methods.R bin_class.R

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

Binary_Performance$methods(summarize = function(x, y, w) {
  # browser()
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
  signature = c(.self="Binary_Performance"),
  function(.self, ...) {
    print("Binary Performance update!")
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

    ###round(data.frame(do.call(rbind, tmp), check.names = F), 3)
    ## Calculate values for NAs, Exceptions, and Normal Values
  })


Binary_Performance$methods(update = update_)
