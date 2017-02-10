#' @include performance_class.R continuous_class.R discrete_class.R

#' @export Binary_Performance
#' @exportClass Binary_Performance
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
  function(.self, b, ...) {

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
    #browser()
    ## discretize the x var based on the bin type and the transform
    info <- b$factorize()

    ## can now split x and y and w and calculate
    out <- lapply(info$types, function(f) {
      .self$summarize(factor(info$factor[f]), .self$y[f], .self$w[f])
    })

    out$Total <- colSums(do.call(rbind, out))
    out$Total[c("P(1)", "WoE", "Pred")] <- 0
    out

  })

Binary_Performance$methods(update = update_)

make_bars_ <- function(v, width=0.70, ...) {
  left <- pmin(v, 0)
  right <- pmax(v, 0)
  center <- seq_along(left)
  top <- center - 0.5 * width
  bottom <- center + 0.5 * width
  rect(left, bottom, right, top, ...)
  center
}

setMethod(
  "plot_",
  signature = c(.self="Binary_Performance", b="Bin"),
  function(.self, b) {

    #opar <- par()$oma # save the current settings
    on.exit(par(oma=rep(0, 4))) # restore them on exit

    tmp <- head(b$show(), -1)
    lbls <- rev(row.names(tmp))
    woe <- rev(tmp[,"WoE"])
    val <- rev(tmp[,"Pred"])
    pctN <- sprintf("%0.1f%%", rev(tmp[, "%N"] * 100))

    ## find the max and min
    xlim <- range(c(woe, val)) + c(-0.5, 0.5)

    ## set margin based on nchars
    w <- max(nchar(lbls))
    par(oma=c(0,w/6,0,0))

    plot(NA, xlim=xlim, ylim=c(0.5, length(woe) + 0.5),
      xlab = "Weight of Evidence", ylab=NA, yaxt="n", main = b$name)

    abline(v = 0, lty=3)
    center <- make_bars_(woe, col=rgb(0, 0, 0, alpha = 0.30))
    center <- make_bars_(val, width=0.2, col="red")

    text(x = min(xlim), y = center, labels = sprintf(" [%02d]",
      rev(seq_along(lbls))), cex=0.80)

    text(x = max(xlim) - 0.1, y = center, labels = pctN, cex=0.80)

    axis(side = 2, labels = lbls, at = center, las = 2, lwd.ticks = 0,
      cex.axis = 0.80)

  })

Binary_Performance$methods(plot = plot_)


