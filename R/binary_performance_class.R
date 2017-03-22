#' @include performance_class.R continuous_class.R discrete_class.R

#' @export Binary_Performance
#' @exportClass Binary_Performance
Binary_Performance <- setRefClass("Binary_Performance", fields = c(
  ones = "numeric",
  zeros = "numeric"),
  contains = "Performance")

Binary_Performance$methods(initialize =  function(y=.self$y, ...) {
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

    b$tf@exceptions <- setNames(rep(0, length(exceptions)), exceptions)

  })

setMethod("bin_",
  signature = c(.self="Binary_Performance", b="Discrete"),
  function(.self, b, exceptions=numeric(0), ...) {

    #browser()

    b$tf@tf <- as.list(levels(b$x))
    names(b$tf@tf) <- levels(b$x)
    b$tf@exceptions <- setNames(rep(0, length(exceptions)), exceptions)
  })

Binary_Performance$methods(bin = bin_)

Binary_Performance$methods(summarize = function(x, y, w) {
  N1 <- tapply((y == 1) * w, x, sum)
  N0 <- tapply((y == 0) * w, x, sum)

  N <- tapply(w, x, sum)
  P1 <- N1 / ones
  P0 <- N0 / zeros
  WoE <- log(P1 / P0)
  IV <- (P1 - P0) * WoE

  #c("N", "#1", "#0", "%N","%1","%0","P(1)","WoE","IV", "Pred")
  res <- cbind(N = N, `#1` = N1, `#0` = N0, `%N` = N / sum(ones, zeros),
    `%1` = N1 / ones, `%0` = N0 / zeros, `P(1)` = N1 / N,
    WoE = WoE, IV = IV, Pred = WoE)

  res[is.na(res) | is.infinite(res)] <- 0
  res

})

Binary_Performance$methods(update = function(b, ...) {
    info <- b$factorize()

    ## can now split x and y and w and calculate
    out <- list()
    f <- info$types$normal
    out$normal <- .self$summarize(factor(info$factor[f]), .self$y[f], .self$w[f])

    f <- info$types$exception
    out$exception <- .self$summarize(factor(info$factor[f],
      levels=names(b$tf@exceptions)), .self$y[f], .self$w[f])

    f <- info$types$missing
    out$missing <- .self$summarize(factor(info$factor[f]), .self$y[f], .self$w[f])

    out$Total <- colSums(do.call(rbind, out), na.rm=TRUE)
    out$Total[c("P(1)", "WoE", "Pred")] <- 0
    out

  })

make_bars_ <- function(v, width=0.70, ...) {
  left <- pmin(v, 0)
  right <- pmax(v, 0)
  center <- seq_along(left)
  top <- center - 0.5 * width
  bottom <- center + 0.5 * width
  rect(left, bottom, right, top, ...)
  center
}

Binary_Performance$methods(plot = function(b, ...) {

    on.exit(par(oma=rep(0, 4))) # restore them on exit


    tmp <- head(b$as.matrix(), -1)
    lbls <- rev(row.names(tmp))
    woe <- rev(tmp[,"WoE"])
    val <- rev(tmp[,"Pred"])
    pctN <- sprintf("%0.1f%%", rev(tmp[, "%N"] * 100))

    ## find the max and min
    xlim <- range(c(woe, val)) + c(-0.5, 0.5)

    ## set margin based on nchars
    width <- max(nchar(lbls))
    par(oma=c(0, width/6, 0, 0))

    graphics::plot(NA, xlim=xlim, ylim=c(0.5, length(woe) + 0.5),
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


Binary_Performance$methods(summary = function(tf, ...) {
  ## return the information value of the bin
  tot <- tf@repr$Total[c("IV", "N", "#1", "#0", "P(1)")]
  nas <- unname(colSums(tf@repr$missing[,"N", drop=F], na.rm=T))
  exc <- unname(colSums(tf@repr$exception[,"N", drop=F], na.rm=T))

  out <- c(tot, "N missing"=nas, "N Exceptions"=exc)
  out[is.na(out)] <- 0
  out

})

Binary_Performance$methods(sort_value = function(b, ...) {
  b$tf@repr$Total["IV"]
})
