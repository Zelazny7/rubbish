

plot_bin <- function(b) {

}

opar <- par()



b$mono(-1)
b$expand(2)
b$neutralize(1:4)


## easiest to grab from show method?
b$show()
tmp <- d$show()
lbls <- rev(row.names(tmp))
woe <- rev(tmp[,"WoE"])
val <- rev(tmp[,"Pred"])
pctN <- sprintf("%0.1f%%", rev(tmp[, "%N"] * 100))

## find the max and min
xlim <- range(c(woe, val)) + c(-0.5, 0.5)

## set margin based on nchars
w <- max(nchar(lbls))

par(oma=c(0,w/6,0,0) )
#text(x = 1, y = c(0.5, length(woe) + 0.5))

make_bars <- function(v, width=0.70, ...) {
  left <- pmin(v, 0)
  right <- pmax(v, 0)
  center <- seq_along(left)
  top <- center - 0.5 * width
  bottom <- center + 0.5 * width
  rect(left, bottom, right, top, ...)
  center
}


plot(NA, xlim=xlim, ylim=c(0.5, length(woe) + 0.5), xlab = "Weight of Evidence",
  ylab=NA, yaxt="n", main = b$name)

abline(v = 0, lty=3)

center <- make_bars(woe, col=rgb(0,0,0,alpha = 0.30))
center <- make_bars(val, width=0.2, col="black")

text(x = min(xlim), y = center, labels = sprintf(" [%02d]", rev(seq_along(lbls))), cex=0.80)
text(x = max(xlim) - 0.1, y = center, labels = pctN, cex=0.80)
axis(side = 2, labels = lbls, at = center, las = 2, lwd.ticks = 0, cex.axis = 0.80)


par(opar)


## do this at the end of a function

op <- par("mypar"=myvalue)
on.exit(par(op))

