
tmp <- tail(b$cache, 1)[[1]]
woe <- c(tmp$normal[,'WoE'], tmp$exception[,'WoE'])
val <- c(tmp$normal[,'Pred'], tmp$exception[,'Pred'])
both <- woe

## find the max and min
xlim <- range(both) + c(-0.5, 0.5)

opar <- par()

# m <- rbind(c(2, 1))
# nf <- layout(mat = m,  widths = c(1,3), heights = 1)
#par(mar=c(3.1, 3.1, 1.1, 2.1))
#par(mar=c(0,0,0,0))


par(oma=c(0,4,0,0) )
#par(mar=c(4,4.5,2,1))

#text(x = 1, y = c(0.5, length(woe) + 0.5))
plot(NA, xlim=xlim, ylim=c(0.5, length(woe) + 0.5), xlab = "Weight of Evidence",
     ylab=NA, yaxt="n")

axis(side = 2, labels = names(woe), at = center, las = 2, lwd.ticks = 0, cex.axis = 0.80)

#par(mfrow=c(1,1))
l <- pmin(both, 0)
r <- pmax(both, 0)
width <- 0.70
center <- seq_along(l)
top <- center - 0.5 * width
bot <- center + 0.5 * width
rect(l, bot, r, top)
title(b$name)

# plot(rep(0, length(center)), center, ann = F, bty = 'n', type = 'n',
#      xaxt = 'n', yaxt = 'n', mar=c(0,0,0,0) + 0.1)
#par(mar=c(0,0,0,0))
text(x=0, y=center, labels = names(woe), cex=0.9)

par(opar)


## do this at the end of a function

op <- par("mypar"=myvalue)
on.exit(par(op))

