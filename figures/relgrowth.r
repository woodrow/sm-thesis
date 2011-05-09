xrange = c(as.Date('1999-01-01'),as.Date('2006-09-30'))
xlims = c(as.Date(paste(c('19', as.POSIXlt(min(xrange))$year, '-01-01'), collapse='')),
    as.Date(paste(c('200', (as.POSIXlt(max(xrange))$year+1)%%100, '-01-01'), collapse='')))
ylims = c(1,2.2)
cex = 0.75

plot_relgrowth <- function() {
    par(cex=cex)
    par(mar=c(5,5,2,2))
    plot(
        x=xrange,
        y=c(2,2),
        xlab="Date",
        ylab="Growth Factor",
        yaxt="n",
        xaxt="n",
        type="l",
        col="firebrick",
        lwd=2,
        ylim=ylims,
        xlim=xlims
        )
    axis.Date(1, at=seq(min(xlims), max(xlims), "1 year"))
    y_labels = pretty(ylims,7)
    axis(2, y_labels)
    text(min(xrange), 2, "Moore's Law", adj=c(0,2), cex=cex)
    par(new=TRUE)
    plot(
        x=xrange,
        y=c(1.3,1.3),
        xlab="",
        ylab="",
        yaxt="n",
        xaxt="n",
        type="l",
        col="forestgreen",
        lwd=2,
        ylim=ylims,
        xlim=xlims
        )
    text(min(xrange), 1.3, "Constant Chip Cost", adj=c(0,2), cex=cex)
    par(new=TRUE)
    plot(
        x=xrange,
        y=c(1.2,1.2),
        xlab="",
        ylab="",
        yaxt="n",
        xaxt="n",
        type="l",
        col="navy",
        lwd=2,
        ylim=ylims,
        xlim=xlims
        )
    text(min(xrange), 1.2, "Constant BGP Convergence", adj=c(0,2), cex=cex)
}

pdf_relgrowth <- function() {
    pdf("relgrowth.pdf", width=6, height=3.5)
    plot_relgrowth()
    dev.off()
}
