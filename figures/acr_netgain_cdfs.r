acr_netgain_freqs <- read.csv("acr_netgain_cdfs.csv")
showpoints=T
lwd=2
cex=0.75
log="x"
gridcol='grey40'
options(scipen=0)

#graphics.off()
plot_netgain_cdfs <- function() {
    par(cex=cex)
    par(mar=c(5,5,2,2))
    plot.ecdf(
        acr_netgain_freqs$max,
        main="",#"CDF of weeks per AS on the CIDR Report",
        xlab="Aggregable prefixes (netgain)",
        ylab=expression(P(X<=x)),
        do.points=showpoints,
        lwd=lwd,
        xlim=c(1,max(acr_netgain_freqs$max)),
        log=log,
        yaxp=c(0,1,10),
        col='red')
    par(new=T)
    plot.ecdf(
        acr_netgain_freqs$min,
        main="",#"CDF of weeks per AS on the CIDR Report",
        xlab="",
        ylab="",
        xaxt='n',
        yaxt='n',
        do.points=showpoints,
        lwd=lwd,
        xlim=c(1,max(acr_netgain_freqs$max)),
        log=log,
        yaxp=c(0,1,10),
        col='black')
    par(yaxp=c(0,1,10))
    grid(col=gridcol)
    legend(
        'topleft',
        c("CDF of AS' minimum netgain",
        "CDF of AS' maximum netgain"),
        col=c("black","red"),
        lty=c(1,1),
        lwd=2:2,
        bg="white",
        inset=0.02)

}

#plot_netgain_minmax <- function() {
#    par(cex=cex)
#    par(mar=c(5,5,2,2))
#    plot(
#        x=seq(1,length(acr_netgain_freqs$origin_as)),
#        y=acr_netgain_freqs$max,
#        main="",#"CDF of weeks per AS on the CIDR Report",
#        xlab="",
#        ylab="",
#        lwd=lwd,
#        type='l',
#        #xlim=c(1,max(acr_netgain_freqs$origin_as)),
#        ylim=c(1,max(acr_netgain_freqs$max)),
#        col='red'
#    )
#    par(new=T)
#    plot(
#        x=seq(1,length(acr_netgain_freqs$origin_as)),
#        y=acr_netgain_freqs$min,
#        main="",#"CDF of weeks per AS on the CIDR Report",
#        xlab="",
#        ylab="",
#        lwd=lwd,
#        type='l',
#        #xlim=c(1,max(acr_netgain_freqs$origin_as)),
#        ylim=c(1,max(acr_netgain_freqs$max)),
#        col='blue'
#    )
##    par(yaxp=c(0,1,10))
##    grid(col=gridcol)
#}


pdf_netgain_cdfs <- function() {
    pdf("acr_netgain_cdfs.pdf", width=6, height=3)
    plot_netgain_cdfs()
    dev.off()
}
