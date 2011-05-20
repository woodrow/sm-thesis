agnc <- read.csv("acr_gcr_netcompare.csv")
agnc$date = as.Date(agnc$date)

lwd=2
cex=0.75

plot_acr_gcr_netcompare <- function() {
    par(cex=cex)
    par(mar=c(5,5,2,2))
    ylims=c(0, 1)
    xlims = c(
        as.Date(paste(c('19',
            as.POSIXlt(min(agnc$date))$year, '-01-01'),
            collapse='')),
        as.Date(paste(c('20',
            (as.POSIXlt(max(agnc$date))$year+1)%%100,
            '-01-01'), collapse=''))
        )
    plot(
        x=agnc$date,
        y=agnc$acr_netsnow/agnc$gcr_netsnow,
        main="",
        xlab="Date",
        ylab="Fraction",
        xaxt='n',
        xlim=xlims,
        ylim=ylims,
        type='l',
        col='red'
        )
    par(new=T)
    plot(
        x=agnc$date,
        y=agnc$acr_netgain/agnc$gcr_netgain,
        main="",
        xlab="",
        ylab="",
        xaxt='n',
        yaxt='n',
        xlim=xlims,
        ylim=ylims,
        type='l',
        col='blue'
        )
    axis.Date(1, at=seq(min(xlims), max(xlims), "1 years"))
    grid(nx=NA, ny=NULL, col='grey40')
    legend(
        'topleft',
        c("Fraction of total routing table prefixes advertised by ACR ASes",
        "Fraction of total aggregable prefixes advertised by ACR ASes"),
        col=c('red','blue'),
        lty=c(1,1),
        lwd=2:2,
        bg="white",
        inset=0.02)
}

pdf_acr_gcr_netcompare <- function() {
    pdf("acr_gcr_netcompare.pdf", width=6, height=3)
    plot_acr_gcr_netcompare()
    dev.off()
}
