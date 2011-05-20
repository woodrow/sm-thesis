acr_freqs <- read.csv("acr_cdfs.csv")
showpoints=T
lwd=2
cex=0.75
log="x"
gridcol='grey40'
options(scipen=0)

#graphics.off()
plot_cdf_weeks <- function() {
    par(cex=cex)
    par(mar=c(5,5,2,2))
    plot.ecdf(
        acr_freqs$weeks,
        main="",#"CDF of weeks per AS on the CIDR Report",
        xlab=expression(Total~weeks~present~on~the~ACR),
        ylab=expression(P(X<=x)),
        do.points=showpoints,
        lwd=lwd,
        xlim=c(1,max(acr_freqs$weeks)),
        log=log,
        yaxp=c(0,1,10)
    )
    par(yaxp=c(0,1,10))
    grid(col=gridcol)
}

plot_cdf_rankweeks <- function() {
    par(cex=cex)
    par(mar=c(5,5,2,2))
    plot.ecdf(
        acr_freqs$rankweeks,
        main="",#"CDF of rank-weeks per AS on the CIDR Report",
        xlab=expression(Total~"rank-weeks"~present~on~the~ACR),
        ylab=expression(P(X<=x)),
        do.points=showpoints,
        lwd=lwd,
        xlim=c(1,max(acr_freqs$rankweeks)),
        log=log,
        yaxp=c(0,1,10)
    )
    par(yaxp=c(0,1,10))
    grid(col=gridcol)
}

plot_cdf_ngweeks <- function() {
    par(cex=cex)
    par(mar=c(5,5,2,2))
    plot.ecdf(
        acr_freqs$ngweeks,
        main="",#"CDF of rank-weeks per AS on the CIDR Report",
        xlab=expression(Total~"netgain-weeks"~present~on~the~ACR),
        ylab=expression(P(X<=x)),
        do.points=showpoints,
        lwd=lwd,
        xlim=c(1,1e6),
        log=log,
        yaxp=c(0,1,10)
    )
    par(yaxp=c(0,1,10))
    grid(col=gridcol)
}

pdf_cdf_weeks <- function() {
    pdf("acr_cdf_weeks.pdf", width=6, height=3)
    plot_cdf_weeks()
    dev.off()
}

pdf_cdf_rankweeks <- function() {
    pdf("acr_cdf_rankweeks.pdf", width=6, height=3)
    plot_cdf_rankweeks()
    dev.off()
}

pdf_cdf_ngweeks <- function() {
    pdf("acr_cdf_ngweeks.pdf", width=6, height=3)
    plot_cdf_ngweeks()
    dev.off()
}

pdf_all <- function() {
    pdf_cdf_weeks()
    pdf_cdf_rankweeks()
    pdf_cdf_ngweeks()
}
