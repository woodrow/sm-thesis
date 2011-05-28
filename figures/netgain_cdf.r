netgain_cdf_acr <- read.csv("netgain_cdf_acr.csv")
netgain_cdf_gcr <- read.csv("netgain_cdf_gcr.csv")
netgain_cdf_gcr$netgain = netgain_cdf_gcr$nets_reduced
netgain_cdf_gcr$netsnow = netgain_cdf_gcr$nets_current

showpoints=F
verticals=T
lwd=2
cex=0.75
#log="x"
gridcol='grey40'
options(scipen=0)

#graphics.off()
plot_netgain_cdf_acr <- function(log='') {
    plot_netgain_cdf(netgain_cdf_acr, 'netgain', log)
}

plot_netgain_cdf_gcr <- function(log='', ylims=c(0.9,1), printTOP30=F) {
    plot_netgain_cdf(netgain_cdf_gcr, 'netgain', log, ylims, printTOP30)
}

plot_netsnow_cdf_acr <- function(log='') {
    plot_netgain_cdf(netgain_cdf_acr, 'netsnow', log)
}

plot_netsnow_cdf_gcr <- function(log='', ylims=c(0.9,1), printTOP30=F) {
    plot_netgain_cdf(netgain_cdf_gcr, 'netsnow', log, ylims, printTOP30)
}

plot_frac_aggr_cdf_gcr <- function(log='', ylims=c(0.6,1), printTOP30=F) {
    netgain_cdf_gcr$frac_aggr = as.numeric(netgain_cdf_gcr$netgain)/as.numeric(netgain_cdf_gcr$netsnow)
    print(length(netgain_cdf_gcr$frac_aggr))
    print(length(factor(netgain_cdf_gcr$frac_aggr)))
    plot_netgain_cdf(netgain_cdf_gcr, 'frac_aggr', log, ylims, printTOP30)
}


plot_netgain_cdf <- function(d, series='netgain', log='', ylims=c(0,1), printTOP30=F) {
    par(cex=cex)
    par(mar=c(5,5,2,2))
    d$date = 1900 + as.POSIXlt(d$date)$year
    yrs = split(d, f=as.factor(d$date))
    jet.colors = colorRampPalette(
        c("#00007F", "blue", "#007FFF", "cyan",
        "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    colvec = jet.colors(length(yrs))
    names(colvec) = names(yrs)
    print_axis=T
    xlabs = c(netgain="Aggregable prefixes (netgain)",
        netsnow="Total advertised prefixes (netsnow)",
        frac_aggr="Fraction of announced prefixes that are aggregable")
    print(xlabs)
    if(log=='x') {
        xlims = c(max(1,min(d[[series]])),max(d[[series]]))
    } else {
        xlims = c(min(d[[series]]),max(d[[series]]))
    }
#    ylims = c(0.99,1)
    for(y in names(yrs)) {
        print(paste("max:", y, max(yrs[[y]][[series]])))
        print(paste("min:", y, min(yrs[[y]][[series]])))
        if(print_axis) {
            plot.ecdf(
                yrs[[y]][[series]],
                main="",
                xlab=xlabs[series],
                ylab=expression(P(X<=x)),
                do.points=showpoints,
                verticals=verticals,
                lwd=lwd,
                xlim=xlims,
                ylim=ylims,
                log=log,
                #xaxp=c(0,1,10),
                col=colvec[y]
                )
            print_axis=F
        } else {
            plot.ecdf(
                yrs[[y]][[series]],
                main="",
                xlab="",
                ylab="",
                xaxt='n',
                yaxt='n',
                do.points=showpoints,
                verticals=verticals,
                lwd=lwd,
                xlim=xlims,
                ylim=ylims,
                log=log,
                #xaxp=c(0,1,10),
                col=colvec[y],
                add=T
                )
        }
        if(printTOP30) {
            hpos=1-(30/length(yrs[[y]][['netgain']]))
            abline(h=hpos)
            text(1, hpos, paste("Top 30,", y), adj=c(0,2), cex=cex)
        }
    }
    grid()
    par(new=F)
    legend(
        'bottomright',
        names(yrs),
        cex=cex,
        col=colvec,
        lty=1:1,
        lwd=2:2,
        bg="white",
        inset=0.02
        )
    if(series == 'netgain' && length(yrs[[1]][[series]]) > 30) {
        hpos=1-(30/length(yrs[[1]][[series]]))
        col='black'
        abline(h=hpos, col=col)
        text(1, hpos, paste("Top 30,", names(yrs)[1]), adj=c(0,1.3), cex=cex, col=col)
        hpos=1-(30/length(yrs[[length(yrs)]][[series]]))
        col='red3'
        abline(h=hpos, col=col)
        text(1, hpos, paste("Top 30,", names(yrs)[length(yrs)]), adj=c(0,-0.3), cex=cex, col=col)
    }
}

pdf_netgain_cdf_acr <- function() {
    pdf("netgain_cdf_acr.pdf", width=6, height=3)
    plot_netgain_cdf_acr()
    dev.off()
}

pdf_netsnow_cdf_gcr <- function() {
    pdf("netgain_netsnow_cdf_gcr.pdf", width=6, height=3)
    plot_netsnow_cdf_gcr(log='x', ylims=c(0.75,1))
    dev.off()
}

pdf_netgain_cdf_gcr <- function() {
    pdf("netgain_cdf_gcr.pdf", width=6, height=3)
    plot_netgain_cdf_gcr(log='x', ylims=c(0.75,1.005))
    dev.off()
}

pdf_fracaggr_cdf_gcr <- function() {
    pdf("netgain_frac_aggr_cdf_gcr.pdf", width=6, height=3)
    plot_frac_aggr_cdf_gcr()
    dev.off()
}
