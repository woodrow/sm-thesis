acr_netgain_time <- read.csv("acr_netgain_time.csv")
acr_netgain_time$date = as.Date(acr_netgain_time$date)

lwd=2
cex=0.75

plot_netgain_time <- function() {
    par(cex=cex)
    par(mar=c(5,5,2,2))
    ylims=c(0, max(acr_netgain_time$rank0))
    xlims = c(
        as.Date(paste(c('19',
            as.POSIXlt(min(acr_netgain_time$date))$year, '-01-01'),
            collapse='')),
        as.Date(paste(c('20',
            (as.POSIXlt(max(acr_netgain_time$date))$year+1)%%100,
            '-01-01'), collapse=''))
        )
    plot(
        x=acr_netgain_time$date,
        y=acr_netgain_time$rank0,
        main="",
        xlab="Date",
        ylab="Aggregable prefixes (netgain)",
        xaxt='n',
        xlim=xlims,
        ylim=ylims,
        type='l',
        col='red'
        )
    par(new=T)
    plot(
        x=acr_netgain_time$date,
        y=acr_netgain_time$rank1,
        main="",
        xlab="",
        ylab="",
        xaxt='n',
        yaxt='n',
        xlim=xlims,
        ylim=ylims,
        type='l',
        col='orange'
        )
    par(new=T)
    plot(
        x=acr_netgain_time$date,
        y=acr_netgain_time$rank2,
        main="",
        xlab="",
        ylab="",
        xaxt='n',
        yaxt='n',
        xlim=xlims,
        ylim=ylims,
        type='l',
        col='gold'
        )
    par(new=T)
    plot(
        x=acr_netgain_time$date,
        y=acr_netgain_time$rank3,
        main="",
        xlab="",
        ylab="",
        xaxt='n',
        yaxt='n',
        xlim=xlims,
        ylim=ylims,
        type='l',
        col='chartreuse'
        )
    par(new=T)
    plot(
        x=acr_netgain_time$date,
        y=acr_netgain_time$rank4,
        main="",
        xlab="",
        ylab="",
        xaxt='n',
        yaxt='n',
        xlim=xlims,
        ylim=ylims,
        type='l',
        col='forestgreen'
        )
    par(new=T)
    plot(
        x=acr_netgain_time$date,
        y=acr_netgain_time$rank9,
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
    par(new=T)
    plot(
        x=acr_netgain_time$date,
        y=acr_netgain_time$rank14,
        main="",
        xlab="",
        ylab="",
        xaxt='n',
        yaxt='n',
        xlim=xlims,
        ylim=ylims,
        type='l',
        col='purple'
        )
    par(new=T)
    plot(
        x=acr_netgain_time$date,
        y=acr_netgain_time$rank29,
        main="",
        xlab="",
        ylab="",
        xaxt='n',
        yaxt='n',
        xlim=xlims,
        ylim=ylims,
        type='l',
        col='black'
        )
    axis.Date(1, at=seq(min(xlims), max(xlims), "1 years"))
    grid(nx=NA, ny=NULL, col='grey40')
    legend(
        'topleft',
        c("netgain of rank 1",
        "netgain of rank 2",
        "netgain of rank 3",
        "netgain of rank 4",
        "netgain of rank 5",
        "netgain of rank 10",
        "netgain of rank 15",
        "netgain of rank 30"),
        col=c('red','orange','gold','chartreuse','forestgreen',
        'blue','purple','black'),
        lty=rep(1,8),
        lwd=2:2,
        bg="white",
        inset=0.02)
}

plot_netgain_time_min <- function() {
    par(cex=cex)
    par(mar=c(5,5,2,2))
    ylims=c(0, max(acr_netgain_time$rank29))
    xlims = c(
        as.Date(paste(c('19',
            as.POSIXlt(min(acr_netgain_time$date))$year, '-01-01'),
            collapse='')),
        as.Date(paste(c('20',
            (as.POSIXlt(max(acr_netgain_time$date))$year+1)%%100,
            '-01-01'), collapse=''))
        )
    plot(
        x=acr_netgain_time$date,
        y=acr_netgain_time$rank29,
        main="",
        xlab="Date",
        ylab="Aggregable prefixes (netgain)",
        xaxt='n',
        xlim=xlims,
        ylim=ylims,
        type='l',
        col='black'
        )
    axis.Date(1, at=seq(min(xlims), max(xlims), "1 years"))
    grid(nx=NA, ny=NULL, col='grey40')
}

pdf_netgain_time <- function() {
    pdf("acr_netgain_time.pdf", width=6, height=6)
    plot_netgain_time()
    dev.off()
}

pdf_netgain_time_min <- function() {
    pdf("acr_netgain_time_min.pdf", width=6, height=3)
    plot_netgain_time_min()
    dev.off()
}

pdf_all <- function() {
    pdf_netgain_time()
    pdf_netgain_time_min()
}
