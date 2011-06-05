library(RColorBrewer)

FIRST_WEEK_INDEX=1
WEEKS=c(4,12,25,50,100,150,200,250,300)
NUM_WEEKS=5 #length(WEEKS)
d = read.csv('counterfactual-4_12_25_50_100_150_200_250_300-hole_2.csv',
    colClasses=c("Date", rep('numeric', length(WEEKS))))
t = read.csv('acr_gcr_netcompare2.csv', colClasses=c('Date', rep('numeric', 4)))
ylims = c(0, max(d[[paste('ge', WEEKS[FIRST_WEEK_INDEX], sep='')]]))
xlims = c(min(t$date, d$date), max(t$date, d$date))

cex=0.75

plot_counterfactual <- function() {
    colors = brewer.pal(NUM_WEEKS - (FIRST_WEEK_INDEX-1), 'Set1')
    par(cex=cex)
    par(mar=c(5,5,2,2))
    for(i in c(FIRST_WEEK_INDEX:NUM_WEEKS)) {
        plot(
            d$date,
            d[[paste('ge', WEEKS[i], sep='')]],
            type='l',
            col=colors[i],
            xlim=xlims,
            ylim=ylims,
            xaxt='n',
            yaxt='n',
            xlab='',
            ylab='',
            lwd=1.5
            )
        par(new=T)
    }
    plot(
        t$date,
        t$gcr_netsnow,
        type='l',
        col='black',
        lwd=1.5,
        xlim=xlims,
        ylim=ylims,
        xlab='Date',
        ylab='prefixes'
        )
    grid(nx=NA, ny=NULL, col='grey40')
    legend(
        'topleft',
        legend=paste(c(paste('cumulative number of prefixes in table >=',
            WEEKS[FIRST_WEEK_INDEX:NUM_WEEKS], 'weeks'),
            'actual number of prefixes in table')),
        col=c(colors, 'black'),
        lty=1,
        lwd=1.5,
        inset=0.02,
        cex=cex,
        bg='white'
        )
    par(new=F)
}

pdf_counterfactual <- function() {
    pdf('counterfactual.pdf', width=6, height=3)
    plot_counterfactual()
    dev.off()
}
