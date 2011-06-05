d = read.csv('cr_email_freq.csv', colClasses=c("Date"))
cex=0.75

plot_cr_email_freq <- function() {
    par(cex=cex)
    par(mar=c(5,5,2,2))
    t = table(cut(d$date, 'week'))
    dates = as.Date(names(t))
    freq = as.integer(t)
    plot(
        x=dates,
        y=freq,
        type='h',
        col='black',
        xlab='Date',
        ylab='emails per week',
        log='',
        xaxt='n',
        lwd=1,
        ylim=c(0,10)
        )
    axis.Date(1, at=seq(min(dates), max(dates), "1 year"))
}

pdf_cr_email_freq <- function() {
    pdf('cr_email_freq.pdf', width=6, height=3)
    plot_cr_email_freq()
    dev.off()
}
