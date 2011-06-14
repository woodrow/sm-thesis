source('database_credentials.r')

library(RColorBrewer)
data_sets = c("as_rank_freq", "as_rank_durations")
for(e in data_sets) {
    if(!exists(e)) {
        library(RPostgreSQL)
        conn <- dbConnect(
            PostgreSQL(),
            host=db_host,
            dbname=db_name,
            user=db_user,
            password=db_password
        )
        ########################################################################
        res = dbSendQuery(conn, paste(
            "SELECT tmp.rank, COUNT(tmp.as_count) from (",
            "    SELECT rank, COUNT(origin_as) as as_count",
            "    FROM email_cidr_reports",
            "    GROUP BY rank, origin_as",
            "    ORDER BY rank) as tmp",
            "WHERE tmp.rank < 30",
            "GROUP BY tmp.rank",
            "ORDER BY tmp.rank;"))
        as_rank_freq = fetch(res, n=-1)

        res = dbSendQuery(conn, paste(
            "SELECT rank, COUNT(origin_as) as as_count",
            "FROM email_cidr_reports",
            "WHERE rank < 30",
            "GROUP BY rank, origin_as",
            "ORDER BY rank;"))
        as_rank_durations = fetch(res, n=-1)

        #####
        # TODO: this is the start of a query for looking at behavior by year...
        # plot as x/y with z as color
        # X = num ases in rank y for year z
        # Y = rank
        # Z = year
        res = dbSendQuery(conn, paste(
            "SELECT rank, extract(year from date) as year, COUNT(origin_as) as as_count",
            "FROM email_cidr_reports",
            "WHERE rank < 30",
            "GROUP BY rank, year, origin_as",
            "ORDER BY year, rank;"))
        as_rank_durations_by_year = fetch(res, n=-1)
        ########################################################################
        dbDisconnect(conn)
        break
    }
}

cex=0.7
showpoints=F
autorange=T
smallmax=50

plot_cr_rank_cdfs <- function() {
#    x11()
#    plot(
#        as_rank_freq$rank+1,
#        as_rank_freq$count,
#        type='h',
#        ylim=c(0,max(as_rank_freq$count))
#    )
#
    ard.split = split(as_rank_durations,
        factor(as_rank_durations$rank, levels=unique(as_rank_durations$rank)))
#
#    x11()
    par(mfrow=c(2,3))
    par(cex=cex)
    xlims = c(0,max(as_rank_durations[['as_count']]))
    jet.colors = colorRampPalette(
        c("#00007F", "blue", "#007FFF", "cyan",
        "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    colvec = brewer.pal(5,'Set1')
    for(i in c(0:5)) {
        if(autorange) {
            xlims = c(0,max(sapply(paste((i*5):(i*5+4)),
                (function(x)max(ard.split[[x]][['as_count']]))),smallmax))
        }
        par(mar=c(3,3,1,1))
        par(cex=cex)
        for(x in c(0:29)) {
            plot.ecdf(
                ard.split[[as.character(x)]][['as_count']],
                xlim=xlims,
                col='grey90',
                verticals=TRUE,
                do.points=showpoints,
                lwd=2,
                main='',
                xaxt=ifelse(x==0,'s','n'),
                yaxt=ifelse(x==0,'s','n'),
                xlab=ifelse(x==0,"Number of weeks at rank",""),
                ylab=ifelse(x==0,"Fraction of ASes",""),
                add=ifelse(x==0,F,T)
                )
            #par(new=T)
         }
        for(j in c(0:4)) {
            plot.ecdf(
                ard.split[[as.character(i*5+j)]][['as_count']],
                xlim=xlims,
                col=colvec[j + 1],
                verticals=TRUE,
                do.points=showpoints,
                lwd=2,
                main='',
                xaxt='n',
                yaxt='n',
                xlab="",
                ylab="",
                add=T
                )
            #par(new=T)
        }
#        plot.ecdf(
#            ard.split[[as.character(i*5+j)]][['as_count']],
#            xlim=xlims,
#            col=colvec[5],
#            verticals=TRUE,
#            do.points=showpoints,
#            lwd=2,
#            main='',
#            #xlab="",
#            #ylab=""
#            xlab="Number of weeks at rank",
#            ylab="",#expression(P(X<=x)),
#            add=T
#            )
#        grid()
        legend(
            'bottomright',
            legend=paste('CDF of rank', (i*5+1):(i*5+5)),
            col=colvec,
            lwd=2:2,
            lty=1:1,
            inset=0.02,
            bg='white',
            cex=cex
            )
        par(new=F)
    }
}

pdf_cr_rank_cdfs <- function() {
    pdf("cr_rank_cdfs.pdf", width=6, height=4)
    plot_cr_rank_cdfs()
    dev.off()
}
