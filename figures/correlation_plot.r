library(RColorBrewer)
library(digest)

cex=0.75

do_setup <- function() {
#     if(exists('correlation_figures_setup_done')) {
#         return()
#     }

    for (colname in paste("delta_", row_delta_days, sep="")) {
        delta_dists[[colname]][['deagg_factor']] = rep(NA,
            length(delta_dists[[colname]][['frac_deagg']]))
        con_delta_dists[[colname]][['deagg_factor']] = rep(NA,
            length(con_delta_dists[[colname]][['frac_deagg']]))
    }

    # PRELIMINARY THE FIRST, ADD TO TREATMENT GROUP:
    frac_deagg_delta_0 = rep(
        NA, length(delta_dists[['delta_30']][['frac_deagg']]))
    deagg_factor_delta_0 = rep(
        NA, length(delta_dists[['delta_30']][['frac_deagg']]))
    aix = 1
    for(origin_as in names(as_deltas)) {
        for(ai in c(1:length(as_deltas[[origin_as]]))) {
            for (colname in paste("delta_", row_delta_days, sep="")) {
                delta_dists[[colname]][aix,]$deagg_factor = (
                    (as_deltas[[origin_as]][[ai]][colname,]$netsnow
                    + as_deltas[[origin_as]][[ai]]['initial',]$netsnow)
                    / (as_deltas[[origin_as]][[ai]][colname,]$netsnow
                    + as_deltas[[origin_as]][[ai]]['initial',]$netsnow
                    - as_deltas[[origin_as]][[ai]][colname,]$netgain
                    - as_deltas[[origin_as]][[ai]]['initial',]$netgain))
            }
            aix = aix + 1
            frac_deagg_delta_0[aix] = (
                as_deltas[[origin_as]][[ai]]['initial',]$netgain /
                as_deltas[[origin_as]][[ai]]['initial',]$netsnow)
            deagg_factor_delta_0[aix] = (
                as_deltas[[origin_as]][[ai]]['initial',]$netsnow /
                (as_deltas[[origin_as]][[ai]]['initial',]$netsnow
                - as_deltas[[origin_as]][[ai]]['initial',]$netgain))
        }
    }
    delta_dists <<- delta_dists
    frac_deagg_delta_0 <<- frac_deagg_delta_0
    deagg_factor_delta_0 <<- deagg_factor_delta_0

    # PRELIMINARY THE SECOND, ADD TO CONTROL GROUP:
    con_frac_deagg_delta_0 = rep(
        NA, length(con_delta_dists[['delta_30']][['frac_deagg']]))
    con_deagg_factor_delta_0 = rep(
        NA, length(con_delta_dists[['delta_30']][['frac_deagg']]))
    aix = 1
    for(origin_as in names(con_as_deltas)) {
        for(ai in c(1:length(con_as_deltas[[origin_as]]))) {
            for (colname in paste("delta_", row_delta_days, sep="")) {
                con_delta_dists[[colname]][aix,]$deagg_factor = (
                    (con_as_deltas[[origin_as]][[ai]][colname,]$netsnow
                    + con_as_deltas[[origin_as]][[ai]]['initial',]$netsnow)
                    / (con_as_deltas[[origin_as]][[ai]][colname,]$netsnow
                    + con_as_deltas[[origin_as]][[ai]]['initial',]$netsnow
                    - con_as_deltas[[origin_as]][[ai]][colname,]$netgain
                    - con_as_deltas[[origin_as]][[ai]]['initial',]$netgain))
            }
            aix = aix + 1
            con_frac_deagg_delta_0[aix] = (
                con_as_deltas[[origin_as]][[ai]]['initial',]$netgain /
                con_as_deltas[[origin_as]][[ai]]['initial',]$netsnow)
            con_deagg_factor_delta_0[aix] = (
                con_as_deltas[[origin_as]][[ai]]['initial',]$netsnow /
                (con_as_deltas[[origin_as]][[ai]]['initial',]$netsnow
                - con_as_deltas[[origin_as]][[ai]]['initial',]$netgain))
        }
    }
    con_delta_dists <<- con_delta_dists
    con_frac_deagg_delta_0 <<- con_frac_deagg_delta_0
    con_deagg_factor_delta_0 <<- con_deagg_factor_delta_0

    correlation_figures_setup_done <<- TRUE
}

# as_deltas[[ <<insert_as_num>> ]]['initial',]$netgain/$netsnow for delta_0
jet.colors = colorRampPalette(
#c("blue", "darkcyan", "gold", "#FF7F00", "red", "#7F0000"))

c("#000066", "blue3", "#007FFF", "cyan3",
    "#66cc66", "gold", "#FF7F00", "red", "#7F0000"))

plot_corr <- function(
    explicit_xlims=NA, xlog=F, series='frac_deagg', control=F)
# TODO: features
# - arguments for xlims, x-axis log
# - jet color
# - typename
{
    xlabs = vector()
    xlabs['netgain'] = expression(Delta~aggregable~prefixes~"(netgain)")
    xlabs['rel_netgain'] = expression(Relative~Delta~aggregable~prefixes~
        "(fraction of initial netgain)")
    xlabs['netsnow'] = expression(Delta~announced~prefixes~"(netsnow)")
    xlabs['rel_netsnow'] = expression(Relative~Delta~announced~prefixes~
        "(fraction of initial netsnow)")
    xlabs['frac_deagg']  = expression(
        Fraction~of~announced~prefixes~that~are~aggregable~(netgain/netsnow))
    xlabs['deagg_factor']  = expression(
        Deaggregation~factor~(netsnow/(netsnow-netgain)))

    legend_deltas =  c(30,60,90,180,365,547,730)

#    x11() #####################################################################
    cdfs = list()
    if(is.na(explicit_xlims)) {
        xlims = c(0,0)
    }
    if(control) {
        deltas = con_delta_dists
    } else {
        deltas = delta_dists
    }
    if(series == 'frac_deagg' || series == 'deagg_factor') {
        if(control) {
            d = eval(parse(text=paste('con_', series, '_delta_0', sep='')))
        } else {
            d = eval(parse(text=paste(series, '_delta_0', sep='')))
        }
        for(i in c(1:length(d))) {
            v = d[i]
            if(is.na(v) || is.infinite(v) || is.nan(v)) {
                d[i] = 0
            }
        }
        cdf = ecdf(d)
        cdfs[['delta_0']] = cdf
        if(is.na(explicit_xlims)) {
            xlims = c(min(xlims, knots(cdf)), max(xlims, knots(cdf)))
        }
        legend_deltas = c(0, legend_deltas)
    }

    legend_pre_text=character()
    legend_pre_text['netgain'] = 'expression(Delta~prefixes[~t+'
    legend_pre_text['rel_netgain'] = 'expression(Delta~prefixes[~t+'
    legend_pre_text['netsnow'] = 'expression(Delta~prefixes[~t+'
    legend_pre_text['rel_netsnow'] = 'expression(Delta~prefixes[~t+'
    legend_pre_text['frac_deagg']  = 'expression(fraction[~t+'
    legend_pre_text['deagg_factor'] = 'expression(factor[~t+'
    legend_post_text=character()
    legend_post_text['netgain'] = '])'
    legend_post_text['rel_netgain'] = ']/prefixes[~t+0])'
    legend_post_text['netsnow'] =  '])'
    legend_post_text['rel_netsnow'] = ']/prefixes[~t+0])'
    legend_post_text['frac_deagg']  = '])'
    legend_post_text['deagg_factor'] = '])'

    legend_names = eval(parse(file='', text=paste('c(',
        paste(legend_pre_text[series], legend_deltas, legend_post_text[series],
        sep='', collapse=', '), ')', sep='', collapse='')))

    for(name in names(deltas)) {
        for(i in c(1:length(deltas[[name]][[series]]))) {
            v = deltas[[name]][[series]][i]
            if(is.na(v) || is.infinite(v) || is.nan(v)) {
                deltas[[name]][[series]][i] = 0
            }
        }
        cdf = ecdf(
            deltas[[name]][[series]][!is.na(deltas[[name]][[series]])])
        cdfs[[name]] = cdf
        if(is.na(explicit_xlims)) {
            xlims = c(min(xlims, knots(cdf)), max(xlims, knots(cdf)))
        }
    }
    if(is.na(explicit_xlims)) {
        if(length(grep('rel', series)) > 0) {
            xlims = c(max(min(xlims), -1), min(max(xlims), 5))
        }
    } else {
        xlims = explicit_xlims
    }
    if(xlog==T) {
        xlims=c(1, xlims[2])
    }
    print(xlims)

    colors = jet.colors(7)
    #colors = brewer.pal(7,'Spectral')
    if(legend_deltas[1] == 0) {
        colors = c('grey40', colors)
    }
    index = 1
    par(cex=cex)
    lwd=2
    do.points=T
    verticals=T
    #par(mar=c(6,5,2,2))
    if(!identical(names(cdfs), paste('delta_', legend_deltas, sep=''))) {
        print('### NAMING PROBLEM -- delta ordering doesn\'t match###')
    }
    for(name in names(cdfs)) {
        if(index == 1) {
            plot(
                cdfs[[name]],
                col=colors[index],
                xlim=xlims,
                main="",
                ylab=expression(P(X <= x)),
                xlab=xlabs[series],
                add=F,
                do.points=do.points,
                verticals=verticals,
                lwd=lwd,
                log=ifelse(xlog,'x','')
                #xlog=xlog
            )
        } else {
            plot(
                cdfs[[name]],
                col=colors[index],
                xlim=xlims,
                xaxt="n",
                yaxt="n",
                ylab="",
                xlab="",
                main="",
                add=T,
                do.points=do.points,
                verticals=verticals,
                lwd=lwd,
                log=ifelse(xlog,'x','')
                #xlog=xlog
            )
        }
        index = index + 1
    }
    mtext(ifelse(control, "(Control)", "(Treatment)"),
        side=1, line=4, cex=cex)
    grid(col='grey40')
    legend(
        x='bottomright',
        legend=legend_names,
        col=colors,
        lty=1,
        lwd=2,
        bg="white",
        inset=0.02,
        cex=cex
    )
}


plot_special <- function(
    rdata_paths, explicit_xlims=NA, xlog=F, series='frac_deagg', tcb='T',
    legend_loc='bottomright')
{
    tcb = tolower(tcb)

    xlabs = vector()
    xlabs['netgain'] = expression(Delta~aggregable~prefixes~"(netgain)")
    xlabs['rel_netgain'] = expression(Relative~Delta~aggregable~prefixes~
        "(fraction of initial netgain)")
    xlabs['netsnow'] = expression(Delta~announced~prefixes~"(netsnow)")
    xlabs['rel_netsnow'] = expression(Relative~Delta~announced~prefixes~
        "(fraction of initial netsnow)")
    xlabs['frac_deagg']  = expression(
        Fraction~of~announced~prefixes~that~are~aggregable~(netgain/netsnow))
    xlabs['deagg_factor']  = expression(
        Deaggregation~factor~(netsnow/(netsnow-netgain)))

    cdfs = list()
    dates = list()
    legend_components = list()
    index = 0
    lcix = 1

    for(path in rdata_paths) {
        print(path)
        load(path, envir=globalenv())
        if(exists('correlation_figures_setup_done')){
            rm(correlation_figures_setup_done, inherits=T)
        }
        print(exists('correlation_figures_setup_done'))
        do_setup()
        dates[[path]] = sapply(gregexpr('\\d{4}', path, perl=T)[[1]],
            function(x){return(as.integer(
            substr(path, x, x+3)))})
        dates[[path]][1] = dates[[path]][1] + 1
        #dates[[path]][2] = dates[[path]][2] + 1

        if(is.na(explicit_xlims)) {
            xlims = c(0,0)
        }

        # print('delta_dists')
        # print(digest(delta_dists))
        # print('con_delta_dists')
        # print(digest(con_delta_dists))

        if(tcb == 't' || tcb == 'b') {
            if(series == 'frac_deagg' || series == 'deagg_factor') {
                d = eval(parse(text=paste(series, '_delta_0', sep='')))
                for(i in c(1:length(d))) {
                    v = d[i]
                    if(is.na(v) || is.infinite(v) || is.nan(v)) {
                        d[i] = 0
                    }
                }
                cdf = ecdf(d)
                cdfs[[paste(index, 'delta_0', sep='')]] = cdf
                if(is.na(explicit_xlims)) {
                    xlims = c(min(xlims, knots(cdf)), max(xlims, knots(cdf)))
                }
                legend_components[[lcix]] = c('treatment', 0,
                    paste(dates[[path]], collapse='-'))
                lcix = lcix + 1
            } else {
                for(i in c(1:length(delta_dists[['delta_30']][[series]]))) {
                    v = delta_dists[['delta_30']][[series]][i]
                    if(is.na(v) || is.infinite(v) || is.nan(v)) {
                        delta_dists[['delta_30']][[series]][i] = 0
                    }
                }
                cdf = ecdf(delta_dists[['delta_30']][[series]][
                    !is.na(delta_dists[['delta_30']][[series]])])
                cdfs[[paste(index, 'delta_30', sep='')]] = cdf
                if(is.na(explicit_xlims)) {
                    xlims = c(min(xlims, knots(cdf)), max(xlims, knots(cdf)))
                }
                legend_components[[lcix]] = c('treatment', 30,
                    paste(dates[[path]], collapse='-'))
                lcix = lcix + 1
            }
            for(i in c(1:length(delta_dists[['delta_730']][[series]]))) {
                v = delta_dists[['delta_730']][[series]][i]
                if(is.na(v) || is.infinite(v) || is.nan(v)) {
                    delta_dists[['delta_730']][[series]][i] = 0
                }
            }
            cdf = ecdf(delta_dists[['delta_730']][[series]][
                !is.na(delta_dists[['delta_730']][[series]])])
            cdfs[[paste(index, 'delta_730', sep='')]] = cdf
            if(is.na(explicit_xlims)) {
                xlims = c(min(xlims, knots(cdf)), max(xlims, knots(cdf)))
            }
            legend_components[[lcix]] = c('treatment', 730,
                paste(dates[[path]], collapse='-'))
            lcix = lcix + 1
        }

        if(tcb == 'c' || tcb == 'b') {
            if(series == 'frac_deagg' || series == 'deagg_factor') {
                d = eval(parse(text=paste('con_', series, '_delta_0', sep='')))
                for(i in c(1:length(d))) {
                    v = d[i]
                    if(is.na(v) || is.infinite(v) || is.nan(v)) {
                        d[i] = 0
                    }
                }
                cdf = ecdf(d)
                cdfs[[paste(index, 'con_delta_0', sep='')]] = cdf
                if(is.na(explicit_xlims)) {
                    xlims = c(min(xlims, knots(cdf)), max(xlims, knots(cdf)))
                }
                legend_components[[lcix]] = c('control', 0,
                    paste(dates[[path]], collapse='-'))
                lcix = lcix + 1
            } else {
                for(i in c(1:length(con_delta_dists[['delta_30']][[series]]))) {
                    v = con_delta_dists[['delta_30']][[series]][i]
                    if(is.na(v) || is.infinite(v) || is.nan(v)) {
                        con_delta_dists[['delta_30']][[series]][i] = 0
                    }
                }
                cdf = ecdf(con_delta_dists[['delta_30']][[series]][
                    !is.na(con_delta_dists[['delta_30']][[series]])])
                cdfs[[paste(index, 'con_delta_30', sep='')]] = cdf
                if(is.na(explicit_xlims)) {
                    xlims = c(min(xlims, knots(cdf)), max(xlims, knots(cdf)))
                }
                legend_components[[lcix]] = c('control', 30,
                    paste(dates[[path]], collapse='-'))
                lcix = lcix + 1
            }
            for(i in c(1:length(con_delta_dists[['delta_730']][[series]]))) {
                v = con_delta_dists[['delta_730']][[series]][i]
                if(is.na(v) || is.infinite(v) || is.nan(v)) {
                    con_delta_dists[['delta_730']][[series]][i] = 0
                }
            }
            cdf = ecdf(con_delta_dists[['delta_730']][[series]][
                !is.na(con_delta_dists[['delta_730']][[series]])])
            cdfs[[paste(index, 'con_delta_730', sep='')]] = cdf
            if(is.na(explicit_xlims)) {
                xlims = c(min(xlims, knots(cdf)), max(xlims, knots(cdf)))
            }
            legend_components[[lcix]] = c('control', 730,
                paste(dates[[path]], collapse='-'))
            lcix = lcix + 1
        }
        index = index + 1
        print(path)
        rm(delta_dists, inherits=T)
        rm(con_delta_dists, inherits=T)
    }
    # plot_special_cdfs <<- cdfs
    # plot_special_lcs <<- legend_components
    # plot_special_index <<- index
    # plot_special_xlims <<- xlims

    # legend_components = plot_special_lcs
    # cdfs = plot_special_cdfs
    # index = plot_special_index
    # xlims = plot_special_xlims
    print(names(cdfs))
    ###########################################################################
    if(is.na(explicit_xlims)) {
        if(length(grep('rel', series)) > 0) {
            xlims = c(max(min(xlims), -1), min(max(xlims), 5))
        }
    } else {
        xlims = explicit_xlims
    }
    if(xlog==T) {
        xlims=c(1, xlims[2])
    }
    print(xlims)

    colors = brewer.pal(12, 'Paired')[c(1,2,5,6,3,4,7,8,9,10,11,12)]
    colors = colors[1:(index * 4)]
    if(tcb == 't') {
        colors = colors[rep(c(T, T, F, F), index)]
        #colors = c(sapply(colors, function(x){c(x,x)}))
        #colors = rep(colors, 2)
    } else if(tcb == 'c') {
        colors = colors[rep(c(F, F, T, T), index)]
        #colors = brewer.pal((index * 2), 'Paired')[1:(index * 2)]
    }
    colors = colors[!is.na(colors)]
    colors = c(colors, '#AAAAAA', '#666666')
    print(index)
    print(colors)
    print("############################################################")
    index = 1
    par(cex=cex)

    legend_pre_text=character()
    legend_pre_text['netgain'] = 'Delta~prefixes['
    legend_pre_text['rel_netgain'] = 'Delta~prefixes['
    legend_pre_text['netsnow'] = 'Delta~prefixes['
    legend_pre_text['rel_netsnow'] = 'Delta~prefixes['
    legend_pre_text['frac_deagg']  = 'fraction['
    legend_pre_text['deagg_factor'] = 'factor['
    legend_post_text=character()
    legend_post_text['netgain'] = ']'
    legend_post_text['rel_netgain'] = ']/prefixes[t==0]'
    legend_post_text['netsnow'] =  ']'
    legend_post_text['rel_netsnow'] = ']/prefixes[t==0]'
    legend_post_text['frac_deagg']  = ']'
    legend_post_text['deagg_factor'] = ']'

    legend_names = character()
    for(i in 1:length(legend_components)) {
        # legend_names[i] = eval(parse(file='', text=paste(
        #     'expression(', legend_components[[i]][1], '~',
        #     legend_pre_text[series], legend_components[[i]][3],
        #     legend_post_text[series], '~t+', legend_components[[i]][2], ')',
        #     sep='')))
        # legend_names[i] = eval(parse(file='', text=paste(
        #     'expression(', legend_components[[i]][1], '[',
        #     legend_components[[i]][3], ']~"t+', legend_components[[i]][2],
        #     '")', sep='')))

        legend_names[i] = paste(legend_components[[i]][1], ',\t',
            legend_components[[i]][3], ', t+', legend_components[[i]][2],
            sep='')

        legend_names[i] = paste(legend_components[[i]][3], ', ',
            legend_components[[i]][1], ', t+', legend_components[[i]][2],
            sep='')
    }

    lwd = 2
    for(name in names(cdfs)) {
        if(index == 1) {
            plot(
                cdfs[[name]],
                col=colors[index],
                xlim=xlims,
                main="",
                ylab=expression(P(X <= x)),
                xlab=xlabs[series],
                add=F,
                do.points=F,
                verticals=T,
                log=ifelse(xlog,'x',''),
                lwd=lwd
                #xlog=xlog
            )
        } else {
            if((length(grep('730', name)) > 0 && tcb == 'b')) {
                is_730 = F #T
            } else {
                is_730 = F
            }
            plot(
                cdfs[[name]],
                col=colors[index],
                xlim=xlims,
                xaxt="n",
                yaxt="n",
                ylab="",
                xlab="",
                main="",
                add=T,
                do.points=F,
                verticals=T,
                log=ifelse(xlog,'x',''),
                lty=ifelse(is_730, 3, 1),
                lwd=lwd
                #xlog=xlog
            )
        }
        index = index + 1
        print("plot!")
        print(name)
    }
    grid(col='grey40')
    print('legend')
    print(length(legend_names))
    print(length(colors))
    legend(
        x=legend_loc,
        legend=legend_names,
        col=colors,
        #lty=if(tcb=='b'){rep(c(1,3),length(legend_names)/2)} else{1},
        lty=1,
        lwd=2,
        bg="white",
        inset=0.02,
        cex=cex
    )
}

# PRELIMINARIES TO GET SET UP
row_delta_days = c(30, 60, 90, 180, 365, 547, 730)
row_names = c("initial", paste("delta_", row_delta_days, sep=""))
do_setup()
