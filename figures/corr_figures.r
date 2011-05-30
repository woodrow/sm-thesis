make_plots <- function() {
    # MAIN FIGURES
    ###############################################################################
    rm(list=ls(), inherits=T)
    load('correlation_data/_complete_1997-11-09_2011-01-30_0_29_FALSE__correlation.RData',
    envir=globalenv())
    source('correlation_plot.r')
    #### netgain, 1997-2010
    pdf('behavior-netgain-1997_2011-corr.pdf', width=6, height=6)
    layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=c(1), heights=c(1,1))
    par(mar=c(5,5,2,2))
    plot_corr(series='netgain', explicit_xlims=c(-500,500), control=F)
    par(mar=c(6,5,1,2))
    plot_corr(series='netgain', explicit_xlims=c(-50,50), control=T)
    dev.off()

    #### rel_netgain, 1997-2010
    pdf('behavior-rel_netgain-1997_2011-corr.pdf', width=6, height=6)
    layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=c(1), heights=c(1,1))
    par(mar=c(5,5,2,2))
    plot_corr(series='rel_netgain', control=F)
    par(mar=c(6,5,1,2))
    plot_corr(series='rel_netgain', control=T)
    dev.off()

    #### netsnow, 1997-2010
    pdf('behavior-netsnow-1997_2011-corr.pdf', width=6, height=6)
    layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=c(1), heights=c(1,1))
    par(mar=c(5,5,2,2))
    plot_corr(series='netsnow', control=F, explicit_xlims=c(-500,500))
    par(mar=c(6,5,1,2))
    plot_corr(series='netsnow', control=T, explicit_xlims=c(-50,50));
    dev.off()

    #### rel_netsnow, 1997-2010
    pdf('behavior-rel_netsnow-1997_2011-corr.pdf', width=6, height=6)
    layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=c(1), heights=c(1,1))
    par(mar=c(5,5,2,2))
    plot_corr(series='rel_netsnow', control=F)
    par(mar=c(6,5,1,2))
    plot_corr(series='rel_netsnow', control=T)
    dev.off()

    #### frac_deagg, 1997-2010
    pdf('behavior-frac_deagg-1997_2011-corr.pdf', width=6, height=6)
    layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=c(1), heights=c(1,1))
    par(mar=c(5,5,2,2))
    plot_corr(series='frac_deagg', control=F)
    par(mar=c(6,5,1,2))
    plot_corr(series='frac_deagg', control=T)
    dev.off()

    #### deagg_factor, 1997-2010
    pdf('behavior-deagg_factor-1997_2011-corr.pdf', width=6, height=6)
    layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=c(1), heights=c(1,1))
    par(mar=c(5,5,2,2))
    plot_corr(series='deagg_factor', control=F, xlog=T)
    par(mar=c(6,5,1,2))
    plot_corr(series='deagg_factor', control=T, xlog=T, explicit_xlims=c(1,120))
    dev.off()

    #### frac_deagg_special, 1997-2010
    pdf('behavior-frac_deagg-1997_2011-special_tc.pdf', width=6, height=3)
    par(mar=c(5,5,2,2))
    plot_special(
    'correlation_data/_complete_1997-11-09_2011-01-30_0_29_FALSE__correlation.RData'
    , series='frac_deagg', tcb='B')
    dev.off()

    #### frac_deagg_special, 1997-2010
    pdf('behavior-deagg_factor-1997_2011-special_tc.pdf', width=6, height=3)
    par(mar=c(5,5,2,2))
    plot_special(
    'correlation_data/_complete_1997-11-09_2011-01-30_0_29_FALSE__correlation.RData'
    , series='deagg_factor', tcb='B', xlog=T)
    dev.off()

    # SPECIAL FIGURES
    ###############################################################################
    fnames=paste('correlation_data/_complete_',
        c('1997-11-09_2000-12-31', '2000-12-31_2003-12-28',
          '2003-12-28_2006-12-31', '2006-12-31_2009-12-27'),
        '_0_29_FALSE__correlation.RData', sep='')

    pdf('behavior-frac_deagg-all-special_t.pdf', width=6, height=4)
    par(mar=c(5,5,2,2))
    plot_special(fnames, series='frac_deagg', tcb='T', legend_loc='topleft')
    dev.off()

    pdf('behavior-frac_deagg-all-special_c.pdf', width=6, height=4)
    par(mar=c(5,5,2,2))
    plot_special(fnames, series='frac_deagg', tcb='C')
    dev.off()

    pdf('behavior-deagg_factor-all-special_t.pdf', width=6, height=4)
    par(mar=c(5,5,2,2))
    plot_special(fnames, series='deagg_factor', tcb='T', xlog=T)
    dev.off()

    pdf('behavior-deagg_factor-all-special_c.pdf', width=6, height=4)
    par(mar=c(5,5,2,2))
    plot_special(fnames, series='deagg_factor', tcb='C', xlog=T)
    dev.off()

    # APPENDIX FIGURES
    ###############################################################################
    rm(list=ls(), inherits=T)

    fnames=paste('correlation_data/_complete_',
        c('1997-11-09_2000-12-31', '2000-12-31_2003-12-28',
          '2003-12-28_2006-12-31', '2006-12-31_2009-12-27'),
        '_0_29_FALSE__correlation.RData', sep='')

    for(filename in fnames) {
        rm(list=ls()[!ls() %in% c('fnames', 'filename')], inherits=T)
        load(filename, envir=globalenv())
        source('correlation_plot.r')
        dates = sapply(gregexpr('\\d{4}', filename, perl=T)[[1]],
            function(x){return(as.integer(
            substr(filename, x, x+3)))})
        dates[1] = dates[1] + 1
#         if(dates[2] < 2011) {
#             dates[2] = dates[2] + 1
#         }
        datestr = paste(dates, collapse='_')

    #### netgain, 1998-2001
        pdf(paste('behavior-netgain-', datestr, '-corr.pdf', sep=''),
            width=6, height=6)
        layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=c(1), heights=c(1,1))
        par(mar=c(5,5,2,2))
        plot_corr(series='netgain', explicit_xlims=c(-500,500), control=F)
        par(mar=c(6,5,1,2))
        plot_corr(series='netgain', explicit_xlims=c(-50,50), control=T)
        dev.off()

    #### rel_netgain, 1998-2001
        pdf(paste('behavior-rel_netgain-', datestr, '-corr.pdf', sep=''),
            width=6, height=6)
        layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=c(1), heights=c(1,1))
        par(mar=c(5,5,2,2))
        plot_corr(series='rel_netgain', control=F)
        par(mar=c(6,5,1,2))
        plot_corr(series='rel_netgain', control=T)
        dev.off()

    #### netsnow, 1998-2001
        pdf(paste('behavior-netsnow-', datestr, '-corr.pdf', sep=''),
            width=6, height=6)
        layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=c(1), heights=c(1,1))
        par(mar=c(5,5,2,2))
        plot_corr(series='netsnow', control=F, explicit_xlims=c(-500,500))
        par(mar=c(6,5,1,2))
        plot_corr(series='netsnow', control=T, explicit_xlims=c(-50,50));
        dev.off()

    #### rel_netsnow, 1998-2001
        pdf(paste('behavior-rel_netsnow-', datestr, '-corr.pdf', sep=''),
            width=6, height=6)
        layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=c(1), heights=c(1,1))
        par(mar=c(5,5,2,2))
        plot_corr(series='rel_netsnow', control=F)
        par(mar=c(6,5,1,2))
        plot_corr(series='rel_netsnow', control=T)
        dev.off()

    #### frac_deagg, 1998-2001
        pdf(paste('behavior-frac_deagg-', datestr, '-corr.pdf', sep=''),
            width=6, height=6)
        layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=c(1), heights=c(1,1))
        par(mar=c(5,5,2,2))
        plot_corr(series='frac_deagg', control=F)
        par(mar=c(6,5,1,2))
        plot_corr(series='frac_deagg', control=T)
        dev.off()

    #### deagg_factor, 1998-2001
        pdf(paste('behavior-deagg_factor-', datestr, '-corr.pdf', sep=''),
            width=6, height=6)
        layout(matrix(c(1,2), 2, 1, byrow = TRUE), widths=c(1), heights=c(1,1))
        par(mar=c(5,5,2,2))
        plot_corr(series='deagg_factor', control=F, xlog=T)
        par(mar=c(6,5,1,2))
        plot_corr(series='deagg_factor', control=T, xlog=T, explicit_xlims=c(1,120))
        dev.off()

    # #### frac_deagg_special, 1998-2001
    #     pdf(paste('behavior-frac_deagg-', datestr, '-special_tc.pdf', sep=''),
    #         width=6, height=3)
    #     par(mar=c(5,5,2,2))
    #     plot_special(filename, series='frac_deagg', tcb='B')
    #     dev.off()
    }
}

pdf_frac_deagg_series <- function() {
    rm(list=ls(), inherits=T)

    fnames=paste('correlation_data/_complete_',
        c('1997-11-09_2000-12-31', '2000-12-31_2003-12-28',
          '2003-12-28_2006-12-31', '2006-12-31_2009-12-27'),
        '_0_29_FALSE__correlation.RData', sep='')

    pdf(paste('behavior-frac_deagg-vseries-special_tc.pdf', sep=''),
        width=6, height=8)
    par(mar=c(2,5,1,2))

    layout(matrix(c(1,2,3,4), 4, 1, byrow = TRUE), heights=c(1, 1, 1, 1.2))

    for(filename in fnames) {
        rm(list=ls()[!ls() %in% c('fnames', 'filename')], inherits=T)
        print(filename)
        load(filename, envir=globalenv())
        print('pre-ls')
        ls()
        print('post-ls')
        source('correlation_plot.r')
        dates = sapply(gregexpr('\\d{4}', filename, perl=T)[[1]],
            function(x){return(as.integer(
            substr(filename, x, x+3)))})
        dates[1] = dates[1] + 1
#         if(dates[2] < 2011) {
#             dates[2] = dates[2] + 1
#         }
        datestr = paste(dates, collapse='_')

        if(dates[1] > 2006) {
            par(mar=c(5,5,1,2))
        }
        plot_special(filename, series='frac_deagg', tcb='B',
            legend_loc=ifelse(dates[1] > 2002, 'topleft', 'bottomright'))
    }
    dev.off()
}

pdf_deagg_factor_series <- function() {
    rm(list=ls(), inherits=T)

    fnames=paste('correlation_data/_complete_',
        c('1997-11-09_2000-12-31', '2000-12-31_2003-12-28',
          '2003-12-28_2006-12-31', '2006-12-31_2009-12-27'),
        '_0_29_FALSE__correlation.RData', sep='')

    pdf(paste('behavior-deagg_factor-vseries-special_tc.pdf', sep=''),
        width=6, height=8)
    par(mar=c(2,5,1,2))

    layout(matrix(c(1,2,3,4), 4, 1, byrow = TRUE), heights=c(1, 1, 1, 1.2))

    for(filename in fnames) {
        rm(list=ls()[!ls() %in% c('fnames', 'filename')], inherits=T)
        print(filename)
        load(filename, envir=globalenv())
        print('pre-ls')
        ls()
        print('post-ls')
        source('correlation_plot.r')
        dates = sapply(gregexpr('\\d{4}', filename, perl=T)[[1]],
            function(x){return(as.integer(
            substr(filename, x, x+3)))})
        dates[1] = dates[1] + 1
#         if(dates[2] < 2011) {
#             dates[2] = dates[2] + 1
#         }
        datestr = paste(dates, collapse='_')

        if(dates[1] > 2006) {
            par(mar=c(5,5,1,2))
        }
        plot_special(filename, series='deagg_factor', tcb='B', xlog=T,
            legend_loc='bottomright', explicit_xlims=c(1,100))
    }
    dev.off()
}

make_absolutely_everything <- function() {
    make_plots()
    pdf_frac_deagg_series()
    pdf_deagg_factor_series()
}

#
# rm(list=ls())
# load('correlation_data/_complete_2000-12-31_2003-12-28_0_29_FALSE__correlation.RData')
# source('correlation_plot.r')
#
# rm(list=ls())
# load('correlation_data/_complete_2003-12-28_2006-12-31_0_29_FALSE__correlation.RData')
# source('correlation_plot.r')
#
# rm(list=ls())
# load('correlation_data/_complete_2006-12-31_2009-12-27_0_29_FALSE__correlation.RData')
# source('correlation_plot.r')

# filedates = c('1997-11-09_2000-12-31', '2000-12-31_2003-12-28',
#     '2003-12-28_2006-12-31', '2006-12-31_2009-12-27')
# datafiles = paste('correlation_data/_complete_', filedates,
#     '_0_29_FALSE__correlation.RData', sep='')
# plot_special(datafiles, series='frac_deagg', tcb='B')
