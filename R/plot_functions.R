#' @title Functions to create (mostly) final plots for McNellis2021 manuscript.
#'
#' @description
#'
#' Input data comes from the `McNellis2021/data`` directory.
#'
#' Setup script is in McNellis2021/exec/make_figures.R
#'
#' See the README for additional details.
#'
#' @author Brandon McNellis
#' @name plot_functions
NULL
#' @rdname plot_functions
#' @export
Multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    # BEM note: this is pretty much verbatim copied from the R cookbook
    #
    # Multiple plot function
    #
    # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
    # - cols:   Number of columns in layout
    # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
    #
    # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
    # then plot 1 will go in the upper left, 2 will go in the upper right, and
    # 3 will go all the way across the bottom.
    require(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots==1) {
        print(plots[[1]])

    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}
#' @rdname plot_functions
#' @export
PlotOverallImportance <- function(data_df) {
    featureImportance_mort <- data_df

    p1 <- ggplot(featureImportance_mort, aes(x = reorder(Feature, Importance), y = Importance, fill = class)) +
        geom_bar(stat = "identity", width = 0.65) +
        coord_flip() +
        theme_light(base_size = 8) +
        theme(axis.title.x = element_text(size = 8, color = "black"),
              axis.title.y = element_blank(),
              axis.text.x  = element_text(size = 8, color = "black"),
              axis.text.y  = element_text(size = 6, color = "black")) +
        ylab('Mean decrease in mortality model accuracy') +
        ylim(0, 0.105) +
        scale_fill_grey(start = 0.1, end = 0.7) +
        #theme(legend.position = "none")
        theme(
            #legend.title = element_text(size = 18, color = 'black', face = 'bold'),
            legend.title = element_blank(),
            legend.text = element_text(size = 8),
            legend.justification = c(1,0),
            legend.position=c(0.9, 0.10),
            legend.background = element_rect(colour = 'black', fill = 'white', linetype = 'solid'),
            legend.key = element_blank()) +
        theme(plot.margin = grid::unit(c(2, 2, 2, 2), 'points'))

    return(p1)
}
#' @rdname plot_functions
#' @export
PlotAgentImportance <- function(data_df) {
    featureImportance_agent <- data_df

    p2 <- ggplot(featureImportance_agent, aes(x = reorder(Feature, Importance), y = Importance, fill = class)) +
        geom_bar(stat = "identity", width = 0.65) +
        coord_flip() +
        theme_light(base_size = 8) +
        theme(axis.title.x = element_text(size = 8, color = "black"),
              axis.title.y = element_blank(),
              axis.text.x  = element_text(size = 8, color = "black"),
              axis.text.y  = element_text(size = 6, color = "black")) +
        ylab('Mean decrease in agent model accuracy') +
        ylim(0, 0.12) +
        scale_fill_grey(start = 0.1, end = 0.7) +
        labs(fill = 'Variable type') +
        theme(legend.title = element_text(size = 8, color = 'black', face = 'bold'),
              legend.text = element_text(size = 8),
              legend.justification = c(1,0),
              legend.position = c(0.9, 0.10),
              legend.background = element_rect(colour = 'black', fill = 'white', linetype = 'solid'),
              legend.key = element_blank()) +
        theme(plot.margin = grid::unit(c(2, 10, 2, 2), 'points'))
    p2
}
#' @rdname plot_functions
#' @export
PlotFeatureContrib <- function(plot_df, ylab0, panel_lab = NULL, panel_coord = NULL,
                               xlim0 = NULL, ylim0 = NULL, order_descending = TRUE) {
    require(ggplot2)
    stopifnot(all(
        'variable' %in% colnames(plot_df),
        'contribution' %in% colnames(plot_df),
        'sd' %in% colnames(plot_df)
    ))

    if (order_descending) {
        plot_df$variable <- as.factor(plot_df$variable)
        plot_df$variable <- reorder(plot_df$variable, plot_df$contribution * -1)
    }

    mc0 <- ggplot(data = plot_df, aes(x = variable, y = contribution)) +
        #geom_bar(stat = 'identity') +
        geom_bar(stat = 'identity', position = 'dodge') + # dodge makes side-by-side barplot
        geom_errorbar(aes(x = variable, ymin = contribution - sd, ymax = contribution + sd)) +
        theme_bw() +
        #theme(axis.text.x = element_text(angle = 45, hjust = 1),
        #      axis.text = element_text(size = 8)) +
        theme(axis.title.x = element_text(size = 8, color = "black"),
              axis.title.y = element_text(size = 8),
              axis.text.x  = element_text(angle = 45, hjust = 1, size = 8, color = "black"),
              axis.text.y  = element_text(size = 8, color = "black")) +
        xlab('') +
        ylab(ylab0) +
        theme(plot.margin = grid::unit(c(2, 2, 2, 2), 'points'))

    if (length(ylim0) > 0) {
        mc0 <- mc0 + ylim(ylim0)
    }
    if (length(xlim0) > 0) {
        mc0 <- mc0 + xlim(xlim0)
    }

    if (length(panel_lab) > 0) {
        #panel <- as.character(panel_lab)
        mc0 <- mc0 + annotate('text', panel_coord[1], panel_coord[2], label = panel_lab, size = 8)
    }

    return(mc0)
}
#' @rdname plot_functions
#' @export
PlotPartialDepend <- function(plot_df, y0, order_descending = T) {

    if (order_descending) {
        plot_df$SPCD <- as.factor(plot_df$SPCD)
        plot_df$SPCD <- reorder(plot_df$SPCD, plot_df$partial * -1)
    }

    q0 <- ggplot(plot_df, aes(x = SPCD, y = partial)) +
        geom_bar(stat = 'identity', fill = 'grey20', width = 0.65) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1, color = 'black'),
              axis.title.y = element_text(size = 8),
              axis.text.y = element_text(size = 8, color = 'black'),
              axis.title.x = element_blank()
              ) +
        xlab('') +
        ylab(y0) +
        theme(plot.margin = grid::unit(c(2, 2, 2, 2), 'points'))
    return(q0)
}
#' @rdname plot_functions
#' @export
PlotCorr <- function(plot_df) {
    h0df <- plot_df

    lmdf <- h0df[, c('baseline', 'future45')]
    colnames(lmdf) <- c('x', 'y')

    h0 <- ggplot(data = h0df, aes(x = baseline, y = future45)) +
        theme_bw() +
        geom_point(stat = 'identity', size = 0.8) +
        geom_smooth(method = 'lm', se = T, colour = 'black') +
        xlab(expression(paste('Recent mortality rate (% ', year^{-1}, ")"))) +
        ylab(expression(paste('Future mortality rate (% ', year^{-1}, ")"))) +
        theme(axis.text.x = element_text(size = 8, color = 'black'),
              axis.title.y = element_text(size = 8),
              axis.text.y = element_text(size = 8),
              axis.title.x = element_text(size = 8)
        ) +
        theme(plot.margin = grid::unit(c(2, 2, 2, 2), 'points')) +
        #annotate('text', size = 2.6, x = 2.2, y = 0, label = lm_eqn(lmdf), parse = T)
        geom_abline(slope = 1, intercept = 0)

    return(h0)
}
#' @rdname plot_functions
#' @export
HighResTiff <- function(plot_obj, file, width_in, height_in, resolution_dpi) {

    if (inherits(plot_obj, 'ggplot')) {
        tiff(filename = file, width = width_in, height = height_in, units = 'in', res = resolution_dpi)
        print(plot_obj)
        dev.off()
    } else if (inherits(plot_obj, 'list')) {
        tiff(filename = file, width = width_in, height = height_in, units = 'in', res = resolution_dpi)
        Multiplot(plot_obj[[1]], plot_obj[[2]])
        dev.off()
    }
    invisible()

}
#' @rdname plot_functions
#' @export
plot_diff_mort_map <- function(mort_df, diff_var = 'mort_rate', legend_title = NA,
                               scale_adjust = 1) {
    require(viridis)
    require(ggplot2)
    if (is.na(legend_title)) {
        lt0 <- 'Mortality rate'
    } else {
        lt0 <- legend_title
    }

    colnames(mort_df)[which(colnames(mort_df) == diff_var)] <- 'mort_rate'

    # Sanity checks:
    if (!('spData' %in% installed.packages()[, 1])) {
        stop('Need suggested package spData to plot the mortality map.')
    }

    # Obj prep for both graphs:
    #full_mort <- mort_df[, c('lon', 'lat', 'mort_rate', 'ECOSUBCD')]

    state_overlay <- spData::us_states[which(spData::us_states$REGION == 'West'), ]
    state_overlay <- state_overlay[-which(state_overlay$NAME == 'Wyoming'), ]

    # Obj prep for section graph:
    sects <- Cleland2007_eco_map[[2]]
    #mort_df$ECOSUBCD <- ClelandEcoregions::ScaleUpClelandCode(mort_df$ECOSUBCD)
    ecodf <- aggregate(mort_df$mort_rate, by = list(mort_df$ECOSUBCD), FUN = mean)
    colnames(ecodf) <- c('ECOSUBCD', 'mort_rate')
    s0 <- as.character(sects$MAP_UNIT_S)
    sects <- sects[which(s0 %in% ecodf$ECOSUBCD), ]
    vals <- ecodf$mort_rate[match(sects$MAP_UNIT_S, ecodf$ECOSUBCD)]
    vals <- round(vals, 5)

    a <- 0
    while(any(duplicated(vals))) {
        a <- a + 1
        vals[which(duplicated(vals))] <- vals[which(duplicated(vals))] + 1e-5
        if (a > 1000) stop('broke shit')
    }

    row.names(sects) <- as.character(vals)
    sects <- ggplot2::fortify(sects)
    sects$id <- as.numeric(sects$id)
    sects$id <- round(sects$id, 6)

    sv1 <- trunc_dec(min(vals), 'floor', level = 1)
    cat('\nsv1:', sv1)
    sv2 <- trunc_dec(max(vals), 'ceiling', level = 1)
    cat('\nsv2:', sv2, '\n')

    scales0 <- c(-5, 0, 10, 20, 30) * scale_adjust
    breaks0 <- seq(from = -10, to = 40, by = 2.5) * scale_adjust
    scales00 <- scales::rescale(scales0)
    colors0 <- viridis(5)

    out_plot1 <- ggplot() +
        geom_sf(aes_(), data = state_overlay) +
        geom_polygon(data = sects, mapping = aes(x = long, y = lat, group = group, fill = id),
                     colour = 'black', size = 1) +
        theme_bw(base_size = 8) +
        scale_fill_gradientn(colours = colors0,
                             values = scales00,
                             breaks = breaks0) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        xlab('') + ylab('')

    plot(out_plot1)
    invisible(out_plot1)
}
#' @rdname plot_functions
#' @export
make_mort_bar_plot <- function(plot_df) {

    plot_df$sdmin <- plot_df$rate_mean - plot_df$rate_sd
    plot_df$sdmax <- plot_df$rate_mean + plot_df$rate_sd
    plot_df$time <- ifelse(plot_df$time == 'Baseline (2000-2015)', 'Recent mortality', plot_df$time)
    plot_df$time <- ifelse(plot_df$time == 'RCP 4.5 (2010-2025)', 'Future mortality', plot_df$time)
    plot_df$time <- factor(plot_df$time, levels = c('Recent mortality', 'Future mortality'))

    w0 <- position_dodge(0.9)

    m0 <- ggplot(data = plot_df, aes(x = AGENT, fill = time)) +
        geom_bar(aes(y = rate_mean), stat = 'identity', position = w0) +
        geom_errorbar(aes(ymin = sdmin, ymax = sdmax, width = 0.5), position = w0) +

        scale_fill_grey(start = 0.4, end = 0.7) +
        theme_bw(base_size = 8) +
        theme(axis.text.x = element_text(color = 'black'),
              axis.text.y = element_text(color = 'black'),
              #axis.title = element_text(size = 12),
              legend.text = element_text(size = 10),
              legend.title = element_blank(),
              legend.position = c(0.8, 0.85),
              legend.spacing.x = unit(0.2, 'cm'),
              legend.background = element_blank(),
              legend.box.background = element_rect(colour = "black")) +
        ylab(expression(paste('Mortality rate (% ', year^{-1}, ")"))) +
        xlab('')
    m0
}
