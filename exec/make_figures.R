# BEM 2020
library(McNellis2021)
library(ggplot2)

# Figure width must be 2.9", 3.9", or 6.1" with max height of 6.8"
# BUT, email for proofs says 6.14" x 7.87" (15.6 x 20 cm)
# Minimum resolution 300 dpi, maximum resolution 600 dpi
# Text size should be 8pt, minimum 6pt
# Lines should not be smaller than 0.5 pt

#figure_dir <- getwd()
figure_dir <- '/home/bem/Documents/FIA_project/fourth_submission/figures'

# Figure 1
data("figure_1_df")
for (i in c(1:2)) {
  # needs to run twice to work
  #figure_1 <- PlotDiffMortMap(mort_df = figure_1_df, diff_var = 'baseline')
  figure_1 <- try(plot_diff_mort_map(mort_df = figure_1_df, diff_var = 'mort_rate', scale_adjust = 0.1))
}
# needs to run twice to work
# add legend
figure_1 <- figure_1 + theme(legend.position = 'right')
figure_1 <- figure_1 + guides(fill = guide_legend(expression(paste('Mortality rate (% ', year^{-1}, ")"))))
# change from grey to black
figure_1 <- figure_1 + theme(axis.text.x = element_text(colour = 'black'))
figure_1 <- figure_1 + theme(axis.text.y = element_text(colour = 'black'))
# fix margin
figure_1 <- figure_1 + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
f1 <- paste0(figure_dir, '/figure_1.tiff')
HighResTiff(figure_1, f1, width_in = 6.1, height_in = 5, 450)
# looks OK with base size = 10

# Figure 2
data("figure_2_df")
figure_2 <- PlotOverallImportance(figure_2_df)
f2 <- paste0(figure_dir, '/figure_2.tiff')
HighResTiff(figure_2, f2, 6.1, 6.8, 450)

# Figure 3
data("figure_3_df")
figure_3 <- PlotFeatureContrib(plot_df = figure_3_df, ylab0 = 'Median contribution to probability of tree mortality')
# fix margin
figure_3 <- figure_3 + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.7), "cm"))
f3 <- paste0(figure_dir, '/figure_3.tiff')
HighResTiff(figure_3, f3, 6.1, 5.5, 450)

# Figure 4
y0 <- 'Partial dependence of species on predicting mortality'
data("figure_4_df")
figure_4 <- PlotPartialDepend(figure_4_df, y0, order_descending = T)
figure_4 <- figure_4 + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.3), "cm"))
f4 <- paste0(figure_dir, '/figure_4.tiff')
HighResTiff(figure_4, f4, 6.1, 4.5, 450)

# Figure 5
data("figure_5_df")
figure_5 <- PlotAgentImportance(figure_5_df)
f5 <- paste0(figure_dir, '/figure_5.tiff')
HighResTiff(figure_5, f5, 6.1, 6.1, 450)

# Figure 6
data("figure_6_df")
for (i in c(1:2)) {
  # needs to run twice to work
  #figure_1 <- try(plot_diff_mort_map(mort_df = figure_1_df, diff_var = '', scale = 0.1))
  figure_6 <- try(plot_diff_mort_map(mort_df = figure_6_df, diff_var = 'diff45', scale_adjust = 0.1))
}
# add legend
figure_6 <- figure_6 + theme(legend.position = 'right')
figure_6 <- figure_6 + guides(fill = guide_legend(expression(paste('Increase in \nmortality rate (% ', year^{-1}, ")"))))
# change from grey to black
figure_6 <- figure_6 + theme(axis.text.x = element_text(colour = 'black'))
figure_6 <- figure_6 + theme(axis.text.y = element_text(colour = 'black'))
f6 <- paste0(figure_dir, '/figure_6.tiff')
HighResTiff(figure_6, f6, 6.1, 5, 450)

# Figure 7
data("figure_7_df")
figure_7 <- PlotCorr(figure_7_df)
f7 <- paste0(figure_dir, '/figure_7.tiff')
HighResTiff(figure_7, f7, 3.9, 3.9, 450)

# Figure 8
data("figure_8_df")
figure_8 <- make_mort_bar_plot(figure_8_df)
f8 <- paste0(figure_dir, '/figure_8.tiff')
HighResTiff(figure_8, f8, 4.5, 4.5, 450)

# end
