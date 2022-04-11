# Comparison of PCA models
using <- function(...) {
    libs <- unlist(list(...))
    req <- unlist(lapply(libs, require, character.only = TRUE))
    need <- libs[req == FALSE]
    if (length(need) > 0) {
    install.packages(need)
    lapply(need, require, character.only = TRUE)
    }
}

# load the required packages
using(
    "readxl",
    "ggplot2",
    "corrplot",
    "reshape",
    "pcaMethods",
    "PLNmodels",
    "factoextra",
    "future",
    "jsonlite",
    "here"
)

# set current working directory
i_am("src/Comparison_of_PCA_models.r")

# get today's date
today <- format(Sys.time(), "%b_%d_%Y")

# load the author profiles
profiles <- read_excel("/media/Bibliometrics/matt analysis/Datasets/PCA_inputs_feb18.xlsx") # nolint
auth_vars <- profiles[, 5:21]

### Models
source("src/pca_models.r")

# build the PCA models
models <- get_pca_models(auth_vars)

# get the PCA loadings and scores
loadings <- get_pca_loadings(models)
scores <- get_pca_scores(models)

# store the scores from the various models with the profiles
profiles_with_scores <- cbind(
    profiles,
    scores
)

### Plots
source("src/pca_plots.r")

# plot the correlation matrix of the variables
auth_var_cor <- cor(auth_vars)
plot_correlations(auth_var_cor)

# plot the loadings from the various models
lapply(loadings, plot_loadings)

# load_vars <- rownames(loadings)
# loadings <- data.frame(load_vars, loadings)
# loadings <- melt(loadings, id.vars = c("load_vars"), variable.name = "loading")
# colnames(loadings) <- c("variable", "PC", "loading")

# # dev.new()
# # loadings_plot <- ggplot(loadings, aes(loading, variable)) +
# #   geom_bar(stat = "identity", fill = "#4682B4") +
# #   xlab("Variable") +
# #   ylab("Loading") +
# #   theme_bw() +
# #   theme(
# #     axis.text.x = element_text(angle = 45, hjust = 1)
# #   ) +
# #   facet_wrap(~PC, nrow = 1) +
# #   ggtitle("Loadings for PC1-5")
# # plot(loadings_plot)

# # # save the profiles with the PC scores
# # new_pcs <- auth_pca$x[, 1:3]
# new_pcs <- auth_pca@scores[, 1:3]
# profiles <- data.frame(profiles, new_pcs)

# # write.csv(profiles, "/media/Bibliometrics/matt analysis/Datasets/author_profiles_with_pc_scores_apr8.csv") # nolint

# ## Make density plots
# # Publication PCA distributions
# # dev.new()
# # dist_plot <- ggplot(profiles, aes(x = PC1)) +
# #   geom_density(
# #     aes(fill = factor(affil))
# #   ) +
# #   scale_fill_manual(
# #     values = c(
# #       "coral",
# #       "cadetblue1",
# #       "darkorchid",
# #       "pink1",
# #       "#22120a",
# #       "darkorange",
# #       "blue3",
# #       "darkolivegreen2"
# #     )
# #   ) +
# #   xlim(c(-1e4, 1e4)) +
# #   facet_wrap(~affil, ncol = 1) +
# #   labs(
# #     title = c("PC1 (Overall Publication Performance)"),
# #     x = "PC Score",
# #     fill = "Group",
# #     size = 1
# #   ) +
# #   theme_bw() +
# #   theme(legend.position = "none") +
# #   theme(
# #     panel.grid.major = element_blank(),
# #     panel.grid.minor = element_blank()
# #   )
# # plot(dist_plot)

# # Scatterplot of PC1 vs PC2
# dev.new()
# pc_plot <- ggplot(profiles, aes(PC1, PC2, color = factor(affil)))
# pc_plot <- pc_plot +
#   geom_point() +
#   scale_colour_manual(
#     values = c(
#       "coral",
#       "cadetblue1",
#       "darkorchid",
#       "pink1",
#       "#22120a",
#       "darkorange",
#       "blue3",
#       "darkolivegreen2"
#     )
#   )
# pc_plot <- pc_plot +
#   theme_bw() +
#   ylim(c(-300, 100)) +
#   xlim(c(-5000, 0)) +
#   geom_density2d(aes(colour = factor(affil)), size = 0.1) +
#   stat_density2d(aes(fill = ..level..), geom = "polygon", alpha = 0.1) +
#   xlab("PC1") +
#   ylab("PC2") +
#   ggtitle("A. PC1 vs PC2") +
#   theme(plot.title = element_text(
#     color = "black",
#     face = "bold",
#     size = 14,
#     hjust = -0.15)) +
#   theme(axis.title = element_text(color = "black", size = 10))
# plot(pc_plot)

# # ggsave("/media/Bibliometrics/matt analysis/visuals/pca/pc1 vs pc2 apr8.pdf")

# # Barplots of sums and means of PC1
# # aggregate PC1 scores by affiliation
# pc_sums <- aggregate(
#   profiles$PC1 - min(profiles$PC1),
#   by = list(factor(profiles$affil)),
#   FUN = "sum"
# )
# colnames(pc_sums) <- c("Institute", "Sum of PC1")

# pc_means <- aggregate(
#   profiles$PC1 - min(profiles$PC1),
#   by = list(factor(profiles$affil)),
#   FUN = "mean"
# )
# colnames(pc_means) <- c("Institute", "Mean PC1")

# # plot the sums and means of PC1
# # dev.new()
# # sumplot <- ggplot(pc_sums, aes(x = Institute, y = `Sum of PC1`)) +
# #   geom_col() +
# #   scale_fill_manual(
# #     values = c(
# #       "coral",
# #       "cadetblue1",
# #       "darkorchid",
# #       "pink1",
# #       "#22120a",
# #       "darkorange",
# #       "blue3",
# #       "darkolivegreen2"
# #     )
# #   )
# # plot(sumplot)

# # dev.new()
# # meanplot <- ggplot(pc_means, aes(x = Institute, y = `Mean PC1`)) +
# #   geom_col() +
# #   scale_fill_manual(
# #     values = c(
# #       "coral",
# #       "cadetblue1",
# #       "darkorchid",
# #       "pink1",
# #       "#22120a",
# #       "darkorange",
# #       "blue3",
# #       "darkolivegreen2"
# #     )
# #   )
# # plot(meanplot)

# # Make plots of top 20% of researchers
# # top_20 <- profiles[order(-profiles$PC1), ]
# # top_20 <- top_20[1:446, ]

# # achri_top_20 <- top_20[which(top_20$affil == "ACHRI"), ]
# # bcchr_top_20 <- top_20[which(top_20$affil == "BCCHR"), ]
# # chrim_top_20 <- top_20[which(top_20$affil == "CHRIM"), ]
# # chusj_top_20 <- top_20[which(top_20$affil == "CHUSJ"), ]
# # hbv_top_20 <- top_20[which(top_20$affil == "HBV"), ]
# # mcmaster_top_20 <- top_20[which(top_20$affil == "McMaster"), ]
# # sickkids_top_20 <- top_20[which(top_20$affil == "Sickkids"), ]
# # wchri_top_20 <- top_20[which(top_20$affil == "WCHRI"), ]

# # plot_site <- function(df_arg, fill_arg) {
# #   dev.new()
# #   site_top_plot <- ggplot(
# #     df_arg,
# #     aes(indexed_name, PC1)
# #     ) +
# #     geom_col(
# #       aes(reorder(indexed_name, PC1), PC1),
# #       fill = fill_arg
# #     ) +
# #     coord_flip()
# #   plot(site_top_plot)
# # }

# # plot_site(achri_top_20, fill_arg = "coral")
# # plot_site(bcchr_top_20, fill_arg = "cadetblue1")
# # plot_site(chrim_top_20, fill_arg = "darkorchid")
# # plot_site(chusj_top_20, fill_arg = "pink1")
# # plot_site(hbv_top_20, fill_arg = "#22120a")
# # plot_site(mcmaster_top_20, fill_arg = "darkorange")
# # plot_site(sickkids_top_20, fill_arg = "blue3")
# # plot_site(wchri_top_20, fill_arg = "darkolivegreen2")