library(corrplot)
library(ggplot2)

plot_correlations <- function(correlation_matrix) {
    # Plot a correlation matrix

    dev.new()
    cplot <- corrplot.mixed(
        correlation_matrix,
        lower = "circle",
        upper = "number",
        upper.col = "black",
        number.cex = .7,
        order = "hclust"
    )

    cplot <- corrplot(
        correlation_matrix,
        type = "lower",
        order = "hclust"
    )
}

plot_loadings <- function(loadings) {
    # Plot the loadings from a single PCA model
    # TODO: only plot PCs for Poisson PCA
    title <- sprintf("Loadings for PC1-%d", seq_len(ncol(loadings)))

    load_vars <- rownames(loadings)
    loadings <- melt(
        data.frame(load_vars, loadings),
        id.vars = c("load_vars"),
        variable.name = "loading"
    )
    colnames(loadings) <- c("Variable", "PC", "Loading")
    dev.new()
    loadings_plot <- ggplot(
            loadings,
            aes(Loading, Variable)
        ) +
        geom_bar(
            stat = "identity",
            fill = "#4682B4"
        ) +
        xlab("Variable") +
        ylab("Variable") +
        theme_bw() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        facet_wrap(~PC, nrow = 1) +
        ggtitle(title)
    plot(loadings_plot)
}