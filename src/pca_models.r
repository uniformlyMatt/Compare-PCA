get_pca_models <- function(data, Abundance = NULL, Covariate = NULL) {
    # Perform PCA with the following models:
    # standard PCA
    # probabilistic PCA
    # Poisson PCA
    # posterior mixture PCA

    standard_pca <- prcomp(data, scale = TRUE)
    probabilistic_pca <- pca(
        data,
        method = "ppca",
        nPcs = 17,
        seed = 1234
    )

    if(!is.null(Abundance) && !is.null(Covariate)) {
        df <- prepare_data(counts = Abundance, covariates = Covariate)

        pln_pca_models <- PLNPCA(
            Abundance ~ 1 + MedianAuthorCount + MedianAuthorPosition +
                MedianAuthorWeight + MedianSJR + MedianJournalHIndex,
            data = df,
            ranks = 1:10
        )

        best_BIC <- getBestModel(pln_pca_models, "BIC")
        poisson_pca <- best_BIC
    }

    return(standard_pca, probabilistic_pca, poisson_pca)
}