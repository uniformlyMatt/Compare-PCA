library(PLNmodels)

get_pca_models <- function(data) {
    # Perform PCA with the following models:
    # standard PCA
    # probabilistic PCA
    # Poisson PCA
    # posterior mixture PCA

    standard_pca <- prcomp(
        data,
        scale = TRUE
    )
    probabilistic_pca <- pca(
        data,
        method = "ppca",
        nPcs = 17,
        seed = 1234
    )

    # get the count variables
    Abundance <- data.frame( # nolint
        data[,
            c(
                "Article_sum",
                "document_count",
                "openaccess_sum",
                "coauthor_count",
                "first_author_sum",
                "last_author_sum",
                "citation_count",
                "cited_by_count",
                "h_index"
            )
        ]
    )

    # get the continuous variables
    Covariate <- data.frame(  # nolint
        data[,
            c(
                "author_count_mean",
                "author_weight_mean",
                "chw_author_position_mean",
                "growth_rate",
                "mean_citations_per_year",
                "mean_citations_per_doc",
                "SJR_mean",
                "journal_h_index_mean"
            )
        ]
    )

    df <- prepare_data(counts = Abundance, covariates = Covariate)  # nolint

    pln_pca_models <- PLNPCA(
        Abundance ~ 1 + author_count_mean + chw_author_position_mean +
            growth_rate + mean_citations_per_year + mean_citations_per_doc +
            SJR_mean + journal_h_index_mean,
        data = df,
        ranks = 1:9
    )

    # get the PLNPCA model with the best BIC
    best_BIC <- getBestModel(pln_pca_models, "BIC") # nolint
    poisson_pca <- best_BIC

    return(
        list(
            standard_pca = standard_pca,
            probabilistic_pca = probabilistic_pca,
            poisson_pca = poisson_pca
        )
    )
}

get_pca_loadings <- function(list_of_models) {
    # Extract the variable loadings for various PCA models

    standard_pca <- list_of_models$standard_pca
    probabilistic_pca <- list_of_models$probabilistic_pca
    poisson_pca <- list_of_models$poisson_pca

    # create column names for the Poisson PCA loadings
    poisson_count_part <- data.frame(poisson_pca$model_par$B)
    colnames(poisson_count_part) <- sprintf(
        "PC%d",
        seq_len(ncol(poisson_count_part))
    )

    # for the Poisson PCA loadings, we need the parameters
    # for covariates from the GLM part of the model
    poisson_pca_loadings <- cbind(
        poisson_count_part,
        poisson_pca$model_par$Theta
    )

    return(
        list(
            standard_pca_loadings = data.frame(standard_pca$rotation[, 1:5]),
            probabilistic_pca_loadings = data.frame(
                probabilistic_pca@loadings[, 1:5]
            ),
            poisson_pca_loadings = poisson_pca_loadings
        )
    )
}

get_pca_scores <- function(list_of_models) {
    # Extract the scores from various PCA models

    standard_pca <- list_of_models$standard_pca
    probabilistic_pca <- list_of_models$probabilistic_pca
    poisson_pca <- list_of_models$poisson_pca

    standard_pca_scores <- standard_pca$x[, 1:5]
    probabilistic_pca_scores <- probabilistic_pca@scores[, 1:5]
    poisson_pca_scores <- poisson_pca$scores

    # make column names
    spca_colnames <- sprintf(
        "standard_PC%d",
        seq_len(ncol(standard_pca_scores))
    )
    ppca_colnames <- sprintf(
        "probabilistic_PC%d",
        seq_len(ncol(probabilistic_pca_scores))
    )
    poisson_pca_colnames <- sprintf(
        "pln_PC%d",
        seq_len(ncol(poisson_pca_scores))
    )

    column_names <- c(
        spca_colnames,
        ppca_colnames,
        poisson_pca_colnames
    )

    # combine PCA scores from all models into one dataframe
    result <- cbind(
        standard_pca_scores,
        probabilistic_pca_scores,
        poisson_pca_scores
    )

    colnames(result) <- column_names

    return(result)
}