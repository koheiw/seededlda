require(quanteda)
data(data_corpus_moviereviews, package = "quanteda.textmodels")

toks <- tokens(data_corpus_moviereviews[1:500],
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_number = TRUE)
dfmt <- dfm(toks) %>%
    dfm_remove(stopwords(), min_nchar = 2) %>%
    dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")
sifi <- c("space", "mars", "alien", "earth")

test_that("LDA is working", {

    set.seed(1234)
    lda <- textmodel_lda(dfmt, k = 5)
    # saveRDS(lda, "tests/data/lda_v081.RDS")

    expect_equal(dim(terms(lda, 10)), c(10, 5))
    expect_equal(dim(terms(lda, 20)), c(20, 5))
    expect_equal(
        colnames(terms(lda)),
        c("topic1", "topic2", "topic3", "topic4", "topic5")
    )
    expect_true(
        sum(apply(terms(lda), 2, function(x)  all(sifi %in% x))) == 1 # there is a sifi topic
    )
    expect_equal(
        names(topics(lda)),
        docnames(lda$data)
    )
    expect_setequal(
       topics(lda),
       c("topic1", "topic2", "topic3", "topic4", "topic5")
    )
    expect_equal(
        levels(topics(lda)),
        c("topic1", "topic2", "topic3", "topic4", "topic5")
    )
    expect_equal(
        rowSums(lda$phi),
        c("topic1" = 1, "topic2" = 1, "topic3" = 1, "topic4" = 1, "topic5" = 1)
    )
    expect_equal(
        rowSums(lda$theta),
        structure(rep(1, ndoc(dfmt)), names = docnames(dfmt))
    )
    expect_equal(
        ncol(terms(textmodel_lda(dfmt, k = 1))), 1
    )
    expect_equal(
        ncol(terms(textmodel_lda(dfmt, k = 10))), 10
    )
    expect_error(
        textmodel_lda(dfmt, k = 0),
        "k must be larger than zero"
    )
    expect_output(
        print(lda),
        paste0("\nCall:\n",
               "textmodel_lda(x = dfmt, k = 5)\n\n",
               "5 topics; 500 documents; 22,605 features."),
        fixed = TRUE
    )
    expect_equal(
        names(lda),
        c("k", "max_iter", "last_iter", "alpha", "beta", "phi", "theta",
          "words", "data", "call", "version")
    )
    expect_equivalent(class(lda$words), "dgCMatrix")
})

test_that("verbose works", {

    expect_output(textmodel_lda(dfmt, k = 5, verbose = TRUE, max_iter = 200),
                  paste("Fitting LDA with 5 topics\n",
                        "   ...initializing\n",
                        "   ...Gibbs sampling in 200 itterations\n",
                        "   ...iteration 100\n",
                        "   ...iteration 200\n",
                        "   ...computing theta and phi\n",
                        "   ...complete", sep = ""), fixed = TRUE)

})

test_that("LDA works with empty documents", {

    dfmt_empty <- dfmt
    dfmt_empty[c(100, 200, 300),] <- 0
    dfmt_empty <- as.dfm(dfmt_empty)

    set.seed(1234)
    lda_empty <- textmodel_lda(dfmt_empty, k = 5)
    expect_true(
        all(is.na(topics(lda_empty)[c(100, 200, 300)]))
    )

})

test_that("predict works with LDA", {
    skip_on_cran()

    dfmt_train <- head(dfmt, 450)
    dfmt_test <- tail(dfmt, 50)

    lda <- textmodel_lda(dfmt_train, k = 5)

    expect_warning({
        pred_train <- predict(lda)
    })
    expect_equal(names(pred_train), docnames(dfmt_train))
    expect_equal(
        levels(pred_train),
        c("topic1", "topic2", "topic3", "topic4", "topic5")
    )
    expect_true(sum(topics(lda) == pred_train) / length(pred_train) > 0.9)

    expect_warning({
        pred_test <- predict(lda, newdata = dfmt_test)
    })
    expect_equal(names(pred_test), docnames(dfmt_test))
    expect_equal(
        levels(pred_test),
        c("topic1", "topic2", "topic3", "topic4", "topic5")
    )

})


test_that("model argument works with LDA", {
    skip_on_cran()

    dfmt_train <- head(dfmt, 450)
    dfmt_test <- tail(dfmt, 50)

    # fit new model
    lda <- textmodel_lda(dfmt_train, k = 5)

    # in-sample prediction
    expect_warning({
        lda1 <- textmodel_lda(dfmt_train[1:50,], model = lda)
    }, "k, alpha and beta values are overwriten by the fitted model")
    expect_false(all(lda$phi == lda1$phi))
    expect_identical(dimnames(lda$phi), dimnames(lda1$phi))
    expect_true(mean(topics(lda)[1:50] == topics(lda1)) > 0.8)
    expect_equal(
        levels(topics(lda1)),
        c("topic1", "topic2", "topic3", "topic4", "topic5")
    )

    # out-of-sample prediction
    expect_warning({
        lda2 <- textmodel_lda(dfmt_test, model = lda)
    }, "k, alpha and beta values are overwriten by the fitted model")
    expect_false(all(lda$phi == lda2$phi))
    expect_identical(dimnames(lda$phi), dimnames(lda2$phi))
    expect_equal(
        levels(topics(lda2)),
        c("topic1", "topic2", "topic3", "topic4", "topic5")
    )
})

test_that("divergence() is working", {

    set.seed(1234)
    lda <- textmodel_lda(dfmt, k = 5)

    expect_equal(divergence(lda),
                 2.94, tolerance = 0.1)
})

