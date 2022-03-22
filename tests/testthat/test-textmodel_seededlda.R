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

test_that("seeded LDA is working", {

    dict <- dictionary(list(romance = c("love*", "couple*"),
                            sifi = c("alien*", "star", "space")))

    set.seed(1234)
    lda <- textmodel_seededlda(dfmt, dict, residual = TRUE, weight = 0.02,
                               min_termfreq = 10)

    expect_equal(dim(terms(lda, 10)), c(10, 3))
    expect_equal(dim(terms(lda, 20)), c(20, 3))
    expect_equal(
        colnames(terms(lda)),
        c("romance", "sifi", "other")
    )
    expect_false(
        any(sifi %in% terms(lda)[,"romance"])
    )
    expect_true(
        all(sifi %in% terms(lda)[,"sifi"])
    )
    expect_identical(
        lda$dictionary, dict
    )
    expect_equal(
        lda$residual, 1
    )
    expect_equal(
        lda$weight, 0.02
    )
    expect_false(
        any(sifi %in% terms(lda)[,"other"])
    )
    expect_equal(
        names(topics(lda)),
        docnames(lda$data)
    )
    expect_setequal(
        topics(lda),
        c("romance", "sifi", "other")
    )
    expect_equal(
        levels(topics(lda)),
        c("romance", "sifi", "other")
    )
    expect_equal(
        rowSums(lda$phi),
        c("romance" = 1, "sifi" = 1, "other" = 1)
    )
    expect_equal(
        rowSums(lda$theta),
        structure(rep(1, ndoc(dfmt)), names = docnames(dfmt))
    )
    expect_equal(
        ncol(terms(textmodel_seededlda(dfmt, dict, residual = FALSE))), 2
    )
    expect_error(
        textmodel_seededlda(dfmt, list("aa", "bb")),
        "dictionary must be a dictionary object"
    )
    expect_error(
        textmodel_seededlda(dfmt, dict, weight = -0.1),
        "weight must be pisitive a value"
    )
    expect_output(
        print(lda),
        paste0("\nCall:\n",
               "textmodel_seededlda(x = dfmt, dictionary = dict, residual = TRUE, \n" ,
               "    weight = 0.02, min_termfreq = 10)\n\n",
               "3 topics; 500 documents; 22,605 features."),
        fixed = TRUE
    )
    expect_equal(
        names(lda),
        c("k", "max_iter", "last_iter", "alpha", "beta", "phi", "theta",
          "words", "data", "call", "version",
          "dictionary", "valuetype", "case_insensitive", "residual", "weight")
    )
    expect_equivalent(class(lda$words), "dgCMatrix")
})

test_that("seeded LDA is working", {
    skip_on_cran()

    dict <- dictionary(list(romance = c("love*", "couple*", "couples"),
                            sifi = c("alien*", "star", "space", "dragon")))

    set.seed(1234)
    lda1 <- textmodel_seededlda(dfmt, dict, residual = TRUE)
    expect_true("couples" %in% terms(lda1)[,1])
    expect_true("dragon" %in% terms(lda1)[,2])

    lda2 <- textmodel_seededlda(dfmt, dict, residual = TRUE, min_termfreq = 10)
    expect_false("couples" %in% terms(lda2)[,1])
    expect_false("dragon" %in% terms(lda2)[,2])
})

test_that("predict works with seeded LDA", {
    skip_on_cran()

    dict <- dictionary(list(romance = c("lover", "couple", "marige"),
                            sifi = c("aliens", "star", "space")))

    dfmt_train <- head(dfmt, 450)
    dfmt_test <- tail(dfmt, 50)

    lda <- textmodel_seededlda(dfmt_train, dict, residual = TRUE)

    # original data
    expect_warning({
        pred_train <- predict(lda)
    })
    expect_equal(names(pred_train), docnames(dfmt_train))
    expect_equal(
        levels(pred_train),
        c("romance", "sifi", "other")
    )
    expect_true(sum(topics(lda) == pred_train) / length(pred_train) > 0.9)

    # new data
    expect_warning({
        pred_test <- predict(lda, newdata = dfmt_test)
    })
    expect_equal(names(pred_test), docnames(dfmt_test))
    expect_equal(
        levels(pred_test),
        c("romance", "sifi", "other")
    )
})

test_that("model argument works with seeded LDA", {
    skip_on_cran()

    dict <- dictionary(list(romance = c("lover", "couple", "marige"),
                            sifi = c("aliens", "star", "space")))

    dfmt_train <- head(dfmt, 450)
    dfmt_test <- tail(dfmt, 50)

    # fit new model
    lda <- textmodel_seededlda(dfmt_train, dict, residual = TRUE)

    expect_error(
        textmodel_lda(dfmt_train[1:50,], model = list()),
        "model must be a fitted textmodel_lda"
    )

    # in-sample prediction
    expect_warning({
        lda1 <- textmodel_lda(dfmt_train[1:50,], model = lda)
    }, "k, alpha and beta values are overwriten by the fitted model")
    expect_false(all(lda$phi == lda1$phi))
    expect_identical(dimnames(lda$phi), dimnames(lda1$phi))
    expect_true(mean(topics(lda)[1:50] == topics(lda1)) > 0.9)
    expect_equal(
        levels(topics(lda1)),
        c("romance", "sifi", "other")
    )

    # out-of-sample prediction
    expect_warning({
        lda2 <- textmodel_lda(dfmt_test, model = lda)
    }, "k, alpha and beta values are overwriten by the fitted model")
    expect_false(all(lda$phi == lda2$phi))
    expect_identical(dimnames(lda$phi), dimnames(lda2$phi))
    expect_equal(
        levels(topics(lda2)),
        c("romance", "sifi", "other")
    )
})

test_that("divergence() is working", {

    dict <- dictionary(list(romance = c("love*", "couple*"),
                            sifi = c("alien*", "star", "space")))

    set.seed(1234)
    lda <- textmodel_seededlda(dfmt, dict, residual = TRUE, weight = 0.02,
                               min_termfreq = 10)

    expect_equal(divergence(lda),
                 3.78, tolerance = 0.1)
})
