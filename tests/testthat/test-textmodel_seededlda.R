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
    expect_true(
        lda$residual
    )
    expect_equal(
        lda$weight, 0.02
    )
    expect_false(
        any(sifi %in% terms(lda)[,"other"])
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
        "Topics: 3; 500 documents; 22,605 features\\."
    )
    expect_equal(
        names(lda),
        c("k", "max_iter", "last_iter", "alpha", "beta", "phi", "theta",
          "words", "data", "call", "dictionary", "valuetype", "case_insensitive",
          "residual", "weight")
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
    pred_train <- predict(lda)
    expect_equal(names(pred_train), docnames(dfmt_train))
    expect_equal(
        levels(pred_train),
        c("romance", "sifi", "other")
    )
    expect_true(sum(topics(lda) == pred_train) / length(pred_train) > 0.9)

    # new data
    pred_test <- predict(lda, newdata = dfmt_test)
    expect_equal(names(pred_test), docnames(dfmt_test))
    expect_equal(
        levels(pred_test),
        c("romance", "sifi", "other")
    )
})
