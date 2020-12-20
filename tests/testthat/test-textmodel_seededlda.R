context("textmodel_seededlda")

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
                            sifi = c("arean*", "star", "space")))

    set.seed(1234)
    lda <- textmodel_seededlda(dfmt, dict, residual = TRUE)

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
        "Topics: 3; 500 documents; 22605 features."
    )
})

test_that("seeded LDA is working", {

    dict <- dictionary(list(romance = c("love*", "couple*", "couples"),
                            sifi = c("arean*", "star", "space", "dragon")))

    set.seed(1234)
    lda1 <- textmodel_seededlda(dfmt, dict, residual = TRUE)
    expect_true("couples" %in% terms(lda1)[,1])
    expect_true("dragon" %in% terms(lda1)[,2])

    lda2 <- textmodel_seededlda(dfmt, dict, residual = TRUE, min_termfreq = 10)
    expect_false("couples" %in% terms(lda2)[,1])
    expect_false("dragon" %in% terms(lda2)[,2])
})
