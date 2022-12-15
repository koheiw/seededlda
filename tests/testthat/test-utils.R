require(quanteda)
data(data_corpus_moviereviews, package = "quanteda.textmodels")

toks <- tokens(data_corpus_moviereviews[1:500],
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_number = TRUE)

dfmt <- dfm(toks) %>%
    dfm_remove(stopwords(), min_nchar = 2) %>%
    dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")

dict <- dictionary(list(romance = c("love*", "couple*"),
                        sifi = c("alien*", "star", "space")))
set.seed(1234)
lda <- textmodel_lda(dfmt, k = 5)

set.seed(1234)
slda <- textmodel_seededlda(dfmt, dict, residual = TRUE, weight = 0.02,
                            min_termfreq = 10)

test_that("divergence() is working", {

    expect_equal(divergence(lda),
                 0.34, tolerance = 0.01)

    expect_gt(divergence(lda, weighted = FALSE),
              divergence(lda, weighted = TRUE))

    expect_gt(divergence(lda),
              divergence(lda, min_size = 0.1))

    expect_equal(divergence(slda),
                 0.29, tolerance = 0.01)

    expect_silent(divergence(lda, select = c("topic1", "topic2")))
    expect_error(
        divergence(lda, select = 1:3),
        "The type of select must be character"
    )
    expect_error(
        divergence(lda, select = c("topic1")),
        "The length of select must be between 2 and 5"
    )
    expect_error(
        divergence(lda, select = c("topic1", "topic2", "xxxx")),
        "Selected topics must be in the model"
    )

    expect_gt(divergence(slda, weighted = FALSE),
              divergence(slda, weighted = TRUE))

    expect_gt(divergence(slda),
              divergence(slda, min_size = 0.1))

    expect_silent(divergence(slda, select = c("romance", "sifi")))
    expect_error(
        divergence(slda, select = 1:3),
        "The type of select must be character"
    )
    expect_error(
        divergence(slda, select = c("romance")),
        "The length of select must be between 2 and 3"
    )
    expect_error(
        divergence(slda, select = c("romance", "sifi", "xxxx")),
        "Selected topics must be in the model"
    )
})

test_that("sizes() is working", {

    size1 <- sizes(lda)
    expect_true(is.numeric(size1))
    expect_equal(names(size1), c("topic1", "topic2", "topic3", "topic4", "topic5"))
    expect_equal(sum(size1), 1)

    size2 <- sizes(slda)
    expect_true(is.numeric(size2))
    expect_equal(names(size2), c("romance", "sifi", "other"))
    expect_equal(sum(size2), 1)
})

test_that("get_weight() is working", {

    dfmt_seed <- dfm_select(dfmt, dict)
    dfmt_dict <- dfm_lookup(dfmt, dict)
    w <- seededlda:::get_weight(dfmt_seed, dfmt_dict)

    expect_equal(names(w), featnames(dfmt_seed))
    expect_lte(max(w), 1)
    expect_gte(min(w), 0)
})
