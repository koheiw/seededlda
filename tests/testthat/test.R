context("test")

require(quanteda)
toks <- tokens(data_corpus_irishbudget2010)
dfmat <- dfm(toks, remove = stopwords())
dict <- dictionary(file = "../data/topics.yml")

test_that("test tfm", {
    mat1 <- quanteda.seededlda:::tfm(dfmat, dict, residual = FALSE)
    expect_equal(dim(mat1), c(length(dict), nfeat(dfmat)))
    mat2 <- quanteda.seededlda:::tfm(dfmat, dict, residual = TRUE)
    expect_equal(dim(mat2), c(length(dict) + 1, nfeat(dfmat)))

    feat <- colSums(mat1)
    feat <- feat[feat > 0]
    expect_true(setequal(featnames(dfm_select(dfmat, dict)), names(feat)))
})

test_that("test tfm with extreme cases", {
    dict1 <- dictionary(list(economy = "market*", politics = "parliament*", something = "xyz"))
    expect_equal(dim(quanteda.seededlda:::tfm(dfmat, dict1, residual = FALSE)),
                 c(3, 5021))

    dict2 <- dictionary(list(economy = "market*", politics = "parliament*", something = ""))
    expect_equal(dim(quanteda.seededlda:::tfm(dfmat, dict2, residual = FALSE)),
                 c(3, 5021))

    dict3 <- dictionary(list(economy = "", politics = "", something = ""))
    expect_equal(dim(quanteda.seededlda:::tfm(dfmat, dict3, residual = FALSE)),
                 c(3, 5021))
})

test_that("topics and terms methods are working", {
    slda1 <- textmodel_seededlda(dfmat, dict)
    expect_equal(
        topics(slda1),
        names(dict)[topicmodels::topics(slda1$lda)]
    )
    expect_equal(
        names(terms(slda1, k = 1)),
        names(dict)
    )
    expect_equal(
        colnames(terms(slda1, k = 10)),
        names(dict)
    )

    slda2 <- textmodel_seededlda(dfmat, dict, residual = TRUE)
    expect_equal(
        topics(slda1),
        names(dict)[topicmodels::topics(slda1$lda)]
    )
    expect_equal(
        names(terms(slda2, k = 1)),
        c(names(dict), "other")
    )
    expect_equal(
        colnames(terms(slda2, k = 10)),
        c(names(dict), "other")
    )
})
