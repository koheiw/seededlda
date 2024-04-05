require(quanteda)

options(seededlda_residual_name = "other")

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
    expect_setequal(
        topics(lda, select = c("romance", "sifi")),
        c("romance", "sifi")
    )
    expect_equal(
        levels(topics(lda)),
        c("romance", "sifi", "other")
    )
    expect_equal(
        levels(topics(lda, select = c("romance", "sifi"))),
        c("romance", "sifi")
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
        "The value of weight must be between 0 and 1"
    )
    expect_error(
        textmodel_seededlda(dfmt, dict, weight = numeric()),
        "The length of weight must be 1 or equal to dictionary"
    )
    expect_error(
        textmodel_seededlda(dfmt, dict, weight = c(0.01, 0.02, 0.01)),
        "The length of weight must be 1 or equal to dictionary"
    )
    expect_silent(
        textmodel_seededlda(dfmt, dict, weight = c(0.01, 0.02))
    )
    expect_output(
        print(lda),
        paste0("\nCall:\n",
               "textmodel_seededlda(x = dfmt, dictionary = dict, residual = TRUE, \n" ,
               "    weight = 0.02, min_termfreq = 10)\n\n",
               "3 topics; 500 documents; 22,544 features."),
        fixed = TRUE
    )
    expect_equal(
        names(lda),
        c("k", "max_iter", "last_iter", "auto_iter", "alpha", "beta", "gamma","phi", "theta",
          "words", "data", "batch_size", "call", "version",
          "dictionary", "valuetype", "case_insensitive", "seeds",
          "residual", "weight")
    )
    expect_equivalent(class(lda$words), "dgCMatrix")
    expect_equal(rownames(lda$words), colnames(lda$phi))
    expect_equal(colnames(lda$words), rownames(lda$phi))
})

test_that("seeded LDA is working", {
    skip_on_cran()

    dict <- dictionary(list(romance = c("love*", "couple*", "couples"),
                            sifi = c("alien*", "star", "space", "dragon")))

    set.seed(1234)
    lda1 <- textmodel_seededlda(dfmt, dict, residual = TRUE, weight = 0.1)
    expect_true("couples" %in% terms(lda1)[,1])
    expect_true("dragon" %in% terms(lda1)[,2])

    lda2 <- textmodel_seededlda(dfmt, dict, residual = TRUE, min_termfreq = 10, weight = 0.1)
    expect_false("couples" %in% terms(lda2)[,1])
    expect_false("dragon" %in% terms(lda2)[,2])
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
    expect_gt(mean(topics(lda)[1:50] == topics(lda1)), 0.8)
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

test_that("works similar way as before v0.9", {
    skip_on_cran()

    dict <- dictionary(list(romance = c("love*", "couple*"),
                            sifi = c("alien*", "star", "space")))

    set.seed(1234)
    lda <- textmodel_seededlda(dfmt, dict, residual = TRUE, weight = 0.1,
                               max_iter = 2000)
    set.seed(1234)
    lda_old <- textmodel_seededlda(dfmt, dict, residual = TRUE, weight = 0.01, old = TRUE,
                                   max_iter = 2000)

    tb <- table(topics(lda), topics(lda_old))
    expect_true(all(diag(tb) / rowSums(tb) > 0.80))
})


test_that("distributed LDA works", {

    skip_on_cran()

    dict <- dictionary(list(romance = c("love*", "couple*"),
                            sifi = c("alien*", "star", "space")))

    set.seed(1234)
    lda_seri <- textmodel_seededlda(dfmt, dict, residual = TRUE)
    set.seed(1234)
    lda_para <- textmodel_seededlda(dfmt, dict, residual = TRUE, batch_size = 0.01)

    # batch_size
    expect_equal(lda_seri$batch_size, 1.0)
    expect_equal(lda_para$batch_size, 0.01)

    # names of elements
    expect_identical(
        dimnames(lda_seri$phi), dimnames(lda_para$phi)
    )
    expect_identical(
        dimnames(lda_seri$theta), dimnames(lda_para$theta)
    )

    # parameters
    dist_theta <- proxyC::dist(lda_seri$theta + 0.001, lda_para$theta + 0.001,
                               1, method = "jensen", diag = TRUE)
    expect_lt(median(Matrix::diag(dist_theta)), 0.1)

    dist_phi <- proxyC::dist(lda_seri$phi + 0.001, lda_para$phi + 0.001,
                             2, method = "jensen", diag = TRUE)
    expect_lt(median(Matrix::diag(dist_phi)), 0.1)

})


test_that("auto_iter works", {

    skip_on_cran()

    dict <- dictionary(list(romance = c("love*", "couple*"),
                            sifi = c("alien*", "star", "space")))

    set.seed(1234)
    lda_fix <- textmodel_seededlda(dfmt, dict, auto_iter = FALSE, residual = TRUE,
                                   max_iter = 1000)

    set.seed(1234)
    lda_auto <- textmodel_seededlda(dfmt, dict, auto_iter = TRUE, residual = TRUE,
                                    max_iter = 1000)

    # iteration
    expect_equal(lda_fix$last_iter, 1000)
    expect_equal(lda_fix$max_iter, 1000)
    expect_equal(lda_fix$auto_iter, FALSE)
    expect_lt(lda_auto$last_iter, 1000)
    expect_equal(lda_auto$max_iter, 1000)
    expect_equal(lda_auto$auto_iter, TRUE)

    # names of elements
    expect_identical(
        dimnames(lda_fix$phi), dimnames(lda_auto$phi)
    )
    expect_identical(
        dimnames(lda_fix$theta), dimnames(lda_auto$theta)
    )

    # parameters
    dist_theta <- proxyC::dist(lda_fix$theta + 0.001, lda_auto$theta + 0.001,
                               1, method = "jensen", diag = TRUE)
    expect_lt(median(Matrix::diag(dist_theta)), 0.1)

    dist_phi <- proxyC::dist(lda_fix$phi + 0.001, lda_auto$phi + 0.001,
                             2, method = "jensen", diag = TRUE)
    expect_lt(median(Matrix::diag(dist_phi)), 0.1)

})


test_that("levels is working", {

	lis <- list(romance = c("love*", "couple*"),
				sifi = list(space = c("star", "space"),
							monster = c("alien*", "monster")))
	dict <- dictionary(lis)

	lda1 <- textmodel_seededlda(dfmt, dict, levels = 1, residual = TRUE,
								min_termfreq = 10, max_iter = 100)
	expect_equal(
		levels(topics(lda1)),
		c("romance", "sifi", "other")
	)
	lda2 <- textmodel_seededlda(dfmt, dict, levels = 1:2, residual = TRUE,
								min_termfreq = 10, max_iter = 100)
	expect_equal(
		levels(topics(lda2)),
		c("romance", "sifi.space", "sifi.monster", "other")
	)
	lda3 <- textmodel_seededlda(dfmt, dict, levels = 2, residual = TRUE,
								min_termfreq = 10, max_iter = 100)
	expect_equal(
		levels(topics(lda3)),
		c("space", "monster", "other")
	)

	expect_error(
		textmodel_seededlda(dfmt, dict, levels = -1, residual = TRUE,
							min_termfreq = 10, max_iter = 100),
		"The value of levels must be between 1 and Inf"
	)

})

