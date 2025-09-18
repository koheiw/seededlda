require(quanteda)

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

	# in-sample
	div1 <- divergence(lda)
	expect_equal(div1, 0.33, tolerance = 0.01)

	toks_val <- tokens(data_corpus_moviereviews[501:600],
					   remove_punct = TRUE,
					   remove_symbols = TRUE,
					   remove_number = TRUE)
	dfmt_val <- dfm(toks_val) %>%
		dfm_remove(stopwords(), min_nchar = 2) %>%
		dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")

	# out-sample
	set.seed(1234)
	expect_output(
		div2 <- divergence(lda, newdata = dfmt_val, max_iter = 100, verbose = TRUE),
		"Fitting LDA with 5 topics.*"
	)
	expect_equal(div2, 0.34, tolerance = 0.01)
})

test_that("perplexity() is working", {

	# in-sample
	ppl1 <- perplexity(lda)
	expect_equal(ppl1, 7742, tolerance = 1)

	toks_val <- tokens(data_corpus_moviereviews[501:600],
					   remove_punct = TRUE,
					   remove_symbols = TRUE,
					   remove_number = TRUE)
	dfmt_val <- dfm(toks_val) %>%
		dfm_remove(stopwords(), min_nchar = 2) %>%
		dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")

	# out-sample
	set.seed(1234)
	expect_output(
		ppl2 <- perplexity(lda, newdata = dfmt_val, max_iter = 100, verbose = TRUE),
		"Fitting LDA with 5 topics.*"
	)
	expect_equal(ppl2, 7534, tolerance = 1)
})


test_that("regularize is working", {

    # LDA
    expect_equal(divergence(lda),
                 0.34, tolerance = 0.01)

    expect_gt(divergence(lda, regularize = FALSE),
              divergence(lda, regularize = TRUE))

    expect_gt(divergence(lda),
              divergence(lda, min_size = 0.1))

    expect_equal(divergence(slda),
                 0.29, tolerance = 0.02)

    expect_true(divergence(lda, 0.05) != divergence(lda, 0.01))
    expect_error(
        divergence(lda, regularize = 1),
        "The type of regularize must be logical"
    )

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

    # Seeded LDA
    expect_gt(divergence(slda, regularize = FALSE),
              divergence(slda, regularize = TRUE))

    expect_gt(divergence(slda),
              divergence(slda, min_size = 0.1))

    expect_true(divergence(slda, 0.05) != divergence(slda, 0.01))
    expect_error(
        divergence(slda, regularize = 1),
        "The type of regularize must be logical"
    )

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

test_that("get_threads are working", {

	options("seededlda_threads" = "abc")
	expect_error(
		suppressWarnings(seededlda:::get_threads()),
		"seededlda_threads must be an integer"
	)
	options("seededlda_threads" = NA)
	expect_error(
		seededlda:::get_threads(),
		"seededlda_threads must be an integer"
	)

	## respect other settings
	options("seededlda_threads" = NULL)

	Sys.setenv("OMP_THREAD_LIMIT" = 2)
	expect_equal(
		seededlda:::get_threads(),
		ifelse(seededlda::info_tbb()$enabled, 2, 1)
	)
	Sys.unsetenv("OMP_THREAD_LIMIT")

	Sys.setenv("RCPP_PARALLEL_NUM_THREADS" = 3)
	expect_equal(
		seededlda:::get_threads(),
		ifelse(seededlda::info_tbb()$enabled, 3, 1)
	)
	Sys.unsetenv("RCPP_PARALLEL_NUM_THREADS")

	options("seededlda_threads" = NULL)
})

test_that("as.dictionary is working", {

	dict1 <- as.dictionary(lda)
	expect_true(is.dictionary(dict1))
	expect_true(all(lengths(dict1) == 10))

	dict2 <- as.dictionary(slda, n = 20)
	expect_true(is.dictionary(dict2))
	expect_identical(names(dict2), c("romance", "sifi", "other" ))
	expect_true(all(lengths(dict2) == 20))
})
