require(quanteda)

test_that("tfm is working", {

    dict <- dictionary(list(A = c("a", "aa*", "abc"),
                            B = c("b*", "bb*", "bcd"),
                            AB = c("aa", "bb")))
    txt <- c("a aa aa aaa abc", "b b bb bcd bcd")
    dfmt <- dfm(tokens(txt))

    tfm1 <- seededlda:::tfm(dfmt, dict)
    expect_s4_class(tfm1, "dgCMatrix")
    expect_equal(tfm1["A",], c("a" = 1, "aa" = 2, "aaa" = 1, "abc" = 1,
                               "b" = 0, "bb" = 0, "bcd" = 0))
    expect_equal(tfm1["B",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                               "b" = 2, "bb" = 1, "bcd" = 2))
    expect_equal(tfm1["AB",], c("a" = 0, "aa" = 2, "aaa" = 0, "abc" = 0,
                                "b" = 0, "bb" = 1, "bcd" = 0))
    expect_equal(tfm1["other",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                                   "b" = 0, "bb" = 0, "bcd" = 0))

    tfm2 <- seededlda:::tfm(dfmt, dict, residual = FALSE)
    expect_s4_class(tfm2, "dgCMatrix")
    expect_equal(rownames(tfm2), c("A", "B", "AB"))

    tfm3 <- seededlda:::tfm(dfmt, dict, min_termfreq = 2)
    expect_s4_class(tfm3, "dgCMatrix")
    expect_equal(tfm3["A",], c("a" = 0, "aa" = 2, "aaa" = 0, "abc" = 0,
                               "b" = 0, "bb" = 0, "bcd" = 0))
    expect_equal(tfm3["B",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                               "b" = 2, "bb" = 0, "bcd" = 2))
    expect_equal(tfm3["AB",], c("a" = 0, "aa" = 2, "aaa" = 0, "abc" = 0,
                                "b" = 0, "bb" = 0, "bcd" = 0))
    expect_equal(tfm3["other",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                                   "b" = 0, "bb" = 0, "bcd" = 0))

    tfm4 <- seededlda:::tfm(dfmt, dict, weight = 0.02)
    expect_s4_class(tfm4, "dgCMatrix")
    expect_equal(tfm4["A",], c("a" = 2, "aa" = 4, "aaa" = 2, "abc" = 2,
                               "b" = 0, "bb" = 0, "bcd" = 0))
    expect_equal(tfm4["B",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                               "b" = 4, "bb" = 2, "bcd" = 4))
    expect_equal(tfm4["AB",], c("a" = 0, "aa" = 4, "aaa" = 0, "abc" = 0,
                                "b" = 0, "bb" = 2, "bcd" = 0))
    expect_equal(tfm4["other",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                                   "b" = 0, "bb" = 0, "bcd" = 0))

    tfm5 <- seededlda:::tfm(dfmt, dict, weight = c(0.01, 0.01, 0.02))
    expect_s4_class(tfm4, "dgCMatrix")
    expect_equal(tfm5["A",], c("a" = 1, "aa" = 2, "aaa" = 1, "abc" = 1,
                               "b" = 0, "bb" = 0, "bcd" = 0))
    expect_equal(tfm5["B",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                               "b" = 2, "bb" = 1, "bcd" = 2))
    expect_equal(tfm5["AB",], c("a" = 0, "aa" = 4, "aaa" = 0, "abc" = 0,
                                "b" = 0, "bb" = 2, "bcd" = 0))
    expect_equal(tfm5["other",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
                                   "b" = 0, "bb" = 0, "bcd" = 0))

    tfm11 <- seededlda:::tfm(dfmt, dict, min_termfreq = 2, residual = 4)
    expect_s4_class(tfm11, "dgCMatrix")
    expect_equal(rownames(tfm11),
                 c("A", "B", "AB", "other1", "other2", "other3", "other4"))

    options(seededlda_residual_name = "topic")
    tfm12 <- seededlda:::tfm(dfmt, dict, min_termfreq = 2, residual = 4)
    expect_s4_class(tfm12, "dgCMatrix")
    expect_equal(rownames(tfm12),
                 c("A", "B", "AB", "topic1", "topic2", "topic3", "topic4"))
    options(seededlda_residual_name = "other") # reset

})

test_that("tfm is working without matches", {

    dict <- dictionary(list(A = c("a", "aa*", "abc"),
                            B = c("b*", "bb*", "bcd"),
                            AB = c("aa", "bb")))
    txt <- c("a aa aa aaa abc", "b b bb bcd bcd")
    dfmt <- dfm(tokens(txt))

    tfm1 <- seededlda:::tfm(dfmt, dict, residual = 0)
    expect_equal(rowSums(tfm1), c("A" = 5, "B" = 5, "AB" = 3))

    txt2 <- c("a aaa abc", "b b bcd bcd")
    dfmt2 <- dfm(tokens(txt2))

    tfm2 <- seededlda:::tfm(dfmt2, dict, residual = 1)
    expect_equal(rowSums(tfm2), c("A" = 3, "B" = 4, "AB" = 0, "other" = 0))

    # no match
    tfm11 <- seededlda:::tfm(dfmt, dictionary(list(X = "x", Y = "y")),
                             residual = 1)
    expect_equal(rowSums(tfm11), c("X" = 0, "Y" = 0, "other" = 0))

    tfm12 <- seededlda:::tfm(dfmt, dictionary(list(X = character(), Y = character())),
                             residual = 1)
    expect_equal(rowSums(tfm12), c("X" = 0, "Y" = 0, "other" = 0))
})


test_that("tfm works with ngrams", {

    txt <- c("UN is the United Nations.",
             "ICC is the International Criminal Court.")
    toks <- tokens(txt, concatenator = " ")
    dict <- dictionary(list("un" = c("united nations", "un"),
                            "icc" = c("international criminal court", "icc")))
    toks1 <- tokens_compound(toks, dict)
    dfmt1 <- dfm(toks1)
    expect_equal(rowSums(seededlda:::tfm(dfmt1, dict)),
                 c("un" = 2, "icc" = 2, "other" = 0))

    skip_if_not_installed("quanteda", "4.2.0")
    toks2 <- tokens_compound(toks, dict, concatenator = "+")
    dfmt2 <- dfm(toks2)
    expect_equal(rowSums(seededlda:::tfm(dfmt2, dict)),
                 c("un" = 1, "icc" = 1, "other" = 0))
})

test_that("tfm works with dfm with x in docvars (#87)", {

	dict <- dictionary(list("A" = "a", "B" = "b"))
	dat <- data.frame(text = c("a b c", "A B C"),
					  x = c(1, 2))
	corp <- corpus(dat)
	toks <- tokens(corp)
	dfmt <- dfm(toks)

	expect_equal(
		as.matrix(seededlda:::tfm(dfmt, dict, residual = 1)),
		matrix(c(2, 0, 0, 0, 2, 0, 0, 0 ,0), nrow = 3,
			   dimnames = list(c("A", "B", "other"), c("a", "b", "c")))
	)

})


test_that("levels is working", {

	dict <- dictionary(list(A = list(
		A2 = c("aa", "aab"),
		c("a", "a*", "abc")
	),
	B = c("b*", "bb*", "bcd"),
	AB = c("aa", "bb")))
	txt <- c("a aa aa aaa abc", "b b bb bcd bcd")
	dfmt <- dfm(tokens(txt))

	tfm1 <- seededlda:::tfm(dfmt, dict)
	expect_s4_class(tfm1, "dgCMatrix")
	expect_equal(tfm1["A",], c("a" = 1, "aa" = 2, "aaa" = 1, "abc" = 1,
							   "b" = 0, "bb" = 0, "bcd" = 0))
	expect_equal(tfm1["B",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
							   "b" = 2, "bb" = 1, "bcd" = 2))
	expect_equal(tfm1["AB",], c("a" = 0, "aa" = 2, "aaa" = 0, "abc" = 0,
								"b" = 0, "bb" = 1, "bcd" = 0))
	expect_equal(tfm1["other",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
								   "b" = 0, "bb" = 0, "bcd" = 0))

	tfm2 <- seededlda:::tfm(dfmt, dict, levels = 1:2)
	expect_s4_class(tfm2, "dgCMatrix")
	expect_equal(tfm2["A.A2",], c("a" = 0, "aa" = 2, "aaa" = 0, "abc" = 0,
								  "b" = 0, "bb" = 0, "bcd" = 0))
	expect_equal(tfm2["A",], c("a" = 1, "aa" = 2, "aaa" = 1, "abc" = 1,
							   "b" = 0, "bb" = 0, "bcd" = 0))
	expect_equal(tfm2["B",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
							   "b" = 2, "bb" = 1, "bcd" = 2))
	expect_equal(tfm2["AB",], c("a" = 0, "aa" = 2, "aaa" = 0, "abc" = 0,
								"b" = 0, "bb" = 1, "bcd" = 0))
	expect_equal(tfm2["other",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
								   "b" = 0, "bb" = 0, "bcd" = 0))

	tfm3 <- seededlda:::tfm(dfmt, dict, levels = 2)
	expect_s4_class(tfm3, "dgCMatrix")
	expect_equal(tfm3["A2",], c("a" = 0, "aa" = 2, "aaa" = 0, "abc" = 0,
								"b" = 0, "bb" = 0, "bcd" = 0))
	expect_equal(tfm3["other",], c("a" = 0, "aa" = 0, "aaa" = 0, "abc" = 0,
								   "b" = 0, "bb" = 0, "bcd" = 0))
})


