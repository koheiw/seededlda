require(quanteda)
require(seededlda)

corp <- readRDS("~/Corpus/data_corpus_guardian2016-10k.rds")

toks <- tokens(corp,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_number = TRUE)
dfmt <- dfm(toks) |>
    dfm_remove(stopwords(), min_nchar = 2)  |>
    dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")

microbenchmark::microbenchmark(
    old = seededlda.old::textmodel_lda(
    	dfmt, k = 20, verbose = TRUE, batch_size = 0.01,
    	max_iter = 500,
    ),
    new = seededlda::textmodel_lda(
    	dfmt, k = 20, verbose = TRUE, batch_size = 0.01,
    	max_iter = 500,
    ),
    times = 10
)

