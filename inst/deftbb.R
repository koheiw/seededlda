#' Print TBB flag in makevars
if (quanteda:::cpp_tbb_enabled()) {
    cat("-DTBB")
}