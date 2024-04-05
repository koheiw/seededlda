#' Print TBB flag in makevars
if (Sys.info()[["sysname"]] == "Windows") {
    if (getRversion() >= "4.3.0") {
        cat("-ltbb12")
    } else {
        cat("-ltbb_static")
    }
} else {
    if (quanteda:::cpp_tbb_enabled()) {
        cat("-ltbb")
    }
}
