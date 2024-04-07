#include "lib.h"

// [[Rcpp::export]]
int cpp_get_max_thread() {
#if QUANTEDA_USE_TBB
	return tbb::this_task_arena::max_concurrency();
#else
	return 1;
#endif
}

// [[Rcpp::export]]
bool cpp_tbb_enabled(){
#if QUANTEDA_USE_TBB
	return true;
#else
	return false;
#endif
}
