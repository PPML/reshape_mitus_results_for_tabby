
construct_cpp_reshaper <- function() {
	cxxfunction(
		signature(ResTab='numeric', ResTabus='numeric', ResTabfb='numeric', res_tab2 = 'numeric'),
		plugin='Rcpp',
		body=readr::read_file(
			system.file('inline_cpp/format_restab2.cpp', package='tabus')))
}
