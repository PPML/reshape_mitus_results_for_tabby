#include <Rcpp.h>
using namespace Rcpp;

// Cross Product the rt_idx Location in the 4D Outputs with the Size of Sequential Dimensional Subspaces
int crossProductWithDimMults(IntegerVector& dim_mults, IntegerVector& rt_idx) {
	int idx = 0;
	for (int k = 0; k < dim_mults.size(); k++) {
		idx = idx + dim_mults[k] * rt_idx[k];
	}
	return idx;
}

// [[Rcpp::export]]
NumericMatrix format_restab2_cpp(
		NumericVector ResTab, 
		NumericVector ResTabus, 
		NumericVector ResTabfb, 
		NumericMatrix res_tab2) {

	// Convert ResTab, ResTabus, ResTabfb to NumericVectors but remember their indexing
	NumericVector rt(ResTab);
  NumericVector rtus(ResTabus);
	NumericVector rtfb(ResTabfb);
	NumericVector dim = rt.attr("dim");
	NumericVector dim2 = rtus.attr("dim");
	NumericVector dim3 = rtfb.attr("dim");

	NumericMatrix rt2(res_tab2);

	// Make sure the dimensions agree
	try {
		// make sure order of arrays matches
		if (dim.size() != dim2.size() | dim2.size() != dim3.size()) {
			throw std::invalid_argument("Dimensions of ResTab, ResTabus, and ResTabfb must match.");
		}
		// make sure dimensions match
		for (int j = 0; j < dim.size(); j++) {
			if (dim[j] != dim2[j] | dim2[j] != dim3[j]) {
				throw std::invalid_argument("Dimensions of ResTab, ResTabus, and ResTabfb must match.");
			}
		}
	} catch(std::exception &ex) {	
		forward_exception_to_r(ex);
	} catch(...) { 
		::Rf_error("c++ exception (unknown reason)"); 
	}

  // Construct sizes of increasing sequential dimensional subspaces,
	// i.e. (unit (1), row-length, face-size, cube-size, etc...)
	IntegerVector dim_mults(dim.size());
	for (int j = 0; j < dim.size(); j++) {
		dim_mults[j] = 1;
		if (j != 0) {
			for (int k = 0; k < j; k++) {
				dim_mults[j] = dim_mults[j] * dim[k];
			}
		}
	 }

	// Set up memory for indexing  -- As we go through the res_tab2's rows
	// we will use the values from each row to determine which computation to 
	// evaluate and to insert into our output rt2 matrix.
	NumericVector ii(8);
	IntegerVector i(7);
	IntegerVector i_zeroidx(7);
	IntegerVector rt_idx(4);
	int idx;

	rt_idx = IntegerVector::create(1,2,3,4);
	int test = crossProductWithDimMults(dim_mults, rt_idx);
	Rcpp::Rcout << "test: " << test;

  for (int j = 0; j < rt2.nrow(); j++) {
		ii = rt2( j, _);
		i = IntegerVector::create(ii[0],ii[1],ii[2],ii[3],ii[4],ii[5],ii[6]);
		i_zeroidx = IntegerVector::create(ii[0]-1,ii[1]-1,ii[2]-1,ii[3]-1,ii[4]-1,ii[5]-1,ii[6]-1);

		// Comparator Switch
		if (i[4] == 1) { // absolute value
			// Population Switch

			if (i[2] == 1) { // All
			  rt_idx = IntegerVector::create(ii[2]-1,ii[6]-1,ii[1],ii[4]-1);
			  idx = 0;
				for (int k = 0; k < dim.size(); k++) {
					idx = idx + dim_mults[k] * rt_idx[k];
				}
			  rt2(j,7) = rt[idx];

			} else if (i[2] == 2) { // US-Born
			  rt_idx = IntegerVector::create(ii[2]-1,ii[6]-1,ii[1],ii[4]-1);
			  idx = 0;
				for (int k = 0; k < dim.size(); k++) {
					idx = idx + dim_mults[k] * rt_idx[k];
				}
			  rt2(j,7) = rtus[idx];

			} else if (i[2] == 3) { // Non-US-Born
			  rt_idx = IntegerVector::create(ii[2]-1,ii[6]-1,ii[1],ii[4]-1);
			  idx = 0;
				for (int k = 0; k < dim.size(); k++) {
					idx = idx + dim_mults[k] * rt_idx[k];
				}
			  rt2(j,7) = rtfb[idx];

			} else {
				Rcpp::Rcout << "The population " << i[2] << " was referenced in res_tab2.";
				throw std::range_error("The population referenced in res_tab2 has not been defined in C++");
			}
		} else if (i[4] == 2) { // pct_basecase_same_year
			// Population Switch

			if (i[2] == 1) { // All
			  rt_idx = IntegerVector::create(ii[2]-1,ii[6]-1,ii[1],ii[4]-1);
			  idx = 0;
				for (int k = 0; k < dim.size(); k++) {
					idx = idx + dim_mults[k] * rt_idx[k];
				}
			  rt2(j,7) = rt[idx];
			  rt_idx = IntegerVector::create(0,ii[6]-1,ii[1],ii[4]-1);
				idx = 0;
				for (int k = 0; k < dim.size(); k++) {
					idx = idx + dim_mults[k] * rt_idx[k];
				}
			  rt2(j,7) = rt2(j,7) / rt[idx] * 100;

			} else if (i[2] == 2) { // US-Born
			  rt_idx = IntegerVector::create(ii[2]-1,ii[6]-1,ii[1],ii[4]-1);
			  idx = 0;
				for (int k = 0; k < dim.size(); k++) {
					idx = idx + dim_mults[k] * rt_idx[k];
				}
			  rt2(j,7) = rtus[idx];
			  rt_idx = IntegerVector::create(0,ii[6]-1,ii[1],ii[4]-1);
				idx = 0;
				for (int k = 0; k < dim.size(); k++) {
					idx = idx + dim_mults[k] * rt_idx[k];
				}
			  rt2(j,7) = rt2(j,7) / rtus[idx] * 100;

			} else if (i[2] == 3) { // Non-US-Born
			  rt_idx = IntegerVector::create(ii[2]-1,ii[6]-1,ii[1],ii[4]-1);
			  idx = 0;
				for (int k = 0; k < dim.size(); k++) {
					idx = idx + dim_mults[k] * rt_idx[k];
				}
			  rt2(j,7) = rtfb[idx];
			  rt_idx = IntegerVector::create(0,ii[6]-1,ii[1],ii[4]-1);
				idx = 0;
				for (int k = 0; k < dim.size(); k++) {
					idx = idx + dim_mults[k] * rt_idx[k];
				}
			  rt2(j,7) = rt2(j,7) / rtfb[idx] * 100;

			} else {
				Rcpp::Rcout << "The population " << i[2] << " was referenced in res_tab2.";
				throw std::range_error("The population referenced in res_tab2 has not been defined in C++");
			}
		} else if (i[4] == 3) { // pct_basecase_2016
			// Population Switch
			if (i[2] == 1) { // All
			  rt_idx = IntegerVector::create(ii[2]-1,ii[6]-1,ii[1],ii[4]-1);
				idx = 0;
				for (int k = 0; k < dim.size(); k++) {
					idx = idx + dim_mults[k] * rt_idx[k];
				}
			  rt2(j,7) = rt[idx];
			  rt_idx = IntegerVector::create(0,ii[6]-1,0,ii[4]-1);
				idx = 0;
				for (int k = 0; k < dim.size(); k++) {
					idx = idx + dim_mults[k] * rt_idx[k];
				}
			  rt2(j,7) = rt2(j,7) / rt[idx] * 100;

			} else if (i[2] == 2) { // US-Born
			  rt_idx = IntegerVector::create(ii[2]-1,ii[6]-1,ii[1],ii[4]-1);
				idx = 0;
				for (int k = 0; k < dim.size(); k++) {
					idx = idx + dim_mults[k] * rt_idx[k];
				}
			  rt2(j,7) = rtus[idx];
			  rt_idx = IntegerVector::create(0,ii[6]-1,0,ii[4]-1);
				idx = 0;
				for (int k = 0; k < dim.size(); k++) {
					idx = idx + dim_mults[k] * rt_idx[k];
				}
			  rt2(j,7) = rt2(j,7) / rtus[idx] * 100;


			} else if (i[2] == 3) { // Non-US-Born
			  rt_idx = IntegerVector::create(ii[2]-1,ii[6]-1,ii[1],ii[4]-1);
				idx = 0;
				for (int k = 0; k < dim.size(); k++) {
					idx = idx + dim_mults[k] * rt_idx[k];
				}
			  rt2(j,7) = rtfb[idx];
			  rt_idx = IntegerVector::create(0,ii[6]-1,0,ii[4]-1);
				idx = 0;
				for (int k = 0; k < dim.size(); k++) {
					idx = idx + dim_mults[k] * rt_idx[k];
				}
			  rt2(j,7) = rt2(j,7) / rtfb[idx] * 100;

			} else {
				Rcpp::Rcout << "The population " << i[2] << " was referenced in res_tab2.";
				throw std::range_error("The population referenced in res_tab2 has not been defined in C++");
			}
		} else {
			Rcpp::Rcout << "The comparator " << i[3] << " was referenced in res_tab2.";
			throw std::range_error("The comparator referenced in res_tab2 has not been defined in C++");
		}
	}

	return rt2;
}
