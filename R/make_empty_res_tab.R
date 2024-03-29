
# make_empty_res_tab <- function(intvs, big_or_small_ages = c('big', 'small')) {
#   if (missing(intvs)) { 
#     intvs <- c('base_case', paste0('intervention_', 1:5), paste0('scenario_', 1:3))
# 	}

# 	if (length(big_or_small_ages) != 1 || ! big_or_small_ages %in% c('big', 'small')) { 
# 	  stop("big_or_small_ages must be one of 'big' or 'small'") }

# 	if (big_or_small_ages == 'small') { 
# 	  ages <- c("0-4",paste(0:8*10+5,1:9*10+4,sep="-"),"95+")
# 	} else {
# 	  ages <- c("all_ages", "age_0_24","age_25_64","age_65p")
# 		outcomes <- c('pct_ltbi', '')
# 	}
# 	# Specify the levels of each dimension to the data
# 	CatList <- list()
# 	CatList[[1]] <- c(
# 		"ltbi_000s",
# 		"pct_ltbi",
# 		"tb_incidence_000s",
# 		"tb_incidence_per_mil",
# 		"tb_mortality_000s",
# 		"tb_mortality_per_mil")
# 	CatList[[2]] <- intvs
# 	CatList[[3]] <- c("all_populations","usb_population","fb_population")
# 	CatList[[4]] <- ages
# 	CatList[[5]] <- c("absolute_value","pct_basecase_same_year","pct_basecase_2016")
# 	CatList[[6]] <- 2018:2049
# 	# CatList[[7]] <- c("mean","ci_low","ci_high")

# 	# Make the specified levels integer-leveled factors
# 	CatList_factors <- lapply(CatList, function(x) {
# 		factor(x=1:length(x), levels=1:length(x), labels=x) })

# 	# Turn the integer-factors into all possible combinations in a dataframe
# 	# with an extra column of NA values for a 'value' column
# 	res_tab2 <-  cbind(expand.grid(CatList_factors),NA)

# 	# Name the columns
# 	colnames(res_tab2) <- c(
# 		"outcome",
# 		"scenario",
# 		"population",
# 		"age_group",
# 		"comparator",
# 		"year",
# 		"value")

# 	return(res_tab2)
# }


make_empty_res_tab2sm <- function(intvs) {
	# If intvs is missing, use the basecase and the 8 pre-defined interventions
	if (missing(intvs)) {
    intvs <- c('base_case', paste0('intervention_', 1:5), paste0('scenario_', 1:3))
	}

  # Specify the levels of each dimension to the data
  CatList <- list()
  CatList[[1]] <- c(
    "ltbi_000s",
    "pct_ltbi",
    "tb_infection_000s",
    "tb_infection_per_100k",
    "tb_incidence_000s",
    "tb_incidence_per_100k",
    "tb_mortality_000s",
    "tb_mortality_per_100k", 
    "ltbi_tests_000s", 
    "ltbi_txinits_000s", 
    "ltbi_txcomps_000s", 
    "tb_txinits_000s", 
    "tb_txcomps_000s")
  CatList[[2]] <- intvs
  CatList[[3]] <- c("all_populations","usb_population","fb_population")
  CatList[[4]] <- c("0-4",paste(0:8*10+5,1:9*10+4,sep="-"),"95+")
  CatList[[5]] <- c("absolute_value","pct_basecase_same_year","pct_basecase_2018")
  CatList[[6]] <- 2020:2050
  # CatList[[7]] <- c("mean","ci_low","ci_high")

  # Make the specified levels integer-leveled factors
  CatList_factors <- lapply(CatList, function(x) {
    factor(x=1:length(x), levels=1:length(x), labels=x) })

  # Turn the integer-factors into all possible combinations in a dataframe
  # with an extra column of NA values for a 'value' column
  res_tab2 <-  cbind(expand.grid(CatList_factors),NA)

  # Name the columns
  colnames(res_tab2) <- c(
    "outcome",
    "scenario",
    "population",
    "age_group",
    "comparator",
    "year",
    "value")

  return(res_tab2)
}

make_empty_res_tab2bg <- function(intvs) {
  # Specify the levels of each dimension to the data
  CatList <- list()
  CatList[[1]] <- c(
    "ltbi_000s",
    "pct_ltbi",
    "tb_infection_000s",
    "tb_infection_per_100k",
    "tb_incidence_000s",
    "tb_incidence_per_100k",
    "tb_mortality_000s",
    "tb_mortality_per_100k",
    #UPDATE ALL THE FOLLOWING TO 000s
    "ltbi_tests_000s", 
    "ltbi_txinits_000s", 
    "ltbi_txcomps_000s", 
    "tb_txinits_000s", 
    "tb_txcomps_000s")
  CatList[[2]] <- intvs
  CatList[[3]] <- c("all_populations","usb_population","fb_population")
  CatList[[4]] <- c("all_ages", "age_0_24","age_25_64","age_65p")
  CatList[[5]] <- c("absolute_value","pct_basecase_same_year","pct_basecase_2018")
  CatList[[6]] <- 2020:2050
  # CatList[[7]] <- c("mean","ci_low","ci_high")

  # Make the specified levels integer-leveled factors
  CatList_factors <- lapply(CatList, function(x) {
    factor(x=1:length(x), levels=1:length(x), labels=x) })

  # Turn the integer-factors into all possible combinations in a dataframe
  # with an extra column of NA values for a 'value' column
  res_tab2 <-  cbind(expand.grid(CatList_factors),NA)

  # Name the columns
  colnames(res_tab2) <- c(
    "outcome",
    "scenario",
    "population",
    "age_group",
    "comparator",
    "year",
    "value")

  return(res_tab2)
}
