
#' Mean the small age group restabs across simulations
mean_small_restabs <- function(restabs, nr, nints) { 
	age_id <- (2020:2050)-1949
	mResTabfb <- mResTabus <- mResTab <- array(NA,dim=c(nints,length(age_id),9,11))

	ResTab <- restabs[['small_results']][['ResTab']]
	ResTabus <- restabs[['small_results']][['ResTabus']]
	ResTabfb <- restabs[['small_results']][['ResTabfb']]

	 for(l in 1:nints){
		 for (i in 1:length(age_id)){
			for (j in 1:9){
				for (k in 1:11){
					mResTab[l,i,j,k]<-mean(na.omit(ResTab[1:nr+((l-1)*nr),i,j,k]))
					mResTabus[l,i,j,k]<-mean(na.omit(ResTabus[1:nr+((l-1)*nr),i,j,k]))
					mResTabfb[l,i,j,k]<-mean(na.omit(ResTabfb[1:nr+((l-1)*nr),i,j,k]))
				} } }}


	ResTabC <- list(mResTab,mResTabus,mResTabfb)
	return(ResTabC)
}

#' Mean the big age group restabs across simulations
mean_big_restabs <- function(restabs, nr, nints) { 

	age_id <- (2020:2050)-1949
  mResTabfb <- mResTabus <- mResTab <- array(NA,dim=c(nints,length(age_id),9,4))

	ResTab <- restabs[['big_results']][['ResTab']]
	ResTabus <- restabs[['big_results']][['ResTabus']]
	ResTabfb <- restabs[['big_results']][['ResTabfb']]

	for(l in 1:nints){
    for (i in 1:length(age_id)){
      for (j in 1:9){
        for (k in 1:4){
          mResTab[l,i,j,k]<-mean(na.omit(ResTab[1:nr+((l-1)*nr),i,j,k]))
          mResTabus[l,i,j,k]<-mean(na.omit(ResTabus[1:nr+((l-1)*nr),i,j,k]))
          mResTabfb[l,i,j,k]<-mean(na.omit(ResTabfb[1:nr+((l-1)*nr),i,j,k]))
        } } }}


	ResTabC <- list(mResTab,mResTabus,mResTabfb)
}



#' Mean the small age group restabs across simulations
mean_small_restabs_indices <- function(restabs, nr, nints) { 
	age_id <- (2020:2050)-1949

	ResTab <- restabs[['small_results']][['ResTab']]
	ResTabus <- restabs[['small_results']][['ResTabus']]
	ResTabfb <- restabs[['small_results']][['ResTabfb']]
	
	mResTabfb <- mResTabus <- mResTab <- array(NA,dim=c(nints,length(age_id),dim(ResTab)[[3]],dim(ResTab)[[4]]))


	 for(l in 1:nints){
		 for (i in 1:length(age_id)){
			for (j in 1:dim(ResTab)[[3]]){
				for (k in 1:dim(ResTab)[[4]]){
					mResTab[l,i,j,k]<-mean(na.omit(ResTab[1:nr+((l-1)*nr),i,j,k]))
					mResTabus[l,i,j,k]<-mean(na.omit(ResTabus[1:nr+((l-1)*nr),i,j,k]))
					mResTabfb[l,i,j,k]<-mean(na.omit(ResTabfb[1:nr+((l-1)*nr),i,j,k]))
				} } }}


	ResTabC <- list(mResTab,mResTabus,mResTabfb)
	return(ResTabC)
}

#' Mean the big age group restabs across simulations
mean_big_restabs_indices <- function(restabs, nr, nints) { 

	age_id <- (2020:2050)-1949

	ResTab <- restabs[['big_results']][['ResTab']]
	ResTabus <- restabs[['big_results']][['ResTabus']]
	ResTabfb <- restabs[['big_results']][['ResTabfb']]
	
	  mResTabfb <- mResTabus <- mResTab <- array(NA,dim=c(nints,length(age_id),dim(ResTab)[[3]],dim(ResTab)[[4]]))


	for(l in 1:nints){
    for (i in 1:length(age_id)){
      for (j in 1:dim(ResTab)[[3]]){
        for (k in 1:dim(ResTab)[[4]]){
          mResTab[l,i,j,k]<-mean(na.omit(ResTab[1:nr+((l-1)*nr),i,j,k]))
          mResTabus[l,i,j,k]<-mean(na.omit(ResTabus[1:nr+((l-1)*nr),i,j,k]))
          mResTabfb[l,i,j,k]<-mean(na.omit(ResTabfb[1:nr+((l-1)*nr),i,j,k]))
        } } }}


	ResTabC <- list(mResTab,mResTabus,mResTabfb)
}
