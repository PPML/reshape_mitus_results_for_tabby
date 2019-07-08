format_mitus_US_results_as_restabs_small_ages <- function() {
  age_id = (2018:2049)-1949
  tt<-0
  #create 3 lists to hold output
  mResTabfb <- mResTabus <- mResTab <- array(NA,dim=c(9,length(age_id),7,11))

  ResTabfb <- ResTabus <- ResTab <- array(NA,dim=c(90,length(age_id),7,11))
  for (intv in 1:9){
    #load the results for all the runs (need to make this dataset)
    load(system.file(paste0("US/US_results_",intv,".rda"), package='MITUS'))

		#dimensions of restab are:
		#number of datasets, number of ages, number of interventions
		# dimensions are
		# scenario, year, outcome, age-group;
    nr<-10
    #define o as the results
    o<-out
		#gather the outputs from that model run
    for (ag in 1:11){

      ################################################################################
      # Total population
      ################################################################################
      ResTab[1:nr+((intv-1)*10),,1,ag]<-o[,age_id,1]
      #number of ltbi prevalence
      ResTab[1:nr+((intv-1)*10),,2,ag]<-apply(o[,age_id,c(54,65)+ag],c(1,2),sum)*1e3
      #percentage of ltbi prevalence
      ResTab[1:nr+((intv-1)*10),,3,ag]<-apply(o[,age_id,c(54,65)+ag],c(1,2),sum)/o[, age_id,32+ag]*1e2
      #TB notifications (alive+dead at diagnosis)
      ResTab[1:nr+((intv-1)*10),,4,ag]<-apply(o[,age_id,c(135,188)+ag],c(1,2),sum)*1e3
      #percentage TB notifications (alive+dead at diagnosis)
      ResTab[1:nr+((intv-1)*10),,5,ag]<-apply(o[,age_id,c(135,188)+ag],c(1,2),sum)/o[, age_id,32+ag]*1e2
      #tb attributable deaths
      ResTab[1:nr+((intv-1)*10),,6,ag]<-apply(o[,age_id,c(87,98)+ag],c(1,2),sum)*1e3
      # percentage tb attributable deaths
      ResTab[1:nr+((intv-1)*10),,7,ag]<-apply(o[,age_id,c(87,98)+ag],c(1,2),sum)/o[, age_id,32+ag]*1e2

      ################################################################################
      # US Born population
      ################################################################################
      ResTabus[1:nr+((intv-1)*10),,1,ag]<-o[,age_id,1]
      #number of ltbi prevalence
      ResTabus[1:nr+((intv-1)*10),,2,ag]<-o[, age_id,54+ag]*1e3
      #percentage of ltbi prevalence
      ResTabus[1:nr+((intv-1)*10),,3,ag]<-o[, age_id,54+ag]/o[, age_id,43+ag]*1e2
      #TB notifications (alive+dead at diagnosis)
      ResTabus[1:nr+((intv-1)*10),,4,ag]<-apply(o[, age_id,c(204,215)+ag],c(1,2),sum)*1e3
      #percentage TB notifications (alive+dead at diagnosis)
      ResTabus[1:nr+((intv-1)*10),,5,ag]<-apply(o[, age_id,c(204,215)+ag],c(1,2),sum)/o[, age_id,43+ag]*1e2
      #tb attributable deaths
      ResTabus[1:nr+((intv-1)*10),,6,ag]<-o[, age_id,87+ag]*1e3
      # percentage tb attributable deaths
      ResTabus[1:nr+((intv-1)*10),,7,ag]<-o[, age_id,87+ag]/o[, age_id,43+ag]*1e2

      ################################################################################
      # non-US Born population
      ################################################################################
      ResTabfb[1:nr+((intv-1)*10),,1,ag]<-o[,age_id,1]
      #number of ltbi prevalence
      ResTabfb[1:nr+((intv-1)*10),,2,ag]<-o[, age_id,65+ag]*1e3
      #percentage of ltbi prevalence
      ResTabfb[1:nr+((intv-1)*10),,3,ag]<-o[, age_id,65+ag]/o[, age_id,2+ag]*1e2
      #TB notifications (alive+dead at diagnosis)
      ResTabfb[1:nr+((intv-1)*10),,4,ag]<-(apply(o[, age_id,c(135,188)+ag],c(1,2),sum)-apply(o[, age_id,c(204,215)+ag],c(1,2),sum))*1e3
      #percentage TB notifications (alive+dead at diagnosis)
      ResTabfb[1:nr+((intv-1)*10),,5,ag]<-(apply(o[, age_id,c(135,188)+ag],c(1,2),sum)-apply(o[, age_id,c(204,215)+ag],c(1,2),sum))/o[, age_id,43+ag]*1e2
      #tb attributable deaths
      ResTabfb[1:nr+((intv-1)*10),,6,ag]<-o[, age_id,98+ag]*1e3
      # percentage tb attributable deaths
      ResTabfb[1:nr+((intv-1)*10),,7,ag]<-o[, age_id,98+ag]/o[, age_id,2+ag]*1e2
    }
    } #end batch loop

  ################################################################################
  #take the mean for each of these
 for(l in 1:9){
   for (i in 1:length(age_id)){
    for (j in 1:7){
      for (k in 1:11){
        mResTab[l,i,j,k]<-mean(na.omit(ResTab[1:nr+((l-1)*10),i,j,k]))
        mResTabus[l,i,j,k]<-mean(na.omit(ResTabus[1:nr+((l-1)*10),i,j,k]))
        mResTabfb[l,i,j,k]<-mean(na.omit(ResTabfb[1:nr+((l-1)*10),i,j,k]))
      } } }}


 ResTabC <- list(mResTab,mResTabus,mResTabfb)
 return(ResTabC)
}
