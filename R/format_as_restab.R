#' Take MITUS Outputs and Format into 3 DataFrames for Small Age Groups: ResTab, ResTabus, ResTabfb
#' 
#' @param simulation_data A list of outputs from MITUS, each the result of OutputsInt.
#' Each list entry should correspond to one scenario/intervention, and the first should 
#' be the basecase.
#'
#' 

format_as_restab_small_ages <- function(simulation_data) { 
  
  model_years = (2020:2050)-1949
  tt<-0
  
  # preflight checklist
  if (class(simulation_data) != 'list') {
    stop("simulation_data must be a list")
  }
  if (! all(sapply(simulation_data, function(sim) length(dim(sim))) == 3)) {
    stop("each simulation_data list entry must have 3 dimensions")
    # simulation index, model years, and age ids.
  }
  if (! all(apply(sapply(simulation_data, dim), 1, function(dim_row) length(unique(dim_row))) == 1)) { 
    stop("simulation_data list entries must have the same dimensions.")
  }
  
  # determine number of interventions and number of simulation runs from 
  # simulation_data.
  nintvs <- length(simulation_data)
  nr <- dim(simulation_data[[1]])[[1]]
  
  #create 3 lists to hold output
  ResTabfb <- ResTabus <- ResTab <- array(NA,dim=c(nr*nintvs,length(model_years),14,11))
  
  # For each scenario/intervention, fill in the results from the corresponding 
  # simulation_data entry
  for (intv in 1:nintvs){
    
    # Loop through each age group
    for (ag in 1:11){
      ################################################################################
      ################################################################################
      #total population
      #dimensions are
      #scenarios; length age id;
      ################################################################################
      #get the years
      ResTab[1:nr+((intv-1)*nr),,1,ag]<-simulation_data[[intv]][,model_years,1]
      #number of ltbi prevalence in thousands
      ResTab[1:nr+((intv-1)*nr),,2,ag]<-apply(simulation_data[[intv]][,model_years,c(54,65)+ag],c(1,2),sum)*1e3
      #percentage of ltbi prevalence
      ResTab[1:nr+((intv-1)*nr),,3,ag]<-apply(simulation_data[[intv]][,model_years,c(54,65)+ag],c(1,2),sum)/simulation_data[[intv]][,model_years,2+ag]*1e2
      #incident tb infections in thousands
      ResTab[1:nr+((intv-1)*nr),,4,ag]<-apply(simulation_data[[intv]][,model_years,c(564+ag,575+ag)],c(1,2),sum)*1e3
      #incident tb infections per hundreds of thousands
      ResTab[1:nr+((intv-1)*nr),,5,ag]<-apply(simulation_data[[intv]][,model_years,c(564+ag,575+ag)],c(1,2),sum)/simulation_data[[intv]][,model_years,2+ag]*1e5
      #TB notifications (incidence) (alive+dead at diagnosis) in thousands
      ResTab[1:nr+((intv-1)*nr),,6,ag]<-apply(simulation_data[[intv]][,model_years,c(135,188)+ag],c(1,2),sum)*1e3
      #TB notifications (incidence) (alive+dead at diagnosis) per hundreds of thousands
      ResTab[1:nr+((intv-1)*nr),,7,ag]<-apply(simulation_data[[intv]][,model_years,c(135,188)+ag],c(1,2),sum)/simulation_data[[intv]][,model_years,2+ag]*1e5
      #tb attributable deaths in thousands
      ResTab[1:nr+((intv-1)*nr),,8,ag]<-apply(simulation_data[[intv]][,model_years,c(87,98)+ag],c(1,2),sum)*1e3
      #  tb attributable deaths per hundreds of thousands
      ResTab[1:nr+((intv-1)*nr),,9,ag]<-apply(simulation_data[[intv]][,model_years,c(87,98)+ag],c(1,2),sum)/simulation_data[[intv]][,model_years,2+ag]*1e5
      #  total number of ltbi tests 
      ResTab[1:nr+((intv-1)*nr),,10,ag]<-apply(simulation_data[[intv]][,model_years,c(683,694)+ag],c(1,2),sum)*1e3 
      #  total number of ltbi initiations 
      ResTab[1:nr+((intv-1)*nr),,11,ag]<-apply(simulation_data[[intv]][,model_years,c(705,716)+ag],c(1,2),sum)*1e3 
      #  total number of ltbi completions 
      ResTab[1:nr+((intv-1)*nr),,12,ag]<-apply(simulation_data[[intv]][,model_years,c(727,738)+ag],c(1,2),sum)*1e3
      #  total number of TB Treatment Initations 
      ResTab[1:nr+((intv-1)*nr),,13,ag]<-apply(simulation_data[[intv]][,model_years,c(749,760)+ag],c(1,2),sum)*1e3
      #  total number of TB Treatment Completions 
      ResTab[1:nr+((intv-1)*nr),,14,ag]<-apply(simulation_data[[intv]][,model_years,c(771,782)+ag],c(1,2),sum)*1e3
      ################################################################################
      #US Born population
      ################################################################################
      #get da years
      ResTabus[1:nr+((intv-1)*nr),,1,ag]<-simulation_data[[intv]][,model_years,1]
      #number of ltbi prevalence in thousands
      ResTabus[1:nr+((intv-1)*nr),,2,ag]<-simulation_data[[intv]][,model_years,54+ag]*1e3
      #percentage of ltbi prevalence
      ResTabus[1:nr+((intv-1)*nr),,3,ag]<-simulation_data[[intv]][,model_years,54+ag]/simulation_data[[intv]][,model_years,32+ag]*1e2
      #incident tb infections in thousands
      ResTabus[1:nr+((intv-1)*nr),,4,ag]<-simulation_data[[intv]][,model_years,564+ag]*1e3
      #incident tb infections per hundred thousand
      ResTabus[1:nr+((intv-1)*nr),,5,ag]<-simulation_data[[intv]][,model_years,564+ag]/simulation_data[[intv]][,model_years,32+ag]*1e5
      #TB notifications (alive+dead at diagnosis) in thousands
      ResTabus[1:nr+((intv-1)*nr),,6,ag]<-apply(simulation_data[[intv]][,model_years,c(204,215)+ag],c(1,2),sum)*1e3
      #TB notifications (alive+dead at diagnosis) per hundreds of thousands 
      ResTabus[1:nr+((intv-1)*nr),,7,ag]<-apply(simulation_data[[intv]][,model_years,c(204,215)+ag],c(1,2),sum)/simulation_data[[intv]][,model_years,32+ag]*1e5
      #tb attributable deaths in thousands
      ResTabus[1:nr+((intv-1)*nr),,8,ag]<-simulation_data[[intv]][,model_years,87+ag]*1e3
      #  tb attributable deaths per hundreds of thousands
      ResTabus[1:nr+((intv-1)*nr),,9,ag]<-simulation_data[[intv]][,model_years,87+ag]/simulation_data[[intv]][,model_years,32+ag]*1e5
      #  total number of ltbi tests 
      ResTabus[1:nr+((intv-1)*nr),,10,ag]<-simulation_data[[intv]][,model_years,683+ag]*1e3 
      #  total number of ltbi initiations 
      ResTabus[1:nr+((intv-1)*nr),,11,ag]<-simulation_data[[intv]][,model_years,705+ag]*1e3 
      #  total number of ltbi completions 
      ResTabus[1:nr+((intv-1)*nr),,12,ag]<-simulation_data[[intv]][,model_years,727+ag]*1e3 
      #  total number of TB treatment initations
      ResTabus[1:nr+((intv-1)*nr),,13,ag]<-simulation_data[[intv]][,model_years,749+ag]*1e3 
      #  total number of TB treatment completions
      ResTabus[1:nr+((intv-1)*nr),,14,ag]<-simulation_data[[intv]][,model_years,771+ag]*1e3 
      
      ################################################################################
      #non-US Born population
      ################################################################################
      # years
      ResTabfb[1:nr+((intv-1)*nr),,1,ag]<-simulation_data[[intv]][,model_years,1]
      #number of ltbi prevalence in thousands
      ResTabfb[1:nr+((intv-1)*nr),,2,ag]<-simulation_data[[intv]][,model_years,65+ag]*1e3
      #percentage of ltbi prevalence
      ResTabfb[1:nr+((intv-1)*nr),,3,ag]<-simulation_data[[intv]][,model_years,65+ag]/simulation_data[[intv]][,model_years,43+ag]*1e2
      #incident tb infections in thousands
      ResTabfb[1:nr+((intv-1)*nr),,4,ag]<-simulation_data[[intv]][,model_years,575+ag]*1e3
      #incident tb infections per hundred thousand
      ResTabfb[1:nr+((intv-1)*nr),,5,ag]<-simulation_data[[intv]][,model_years,575+ag]/simulation_data[[intv]][,model_years,43+ag]*1e5
      #TB notifications (alive+dead at diagnosis)
      #calculated as total notifications minus US only notifications in thousands
      ResTabfb[1:nr+((intv-1)*nr),,6,ag]<-(apply(simulation_data[[intv]][,model_years,c(135,188)+ag],c(1,2),sum)-apply(simulation_data[[intv]][,model_years,c(204,215)+ag],c(1,2),sum))*1e3
      #percentage TB notifications (alive+dead at diagnosis) per hundreds of thousands
      ResTabfb[1:nr+((intv-1)*nr),,7,ag]<-(apply(simulation_data[[intv]][,model_years,c(135,188)+ag],c(1,2),sum)-apply(simulation_data[[intv]][,model_years,c(204,215)+ag],c(1,2),sum))/simulation_data[[intv]][,model_years,43+ag]*1e5
      #tb attributable deaths
      ResTabfb[1:nr+((intv-1)*nr),,8,ag]<-simulation_data[[intv]][,model_years,98+ag]*1e3
      # percentage tb attributable deaths per hundreds of thousands
      ResTabfb[1:nr+((intv-1)*nr),,9,ag]<-simulation_data[[intv]][,model_years,98+ag]/simulation_data[[intv]][,model_years,43+ag]*1e5
      #  total number of ltbi tests 
      ResTabfb[1:nr+((intv-1)*nr),,10,ag]<-simulation_data[[intv]][,model_years,694+ag]*1e3 #/simulation_data[[intv]][,model_years,2+ag]*1e5
      #  total number of ltbi initiations 
      ResTabfb[1:nr+((intv-1)*nr),,11,ag]<-simulation_data[[intv]][,model_years,716+ag]*1e3 
      #  total number of ltbi completions 
      ResTabfb[1:nr+((intv-1)*nr),,12,ag]<-simulation_data[[intv]][,model_years,738+ag]*1e3 
      #  total number of TB treatment initations
      ResTabfb[1:nr+((intv-1)*nr),,13,ag]<-simulation_data[[intv]][,model_years,760+ag]*1e3 
      #  total number of TB treatment completions
      ResTabfb[1:nr+((intv-1)*nr),,14,ag]<-simulation_data[[intv]][,model_years,782+ag]*1e3 
  
    } # end age group loop
    
  } # end intervention loop
  
  return(list(ResTab = ResTab,
              ResTabus = ResTabus,
              ResTabfb = ResTabfb))
}



#' Take MITUS Outputs and Format into 3 DataFrames for Big Age Groups: ResTab, ResTabus, ResTabfb
#' 
#' @param simulation_data A list of outputs from MITUS, each the result of OutputsInt.
#' Each list entry should correspond to one scenario/intervention, and the first should 
#' be the basecase.
#'
#' 

format_as_restab_big_ages <- function(simulation_data) { 
  
  model_years = (2020:2050)-1949
  tt<-0
  
  # preflight checklist
  if (class(simulation_data) != 'list') {
    stop("simulation_data must be a list")
  }
  if (! all(sapply(simulation_data, function(sim) length(dim(sim))) == 3)) {
    stop("each simulation_data list entry must have 3 dimensions")
    # simulation index, model years, and age ids.
  }
  if (! all(apply(sapply(simulation_data, dim), 1, function(dim_row) length(unique(dim_row))) == 1)) { 
    stop("simulation_data list entries must have the same dimensions.")
  }
  
  # determine number of interventions and number of simulation runs from 
  # simulation_data.
  nintvs <- length(simulation_data)
  nr <- dim(simulation_data[[1]])[[1]]
  
  #create 3 lists to hold output
  ResTabfb <- ResTabus <- ResTab <- array(NA,dim=c(nr*nintvs,length(model_years),14,4))
  
  
  # For each scenario/intervention, fill in the results from the corresponding 
  # simulation_data entry
  for (intv in 1:nintvs){
    
    #dimensions of restab are:
    #number of datasets, number of ages, number of interventions
    
    #gather the outputs from that model run
    for (b_ag in 1:4){
      ################################################################################
      #total population
      #dimensions are
      #scenarios; length age id;
      # print(1:nr+((intv-1)*10))
      ag_vec<- switch(b_ag,1:11, c(1,2,3),c(4,5,6,7),c(8,9,10,11))
      ################################################################################
      # for (ag_vec in ag_vector){
      ResTab[1:nr+((intv-1)*nr),,1,b_ag]<-simulation_data[[intv]][,model_years,1]
      #number of ltbi prevalence in thousands
      ResTab[1:nr+((intv-1)*nr),,2,b_ag]<-apply(simulation_data[[intv]][,model_years,c(54+ag_vec,65+ag_vec)],c(1,2),sum)*1e3
      #percentage of ltbi prevalence
      ResTab[1:nr+((intv-1)*nr),,3,b_ag]<-apply(simulation_data[[intv]][,model_years,c(54+ag_vec,65+ag_vec)],c(1,2),sum)/rowSums(simulation_data[[intv]][,model_years,2+ag_vec], dims = 2)*1e2
      #incident tb infections in thousands
      ResTab[1:nr+((intv-1)*nr),,4,b_ag]<-apply(simulation_data[[intv]][,model_years,c(564+ag_vec,575+ag_vec)],c(1,2),sum)*1e3
      #incident tb infections per hundreds of thousands
      ResTab[1:nr+((intv-1)*nr),,5,b_ag]<-apply(simulation_data[[intv]][,model_years,c(564+ag_vec,575+ag_vec)],c(1,2),sum)/rowSums(simulation_data[[intv]][,model_years,2+ag_vec], dims = 2)*1e5
      #TB notifications (incidence) (alive+dead at diagnosis) in thousands
      ResTab[1:nr+((intv-1)*nr),,6,b_ag]<-apply(simulation_data[[intv]][,model_years,c(135+ag_vec,188+ag_vec)],c(1,2),sum)*1e3
      # TB notifications (alive+dead at diagnosis) per hundreds of thousands
      ResTab[1:nr+((intv-1)*nr),,7,b_ag]<-apply(simulation_data[[intv]][,model_years,c(135+ag_vec,188+ag_vec)],c(1,2),sum)/rowSums(simulation_data[[intv]][,model_years,2+ag_vec], dims = 2)*1e5
      #tb attributable deaths in thousands
      ResTab[1:nr+((intv-1)*nr),,8,b_ag]<-apply(simulation_data[[intv]][,model_years,c(87+ag_vec,98+ag_vec)],c(1,2),sum)*1e3
      # tb attributable deaths per hundreds of thousands
      ResTab[1:nr+((intv-1)*nr),,9,b_ag]<-apply(simulation_data[[intv]][,model_years,c(87+ag_vec,98+ag_vec)],c(1,2),sum)/rowSums(simulation_data[[intv]][,model_years,2+ag_vec], dims = 2)*1e5
      #  total number of ltbi tests 
      ResTab[1:nr+((intv-1)*nr),,10,b_ag]<-apply(simulation_data[[intv]][,model_years,c(683+ag_vec,694+ag_vec)],c(1,2),sum)*1e3 
      #  total number of ltbi tests 
      ResTab[1:nr+((intv-1)*nr),,11,b_ag]<-apply(simulation_data[[intv]][,model_years,c(705+ag_vec,716+ag_vec)],c(1,2),sum)*1e3 
      #  total number of ltbi tests 
      ResTab[1:nr+((intv-1)*nr),,12,b_ag]<-apply(simulation_data[[intv]][,model_years,c(727+ag_vec,738+ag_vec)],c(1,2),sum)*1e3 
      #  total number of ltbi tests 
      ResTab[1:nr+((intv-1)*nr),,13,b_ag]<-apply(simulation_data[[intv]][,model_years,c(749+ag_vec,760+ag_vec)],c(1,2),sum)*1e3 
      #  total number of ltbi tests 
      ResTab[1:nr+((intv-1)*nr),,14,b_ag]<-apply(simulation_data[[intv]][,model_years,c(771+ag_vec,782+ag_vec)],c(1,2),sum)*1e3 
      ################################################################################
      #US Born population
      ################################################################################
      ResTabus[1:nr+((intv-1)*nr),,1,b_ag]<-simulation_data[[intv]][,model_years,1]
      #number of ltbi prevalence in thousands
      ResTabus[1:nr+((intv-1)*nr),,2,b_ag]<-rowSums(simulation_data[[intv]][,model_years,54+ag_vec], dims = 2)*1e3
      # #percentage of ltbi prevalence
      ResTabus[1:nr+((intv-1)*nr),,3,b_ag]<-(rowSums(simulation_data[[intv]][,model_years,54+ag_vec], dims = 2)/rowSums(simulation_data[[intv]][,model_years,32+ag_vec], dims = 2))*1e2
      #incident tb infections in thousands
      ResTabus[1:nr+((intv-1)*nr),,4,b_ag]<-rowSums(simulation_data[[intv]][,model_years,564+ag_vec], dims=2)*1e3
      #incident tb infections per hundred thousand
      ResTabus[1:nr+((intv-1)*nr),,5,b_ag]<-rowSums(simulation_data[[intv]][,model_years,564+ag_vec], dims=2)/rowSums(simulation_data[[intv]][,model_years,32+ag_vec], dims = 2)*1e5
      #TB notifications (alive+dead at diagnosis) in thousands
      ResTabus[1:nr+((intv-1)*nr),,6,b_ag]<-apply(simulation_data[[intv]][,model_years,c(204+ag_vec, 215+ag_vec)],c(1,2),sum)*1e3
      # # TB notifications (alive+dead at diagnosis) per hundred thousand
      ResTabus[1:nr+((intv-1)*nr),,7,b_ag]<-apply(simulation_data[[intv]][,model_years,c(204+ag_vec, 215+ag_vec)],c(1,2),sum)/rowSums(simulation_data[[intv]][,model_years,32+ag_vec], dims = 2)*1e5
      #tb attributable deaths in thousands
      ResTabus[1:nr+((intv-1)*nr),,8,b_ag]<-rowSums(simulation_data[[intv]][,model_years,87+ag_vec], dims=2)*1e3
      # #  attributable deaths per hundred thousand
      ResTabus[1:nr+((intv-1)*nr),,9,b_ag]<- rowSums(simulation_data[[intv]][,model_years,87+ ag_vec], dims = 2)/rowSums(simulation_data[[intv]][,model_years,32+ag_vec], dims = 2)*1e5
      #  total number of ltbi tests 
      ResTabus[1:nr+((intv-1)*nr),,10,b_ag]<-rowSums(simulation_data[[intv]][,model_years,683+ag_vec], dims = 2)*1e3 
      #  total number of ltbi treatment initations 
      ResTabus[1:nr+((intv-1)*nr),,11,b_ag]<-rowSums(simulation_data[[intv]][,model_years,705+ag_vec],dims = 2)*1e3 
      #  total number of ltbi treatment completions 
      ResTabus[1:nr+((intv-1)*nr),,12,b_ag]<-rowSums(simulation_data[[intv]][,model_years,727+ag_vec],dims = 2)*1e3 
      #  total number of active tb treatment initations 
      ResTabus[1:nr+((intv-1)*nr),,13,b_ag]<-rowSums(simulation_data[[intv]][,model_years,749+ag_vec],dims = 2)*1e3 
      #  total number of active tb treatment completions 
      ResTabus[1:nr+((intv-1)*nr),,14,b_ag]<-rowSums(simulation_data[[intv]][,model_years,771+ag_vec],dims = 2)*1e3 
      # ################################################################################
      # #non-US Born population
      # ################################################################################
      ResTabfb[1:nr+((intv-1)*nr),,1,b_ag]<-simulation_data[[intv]][,model_years,1]
      #number of ltbi prevalence in thousands
      ResTabfb[1:nr+((intv-1)*nr),,2,b_ag]<-rowSums(simulation_data[[intv]][,model_years,65+ag_vec], dims=2)*1e3
      # #percentage of ltbi prevalence
      ResTabfb[1:nr+((intv-1)*nr),,3,b_ag]<-rowSums(simulation_data[[intv]][,model_years,65+ag_vec], dims=2)/rowSums(simulation_data[[intv]][,model_years,43+ag_vec], dims = 2)*1e2
      #incident tb infections in thousands
      ResTabfb[1:nr+((intv-1)*nr),,4,b_ag]<-rowSums(simulation_data[[intv]][,model_years,575+ag_vec], dims=2)*1e3
      #incident tb infections per hundred thousand
      ResTabfb[1:nr+((intv-1)*nr),,5,b_ag]<-rowSums(simulation_data[[intv]][,model_years,575+ag_vec], dims=2)/rowSums(simulation_data[[intv]][,model_years,43+ag_vec], dims = 2)*1e5
      #calculated as total notifications minus US only notifications in thousands
      ResTabfb[1:nr+((intv-1)*nr),,6,b_ag]<-(apply(simulation_data[[intv]][,model_years,c(135+ag_vec, 188+ag_vec)],c(1,2),sum)-apply(simulation_data[[intv]][,model_years,c(204+ag_vec,215+ag_vec)],c(1,2),sum))*1e3
      # TB notifications (alive+dead at diagnosis) per hundred thousand
      ResTabfb[1:nr+((intv-1)*nr),,7,b_ag]<-(apply(simulation_data[[intv]][,model_years,c(135+ag_vec,188+ag_vec)],c(1,2),sum)-apply(simulation_data[[intv]][,model_years,c(204+ag_vec,215+ag_vec)],c(1,2),sum))/rowSums(simulation_data[[intv]][,model_years,43+ag_vec], dims = 2)*1e5
      #tb attributable deaths
      ResTabfb[1:nr+((intv-1)*nr),,8,b_ag]<-rowSums(simulation_data[[intv]][,model_years,98+ag_vec], dims=2)*1e3
      # tb attributable deaths per hundred thousand
      ResTabfb[1:nr+((intv-1)*nr),,9,b_ag]<- rowSums(simulation_data[[intv]][,model_years,98+ ag_vec],dims = 2)/rowSums(simulation_data[[intv]][,model_years,43+ag_vec], dims = 2)*1e5
      #  total number of ltbi tests 
      ResTabfb[1:nr+((intv-1)*nr),,10,b_ag]<-rowSums(simulation_data[[intv]][,model_years,694+ag_vec],dims = 2)*1e3 
      #  total number of ltbi treatment initations 
      ResTabfb[1:nr+((intv-1)*nr),,11,b_ag]<-rowSums(simulation_data[[intv]][,model_years,716+ag_vec],dims = 2)*1e3 
      #  total number of ltbi treatment completions 
      ResTabfb[1:nr+((intv-1)*nr),,12,b_ag]<-rowSums(simulation_data[[intv]][,model_years,738+ag_vec],dims = 2)*1e3 
      #  total number of active tb treatment initations 
      ResTabfb[1:nr+((intv-1)*nr),,13,b_ag]<-rowSums(simulation_data[[intv]][,model_years,760+ag_vec],dims = 2)*1e3 
      #  total number of active tb treatment completions 
      ResTabfb[1:nr+((intv-1)*nr),,14,b_ag]<-rowSums(simulation_data[[intv]][,model_years,782+ag_vec],dims = 2)*1e3 
    } # end age loop
  } # end intv loop
  
  return(list(ResTab = ResTab, 
              ResTabus = ResTabus, 
              ResTabfb = ResTabfb))
}
