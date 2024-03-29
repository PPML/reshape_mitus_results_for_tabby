tabby2_indices<-function(ResNam){

 index_vector<- c(which(ResNam=="N_0_4"), #total population,
                  which(ResNam=="N_US_0_4"),
                  which(ResNam=="N_FB_0_4"),

                  which(ResNam=="N_US_LTBI_0_4"),  #first US LTBI count
                  which(ResNam=="N_FB_LTBI_0_4"),  #first US LTBI count

                  which(ResNam=="N_newinf_USB_0_4"), #first US new infection
                  which(ResNam=="N_newinf_NUSB_0_4"), #first NUS new infection

                  which(ResNam=="NOTIF_0_4"),
                  which(ResNam=="NOTIF_MORT_0_4"),

                  which(ResNam=="TBMORT_US_0_4"),
                  which(ResNam=="TBMORT_NUS_0_4"),
                  #USB SPECIFIC
                  which(ResNam=="NOTIF_US_0_4"),
                  which(ResNam=="NOTIF_US_MORT_0_4")
#note NUSB notif outcomes are calculated from the difference between total and USB notif outputs.
)
  return(index_vector)
}
