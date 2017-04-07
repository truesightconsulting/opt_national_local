# Post-optm Calc
print("Note: Post-optmization Calc.")
if (check.error==0){
  # # put the start spend in for multi goal seek
  # source(paste(main.path,"opt_modelinput_multispend.r",sep=""),local = T)
  
  # freq capping
  is.mta=as.numeric(is_mta())
  if (is.mta==1) source(paste(main.path,"opt_modelinput_post_calc_freqcap.r",sep=""),local = T)
  
  source("opt_input_post_calc.r",local = T)
  
  # convert to json and upload to DB
  source(paste(main.path,"opt_modelinput_tojson.r",sep=""),local = T)
} 