# Time-variant version part: the sequence of code is fixed!!!
if (ex.setup$optimization_time==1){
  
  # manipulate optm date
  source(paste(main.path,("opt_modelinput_gen_optmdate.r"),sep=""),local=T)
  
  # generate cps file
  print("Note: Generating CPS File")
  ex.cps=optm.dupe.table(ex.cps,range.wk.optm)
  
  # generate curve file
  source(paste(main.path,("opt_modelinput_gen_curve.r"),sep=""),local=T)
  
  # generate event file
  source(paste(main.path,("opt_modelinput_gen_event.r"),sep=""),local=T)
  
}else{
  # generate curve
  curve=ex.curve
  
  # generate event file
  source(paste(main.path,("opt_modelinput_gen_event.r"),sep=""),local=T)
}