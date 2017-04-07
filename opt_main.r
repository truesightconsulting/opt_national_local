#setwd("d:\\Users\\xzhou\\Desktop\\comcast opt test\\")
print("Note: Optimization Process")
start.time=Sys.time()

# load functions
source(paste(main.path,"opt_modelinput_functions.r",sep=""),local = T)

# Load in data
source(paste(main.path,"opt_modelinput_load.r",sep=""),local = T)


# check multi-goal seek
if (ex.setup$optimization_type==10) {
  n=nrow(ex.multigoal)
  marg.list=vector("list",n)
}else n=1

sp.multi=vector("list",n)
for (iter in 1:n){
  print(paste("Note: Optimization Round",iter))
  
  # reload in data
  if (iter>1) source(paste(main.path,"opt_modelinput_load.r",sep=""),local = T)
  
  # generate relevant dim input tables for certain type of optm
  source(paste(main.path,"opt_modelinput_force_flag.r",sep=""),local = T)
  
  # tweak searching pace
  if (ex.setup$input_increment>ex.setup$optimization_type_value & ex.setup$optimization_type %in% c(1,3,5)){
    ex.setup$input_increment=ex.setup$optimization_type_value/5
  }
  
  # generate curve and cps table for time-variant optm
  source(paste(main.path,"opt_modelinput_gen_tables.r",sep=""),local = T)
  
  # time window error check
  if (ex.setup$optimization_time!=1) check.event.date=0
  
  # prepare curve parameters for optm
  source(paste(main.path,"opt_modelinput_curve_tweak.r",sep=""),local = T)
  
  # flag all the selected curves
  source(paste(main.path,"opt_modelinput_flag_table.r",sep=""),local = T)
  
  # update min spend from previous iteration for multi-goal seek
  if (iter>1) source(paste(main.path,"opt_modelinput_update_minsp.r",sep=""),local = T)
  
  # calc final min and max constraints, them merge them with curve, as well as cps
  source(paste(main.path,"opt_modelinput_calc_cstr.r",sep=""),local = T)
  
  # optmization
  source(paste(main.path,"opt_modelinput_optm.r",sep=""),local = T)
  
  # re-generate min spend for multi-goal seek
  if (ex.setup$optimization_type==10) source(paste(main.path,"opt_modelinput_gen_minsp.r",sep=""),local = T)
}

# output
source(paste(main.path,"opt_modelinput_post_calc.r",sep=""),local = T)

#save.image("opt_output.Rdata")
end=Sys.time()-start.time
print(paste("Note: Run time: ",round(end[[1]],digit=2),attr(end,"units"),sep="")) 





