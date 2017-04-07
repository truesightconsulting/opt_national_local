# check which constraint need to be calc'ed
if (sum(!is.na(ex.cstr.input$sp_min))!=0) index1=T else index1=F
if (sum(!is.na(ex.cstr.input$sp_max))!=0) index2=T else index2=F
if (sum(!is.na(ex.cstr.input$sp_plan))!=0) index3=T else index3=F
index=c(index1,index2,index3)
# gen list to save result
n=sum(index)
result=vector("list",n)
names(result)=cstr.name[index]

# constraint gen loop
for (loop.cstr in 1:n){
  # Load in data
  source(paste(main.path,"opt_modelinput_load.r",sep=""),local = T)
  
  if (allone.check==F & ex.setup$optimization_time==2)  ex.cstr.input=copy(ex.cstr.input[cstr.index!=0])
  
  # generate setup input tables for constraint
  source(paste(main.path,"opt_modelinput_check_cstr.r",sep=""),local = T)
  
  print("Note: Building Clusters")
  if (nrow.check==1){
    # calc no. of cluster
    #       if (nrow(temp.cstr.input)>cutoff.cl) no.cl=cutoff.cl else no.cl=1
    #       cl=makeCluster(no.cl,type="SOCK",outfile="")
    #       clusterExport(cl,c("temp.cstr.input"))
    #       registerDoSNOW(cl)
    
    #loop for each row of constraint
    print("Note: Calulating Constraint Allocation")
    result[[names(result)[loop.cstr]]]=
      foreach(iter=1:nrow(temp.cstr.input), .multicombine=T,
              .packages=c("data.table","bit64"),.verbose=F) %do%
              {
                print(paste("Note: ",names(result)[loop.cstr]," ",iter," ",Sys.time(),sep=""))
                
                # load function
                source(paste(main.path,"opt_modelinput_functions.r",sep=""),local=T)
                
                # Load in data
                source(paste(main.path,"opt_modelinput_load.r",sep=""),local=T)
                
                # optm par setup based on constraint 
                source(paste(main.path,"opt_modelinput_cstr_setup.r",sep=""),local=T)
                
                # tweak searching pace
                if (ex.setup$input_increment>ex.setup$optimization_type_value){
                  ex.setup$input_increment=max(ex.setup$optimization_type_value/5,1)
                }
                
                # generate curve and cps tables for time-variant version 
                source(paste(main.path,"opt_modelinput_gen_tables.r",sep=""),local=T)
                
                # prepare curve parameters for optm
                source(paste(main.path,"opt_modelinput_curve_tweak.r",sep=""),local=T)
                
                # flag all the selected curves
                source(paste(main.path,"opt_modelinput_flag_table.r",sep=""),local=T)
                
                # calc final min and max constraints, them merge them with curve, as well as cps
                source(paste(main.path,"opt_modelinput_calc_cstr.r",sep=""),local=T)
                
                #################################################################################
                # merge sp_min result if exist for sp_max optmization iteration
                #################################################################################
                if (names(result)[1]=="sp_min" & names(result)[loop.cstr]=="sp_max") {
                  curve=merge(curve[,!"sp_min",with=F],result[["sp_min"]],by="bdgt_id",all.x=T)
                  curve$sp_min[is.na(curve$sp_min)]=0
                }
                
                # optmization
                source(paste(main.path,"opt_modelinput_optm.r",sep=""),local=T)
                
                # save result
                if(error.missingcurve==0){
                  temp.output=summary.sp[,c("bdgt_id","sp_current"),with=F]
                  setnames(temp.output,"sp_current",paste("sp_current_",iter,sep=""))
                }else {
                  error.sim=error.sim+1
                  temp.output=NULL
                }
                temp.output
              }# loop of each row of constaint
    
    # stopCluster(cl)
    
    print("Note: Transforming Result")
    # merge results and generate final constraint table
    temp.output=Reduce(function(...) merge(...,all=TRUE,by="bdgt_id"),Filter(Negate(is.null), result[[names(result)[loop.cstr]]]))
    
    if (names(result)[loop.cstr]=="sp_min"){
      temp.output=Reduce(function(...) merge(...,all=TRUE,by="bdgt_id"),Filter(Negate(is.null),list(temp.output,temp.cstr.output[,c("bdgt_id","sp_min"),with=F])))
      temp.output[is.na(temp.output)]=0
      result[[names(result)[loop.cstr]]]=data.table(bdgt_id=temp.output[[1]],sp_min=do.call(pmax, temp.output[,-1,with=F]))
    }else if (names(result)[loop.cstr]=="sp_max"){
      temp.output[is.na(temp.output)]=max.level
      result[[names(result)[loop.cstr]]]=data.table(bdgt_id=temp.output[[1]],sp_max=do.call(pmin, temp.output[,-1,with=F]))
    }else if (names(result)[loop.cstr]=="sp_plan"){
      temp.output[is.na(temp.output)]=0
      result[[names(result)[loop.cstr]]]=data.table(bdgt_id=temp.output[[1]],sp_plan=do.call(pmax, temp.output[,-1,with=F]))
    }
  }# nrow.check
}# loop of min/max/plan

print("Note: Exporting Result")
# Load in data
source(paste(main.path,"opt_modelinput_load.r",sep=""),local = T)

# calc final min and max for optm with plan
result.all=Reduce(function(...) merge(...,all=TRUE,by="bdgt_id"), result)
if ("sp_min" %in% names(result.all)) result.all$sp_min[is.na(result.all$sp_min)]=0
if ("sp_max" %in% names(result.all)) result.all$sp_max[is.na(result.all$sp_max)]=max.level
if ("sp_plan" %in% names(result.all)) result.all$sp_plan[is.na(result.all$sp_plan)]=0
#   if (ex.setup$optimization_type %in% c(5,9)){
#     if ("sp_min" %in% names(result.all)) result.all=result.all[,sp_min:=sp_min+sp_plan] else
#       result.all=result.all[,sp_min:=sp_plan]
#     if ("sp_max" %in% names(result.all)) result.all=result.all[,sp_max:=sp_max+sp_plan]
#   }

# update result to ex.cstr
ex.cstr=merge(ex.cstr[,!names(result.all)[-1],with=F],result.all,by="bdgt_id",all.y=T)
ex.cstr=data.table(opt_id=rep(opt_id,nrow(ex.cstr)),ex.cstr)