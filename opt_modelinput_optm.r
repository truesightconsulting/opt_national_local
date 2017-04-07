# optm core part
print("Note: Extrating Optimization Setup Parameters")
# optm pace
spend_inc=ex.setup$input_increment 
value.inc.threshold=ex.setup$input_goal_check

# optm type
if (ex.setup$optimization_type==2) {
  budget=max.level
  goal=ex.setup$optimization_type_value
  goal.seek=1
}else if (ex.setup$optimization_type==1){
  budget=ex.setup$optimization_type_value
  goal=max.level
  goal.seek=0
}else if (ex.setup$optimization_type==5){
  budget=ex.setup$optimization_type_value
  goal=max.level
  goal.seek=0
}else if (ex.setup$optimization_type==6){
  budget=max.level
  goal=ex.setup$optimization_type_value
  goal.seek=1
}else if (ex.setup$optimization_type==3){
  budget=ex.setup$optimization_type_value
  goal=max.level
  goal.seek=0
}else if (ex.setup$optimization_type==4){
  budget=0
  goal=max.level
  goal.seek=0
}else if (ex.setup$optimization_type==7){
  budget=max.level
  goal=ex.setup$optimization_type_value
  goal.seek=1
}else if (ex.setup$optimization_type==8){
  budget=max.level
  goal=max.level
  goal.seek=0
}else if (ex.setup$optimization_type==9){
  budget=max.level
  goal=ex.setup$optimization_type_value
  goal.seek=1
}else if (ex.setup$optimization_type==10){
  budget=max.level
  goal=ex.multigoal[iter]$goal
  goal.seek=1
  if (is.na(ex.multigoal[iter]$check)) multi.check=max.level else multi.check=ex.multigoal[iter]$check
}

# re-calculate budget for incremental optm
if (ex.setup$optimization_type==5) budget=budget+sum(curve$sp_min[!duplicated(curve[,c("bdgt_id"),with=F])])
if (ex.setup$optimization_type==9) goal=goal+sum(calc_npv(curve$sp_plan))
if (ex.setup$optimization_type==3) budget=sum(curve$sp_plan[!duplicated(curve[,c("bdgt_id"),with=F])])-budget

# check if national+local curves existing
isnational_index=F
if(nrow(curve[!is.na(is_national)])!=0) {
  isnational_index=T
  curve_national=curve[is_national==1]
  curve_national[,c(unique(c(grep("all",colnames(curve),value=T),
                             "a","b","hl","hrf","max","wks","clv","approval","curvegroup_id","id","client_id","bdgt_id",
                             grep("beta",colnames(curve),value=T),
                             grep("factor",colnames(curve),value=T),
                             grep("flag",colnames(curve),value=T),
                             grep("dma",colnames(curve),value=T)))):=NULL]
  
  curve_local=curve[is.na(is_national),c(unique(c(grep("sales",colnames(curve),value=T),
                                                  grep("salchan",colnames(curve),value=T),
                                                  grep("all",colnames(curve),value=T),
                                                  "a","b","hl","hrf","max","wks","clv","approval","curvegroup_id","id","client_id","bdgt_id",
                                                  grep("beta",colnames(curve),value=T),
                                                  grep("factor",colnames(curve),value=T),
                                                  grep("flag",colnames(curve),value=T),
                                                  grep("dma",colnames(curve),value=T),
                                                  grep("chan",colnames(curve),value=T)))),with=F]
  dim_temp=c()
  # alldim=get_dim()
  # alldim_nodma=
  for(i in get_dim()) {
    if(i!="dma")
    dim_temp=c(dim_temp,get_dim_n(i))
  }
  # dim_temp=c(get_dim_n("chan"),get_dim_n("salchan"),get_dim_n("sales"))
  dim_temp=c(dim_temp,gsub("id","name",dim_temp))
  
  dim_local_national=c()
  # alldim=get_dim()
  # alldim_nodma=
  for(i in get_dim()) {
      dim_local_national=c(dim_local_national,get_dim_n(i))
  }
  # dim_local_national=c(get_dim_n("chan"),get_dim_n("dma"),get_dim_n("salchan"),get_dim_n("sales"))
  curve_local=merge(curve_local,curve_national,by=c(dim_temp))
  curve_local=merge(curve_local,ex.national.setup[,c("bdgt_id","percentage"),with=F],by=c("bdgt_id"))
  curve_local[,sp_min:=sp_min*percentage]
  curve_local[,sp_max:=sp_max*percentage]
  curve_local[,sp_plan:=sp_plan*percentage]
}

# check constrinat error
print("Note: Checking Constraint Logic")
check.error=0
error.missingcurve=0
if (nrow(curve)==0){
  check.error=1
  print(paste(print.msg,"Error: There is no response curve under selected dimensions.",sep=""))
  if (print.msg=="cstr") {
    error.missingcurve=1
  }
}else if (sum(curve$sp_min[!duplicated(curve[,c("bdgt_id"),with=F])])>budget&ex.setup$optimization_type!=4){
  check.error=1
  print(paste(print.msg,"Error: Total start spend exceeds total optimization budget.",sep=""))
} else if (sum(curve$sp_min>curve$sp_max) !=0&ex.setup$optimization_type!=4) {
  check.error=1
  print(paste(print.msg,"Error: At least one start spend exceeds its maximum constraint.",sep=""))
} else if ((sum(curve$sp_max[!duplicated(curve[,c("bdgt_id"),with=F])])<budget)&goal.seek==0&ex.setup$optimization_type!=4){
  check.error=1
  print(paste(print.msg,"Error: Total optimization budget exceeds total maximum constraint.",sep=""))
}else{
  # set current spend
  curve$sp_current=curve$sp_min
  if(isnational_index) curve_local$sp_current=curve_local$sp_min
  
  # check max constrain and flag violated ones
  curve$flag=rep(0,nrow(curve))
  curve$flag[(curve$sp_current+spend_inc)>curve$sp_max]=1
  
  # create some varaibles for optm
  curve$value_current=curve$sp_next=rep(0,nrow(curve))
  if(isnational_index) curve_local$value_current=curve_local$sp_next=rep(0,nrow(curve_local))
  #######################################################################################
  # Optimization hill climbing
  #######################################################################################
  print("Note: Optimizing")
  # compute the number of iteration
  sp_initial_sum=sum(curve$sp_min[!duplicated(curve[,c("bdgt_id"),with=F])])
  loop=floor((budget-sp_initial_sum)/spend_inc)
  
  if ((budget-sp_initial_sum)%%spend_inc!=0) {
    sp_inc_last=budget-sp_initial_sum-loop*spend_inc
    loop=loop+1  
  }else{
    sp_inc_last=spend_inc
  }
  
  # compute initial value
  #???????????????national?????????,?????????value
  if(isnational_index) {
    curve_local$value_current=calc_npv(curve_local$sp_current,curve_local)
    curve_local$decomp_current=calc_decomp(curve_local$sp_current,curve_local)
    curve$value_current=calc_npv(curve$sp_current,curve)
    curve$decomp_current=calc_decomp(curve$sp_current,curve)
    
    # store the start decomp value and npv value
    curve_local$decomp_start=curve_local$decomp_current
    curve$decomp_start=curve$decomp_current
    curve_local$value_start=curve_local$value_current
    curve$value_start=curve$value_current
    
    # national_temp=curve_local[,list(value_current=sum(value_current)),by=c(dim_temp)]
    national_spd=unique(curve[,c("bdgt_id","sp_current"),with=F])  #from pure local
    setnames(national_spd,"sp_current","sp_current_curve")
    
    curve_local=merge(curve_local,national_spd,by=c("bdgt_id"),all.x=T)
    curve_local[,sp_current_curve:=sp_current_curve+sp_current]
    # curve_local$value_current_curve=calc_npv(curve_local$sp_current_curve,curve_local)
    # curve_local[,value_current_local:=value_current_curve-value_current]
    ### value_current,curve???local???,?????????
    
    curve[,sp_current_curve:=sp_current]
    # curve[,value_current_curve:=value_current]
    # curve[,value_current_curve:=NULL]
    
    # calculate national tv in big curve file
    # local_spd=unique(curve_local[,c(dim_temp,"bdgt_id","sp_current_curve"),with=F])
    # local_spd[,bdgt_id:=NULL]
    # local_spd=local_spd[,lapply(.SD,sum),by=c(dim_temp)]
    local=curve_local[,list(value_start=sum(value_start),value_current=sum(value_current),
                                decomp_start=sum(decomp_start),decomp_current=sum(decomp_current),sp_current_curve=sum(sp_current_curve)),by=c(dim_temp)]
    # local=merge(local_spd,local_npv,by=c(dim_temp))
    local[,is_national:=1]
    
    setkeyv(curve,c(dim_temp,"is_national"))
    setkeyv(local,c(dim_temp,"is_national"))
    
    curve=curve[local,':='(value_start=i.value_start,value_current=i.value_current,
                           sp_current_curve=i.sp_current_curve,
                           decomp_start=i.decomp_start,
                           decomp_current=i.decomp_current)]
    
    # calculate local tv in big curve file
    local=curve_local[,c(dim_local_national,"sp_current_curve"),with=F]
    # ,"value_current_curve","value_current_local"
    local[,is_national:=NA]
    # setnames(local,"value_current","value_current_curve")
    
    setkeyv(curve,c(dim_local_national,"is_national"))
    setkeyv(local,c(dim_local_national,"is_national"))
    curve=curve[local,':='(sp_current_curve=i.sp_current_curve)]
    
    # update local value&decomp
    # use bdgt_id to filter out local curve in 'curve' file
    curve[,ind:=bdgt_id %in% unique(curve_local$bdgt_id)]
    curve_temp=curve[ind==T]
    curve=curve[ind==F]
    curve_temp[,value_current:=calc_npv(curve_temp$sp_current_curve,curve_temp)-
                 calc_npv((curve_temp$sp_current_curve-curve_temp$sp_current),curve_temp)]
    curve_temp[,decomp_current:=calc_decomp(curve_temp$sp_current_curve,curve_temp)-
                 calc_decomp((curve_temp$sp_current_curve-curve_temp$sp_current),curve_temp)]
    curve_temp[,value_start:=value_current]
    curve_temp[,decomp_start:=decomp_current]
    curve=rbind(curve,curve_temp)
    curve[,ind:=NULL]
  } else {
    curve$value_current=calc_npv(curve$sp_current,curve)
    curve$decomp_current=calc_decomp(curve$sp_current,curve)
    curve$decomp_start=curve$decomp_current
    curve$value_start=curve$value_current
    curve[,sp_current_curve:=sp_current]
    # curve[,value_current_curve:=value_current]
  }
  setkey(curve, NULL)
  if(isnational_index) setkey(curve_local,NULL)
  
  # goal seek optm initial check
  check.goal.ini=0
  if (ex.setup$optimization_type %in% c(2,9,10)){
    kpi_old=sum(curve$value_current)
    if (kpi_old >=goal) check.goal.ini=1
  }else if(ex.setup$optimization_type==6){
    summary.sp=curve[!duplicated(curve[,c("bdgt_id"),with=F]),]
    if (sum(curve$value_current)==0) kpi_old=0 else
      kpi_old=sum(summary.sp$sp_current)/sum(curve$value_current)
    if (kpi_old >=goal) check.goal.ini=1
  }else if(ex.setup$optimization_type==7){
    summary.sp=curve[!duplicated(curve[,c("bdgt_id"),with=F]),]
    if (sum(curve$value_current)==0) kpi_old=0 else
      kpi_old=sum(curve$value_current)/(sum(summary.sp$sp_current)*(1-tax))-1
    if (kpi_old >=goal) check.goal.ini=1
  }else if(ex.setup$optimization_type==8){
    summary.sp=curve[!duplicated(curve[,c("bdgt_id"),with=F]),]
    if (sum(curve$value_current)==0) kpi_old=0 else
      kpi_old=sum(curve$value_current)-(sum(summary.sp$sp_current)*(1-tax))
  }else{
    kpi_old=0
  }
  
  # optm loop
  if(round(loop/20)==0) int=5 else int=round(loop/20)
  if (loop<0 & ex.setup$optimization_type!=4){
    check.error=1
    print(paste("Error: There is confilict in constraint setup. Please check it or contanct us for assistance.",sep=""))
  }else if (loop==0 | ex.setup$optimization_type==4 | check.goal.ini==1) {
    summary.sp=curve[!duplicated(curve[,c("bdgt_id"),with=F]),]
    print("Note: Optimization is completed.")
    marg=vector("list",1)
    if (check.goal.ini==1) print("Note: Optimization reached goal value.")
  }else if (length(unique(curve$bdgt_id))==1){
    curve$sp_current=budget
    summary.sp=curve[!duplicated(curve[,c("bdgt_id"),with=F]),]
    print("Note: Optimization is completed.")
    marg=vector("list",1)
  }else{
    # create log
    marg=vector("list",1)
    
    # main loop part
    #withProgress(message = '', value = 0, {
    for (i in 1:loop){
      #incProgress(100*i/loop, detail = paste("Optimization: ",round(100*i/loop,digit=0),"% completed. ",sep=""))
      if (i%%int==0) print(paste("Note: Optimization: ",round(100*i/loop,digit=0),"% completed. ",Sys.time(),sep=""))
      
      if (i==loop) {
        sp_inc=sp_inc_last
      } else{
        sp_inc=spend_inc
      }
      
      # compute spend_next
      curve$sp_next[curve$flag==0]=curve$sp_current[curve$flag==0]+sp_inc
      curve$sp_next_curve[curve$flag==0]=curve$sp_current_curve[curve$flag==0]+sp_inc
      # compute value next
      # ???value_next?????????national?????????
      if(isnational_index) {
        curve$value_inc=calc_npv(curve$sp_next_curve,curve)-calc_npv(curve$sp_current_curve,curve)
        curve$decomp_inc=calc_decomp(curve$sp_next_curve,curve)-calc_decomp(curve$sp_current_curve,curve)
        
        curve_local[,sp_next_curve:=0]
        
        local=unique(curve[is_national==1,c(dim_temp,"sp_next","sp_next_curve"),with=F])
        setkeyv(local,dim_temp)
        setkeyv(curve_local,dim_temp)
        curve_local=curve_local[local,':='(sp_next=i.sp_next,sp_next_curve=i.sp_next_curve)]
        curve_local[,':='(sp_next=sp_next*percentage,sp_next_curve=sp_current_curve+sp_inc*percentage)]
        curve_local$value_inc=calc_npv(curve_local$sp_next_curve,curve_local)-calc_npv(curve_local$sp_current_curve,curve_local)
        curve_local$decomp_inc=calc_decomp(curve_local$sp_next_curve,curve_local)-calc_decomp(curve_local$sp_current_curve,curve_local)
        
        national=curve_local[,list(value_inc=sum(value_inc),decomp_inc=sum(decomp_inc)),by=c(dim_temp)]
        national[,is_national:=1]
        setkeyv(national,c(dim_temp,"is_national"))
        setkeyv(curve,c(dim_temp,"is_national"))
        curve=curve[national,':='(value_inc=i.value_inc,decomp_inc=i.decomp_inc)]
        curve$value_inc[curve$flag==1]=0
        curve$decomp_inc[curve$flag==1]=0
      } else {
        # curve$value_next_curve=calc_npv(curve$sp_next_curve,curve)
        # curve$value_next_curve[curve$flag==1]=0
        # value_inc=curve$value_next_curve-curve$value_current_curve
        # value_inc[curve$flag==1]=0
        curve$value_inc=calc_npv(curve$sp_next_curve,curve)-calc_npv(curve$sp_current_curve,curve)
        curve$decomp_inc=calc_decomp(curve$sp_next_curve,curve)-calc_decomp(curve$sp_current_curve,curve)
        curve$value_inc[curve$flag==1]=0
        curve$decomp_inc[curve$flag==1]=0
      }
      
      # compute delta sales
      # value_inc_agg=data.table(bdgt_id=curve$bdgt_id,value_inc)[,list(sum=sum(value_inc)),by=bdgt_id]
      value_inc_agg=curve[,list(sum=sum(value_inc)),by=bdgt_id]
      
      # compute kpi corresponding to optm type and select curve to allocate budget
      if (ex.setup$optimization_type==6){
        summary.sp=curve[!duplicated(curve[,c("bdgt_id"),with=F]),]
        kpi=(sum(summary.sp$sp_current)+sp_inc)/(value_inc_agg$sum+sum(curve$value_current))
        index=value_inc_agg$bdgt_id %in% unique(curve$bdgt_id[curve$flag==1]) 
        kpi[index]=max.level
        index=which.min(kpi)
        if (kpi[index]-kpi_old==0) break
        kpi_old=kpi[index]
      }else if (ex.setup$optimization_type==7){
        summary.sp=curve[!duplicated(curve[,c("bdgt_id"),with=F]),]
        kpi=(value_inc_agg$sum+sum(curve$value_current))/((sum(summary.sp$sp_current)+sp_inc)*(1-tax))-1
        index=value_inc_agg$bdgt_id %in% unique(curve$bdgt_id[curve$flag==1]) 
        kpi[index]=0
        index=which.max(kpi)
        if (kpi[index]-kpi_old==0) break
        kpi_old=kpi[index]
      }else if (ex.setup$optimization_type==8){
        summary.sp=curve[!duplicated(curve[,c("bdgt_id"),with=F]),]
        kpi=(value_inc_agg$sum+sum(curve$value_current))-(sum(summary.sp$sp_current)+sp_inc)*(1-tax)
        index=value_inc_agg$bdgt_id %in% unique(curve$bdgt_id[curve$flag==1]) 
        kpi[index]=0
        index=which.max(kpi)
        if (kpi[index]-kpi_old==0) break
        kpi_old=kpi[index]
      }else {
        index=which.max(value_inc_agg$sum)
        if (value_inc_agg$sum[index]==0) break
      }
      
      bdgt_id1=value_inc_agg$bdgt_id[index]
      curve[,index.curve:=bdgt_id %in% bdgt_id1]
      
      # ?????????national tv???case
      if (isnational_index) {
        if (sum(!is.na(curve[index.curve==T,is_national]))!=0) { # if spend added to a national level bdgt_id
          curve_temp=curve[index.curve==T,c(dim_temp),with=F]
          curve_temp[,ind:=1]
          curve_local=merge(curve_local,curve_temp,by=c(dim_temp),all.x=T)
          
          curve_local[ind==1,sp_current:=sp_next]
          curve_local[ind==1,sp_current_curve:=sp_next_curve]
          curve_local[ind==1,value_current:=value_current+value_inc]
          curve_local[ind==1,decomp_current:=decomp_current+decomp_inc]
          
          # curve_local[ind==1,value_current_curve:=value_current_curve+value_inc]
          
          # calculate national tv in big curve file
          setkeyv(curve_local,c(dim_temp,"bdgt_id"))
          local_spd=unique(curve_local[ind==1,c(dim_temp,"bdgt_id","sp_current","sp_current_curve"),with=F])
          local_spd[,bdgt_id:=NULL]
          local_spd=local_spd[,lapply(.SD,sum),by=c(dim_temp)]
          local_npv=curve_local[ind==1,list(value_current=sum(value_current),decomp_current=sum(decomp_current)),by=c(dim_temp)]
          local=merge(local_spd,local_npv,by=c(dim_temp))
          local[,is_national:=1]
          
          setkeyv(curve,c(dim_temp,"is_national"))
          setkeyv(local,c(dim_temp,"is_national"))
          
          curve=curve[local,':='(sp_current=i.sp_current,sp_current_curve=i.sp_current_curve,
                                 decomp_current=i.decomp_current,value_current=i.value_current)]
          
          # calculate local tv in big curve file
          local=curve_local[ind==1,c(dim_local_national,"sp_current_curve"),with=F]
          local[,is_national:=NA]
          # setnames(local,"value_current_local","value_current")
          
          setkeyv(curve,c(dim_local_national,"is_national"))
          setkeyv(local,c(dim_local_national,"is_national"))
          curve=curve[local,':='(sp_current_curve=i.sp_current_curve)]
          
          
          curve_local[,ind:=NULL]
          
        } else if (all(is.na(curve[index.curve==T,is_national])) &
                   sum(unique(curve[index.curve==T,c(dim_temp),with=F]) %in% unique(curve_local[,c(dim_temp),with=F]))==length(dim_temp)) { # if spend is added to a local level bdgt_id
          # curve$sp_current[index.curve]=curve$sp_current[index.curve]+sp_inc
          # curve$value_current[index.curve]=curve$value_next[index.curve] 
          curve[index.curve==T,sp_current:=sp_current+sp_inc]
          curve[index.curve==T,sp_current_curve:=sp_current_curve+sp_inc]
          curve[index.curve==T,value_current:=value_current+value_inc]
          curve[index.curve==T,decomp_current:=decomp_current+decomp_inc]
          
          national=curve[index.curve==T,c("bdgt_id",dim_local_national,"sp_current_curve"),with=F]
          
          setkeyv(curve_local,c(dim_local_national,"bdgt_id"))
          setkeyv(national,c(dim_local_national,"bdgt_id"))
          curve_local=curve_local[national,':='(sp_current_curve=i.sp_current_curve)]
          
          #update sp_current_curve in curve file for is_national=1
          setkeyv(curve_local,c(dim_temp,"bdgt_id"))
          local_spd=unique(curve_local[,c(dim_temp,"bdgt_id","sp_current_curve"),with=F])
          local_spd[,bdgt_id:=NULL]
          local_spd=local_spd[,lapply(.SD,sum),by=c(dim_temp)]
          # local_npv=curve_local[,list(value_current=sum(value_current),decomp_current=sum(decomp_current)),by=c(dim_temp)]
          # local=merge(local_spd,local_npv,by=c(dim_temp))
          local_spd[,is_national:=1]
          
          setkeyv(curve,c(dim_temp,"is_national"))
          setkeyv(local_spd,c(dim_temp,"is_national"))
          
          curve=curve[local_spd,':='(sp_current_curve=i.sp_current_curve)]
          
          
        } else {
          curve[index.curve==T,sp_current:=sp_current+sp_inc]
          curve[index.curve==T,sp_current_curve:=sp_current_curve+sp_inc]
          curve[index.curve==T,value_current:=value_current+value_inc]
          curve[index.curve==T,decomp_current:=decomp_current+decomp_inc]
        }
      } else {
        curve[index.curve==T,sp_current:=sp_current+sp_inc]
        curve[index.curve==T,sp_current_curve:=sp_current_curve+sp_inc]
        curve[index.curve==T,value_current:=value_current+value_inc]
        curve[index.curve==T,decomp_current:=decomp_current+decomp_inc]
        # curve[index.curve==T,value_current_curve:=value_current_curve+value_inc]
        
      }  
      
      setkey(curve, NULL)
      if (isnational_index) setkey(curve_local,NULL)
      
      # curve$sp_current[index.curve]=curve$sp_current[index.curve]+sp_inc
      # curve$value_current[index.curve]=curve$value_next[index.curve] 
      
      # check max constrain and flag if necessary
      index.check=(curve[index.curve==T,sp_current]+sp_inc)>curve[index.curve==T,sp_max]
      curve[index.curve==T]$flag[index.check]=1 
      
      # record marginal
      max.value.inc=value_inc_agg$sum[index]
      marg[[i]]=data.frame(Iteration=i,bdgt_id1,Value_inc=max.value.inc,Spend_inc=sp_inc)
      
      # check goal-based optimization
      if (ex.setup$optimization_type %in% c(1,3,5)){
        goal.check=0
      }else if (ex.setup$optimization_type %in% c(2,9,10)){
        goal.check=sum(curve$value_current)
        source(paste(main.path,"opt_modelinput_multicheck.r",sep=""),local = T)
        if (sum(curve$value_current)>=goal) break
        if (max.value.inc<value.inc.threshold) break  
      }else if (ex.setup$optimization_type==6){
        goal.check=kpi_old
        if(kpi_old>=goal) break
        if (max.value.inc<(value.inc.threshold)) break  
      }else if (ex.setup$optimization_type==7){
        goal.check=kpi_old
        if (i==1 & kpi_old<goal) break
        if (max.value.inc<value.inc.threshold) break 
        #check next step
        curve1=curve
        if (i==loop) {
          sp_inc=sp_inc_last
        } else{
          sp_inc=spend_inc
        }
        # compute spend and value next
        curve1$sp_next[curve1$flag==0]=curve1$sp_current[curve1$flag==0]+sp_inc
        curve1$value_next=calc_npv(curve1$sp_next)
        curve1$value_next[curve$flag==1]=0
        # compute delta sales
        value_inc=curve1$value_next-curve1$value_current
        value_inc[curve1$flag==1]=0
        # compute next kpi
        value_inc_agg=data.table(bdgt_id=curve1$bdgt_id,value_inc)[,list(sum=sum(value_inc)),by=bdgt_id]
        summary.sp=curve1[!duplicated(curve1[,c("bdgt_id"),with=F]),]
        kpi1=(value_inc_agg$sum+sum(curve1$value_current))/((sum(summary.sp$sp_current)+sp_inc)*(1-tax))-1
        index1=value_inc_agg$bdgt_id %in% unique(curve1$bdgt_id[curve1$flag==1]) 
        kpi1[index1]=0
        if(kpi_old>=goal & max(kpi1)<goal) break
      }else if (ex.setup$optimization_type==8){
        goal.check=kpi_old
        curve1=curve
        if (i==loop) {
          sp_inc=sp_inc_last
        } else{
          sp_inc=spend_inc
        }
        # compute spend and value next
        curve1$sp_next[curve1$flag==0]=curve1$sp_current[curve1$flag==0]+sp_inc
        curve1$value_next=calc_npv(curve1$sp_next)
        curve1$value_next[curve$flag==1]=0
        # compute delta sales
        value_inc=curve1$value_next-curve1$value_current
        value_inc[curve1$flag==1]=0
        # compute next kpi
        value_inc_agg=data.table(bdgt_id=curve1$bdgt_id,value_inc)[,list(sum=sum(value_inc)),by=bdgt_id]
        summary.sp=curve1[!duplicated(curve1[,c("bdgt_id"),with=F]),]
        kpi1=(value_inc_agg$sum+sum(curve1$value_current))-(sum(summary.sp$sp_current)+sp_inc)*(1-tax)
        index1=value_inc_agg$bdgt_id %in% unique(curve1$bdgt_id[curve1$flag==1]) 
        kpi1[index1]=0
        if (max(kpi1)<goal.check) break
      }
      if (i==loop) print("Note: Optimization is completed.")
    } # optm for loop 
    #})#withProgress
      
    # output message
    if((goal.check>=goal)&(ex.setup$optimization_type %in% c(2,6,7,9,10))) {
      print("Note: Optimization reached goal value.")
    }
    
    if ((goal.check<goal)&(ex.setup$optimization_type %in% c(2,6,7,9,10))) print(paste(print.msg,"Warning: Optimization cannot hit goal. ","Goal:",format(round(goal,digits=0),big.mark=",", trim=T,scientific = F)," Actual generated:",format(round(goal.check,digits=0),big.mark=",", trim=T,scientific = F),sep=""))
    if (ex.setup$optimization_type ==10) if (multi.stop==1) print(paste(print.msg,"Warning: Goal seek hit stop criterion.",sep=""))
    
    summary.sp=curve[!duplicated(curve[,c("bdgt_id"),with=F]),]
    bdgt.left=budget-sum(summary.sp$sp_current)
    if (value_inc_agg$sum[index]==0&goal.seek==0){
      if (sum(curve$flag==0)==0) {
        print(paste(print.msg,"Warning: Optimization cannot allocate all budget since all response curves have hit their maximum constraints. ","Budget left:",format(round(bdgt.left,digits=0),big.mark=",", trim=T,scientific = F),sep=""))
      }else{
        print(paste(print.msg,"Warning: Optimization cannot allocate all budget since all response curves have hit their saturation points. ","Budget left:",format(round(bdgt.left,digits=0),big.mark=",", trim=T,scientific = F),sep=""))
      } 
    }
    
    # create final log
    if (!is.null(marg[[1]])){
      marg1=marg[!sapply(marg,is.null)]
      marg1=data.table(do.call("rbind",marg1))
      marg1=marg1[,spend_start:=sum(ex.cstr.final$sp_min)]
      marg1=marg1[,value_start:=sum(calc_npv(curve$sp_min,curve),na.rm=T)]
      marg1=marg1[order(marg1$Iteration),]
      bdgt_dim=str_split(ex.bdgt$bdgt_dim,",")[[1]]
      bdgt_dim=paste(as.vector(do.call(cbind,strsplit(bdgt_dim,"_id"))),"_name",sep="")
      setnames(marg1,"bdgt_id1","bdgt_id")
      marg1=merge(marg1,unique(curve[,c(bdgt_dim,"bdgt_id"),with=F]),by="bdgt_id")
    }
  }
}# constraint error check