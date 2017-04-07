# Load in data
print("Note: Loading Data")
if (db.usage){
  options(warn = -1)
  beta=get_beta()
  ex.chan=data.table(dbGetQuery(conn,paste("select * from opt_userinput_dim_chan where opt_id=",opt_id,sep="")))
  ex.sales=data.table(dbGetQuery(conn,paste("select * from opt_userinput_dim_sales where opt_id=",opt_id,sep="")))
  ex.dma=data.table(dbGetQuery(conn,paste("select * from opt_userinput_dim_dma where opt_id=",opt_id,sep="")))
  ex.salchan=data.table(dbGetQuery(conn,paste("select * from opt_userinput_dim_salchan where opt_id=",opt_id,sep="")))
  ex.cps=data.table(dbGetQuery(conn,paste("select * from opt_userinput_cps where opt_id=",opt_id,sep="")))
  ex.cstr.input=data.table(dbGetQuery(conn,paste("select * from opt_userinput_cstr where opt_id=",opt_id,sep="")))
  temp1=ex.cstr.input[is.na(sp_plan),!c("id","opt_id","sp_plan"),with=F]
  temp2=ex.cstr.input[!is.na(sp_plan),!c("id","opt_id","sp_max","sp_min"),with=F]
  ex.cstr.input=merge(temp1,temp2,by=c("date_start","date_end",grep("_id",names(temp1),value=T)),all=T)
  ex.cstr.input$opt_id=opt_id
  ex.national.setup=data.table(dbGetQuery(conn,paste("select * from opt_modelinput_national_setup where client_id=",client_id,sep="")))
  
  ex.multigoal=data.table(dbGetQuery(conn,paste("select * from opt_userinput_multigoal where opt_id=",opt_id,sep="")))
  ex.curvegroup=data.table(dbGetQuery(conn,paste("select * from opt_userinput_curvegroup where opt_id=",opt_id,sep="")))
  max.level=1e+10 # max constrain if missing or goal seek 
  if (run.cstr==T) {
    ex.cstr=data.table(dbGetQuery(conn,paste("select * from opt_input_cstr_output where client_id=",client_id,sep="")))
  }else{
    ex.cstr=data.table(dbGetQuery(conn,paste("select * from opt_userinput_cstr_output where opt_id=",opt_id,sep="")))
  }
  ex.setup=data.table(dbGetQuery(conn,paste("select * from opt_userinput_setup where opt_id=",opt_id,sep="")))
  ex.cstr.hidden=data.table(dbGetQuery(conn,paste("select * from opt_modelinput_hidden_cstr where client_id=",client_id,sep="")))
  ex.bdgt=data.table(dbGetQuery(conn,paste("select * from opt_modelinput_bdgt where client_id=",client_id,sep="")))
  ex.event=data.table(dbGetQuery(conn,paste("select * from opt_userinput_event where opt_id=",opt_id,sep="")))
  ex.season=data.table(dbGetQuery(conn,paste("select * from opt_modelinput_season where client_id=",client_id,sep="")))
  ex.output=data.table(dbGetQuery(conn,paste("select * from opt_modelinput_output where client_id=",client_id,sep="")))
  ex.output=ex.output[,!"client_id",with=F]
  ex.clv=data.table(dbGetQuery(conn,paste("select * from opt_modelinput_clv where client_id=",client_id,sep="")))
  ex.curve=data.table(dbGetQuery(conn,paste("select * from opt_modelinput_curve where client_id=",client_id,sep="")))
  dim=data.table(dbGetQuery(conn,paste("select * from opt_modules_dim where client_id=",client_id,sep="")))$dim
  for (i in 1:length(dim)){
    temp=data.table(dbGetQuery(conn,paste("select * from opt_label_",dim[i],sep="")))
    setnames(temp,names(temp),paste(dim[i],c("id","name"),sep="_"))
    ex.curve=merge(ex.curve,temp,by=paste(dim[i],"id",sep="_"),all.x=T)
  }
  options(warn = 0)
}else{
  ####################################################################################
  # Customized part:
  ####################################################################################
  # customized part
  ex.chan=fread("opt_input_dim_chan.csv",drop="client_id",na.strings="NULL")
  ex.sales=fread("opt_input_dim_sales.csv",drop="client_id",na.strings="NULL")
  ex.dma=fread("opt_input_dim_dma.csv",drop="client_id",na.strings="NULL")
  ex.cps=fread("opt_input_cps.csv",drop="client_id",na.strings="NULL")
  ex.cstr.input=fread("opt_input_cstr.csv",colClasses=c(rep("numeric",4),rep("character",6)),drop="client_id",na.strings="NULL")
  ex.multigoal=fread("opt_input_multigoal.csv",colClasses=c(rep("numeric",3),rep("character",5)),drop="client_id",na.strings="NULL")
  ex.curvegroup=fread("opt_input_curvegroup.csv",drop="client_id",na.strings="NULL")
  max.level=1e+10 # max constrain if missing or goal seek 
  ex.salchan=fread("opt_input_dim_salchan.csv",drop="client_id",na.strings="NULL")
  
  # fixed part
  ex.curve=fread("opt_modelinput_curve.csv",drop="client_id",na.strings="NULL")
  ex.cstr=fread("opt_input_cstr_output.csv",drop="client_id",na.strings="NULL")
  ex.cstr$sp_min=as.numeric(ex.cstr$sp_min)
  ex.cstr$sp_max=as.numeric(ex.cstr$sp_max)
  ex.cstr$sp_plan=as.numeric(ex.cstr$sp_plan)
  #ex.dim=fread("opt_modelinput_dim.csv",drop="client_id",na.strings="NULL")
  ex.setup=fread("opt_input_setup.csv",na.strings="NULL")
  ex.cstr.hidden=fread("opt_modelinput_hidden_cstr.csv",drop="client_id",na.strings="NULL")
  ex.cstr.hidden$sp_min_h=as.numeric(ex.cstr.hidden$sp_min_h)
  ex.cstr.hidden$sp_max_h=as.numeric(ex.cstr.hidden$sp_max_h)
  ex.cstr.hidden$percent_max=as.numeric(ex.cstr.hidden$percent_max)
  ex.bdgt=fread("opt_modelinput_bdgt.csv",drop="client_id",na.strings="NULL")
  if (ex.setup$optimization_time==1){
    ex.event=fread("opt_input_event.csv",drop="client_id",na.strings="NULL")
    ex.season=fread("opt_modelinput_season.csv",drop="client_id",na.strings="NULL")
  }
  ex.output=fread("opt_modelinput_output.csv",na.strings="NULL")
  ####################################################################################
}
