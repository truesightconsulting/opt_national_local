# format output
summary_output=vector("list",nrow(ex.output))
print("Note: Outputing result.")
for (i in 1:nrow(ex.output)){
  # i=7
  dim=str_split(ex.output$dim[i],",")[[1]]
  if ("week_id" %in% dim==F){
    temp=summary[[ex.output$label[i]]][(spend!=0|spend_start!=0)&(decomp!=0|decomp_start!=0),!dim,with=F]
  }else temp=summary[[ex.output$label[i]]]
  
  if (is.mta==1){
    # freq cap
    if ("chan3_id" %in% dim){
      temp=merge(temp,unique(curve[,c("chan2_name","chan3_name","cap"),with=F],by=NULL),
                 by=c("chan2_name","chan3_name"),all.x=T)
    }else temp$cap=rep(NA,nrow(temp))
    setnames(temp,"cap","Freq Capping")
  }
  
  # calc efficiency
  temp=temp[,eval(expr_eff(metric_eff,f_eff))]
  for(j in metric_eff){
    temp[[j]][temp[[j]]==Inf]=0
  }
  temp[is.na(temp)]=0
  
  # round columns
  temp.dim=names(temp)[grep("_name",names(temp))]
  if (ex.output$type[i]!="excel"){
    temp[,metric_reg]=round(temp[,metric_reg,with=F],digits = 0)
    temp[,metric_eff]=round(temp[,metric_eff,with=F],digits = 1)
  }
  
  # re-order columns
  if (dim[1]=="all_id") {
    temp=data.table(temp[,temp.dim,with=F],temp[,!temp.dim,with=F][,order_all,with=F])
  } else{
    temp=data.table(temp[,temp.dim,with=F],temp[,!temp.dim,with=F][,order_other,with=F])
  }
  
  # rename columns
  temp=temp[order(-spend)]
  setnames(temp,c(metric_reg,metric_eff),c(name_reg,name_eff))
  
  # delete columns
  if (dim[1]=="all_id"){
    temp=drop_col(drop_all)
  }else if (ex.output$type[i]=="excel"){
    # convert column names
    dim.id=data.table(dbGetQuery(conn,paste("select * from opt_modules_dim a inner join opt_label_modules_dim b on a.opt_label_modules_dim_id =b.id where client_id=",client_id,sep="")))
    dim.id$dim=paste(dim.id$dim,"_name",sep="")
    index=grepl("_name",names(temp))
    dim.name=merge(data.table(dim=names(temp)[index]),dim.id[,c("dim","label"),with=F],by="dim",all.x=T)
    setnames(temp,dim.name$dim,dim.name$label)
    # drop columns
    temp=drop_col(drop_excel)
  }else{
    temp=drop_col(drop_other)
  }

  # check with or w/o plan
  if (!ex.setup$optimization_type %in% c(3,5,9)){
    temp=temp[,!c(grep("Planned",names(temp),value=T)),with=F]
  }
  
  # save to list
  summary_output[[i]]=temp
  names(summary_output)[i]=ex.output$label[i]
  
  # export
  if (db.usage){
    index=ex.output$label==ex.output$label[i]
    ex.output$json[index]=toJSON(temp)
  }else write.csv(temp,paste("opt_output_",ex.output$label[i],".csv",sep=""),row.names = F)
  
  if (is.mta==1) source(paste(main.path,"opt_modelinput_post_calc_curve.r",sep=""),local = T)
}