# generate event file
print("Note: Generating Event File")
event.name=dbGetQuery(conn,paste("select dim from opt_modules_dim where flag_event=1 and client_id=",client_id,sep=""))$dim
dim.event=paste(event.name,"_id",sep="")
check.event.date=0
if (nrow(ex.event)!=0) {
  temp.event=vector("list",nrow(ex.event))
  for (j in 1:nrow(ex.event)){
    if (ex.setup$optimization_time==1){
      date.temp=optm.date(ex.event$date_start[j],ex.event$date_end[j])
      range.wk=date.temp$range.wk
      out.wk=date.temp$out.wk
      if (range.wk[1]==range.wk[length(range.wk)] & length(range.wk)!=1) range.wk[length(range.wk)]=-1
    }
    sales_count=ex.event[j,dim.event,with=F]
    cj.list=foreach (k=1:ncol(sales_count),.multicombine = T) %do% {
      as.integer(strsplit(sales_count[[k]],',')[[1]])
    }
    sales_count1=do.call(CJ,cj.list)
    setnames(sales_count1,names(sales_count1),names(sales_count))
    if (ex.setup$optimization_time==1){
      sales_count2=sales_count1[rep(1:nrow(sales_count1),each=length(range.wk))]
    }else sales_count2=sales_count1
    level=rep(ex.event$level[j],nrow(sales_count2))
    if (ex.setup$optimization_time==1){
      temp.event[[j]]=data.table(sales_count2,week_id=rep(range.wk,nrow(sales_count1)),level=level)
    }else temp.event[[j]]=data.table(sales_count2,level=level)
    setnames(temp.event[[j]],"level",paste("level",j,sep="_"))
  }
  if (ex.setup$optimization_time==1){
    temp.output=Reduce(function(...) merge(...,all=TRUE,by=c(dim.event,"week_id")), temp.event)
    temp.output[is.na(temp.output)]=1
    input.event=data.table(temp.output[,c(dim.event,"week_id"),with=F],level=apply(temp.output[,!c(dim.event,"week_id"),with=F],1,prod))
    
    curve=merge(curve,input.event,by=c(dim.event,"week_id"),all.x=T)
  }else {
    temp.output=Reduce(function(...) merge(...,all=TRUE,by=c(dim.event)), temp.event)
    temp.output[is.na(temp.output)]=1
    input.event=data.table(temp.output[,c(dim.event),with=F],level=apply(temp.output[,!c(dim.event),with=F],1,prod))
    
    curve=merge(curve,input.event,by=c(dim.event),all.x=T)
  }

  curve$level[is.na(curve$level)]=1
  curve[[beta]]=curve[[beta]]*curve$level
}




