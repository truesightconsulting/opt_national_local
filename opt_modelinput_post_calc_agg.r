# summarize result
print("Note: Summarizing output.")
if (ex.setup$optimization_time==1){
  month=strftime(curve$week_name,"%m/%Y")
  curve$month_id=as.Date(paste("01/",month,sep=""),format="%d/%m/%Y")
  curve$month_name=curve$month_id
}
summary.sp=curve[!duplicated(curve[,c("bdgt_id"),with=F]),]
summary=vector("list",nrow(ex.output))
bdgt_dim=str_split(ex.bdgt$bdgt_dim,",")[[1]]

for (i in 1:nrow(ex.output)){
  names(summary)[i]=ex.output$label[i]
  dim=str_split(ex.output$dim[i],",")[[1]]
  dim1=c(dim,paste(as.vector(do.call(cbind,strsplit(dim,"_id"))),"_name",sep=""))
  
  summary.sp1=summary.sp[,eval(expr_agg(input=input_sp,output=output_sp)),by=c(bdgt_dim[bdgt_dim %in% dim])]
  summary.npv=curve[,eval(expr_agg(input=input_decomp,output=output_decomp)),by=c(dim1)]
  if(sum(bdgt_dim %in% dim)==0){
    summary[[i]]=data.table(summary.npv,summary.sp1)
  }else{
    summary[[i]]=merge(summary.npv,summary.sp1,by=c(bdgt_dim[bdgt_dim %in% dim]),all.x=T)
  }
  # for summary with filter   
  if(!is.na(ex.output$filter[i])){
    temp.dim=get_dim_n(ex.output$filter[i])
    temp.dim=paste(strsplit(temp.dim,"_id"),"_name",sep="")
    if(nrow(unique(summary[[i]][,temp.dim,with=F],key=NULL))>1) {
      index=grep(ex.output$filter[i],dim)
      dim=dim[-index]
      index=grep(ex.output$filter[i],dim1)
      dim1=dim1[-index]
      
      summary.sp1=summary.sp[,eval(expr_agg(input=input_sp,output=output_sp)),by=c(bdgt_dim[bdgt_dim %in% dim])]
      summary.npv=curve[,eval(expr_agg(input=input_decomp,output=output_decomp)),by=c(dim1)]
      if(sum(bdgt_dim %in% dim)==0){
        temp=data.table(summary.npv,summary.sp1)
      }else{
        temp=merge(summary.npv,summary.sp1,by=c(bdgt_dim[bdgt_dim %in% dim]),all.x=T)
      }
      
      temp=rbindlist(list(summary[[i]],temp),fill=T,use.names = T)
      temp[is.na(temp)]="All"
      summary[[i]]=temp
    }
  }
}