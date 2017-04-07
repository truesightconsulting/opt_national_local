#######################################################################################
# Customized part:
#######################################################################################
print("Note: Dimensions Seletion for Certain Type of Optimization")

if (ex.setup$optimization_type %in% c(3,4,5,9)){
  temp.flag=ex.cstr
  ex.plan.input=temp.flag[!is.na(sp_plan)]
  
  bdgt_dim=get_bdgt_dim()
  for (i in bdgt_dim){
    temp=parse(text=paste("ex.",i,sep=""))
    temp.dim=paste(get_dim_n(i),collapse = ",")
    temp.dim=parse(text=paste("paste(",temp.dim,",sep='+')",sep=""))
    index=eval(temp)[,eval(temp.dim)] %in% unique(ex.plan.input[,eval(temp.dim)]) 
    eval(temp)[,paste("flag_",i,sep=""):=replace(eval(temp)[[paste("flag_",i,sep="")]],index,1)]
    eval(temp)[,paste("flag_",i,sep=""):=replace(eval(temp)[[paste("flag_",i,sep="")]],!index,0)]
  }
  
}else if (ex.setup$optimization_type==10){
  # CUSTOM FOR NOW!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  dim.multi=ex.multigoal[iter]
  index1=ex.sales$sales1_id %in% as.numeric(strsplit(dim.multi[["sales1_id"]],",")[[1]]) 
  index=index1
  ex.sales$flag_sales[index]=1
  ex.sales$flag_sales[!index]=0
}