# optm based on constraint setup
print("Note: Generating OPTM Setup based on Constraint")
temp=temp.cstr.input[iter]
# setup time window
if (ex.setup$optimization_time==1){
  ex.setup$date_start=temp$date_start
  ex.setup$date_end=temp$date_end
}

# setup type and type value
ex.setup$optimization_type=1
ex.setup$optimization_type_value=temp[[names(result)[loop.cstr]]]

#setup dim
bdgt_dim=get_bdgt_dim()
for (i in bdgt_dim){
  temp1=parse(text=paste("ex.",i,sep=""))
  temp.dim=get_dim_n(i)
  index=lapply(temp.dim,function(x) eval(temp1)[[x]] %in% as.numeric(strsplit(temp[[x]],",")[[1]]))
  index=Reduce("&",index)
  eval(temp1)[,paste("flag_",i,sep=""):=replace(eval(temp1)[[paste("flag_",i,sep="")]],index,1)]
  eval(temp1)[,paste("flag_",i,sep=""):=replace(eval(temp1)[[paste("flag_",i,sep="")]],!index,0)]
}
