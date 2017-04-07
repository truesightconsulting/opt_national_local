# flag all the selected curves

print("Note: Filtering Curves")
# merge all the info with curve
dim=get_dim()
for (i in dim){
  temp=parse(text=paste("ex.",i,sep=""))
  temp.dim=get_dim_n(i)
  curve=merge(curve,eval(temp)[,c(temp.dim,paste("flag_",i,sep="")),with=F],
              by=temp.dim)
}
curve=merge(curve,ex.curvegroup[,c("curvegroup_id","flag_curvegroup"),with=F],
            by=c("curvegroup_id"))

bdgt_dim=get_bdgt_dim()
for (i in bdgt_dim){
  temp=parse(text=paste("ex.",i,sep=""))
  temp.dim=get_dim_n(i)
  ex.cstr.hidden=merge(ex.cstr.hidden,eval(temp)[,c(temp.dim,paste("flag_",i,sep="")),with=F],
                       by=temp.dim)
}

# filter out non-selected curves 
dim=paste("flag_",c(dim,"curvegroup"),sep="")
expr=parse(text=paste("flag:=",paste(dim,collapse = "+")))
curve[,eval(expr)]
curve=curve[flag==length(dim),]

bdgt_dim=paste("flag_",c(bdgt_dim),sep="")
expr=parse(text=paste("flag:=",paste(bdgt_dim,collapse = "+")))
ex.cstr.hidden[,eval(expr)]
ex.cstr.hidden=ex.cstr.hidden[flag==length(bdgt_dim),]


if (iter==1) start.sp=ex.cstr[,c("bdgt_id","sp_min"),with=F]


