# generate curve file
print("Note: Generating Curve File")
curve=optm.dupe.table(ex.curve,range.wk.optm)
index=grepl("_id",names(ex.season))
temp.dim=names(ex.season)[index]
temp.dim=temp.dim[temp.dim!="client_id"]
curve=merge(curve,ex.season[,c(temp.dim,"factor"),with=F],by=temp.dim,all.x=T,sort=F)
curve=merge(curve,data.table(week_id=range.wk.optm,week_name=out.wk.optm),by=c("week_id"),all.x=T,sort=F)

curve[[beta]]=curve[[beta]]*curve$factor
