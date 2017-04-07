# calc final min and max constraints
# calculate final max constraint 1,min*%; 2,missing then max level 3,other then max level 4, compare with user input max
print("Note: Generating Final Constraints")
# ex.cstr$sp_max[is.na(ex.cstr$sp_max)]=max.level
# ex.cstr$sp_min[is.na(ex.cstr$sp_min)]=0
# ex.cstr.hidden$sp_max_h[is.na(ex.cstr.hidden$sp_max_h)]=max.level
# ex.cstr.hidden$sp_min_h[is.na(ex.cstr.hidden$sp_min_h)]=0
ex.cstr.final=merge(ex.cstr[,c("bdgt_id","sp_plan","sp_min","sp_max"),with=F],ex.cstr.hidden,
                    by="bdgt_id",all.y=T)
ex.cstr.final$sp_max[is.na(ex.cstr.final$sp_max)]=max.level
ex.cstr.final$sp_min[is.na(ex.cstr.final$sp_min)]=0
ex.cstr.final$sp_max_h[is.na(ex.cstr.final$sp_max_h)]=max.level
ex.cstr.final$sp_min_h[is.na(ex.cstr.final$sp_min_h)]=0

# hidden
sp_max_h_final=rep(max.level,nrow(ex.cstr.final))
sp_max_h_final=ex.cstr.final$sp_min*(1+ex.cstr.final$percent_max)
sp_max_h_final[is.na(sp_max_h_final)]=max.level
sp_max_h_final=pmin(sp_max_h_final,ex.cstr.final$sp_max_h)
ex.cstr.final$sp_max_h_final=sp_max_h_final
ex.cstr.final$sp_max_final=pmin(ex.cstr.final$sp_max,sp_max_h_final)
ex.cstr.final$sp_min_final=ex.cstr.final$sp_min_h
ex.cstr.final$sp_min_final[ex.cstr.final$sp_min!=0]=ex.cstr.final$sp_min[ex.cstr.final$sp_min!=0]

# plan
if (ex.setup$optimization_type==3){
  ex.cstr.final$sp_max_final=pmin(ex.cstr.final$sp_max_final,ex.cstr.final$sp_plan)
  ex.cstr.final$sp_max_final[is.na(ex.cstr.final$sp_max_final)]=max.level
}else if (ex.setup$optimization_type %in% c(4,5,9)){
  ex.cstr.final$sp_min_final=pmax(ex.cstr.final$sp_min_final,ex.cstr.final$sp_plan)
  ex.cstr.final$sp_min_final[is.na(ex.cstr.final$sp_min_final)]=0
}

ex.cstr.final=ex.cstr.final[,c("bdgt_id","sp_max_final","sp_min_final","sp_plan"),with=F]
setnames(ex.cstr.final,c("sp_max_final","sp_min_final"),c("sp_max","sp_min"))
curve=merge(curve,ex.cstr.final,by="bdgt_id",all.x=T)
curve$sp_plan[is.na(curve$sp_plan)]=0


# merge cps with curve
curve=merge(curve,ex.cps[,c("bdgt_id","cps"),with=F],by="bdgt_id",all.x=T)
curve$cps[is.na(curve$cps)]=curve$cpp[is.na(curve$cps)]