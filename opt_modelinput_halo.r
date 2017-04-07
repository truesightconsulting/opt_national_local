# halo effect of goal seek
print("Note: Calculating Halo Effect")
if (ex.setup$optimization_type!=10) temp.sp=summary.sp[,c("bdgt_id","sp_current"),with=F]
curve=curve.org
# merge current spend with curve
curve=merge(curve,temp.sp,by="bdgt_id",all.x=T)
# merge min and max spend with curve
curve=merge(curve,ex.cstr.final,by="bdgt_id",all.x=T)
# merge cps with curve
curve=merge(curve,ex.cps[,c("bdgt_id","cps"),with=F],by="bdgt_id",all.x=T)
# merge curve group with curve
curve=merge(curve,ex.curvegroup[,c("curvegroup_id","flag_curvegroup"),with=F],
            by=c("curvegroup_id"),all.y=T)
# replace missing cps with cpp
curve$cps[is.na(curve$cps)]=curve$cpp[is.na(curve$cps)]
# replace missing values
curve[is.na(curve)]=0

summary.sp=curve[!duplicated(curve[,c("bdgt_id"),with=F]),]
