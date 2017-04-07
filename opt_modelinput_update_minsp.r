print("Note: Updating Min Spend for Multi-goal Seek")
# generate min spend for next multi-goal seek
setnames(temp.sp,"sp_current","sp_min")
setnames(ex.cstr,"sp_min","sp_min1")

# merge current spend with curve
ex.cstr=merge(ex.cstr,temp.sp,by="bdgt_id",all.x=T)
ex.cstr$sp_min[is.na(ex.cstr$sp_min)]=0
ex.cstr$sp_min1[is.na(ex.cstr$sp_min1)]=0
ex.cstr=ex.cstr[,sp_min:=pmax(sp_min,sp_min1)]

