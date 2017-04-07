# re-generate min spend for multi-goal seek
print("Note: Generating Min Spend for Next Goal Seek")
if (ex.setup$optimization_type==10) {
  marg.list[[iter]]=marg1
  if (iter==1) {
    temp.sp=summary.sp[,list(sp_current=sum(sp_current)),by="bdgt_id"]
    }else {
    temp.sp.old=temp.sp
    temp.sp=summary.sp[,list(sp_current=sum(sp_current)),by="bdgt_id"]
    temp.append=temp.sp.old[!(bdgt_id %in% temp.sp$bdgt_id)]
    setnames(temp.append,"sp_min","sp_current")
    temp.sp=data.table(rbind(temp.sp,temp.append))
  }
  sp.multi[[iter]]=temp.sp
}