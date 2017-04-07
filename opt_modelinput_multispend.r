# put the start spend in for multi goal seek
if (ex.setup$optimization_type==10) curve=merge(curve[,!"sp_min",with=F],start.sp,by="bdgt_id")