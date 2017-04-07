# manipulate optm date
print("Note: Time Window Transformation")
start.optm=as.Date(ex.setup$date_start)
end.optm=as.Date(ex.setup$date_end)
date.temp=optm.date(ex.setup$date_start,ex.setup$date_end)
range.wk.optm=date.temp$range.wk
out.wk.optm=date.temp$out.wk
# check range.wk length; if greater than  
if (range.wk.optm[1]==range.wk.optm[length(range.wk.optm)] & length(range.wk.optm)!=1) {
  range.wk.optm[length(range.wk.optm)]=-1
  fac=ex.season[weeknum==range.wk.optm[1]]
  factor=fac$factor-0.01
  season.add=data.table(weeknum=rep(-1,nrow(ex.sales)),sales_count=ex.sales$sales_count,factor=factor)
  ex.season=rbindlist(list(ex.season,season.add))
  
  hidden.add=ex.cstr.hidden[weeknum==range.wk.optm[1]]
  hidden.add$weeknum=rep(-1,nrow(hidden.add))
  hidden.add$bdgt_id=sub(as.character(range.wk.optm[1]),"-1",hidden.add$bdgt_id,fixed=T)
  ex.cstr.hidden=rbindlist(list(ex.cstr.hidden,hidden.add))
}