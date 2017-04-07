# freq cap
print("Note: Freq Capping")
ex.cap=data.table(dbGetQuery(conn,paste("select * from opt_modelinput_freq_cap where client_id=",client_id,sep="")))
curve=curve[,imp:=sp_current/cps]
for (i in 1:nrow(curve)){
  temp=ex.cap[chan3_id==curve$chan3_id[i]&chan2_id==curve$chan2_id[i]&chan1_id==curve$chan1_id[i]]
  temp$imp_cap1=c(0,temp$imp_cap[-length(temp$imp_cap)])
  imp=curve$imp[i]
  index=imp>temp$imp_cap1 & imp<=temp$imp_cap
  if (sum(index)==0) cap=NA else cap=temp$freq[index]
  curve$cap[i]=cap
}