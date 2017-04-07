# multi-goal check
multi.stop=0
if (ex.setup$optimization_type==10) {
  if ((marg[[i]]$Spend_inc/marg[[i]]$Value_inc)>multi.check) {
    multi.stop=1
    break 
  }
} 