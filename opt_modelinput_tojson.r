# convert to json and upload to DB
print("Note: Converting result to JSON.")
if (db.usage) {
  ex.output$opt_id=rep(opt_id,nrow(ex.output))
  setnames(ex.output,"id","output_id")
  group=unique(ex.output[,c("group","chart"),with=F])
  temp=foreach (i=1:nrow(group),.combine="rbind",.multicombine=T) %do%{
    index=ex.output$group==group$group[i] & ex.output$chart==group$chart[i]
    temp=ex.output[index]
    if (temp$chart[1] %in% c("bar","table")){
      temp.json=foreach(j=1:nrow(temp),.combine="c",.multicombine=T) %do%  {
        tag=str_split(temp$label,"_")[[j]][2]
        paste("\"",tag,"\"",":",temp$json[j],sep="")
      }
      temp$json[1]=paste("{",paste(temp.json,collapse = ","),"}",sep="")
      temp=temp[1,!"label",with=F]
    }else{
      temp=temp[,!"label",with=F]
    }
  }
  setnames(temp,"group","label")
  temp$json[temp$label=="overall"]=gsub("\\[|\\]","",temp$json[temp$label=="overall"])
  
  # marg1 output in excel
  if ((!is.null(marg[[1]]))){
    temp=rbindlist(list(temp,data.table(output_id=as.integer(1),label="Marginal",type="excel",tab=as.integer(0),is_chart=as.integer(0),is_table=as.integer(0),
                                        chart="excel",drilldown=0,
                                        dim=NA,filter=NA,json=as.character(toJSON(marg1)),opt_id=opt_id)))
  }

  dbGetQuery(conn,paste("delete from opt_output where opt_id=",opt_id,sep=""))
  dbWriteTable(conn,"opt_output",temp[,!c("output_id","dim" ),with=F],append=T,row.names = F,header=F)
  # upload ex.output to db for scenario comparasion
  dbGetQuery(conn,paste("delete from opt_output_drilldown where opt_id=",opt_id,sep=""))
  dbWriteTable(conn,"opt_output_drilldown",ex.output[,!c("output_id"),with=F],append=T,row.names = F,header=F)
}
# output marginal
if (!db.usage){
  if (!(loop==0|ex.setup$optimization_type==4)) {
    if ((!is.null(marg[[1]]))) write.csv(marg1,"opt_output_marginal.csv",row.names=F)
  } 
}
