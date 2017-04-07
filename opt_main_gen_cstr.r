#setwd("d:\\Archives\\R Code\\OPTM\\new constrant\\optm fios\\")
print("Note: Generating Constraints")
start.time=Sys.time()

cstr.name=c("sp_min","sp_max","sp_plan")
cutoff.cl=4 # if no. of row is more then this number, then build that number of clusters

# load functions
source(paste(main.path,"opt_modelinput_functions.r",sep=""),local = T)

# Load in data
source(paste(main.path,"opt_modelinput_load.r",sep=""),local = T)

if (nrow(ex.cstr.input)!=0){
  dim.cstr=dbGetQuery(conn,paste("select dim as dim from opt_modules_dim where flag_cstr=1 and client_id=",client_id))$dim
  dim.cstr=paste(dim.cstr,"_id",sep="")
  cstr.check.tb=ex.cstr.input[,c(dim.cstr,"date_start","date_end"),with=F]
  # if (sum(duplicated(cstr.check.tb,by=NULL))!=0){
  #   print("Error: There is dimension duplication in your constraint/plan/event setup. Please check.")
  # }else{
    comma.check=function(x) any(unlist(lapply(1:ncol(cstr.check.tb[x,]),function(x) grepl(",",cstr.check.tb[[x]]))))
    cstr.index=unlist(lapply(1:nrow(cstr.check.tb),comma.check))
    if (!any(cstr.index)) allone.check=T else allone.check=F
    
    if (allone.check & ex.setup$optimization_time==2){
      temp.cstr.output=ex.cstr.input[,c("sp_min","sp_max","sp_plan","opt_id",dim.cstr),with=F]
      for (k in which(sapply(temp.cstr.output,is.character))) set(temp.cstr.output, j=k, value=as.integer(temp.cstr.output[[k]]))
      temp.cstr.output=merge(ex.cstr[,c("client_id","bdgt_id",dim.cstr),with=F],temp.cstr.output,by=dim.cstr,all.y=T)[,!"client_id",with=F]
      ex.cstr=temp.cstr.output
    }else if (allone.check==F & ex.setup$optimization_time==2){
      # all one part
      temp.cstr.output=ex.cstr.input[cstr.index==F,c("sp_min","sp_max","sp_plan","opt_id",dim.cstr),with=F]
      for (k in which(sapply(temp.cstr.output,is.character))) set(temp.cstr.output, j=k, value=as.integer(temp.cstr.output[[k]]))
      temp.cstr.output=merge(ex.cstr[,c("client_id","bdgt_id",dim.cstr),with=F],temp.cstr.output,by=dim.cstr) # merge only take overlap part to filter out missing curve
      # not all one part
      ex.cstr.input=ex.cstr.input[cstr.index]
      source(paste(main.path,"opt_modelinput_gen_cstr.r",sep=""),local=T)
      result=vector("list",3)
      names(result)= c("sp_min","sp_max","sp_plan")
      for (i in c("sp_min","sp_max","sp_plan")){
        temp.output=Reduce(function(...) merge(...,all=TRUE,by="bdgt_id"),Filter(Negate(is.null),
                            list(ex.cstr[,c("bdgt_id",i),with=F],
                                 temp.cstr.output[,c("bdgt_id",i),with=F])))
        if (i=="sp_max"){
          temp.output[is.na(temp.output)]=max.level
          result[[i]]=data.table(bdgt_id=temp.output[[1]],sp_max=do.call(pmin, temp.output[,-1,with=F])) 
        }else if (i=="sp_min"){
          temp.output[is.na(temp.output)]=0
          result[[i]]=data.table(bdgt_id=temp.output[[1]],sp_min=do.call(pmax, temp.output[,-1,with=F])) 
        }else if (i=="sp_plan"){
          temp.output[is.na(temp.output)]=0
          result[[i]]=data.table(bdgt_id=temp.output[[1]],sp_plan=do.call(pmax, temp.output[,-1,with=F]))
        }
      }
      result.all=Reduce(function(...) merge(...,all=TRUE,by="bdgt_id"), result)
      result.all$sp_min[is.na(result.all$sp_min)]=0
      result.all$sp_max[is.na(result.all$sp_max)]=max.level
      result.all$sp_plan[is.na(result.all$sp_plan)]=0
      
      ex.cstr=data.table(dbGetQuery(conn,paste("select * from opt_input_cstr_output where client_id=",client_id,sep="")))
      ex.cstr=merge(ex.cstr[,!names(result.all)[-1],with=F],result.all,by="bdgt_id",all.y=T)
      ex.cstr=data.table(opt_id=rep(opt_id,nrow(ex.cstr)),ex.cstr)
      ex.cstr=ex.cstr[,!c("client_id","id"),with=F]
    }else if (ex.setup$optimization_time==1){
      temp.cstr.output=data.table(bdgt_id=as.character(NA),sp_min=as.numeric(NA))
      source(paste(main.path,"opt_modelinput_gen_cstr.r",sep=""),local=T)
      ex.cstr=ex.cstr[,!c("client_id","id"),with=F]
    }
    # upload to DB
    if (db.usage){
      dbGetQuery(conn,paste("delete from opt_userinput_cstr_output where opt_id=",opt_id,sep=""))
      dbWriteTable(conn,"opt_userinput_cstr_output",ex.cstr,append=T,row.names = F,header=F)
    } else
      write.csv(ex.cstr,"opt_input_cstr_output.csv",row.names=F,na="")
    
  # }#duplication check
}

end=Sys.time()-start.time
print(paste("Note: Run time: ",round(end[[1]],digit=2),attr(end,"units"),sep="")) 