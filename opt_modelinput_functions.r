
# date function
optm.date=function(start,end){
  # start and end are the date from input files (raw format); output range.wk is weeknum vector and out.wk is week vector
#   start=as.Date(ex.setup$date_start,"%m/%d/%Y")
#   end=as.Date(ex.setup$date_end,"%m/%d/%Y")
#   start=as.Date(start,"%m/%d/%Y")
#   end=as.Date(end,"%m/%d/%Y")
  start=as.Date(start)
  end=as.Date(end)
  year.start=as.numeric(strftime(start,format="%Y"))
  year.end=as.numeric(strftime(end,format="%Y"))
  if(year.end-year.start==0){
    start.range=as.numeric(strftime(start,format="%U"))
    end.range=as.numeric(strftime(end,format="%U"))
    start.wkd=as.numeric(strftime(start,format="%w"))
    end.wkd=as.numeric(strftime(end,format="%w"))
    if (start.wkd>3) start.range=start.range+1
    if (end.wkd<3) end.range=end.range-1
    range=end.range-start.range+1
  }else{
    year.range=seq(year.start,year.end-1,by=1)
    week.add=as.numeric(strftime(strptime(paste(year.range,"1231"),format="%Y%m%d"),format="%U"))
    start.range=as.numeric(strftime(start,format="%U"))
    end.range=as.numeric(strftime(end,format="%U"))
    start.wkd=as.numeric(strftime(start,format="%w"))
    end.wkd=as.numeric(strftime(end,format="%w"))
    if (start.wkd>3) start.range=start.range+1
    if (end.wkd<3) end.range=end.range-1
    end.range=end.range+week.add
    range=end.range-start.range+1
  }
  if(year.end-year.start==0){
    range.wk=seq(start.range,end.range,by=1)
    range.index=rep(year.end,length(range.wk))
  } else{
    start.range=as.numeric(strftime(start,format="%U"))
    end.range=as.numeric(strftime(end,format="%U"))
    start.wkd=as.numeric(strftime(start,format="%w"))
    end.wkd=as.numeric(strftime(end,format="%w"))
    if (start.wkd>3) start.range=start.range+1
    if (end.wkd<3) end.range=end.range-1
    year.range=seq(year.start,year.end,by=1)
    weeks.year=as.numeric(strftime(strptime(paste(year.range,"1231"),format="%Y%m%d"),format="%U"))
    range.wk.list=vector("list",length(year.start:year.end))
    range.wk.index=vector("list",length(year.start:year.end))
    for (i in 1:length(year.start:year.end)){
      #i=1
      if (i==1) {
        range.wk.list[[i]]=seq(start.range,weeks.year[i],by=1) 
        range.wk.index[[i]]=rep((year.start:year.end)[i],length(range.wk.list[[i]]))
      }else if (i==length(year.start:year.end)){
        if (end.range==0){
          break
        }
        range.wk.list[[i]]=seq(1,end.range,by=1)
        range.wk.index[[i]]=rep((year.start:year.end)[i],length(range.wk.list[[i]]))
      }else
        range.wk.list[[i]]=seq(1,weeks.year[i],by=1)
      range.wk.index[[i]]=rep((year.start:year.end)[i],length(range.wk.list[[i]]))
    }
    range.wk=unlist(range.wk.list)
    range.index=unlist(range.wk.index)
  }
  out.wk=rep("",length(range.wk))
  for (i in 1:length(range.wk)){
    out.wk[i]=as.character(strptime(paste(range.index[i],"0",range.wk[i],sep=""),format="%Y%w%U"))
    if (is.na(out.wk[i])){
      year=range.index[i]-1
      temp=strptime(paste(year,"1231"),format="%Y%m%d")
      temp.wknum=as.numeric(strftime(temp,format="%U"))
      out.wk[i]=as.character(strptime(paste(year,"0",temp.wknum,sep=""),format="%Y%w%U"))
    }
  }
  out.wk=as.Date(out.wk)
  return(list(range.wk=range.wk,out.wk=out.wk))
}

# duplicate table by time window
optm.dupe.table=function(table,time.range){
  # table is the table to be dupelicated; time.range is the time window range from optm.date function; output is the dupe table
#   table=ex.cstr
#   time.range=range.wk.optm
  dupe.table=table[rep(seq_len(nrow(table)), each=length(time.range)),]
  dupe.table=data.table(dupe.table,week_id=rep(time.range,nrow(table)))
  dupe.table$bdgt_id=paste(dupe.table$bdgt_id,dupe.table$week_id,sep="_")
  dupe.table
}


# extract bdgt dim
get_bdgt_dim=function(){
  adm.setup=fread(paste(getwd(),"/admin/opt/admin/adm_setup.csv",sep=""))
  dim=adm.setup$value[adm.setup$attribute=="bdgt_dim"]
  dim=strsplit(dim,",")[[1]]
  dim=dim[!dim %in% c("week_id","month_id")]
  return(unique(substr(dim,1,nchar(dim)-4)))
}

# extract dim number
get_dim_n=function(x){
  #x="chan"
  adm.setup=fread(paste(getwd(),"/admin/opt/admin/adm_setup.csv",sep=""))
  dim=adm.setup$value[adm.setup$attribute==paste("dim_",x,sep="")]
  dim=strsplit(dim,",")[[1]]
  return(dim)
}

# extract dim
get_dim=function(){
  adm.setup=fread(paste(getwd(),"/admin/opt/admin/adm_setup.csv",sep=""))
  dim=grep("dim_",adm.setup$attribute,value=T)
  dim=substr(dim,5,nchar(dim))
  return(dim)
}

# extract formula
get_curve_f=function(){
  adm.setup=fread(paste(getwd(),"/admin/opt/admin/adm_setup.csv",sep=""))
  return(adm.setup$value[adm.setup$attribute=="formula"])
}

# extract beta
get_beta=function(){
  adm.setup=fread(paste(getwd(),"/admin/opt/admin/adm_setup.csv",sep=""))
  return(adm.setup$value[adm.setup$attribute=="beta"])
}

# extract is_mta
is_mta=function(){
  adm.setup=fread(paste(getwd(),"/admin/opt/admin/adm_setup.csv",sep=""))
  return(adm.setup$value[adm.setup$attribute=="is_mta"])
}


# create agg expr
expr_agg=function(input,output){
  expr=paste(paste(output,"=sum(",input,")",sep=""),collapse = ",")
  return(parse(text=paste("list(",expr,")",sep="")))
}

# create eff expr
expr_eff=function(metric_eff,f_eff){
  expr=paste(paste(metric_eff,"=",f_eff,sep=""),collapse = ",")
  return(parse(text=paste("':='(",expr,")",sep="")))
}

# drop output column
drop_col=function(input){
  if (length(input)!=0) return(temp[,!input,with=F]) else return(temp)
}

# negative exponential curve function
neg_exp=function(a,b){
  function(x) a*(1-exp(-b*x))
}