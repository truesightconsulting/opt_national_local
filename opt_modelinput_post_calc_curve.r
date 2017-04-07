# generate curves
data=copy(curve)
x.factor=1.3
y.factor=1.1
x.label="Spend"
data$dim=paste(data$chan2_name,data$chan3_name,sep = " - ")
temp.dim=data[,c("dim","sales1_name"),with=F]
x.spend=seq(0,x.factor*max(data$sp_current),length.out=500)
setnames(data,c("sp_current","dim","value_decomp"),c("spend","variable","value"))
temp.json=vector("list",length(unique(temp.dim$sales1_name)))
for (j in 1:length(unique(temp.dim$sales1_name))){
  group.name=temp.dim$sales1_name[j]
  temp=matrix(0,nc=nrow(temp.dim[sales1_name==group.name]),nr=length(x.spend))
  colnames(temp)=temp.dim$dim[temp.dim$sales1_name==group.name]
  for (i in 1:ncol(temp)){
    para=data[variable==colnames(temp)[i] & sales1_name==group.name]
    temp[,i]=para$a[1]*(1-exp(-para$b[1]*x.spend/(1000*para$cps[1])))
  }
  temp<-data.table(spend=x.spend,temp)
  data1=melt(temp,id.vars = "spend")
  #setnames(data,c("sp_current","dim","value_decomp"),c("spend","variable","value"))
  data2=data[sales1_name==group.name,c("spend","variable","value"),with=F]
  data2=data2[,variable:="Current Allocation"]
  data3=rbind(data1,data2)
  data3=data3[,type:="line"]
  data3=data3[variable=="Current Allocation",type:="scatter"]
  index=data3[!duplicated(variable),c("variable","type"),with=F]
  series=foreach (i=1:nrow(index),.multicombine = T,.combine = "c") %do%{
    name1=index$variable[i]
    type1=index$type[i]
    x=data3[variable==name1,!c("variable","type"),with=F]
    if (name1=="Current Allocation"){
      toJSON(list(data = as.matrix(x), name = name1, type = type1,
                  marker=list(symbol='circle',radius=5)))
    }else {
      toJSON(list(data = as.matrix(x), 
                  name = name1, type = type1,lineWidth=3,marker=list(enable=F)))
    }
  }
  temp.json[[j]]=paste("\"",group.name,"\":{\"data\":[",paste(series,collapse = ","),"],\"label\":{\"x\":\"",x.label,"\",\"y\":\"",group.name,"\"}}",sep="")
}
json=paste("{",paste(unlist(temp.json),collapse = ","),"}",sep="")
temp=data.table(id=NA,label="linechart",group=ex.output$group[ex.output$type=="chan"][1],type=NA,tab=1,is_chart=1,is_table=0,chart="line",
                drilldown=0,dim=NA,filter=NA,json=json)
ex.output=rbind(ex.output,temp)