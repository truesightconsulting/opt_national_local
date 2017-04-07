# args <- commandArgs(T)
# setwd(args[1])
# # main code path
# main.path=args[2]
# opt_id=as.numeric(args[3])
# client_id=as.numeric(args[4])
# # DB server info
# db.server=args[5]
# db.name=args[6]
# port=as.numeric(args[7])
# username=args[8]
# password=args[9]
# .libPaths(args[10])



setwd("C:\\Users\\yuemeng1\\Desktop\\TOOL\\sonic_test\\")
# True is to staging DB and F is to production DB
is.staging=T
# main code path
main.path="C:\\Users\\yuemeng1\\Desktop\\opt\\"
opt_id=696
client_id=46
# DB server info

db.name="nviz"
port=3306
if (is.staging){
  db.server="127.0.0.1"
  username="root"
  password="bitnami"
}else{
  db.server="bitnami.cluster-chdidqfrg8na.us-east-1.rds.amazonaws.com"
  db.server="127.0.0.1"
  username="Zkdz408R6hll"
  password="XH3RoKdopf12L4BJbqXTtD2yESgwL$fGd(juW)ed"
}


#######################################################################
start_time=Sys.time()
suppressMessages(suppressWarnings(library(gdata)))
suppressMessages(suppressWarnings(library(RMySQL)))
suppressMessages(suppressWarnings(library(bit64)))
suppressMessages(suppressWarnings(library(data.table)))
suppressMessages(suppressWarnings(library(doSNOW)))
suppressMessages(suppressWarnings(library(reshape2)))
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(jsonlite)))
suppressMessages(suppressWarnings(library(rCharts)))
MySQL(max.con=900)
# setup for opt on DB
db.usage=T
error.sim=0
if (db.usage) conn <- dbConnect(MySQL(),user=username, password=password,dbname=db.name, host=db.server)
run.cstr=T
print.msg="cstr"
source(paste(main.path,"opt_main_gen_cstr.r",sep=""),local=T)
if (db.usage) keep(db.usage,conn,opt_id,client_id,main.path,start_time,error.sim,sure = T) else keep(db.usage,main.path,error.sim,sure=T)
run.cstr=F
print.msg=""
source(paste(main.path,"opt_main.r",sep=""),local=T)
if (error.sim!=0) print("Warning: There is curve missing in your constraint or plan, which might lead to unexpected result.")
end_time=Sys.time()-start_time
print(paste("Note: Overall Run time:",round(end_time[[1]],digit=0),attr(end_time,"units"))) 
dbGetQuery(conn,paste("update opt_optimizations set completed=NOW() where id=",opt_id,sep=""))
if (db.usage) dbDisconnect(conn)

