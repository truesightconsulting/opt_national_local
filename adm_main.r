#setwd("d:\\Archives\\Git\\opt-fios\\admin\\")
suppressMessages(suppressWarnings(library(bit64)))
suppressMessages(suppressWarnings(library(data.table)))
suppressMessages(suppressWarnings(library(ggplot2)))

comma.check=F
ex.curve=fread("adm_curve.csv")
for (i in names(ex.curve)){
  if(any(grepl(",",ex.curve[[i]]))) {
    comma.check=T
    stop("Error: There is comma in the file.")
  }
}

if (!comma.check){
  # setup file
  adm.setup=fread("adm_setup.csv",na.strings = "NULL")
  # some lists for input files for loop later
  input.list=c("event_level","optimization_targets","optimization_types","optimization_wins")
  id.list=c("event_level_id","optimization_target_id","optimization_type_id","optimization_win_id")
  
  # get table list
  if (db.usage) tb.list=as.vector(as.matrix(dbGetQuery(conn,"show tables from nviz")))
  
  # create client id
  print("Note: Creating Client ID")
  if (db.usage) {
    if (adm.setup[attribute=="update"]$value=="y"){
      client_id=dbGetQuery(conn,paste("select id from clients where name='",adm.setup[attribute=="client_name"]$value,"'",sep=""))$id
      dbGetQuery(conn,paste("update clients set r_script_path='",adm.setup[attribute=="r_path"]$value,"' where id=",client_id,sep=""))
      if (is.na(adm.setup[attribute=="general_message"]$value)){
        dbGetQuery(conn,paste("update clients set general_message=null where id=",client_id,sep=""))
      }else dbGetQuery(conn,paste("update clients set general_message='",adm.setup[attribute=="general_message"]$value,"' where id=",client_id,sep=""))
    }else if (adm.setup[attribute=="update"]$value=="n"){
      temp=data.table(name=adm.setup[attribute=="client_name"]$value,
                      r_script_path=adm.setup[attribute=="r_path"]$value,
                      general_message=adm.setup[attribute=="general_message"]$value)
      if (temp$name %in% dbGetQuery(conn,"select name from clients")$name) {
        stop("Client name already exists. Please change it.")
      }else {
        dbWriteTable(conn,"clients",temp,append=T,row.names = F,header=F)
        client_id=dbGetQuery(conn,paste("select id from clients where name='",temp$name,"'",sep=""))$id
      }
    }
  }else client_id=1023 
  
  
  
  # update label tables
  print("Note: Updating Label Tables")
  if (db.usage){
    data=fread("adm_curve.csv")
    dim=names(data)
    index=grepl("_name",dim)
    dim=dim[index]
    for (i in 1:length(dim)){
      temp=data.table(label=unique(data[[dim[i]]]))
      name=strsplit(dim[i],"_")[[1]][1]
      # check talbe existing or not first, then insert into it
      tb.name=paste("opt_label_",name,sep="")
      if (tb.name %in% tb.list){
        value=dbGetQuery(conn,paste("select label from ",tb.name,sep=""))[,1]
        index=!(temp$label %in% value)
        temp=temp[index]
        if (nrow(temp)!=0) {
          print(paste("Note: New labels Inserted in ",tb.name,sep=""))
          dbWriteTable(conn,tb.name,temp,append=T,row.names = F,header=F)
        }
      }else {
        print(paste("Note: New Table ",tb.name," Created",sep=""))
        dbGetQuery(conn,paste("CREATE TABLE ",tb.name," (`id` INT NOT NULL  AUTO_INCREMENT,`label` VARCHAR(255) NOT NULL,primary key (id),UNIQUE INDEX `label` (`label`))"))
        dbWriteTable(conn,tb.name,temp,append=T,row.names = F,header=F)
      }
    }
  }
  
  
  # create modelinput curve
  print("Note: Creating Curve Table")
  if (db.usage){
    # merge id to curve data
    ex.curve=fread("adm_curve.csv")
    dim=names(ex.curve)
    index=grepl("_name",dim)
    dim=dim[index]
    for (i in 1:length(dim)){
      # convert field type all to character
      if (!is.character(ex.curve[[dim[i]]])) ex.curve[[dim[i]]]=as.character(ex.curve[[dim[i]]])
      # fetch match table
      temp=data.table(dbGetQuery(conn,paste("select * from opt_label_",strsplit(dim[i],"_")[[1]][1],sep="")))
      # merge
      setnames(temp,names(temp),paste(strsplit(dim[i],"_")[[1]][1],c("id","name"),sep="_"))
      ex.curve=merge(ex.curve,temp,by=dim[i],all.x=T)
    }
    # add in client id
    ex.curve=data.table(client_id=rep(client_id,nrow(ex.curve)),ex.curve)
    # create bdgt_id
    bdgt_dim=strsplit(adm.setup[attribute=="bdgt_dim"]$value,",")[[1]]
    bdgt_dim=bdgt_dim[!bdgt_dim %in% c("week_id","month_id")]
    ex.curve$bdgt_id=do.call(paste, c(ex.curve[,bdgt_dim,with=F], sep = "_"))
    # delete name column
    index=!grepl("name",names(ex.curve))
    temp=ex.curve[,names(ex.curve)[index],with=F]
    # check new col which dosen't exit in db
    col.exist=dbGetQuery(conn,"show columns from opt_modelinput_curve")[,1]
    index=!(names(temp) %in% col.exist)
    if (sum(index)!=0){
      col.new=names(temp)[index]
      for (j in 1:length(col.new)){
        print(paste("Note: New Column ",col.new[j]," Inserted",sep=""))
        query=paste(paste(col.new[j]," int null default null",sep = ""))
        dbGetQuery(conn,paste("alter table opt_modelinput_curve add column ",query,sep=""))
      }
    }
    # create a upload template for cps and a lookup table for other upload templates
    upload.cps=ex.curve[,c(bdgt_dim,paste(unlist(strsplit(bdgt_dim,"_id")),"_name",sep = ""),"bdgt_id"),with=F]
    upload.cps$cps=rep("",nrow(upload.cps))
    upload.cps=unique(upload.cps,by=NULL)
    write.csv(upload.cps,"upload_cps.csv",row.names = F)
    upload.lookup=ex.curve[,names(ex.curve)[grepl("_name",names(ex.curve))],with=F]
    upload.lookup=upload.lookup[,!c("curvegroup_name","all_name"),with=F]
    upload.lookup=unique(upload.lookup,by=NULL)
    setnames(upload.lookup,names(upload.lookup),gsub(pattern = "_name",replacement = "_id",x = names(upload.lookup)))
    write.csv(upload.lookup,"upload_lookup.csv",row.names = F)
    
    # upload curve to db
    dbGetQuery(conn,paste("delete from opt_modelinput_curve where client_id=",client_id,sep=""))
    dbWriteTable(conn,"opt_modelinput_curve",temp,append=T,row.names = F,header=F)
  }else{
    ex.curve=fread("opt_modelinput_curve.csv")
  }
  
  
  # setup file
  print("Note: Creating Setup Table")
  temp=data.table(
    client_id=client_id,
    optimization_target=NA,
    optimization_type=NA,
    optimization_type_value=NA,
    optimization_time=as.numeric(adm.setup[attribute=="calendar"]$value),
    date_start=NA,
    date_end=NA,
    input_increment=as.numeric(adm.setup[attribute=="pace"]$value),
    input_goal_check=as.numeric(adm.setup[attribute=="goal_check"]$value),
    optimization_win=NA
  )
  # extra.col=adm.setup[attribute=="extra_input"]$value
  # if (extra.col!=""){
  #   extra=matrix(NA,nr=1,nc=length(extra.col))
  #   extra=data.table(extra)
  #   setnames(extra,names(extra),extra.col)
  #   temp=cbind(ex.setup,extra)
  # }else temp=ex.setup
  if (db.usage){
    #   # check new col which dosen't exit in db
    #   col.exist=dbGetQuery(conn,"show columns from opt_input_setup")[,1]
    #   index=!(names(temp) %in% col.exist)
    #   if (sum(index)!=0){
    #     col.new=names(temp)[index]
    #     for (i in 1:length(col.new)){
    #       query=paste(paste(col.new[i]," int null default null",sep = ""))
    #       dbGetQuery(conn,paste("alter table opt_input_setup add column ",query,sep=""))
    #     }
    #   }
    # upload curve to db
    dbGetQuery(conn,paste("delete from opt_input_setup where client_id=",client_id,sep=""))
    dbWriteTable(conn,"opt_input_setup",temp,append=T,row.names = F,header=F)
  }else write.csv(temp,"opt_input_setup.csv",row.names = F,na="NULL")
  
  # curve group
  print("Note: Creating Curve Group Table")
  group=unique(ex.curve$curvegroup_id)
  temp=data.table(client_id=rep(client_id,length(group)),curvegroup_id=group,flag_curvegroup=c(1,rep(0,length(group)-1)))
  if (db.usage){
    # upload curve to db
    dbGetQuery(conn,paste("delete from opt_input_curvegroup where client_id=",client_id,sep=""))
    dbWriteTable(conn,"opt_input_curvegroup",temp,append=T,row.names = F,header=F)
  }else write.csv(temp,"opt_input_curvegroup.csv",row.names = F,na="NULL")
  
  # dim selection file
  print("Note: Creating Dimension Tables")
  dim=adm.setup$attribute[grepl("dim_",adm.setup$attribute)]
  for (i in 1:length(dim)){
    file.name=strsplit(dim[i],"_")[[1]][2]
    print(paste("Note: Processing ",file.name,sep=""))
    temp.dim=strsplit(adm.setup[attribute==dim[i]]$value,",")[[1]]
    temp.dim=unlist(strsplit(temp.dim,"_id"))
    if (db.usage==F) temp.dim=c(paste(temp.dim,"_id",sep=""),paste(temp.dim,"_name",sep="")) else
      temp.dim=paste(temp.dim,"_id",sep="") 
    temp.table=ex.curve[,temp.dim,with=F]
    temp.table=temp.table[!duplicated(temp.table)]
    temp.table=data.table(client_id=rep(client_id,nrow(temp.table)),temp.table,flag=rep(1,nrow(temp.table)))
    setnames(temp.table,"flag",paste("flag_",file.name,sep=""))
    temp=temp.table[eval(parse(text=paste("order(",paste(temp.dim,collapse = ","),")",sep="")))]
    if (db.usage){
      # check talbe existing or not first, then insert into it
      tb.name=paste("opt_input_dim_",file.name,sep="")
      tb.name1=paste("opt_userinput_dim_",file.name,sep="")
      if (tb.name %in% tb.list){
        # check new col which dosen't exit in db
        col.exist=dbGetQuery(conn,paste("show columns from ",tb.name,sep=""))[,1]
        index=!(names(temp) %in% col.exist)
        if (sum(index)!=0){
          col.new=names(temp)[index]
          for (j in 1:length(col.new)){
            print(paste("Note: New Column ",col.new[j]," Inserted",sep=""))
            query=paste(paste(col.new[j]," int null default null",sep = ""))
            dbGetQuery(conn,paste("alter table ",tb.name," add column ",query,sep=""))
            # for userinput table
            dbGetQuery(conn,paste("alter table ",tb.name1," add column ",query,sep=""))
          }
        }
        # upload curve to db
        dbGetQuery(conn,paste("delete from ",tb.name," where client_id=",client_id,sep=""))
        dbWriteTable(conn,tb.name,temp,append=T,row.names = F,header=F)
      }else{
        # create table and upload to db
        print(paste("Note: New Table ",tb.name," Created",sep=""))
        query=vector("list",ncol(temp))
        for (j in 1:ncol(temp)){
          query[[j]]=paste(paste("`",names(temp)[j],"`"," int null default null",sep = ""))
        }
        dbGetQuery(conn,paste("CREATE TABLE ",tb.name," (`id` INT NOT NULL  AUTO_INCREMENT,",paste(query,collapse = ","),",PRIMARY KEY (`id`))"))
        dbWriteTable(conn,tb.name,temp,append=T,row.names = F,header=F)
        # create userinput table
        setnames(temp,"client_id","opt_id")
        query=vector("list",ncol(temp))
        for (j in 1:ncol(temp)){
          query[[j]]=paste(paste("`",names(temp)[j],"`"," int null default null",sep = ""))
        }
        dbGetQuery(conn,paste("CREATE TABLE ",tb.name1," (`id` INT NOT NULL  AUTO_INCREMENT,",paste(query,collapse = ","),",PRIMARY KEY (`id`))"))
      }
    }else write.csv(temp,paste("opt_input_dim_",file.name,".csv",sep=""),row.names = F,na="NULL")
  }
  
  # modules and modules dim table
  print("Note: Creating Module Relalted Tables")
  if (db.usage){
    # modules
    adm.modules=fread("adm_modules.csv")
    temp=adm.modules[,"label",with=F]
    # check talbe existing or not first, then insert into it
    tb.name="opt_label_modules"
    value=dbGetQuery(conn,paste("select label from ",tb.name,sep=""))[,1]
    index=!(temp$label %in% value)
    temp=temp[index]
    if (nrow(temp)!=0) {
      print(paste("Note: New labels Inserted in ",tb.name,sep=""))
      dbWriteTable(conn,tb.name,temp,append=T,row.names = F,header=F)
    }
    # fetch match table
    temp=data.table(dbGetQuery(conn,paste("select * from ",tb.name,sep="")))
    temp=merge(adm.modules,temp,by="label",all.x=T)
    temp$client_id=rep(client_id,nrow(temp))
    temp=temp[,!"label",with=F]
    setnames(temp,"id","opt_label_module_id")
    dbGetQuery(conn,paste("delete from opt_modules where client_id=",client_id,sep=""))
    dbWriteTable(conn,"opt_modules",temp,append=T,row.names = F,header=F)
    
    # modules dim
    adm.modules_dim=fread("adm_modules_dim.csv")
    adm.modules_dim$dim=unlist(strsplit(adm.modules_dim$dim,"_id"))
    temp=adm.modules_dim[,"label",with=F]
    # check talbe existing or not first, then insert into it
    tb.name="opt_label_modules_dim"
    value=dbGetQuery(conn,paste("select label from ",tb.name,sep=""))[,1]
    index=!(temp$label %in% value)
    temp=temp[index]
    if (nrow(temp)!=0) {
      print(paste("Note: New labels Inserted in ",tb.name,sep=""))
      dbWriteTable(conn,tb.name,temp,append=T,row.names = F,header=F)
    }
    # fetch match table
    temp=data.table(dbGetQuery(conn,paste("select * from ",tb.name,sep="")))
    temp=merge(adm.modules_dim,temp,by="label",all.x=T)
    temp$client_id=rep(client_id,nrow(temp))
    temp=temp[,!"label",with=F]
    setnames(temp,"id","opt_label_modules_dim_id")
    dbGetQuery(conn,paste("delete from opt_modules_dim where client_id=",client_id,sep=""))
    dbWriteTable(conn,"opt_modules_dim",temp,append=T,row.names = F,header=F)
  }
  
  # user input cstr
  print("Note: Creating Constraint Input Table")
  temp.dim=strsplit(adm.setup[attribute=="bdgt_dim"]$value,",")[[1]]
  temp.dim=temp.dim[!temp.dim %in% c("week_id","month_id")]
  temp.dim=c(c("sp_min","sp_max","sp_plan","date_start","date_end"),temp.dim) 
  temp.table=data.table(matrix(ncol=length(temp.dim)))
  setnames(temp.table,names(temp.table),temp.dim)
  temp=data.table(client_id=client_id,temp.table)
  if (db.usage){
    write.csv(temp[-1,!c("sp_plan","client_id"),with=F],"upload_cstr.csv",row.names = F,na="NULL")
    write.csv(temp[-1,!c("sp_min","sp_max","client_id"),with=F],"upload_plan.csv",row.names = F,na="NULL")
    col.exist=dbGetQuery(conn,paste("show columns from ","opt_userinput_cstr",sep=""))[,1]
    index=!(names(temp[,!"client_id",with=F]) %in% col.exist)
    if (sum(index)!=0){
      col.new=names(temp)[index]
      for (j in 1:length(col.new)){
        print(paste("Note: New Column ",col.new[j]," Inserted",sep=""))
        query=paste(paste(col.new[j]," int null default null",sep = ""))
        dbGetQuery(conn,paste("alter table ","opt_userinput_cstr"," add column ",query,sep=""))
        # for cstr save table
        dbGetQuery(conn,paste("alter table ","opt_cstr_save"," add column ",query,sep=""))
        dbGetQuery(conn,paste("alter table ","opt_plan_save"," add column ",query,sep=""))
      }
    }
  }else write.csv(temp[-1],"opt_input_cstr.csv",row.names = F,na="NULL")
  
  # hidden and modelinput cstr
  print("Note: Creating Hidden Constraint Table")
  temp.dim=strsplit(adm.setup[attribute=="bdgt_dim"]$value,",")[[1]]
  temp.dim=temp.dim[!temp.dim %in% c("week_id","month_id")]
  temp.dim=unlist(strsplit(temp.dim,"_id"))
  if (db.usage==F) temp.dim=c(paste(temp.dim,"_id",sep=""),paste(temp.dim,"_name",sep="")) else
    temp.dim=paste(temp.dim,"_id",sep="") 
  temp.table=ex.curve[!duplicated(bdgt_id)][,c(temp.dim,"bdgt_id"),with=F]
  ex.cstr=temp.table
  if(as.numeric(adm.setup[attribute=="calendar"]$value)==1){
    data1=ex.cstr[rep(1:nrow(ex.cstr),54)]
    percent_max=rep(NA,nrow(data1))
    sp_max_h=rep(NA,nrow(data1))
    sp_min_h=sp_max_h
    week_id=rep(0:53,each=nrow(ex.cstr))
    ex.cstr.hidden=data.table(data1,week_id,percent_max,sp_max_h,sp_min_h)
    ex.cstr.hidden$bdgt_id=paste(data1$bdgt_id,week_id,sep="_")
    ex.cstr.hidden=data.table(client_id=rep(client_id,nrow(ex.cstr.hidden)),ex.cstr.hidden)
    temp=ex.cstr.hidden
    if (db.usage){
      # check new col which dosen't exit in db
      col.exist=dbGetQuery(conn,"show columns from opt_modelinput_hidden_cstr")[,1]
      index=!(names(temp) %in% col.exist)
      if (sum(index)!=0){
        col.new=names(temp)[index]
        for (j in 1:length(col.new)){
          print(paste("Note: New Column ",col.new[j]," Inserted",sep=""))
          query=paste(paste(col.new[j]," int null default null",sep = ""))
          dbGetQuery(conn,paste("alter table opt_modelinput_hidden_cstr add column ",query,sep=""))
        }
      }
      # upload curve to db
      dbGetQuery(conn,paste("delete from opt_modelinput_hidden_cstr where client_id=",client_id,sep=""))
      dbWriteTable(conn,"opt_modelinput_hidden_cstr",temp,append=T,row.names = F,header=F)
      # hidden cstr template
      temp.dim1=dbGetQuery(conn,paste("select dim from opt_modules_dim where flag_cstr=1 and client_id=",client_id))$dim
      temp.dim1=paste(temp.dim1,"_name",sep="")
      temp=merge(ex.curve[!duplicated(bdgt_id)][,c(temp.dim1,"bdgt_id"),with=F],temp,by="bdgt_id",all.y=T)
      write.csv(temp,"opt_modelinput_hidden_cstr.csv",row.names = F,na="NULL")
    }else write.csv(temp,"opt_modelinput_hidden_cstr.csv",row.names = F,na="NULL")
    # cstr output table
    temp=copy(ex.cstr.hidden)
    setnames(temp,c("percent_max","sp_max_h","sp_min_h"),c("sp_min","sp_max","sp_plan"))
    if (db.usage){
      # check new col which dosen't exit in db
      col.exist=dbGetQuery(conn,"show columns from opt_input_cstr_output")[,1]
      index=!(names(temp) %in% col.exist)
      if (sum(index)!=0){
        col.new=names(temp)[index]
        for (j in 1:length(col.new)){
          print(paste("Note: New Column ",col.new[j]," Inserted",sep=""))
          query=paste(paste(col.new[j]," int null default null",sep = ""))
          dbGetQuery(conn,paste("alter table opt_input_cstr_output add column ",query,sep=""))
          # for userinput table
          dbGetQuery(conn,paste("alter table opt_userinput_cstr_output add column ",query,sep=""))
        }
      }
      # upload to db
      dbGetQuery(conn,paste("delete from opt_input_cstr_output where client_id=",client_id,sep=""))
      dbWriteTable(conn,"opt_input_cstr_output",temp,append=T,row.names = F,header=F)
    }else write.csv(temp,"opt_input_cstr_output.csv",row.names = F,na="NULL")
  }else{
    temp.na=rep(NA,nrow(ex.cstr))
    ex.cstr.hidden=data.table(ex.cstr,sp_min_h=temp.na,percent_max=temp.na,sp_max_h=temp.na)
    ex.cstr.hidden=data.table(client_id=rep(client_id,nrow(ex.cstr.hidden)),ex.cstr.hidden)
    temp=ex.cstr.hidden
    if (db.usage){
      # check new col which dosen't exit in db
      col.exist=dbGetQuery(conn,"show columns from opt_modelinput_hidden_cstr")[,1]
      index=!(names(temp) %in% col.exist)
      if (sum(index)!=0){
        col.new=names(temp)[index]
        for (j in 1:length(col.new)){
          print(paste("Note: New Column ",col.new[j]," Inserted",sep=""))
          query=paste(paste(col.new[j]," int null default null",sep = ""))
          dbGetQuery(conn,paste("alter table opt_modelinput_hidden_cstr add column ",query,sep=""))
        }
      }
      # upload curve to db
      dbGetQuery(conn,paste("delete from opt_modelinput_hidden_cstr where client_id=",client_id,sep=""))
      dbWriteTable(conn,"opt_modelinput_hidden_cstr",temp,append=T,row.names = F,header=F)
      # hidden cstr template
      temp.dim1=dbGetQuery(conn,paste("select dim from opt_modules_dim where flag_cstr=1 and client_id=",client_id))$dim
      temp.dim1=paste(temp.dim1,"_name",sep="")
      temp=merge(ex.curve[!duplicated(bdgt_id)][,c(temp.dim1,"bdgt_id"),with=F],temp,by="bdgt_id",all.y=T)
      write.csv(temp,"opt_modelinput_hidden_cstr.csv",row.names = F,na="NULL")
    }else write.csv(temp,"opt_modelinput_hidden_cstr.csv",row.names = F,na="NULL")
    # cstr output table
    temp=copy(ex.cstr.hidden)
    setnames(temp,c("percent_max","sp_max_h","sp_min_h"),c("sp_min","sp_max","sp_plan"))
    if (db.usage){
      # check new col which dosen't exit in db
      col.exist=dbGetQuery(conn,"show columns from opt_input_cstr_output")[,1]
      index=!(names(temp) %in% col.exist)
      if (sum(index)!=0){
        col.new=names(temp)[index]
        for (j in 1:length(col.new)){
          print(paste("Note: New Column ",col.new[j]," Inserted",sep=""))
          query=paste(paste(col.new[j]," int null default null",sep = ""))
          dbGetQuery(conn,paste("alter table opt_input_cstr_output add column ",query,sep=""))
          # for userinput table
          dbGetQuery(conn,paste("alter table opt_userinput_cstr_output add column ",query,sep=""))
        }
      }
      # upload curve to db
      dbGetQuery(conn,paste("delete from opt_input_cstr_output where client_id=",client_id,sep=""))
      dbWriteTable(conn,"opt_input_cstr_output",temp,append=T,row.names = F,header=F)
    }else write.csv(temp,"opt_input_cstr_output.csv",row.names = F,na="NULL")
  }
  
  
  # event
  print("Note: Creating Event Table")
  temp.dim=strsplit(adm.setup[attribute=="event_dim"]$value,",")[[1]]
  temp.dim=unlist(strsplit(temp.dim,"_id"))
  if (db.usage==F) temp.dim=c(paste(temp.dim,"_id",sep=""),paste(temp.dim,"_name",sep="")) else
    temp.dim=paste(temp.dim,"_id",sep="") 
  temp.table=data.table(matrix(ncol=length(temp.dim)+4))
  setnames(temp.table,names(temp.table),c("event_name","date_start","date_end","level",temp.dim))
  temp=data.table(client_id=client_id,temp.table)
  if (db.usage){
    write.csv(temp[-1,!"client_id",with=F],"upload_event.csv",row.names = F,na="NULL")
    # check new col which dosen't exit in db
    col.exist=dbGetQuery(conn,"show columns from opt_userinput_event")[,1]
    index=!(names(temp[,!"client_id",with=F]) %in% col.exist)
    if (sum(index)!=0){
      col.new=names(temp[,!"client_id",with=F])[index]
      for (j in 1:length(col.new)){
        print(paste("Note: New Column ",col.new[j]," Inserted",sep=""))
        query=paste(paste(col.new[j]," int null default null",sep = ""))
        dbGetQuery(conn,paste("alter table opt_userinput_event add column ",query,sep=""))
        # for event save table
        dbGetQuery(conn,paste("alter table ","opt_event_save"," add column ",query,sep=""))
      }
    }
  }else write.csv(temp[-1],"opt_input_event.csv",row.names = F,na="NULL")
  
  
  # season factor
  print("Note: Creating Season Table")
  if (as.numeric(adm.setup[attribute=="calendar"]$value)==1){
    seed=1023
    data=fread("adm_season.csv")
    index=grepl("_name",names(data))
    name.list=names(data)[index]
    
    data=merge(data,unique(ex.curve[,name.list,with=F]),by=name.list,all=T)
    if (sum(is.na(data$factor)!=0)) stop("Error: Seasonal data is not complete")
    
    for (i in 1:length(name.list)){
      temp.name=name.list[i]
      temp.name1=c(gsub("_name","_id",temp.name),temp.name)
      temp=unique(ex.curve[,temp.name1,with=F])
      data=merge(data,temp,by=temp.name,all.x=T)
      data=data[,!temp.name,with=F]
    }
    index=grepl("_id",names(data)) & names(data)!="week_id"
    data=copy(data[eval(parse(text=paste("order(",paste(names(data)[index],collapse = ","),",week_id)",sep="")))])
    group=unique(data[,names(data)[index],with=F])
    result=vector("list",nrow(group))
    name=names(group)
    for ( i in 1:nrow(group)){
      set.seed(seed*i)
      index1=vector("list",length(name))
      for (j in 1:length(name)){
        index1[[j]]=data[[name[j]]]==group[[name[j]]][i]
      }
      if (length(index1)==1) temp=data[index1[[1]]] else
        temp=data[do.call("&",index1),]
      
      # yue add
      if(length(unique(temp$factor))==1) {
        temp=unique(temp[,!c("week_id"),with=F])
        result[[i]]=cbind(data.table(week_id=0:53),temp[rep(1,54)])
      } else {
        fit=loess(factor~week_id,temp, span=0.75)
        yhat=predict(fit,temp[["week_id"]])
        yhat=(yhat-mean(yhat))/sd(yhat)
        y1=season.var*yhat+max(abs(yhat))
        y1=y1/mean(y1)
        factor=c(y1[1],y1,y1[52])+rnorm(54,0,0.01)
        temp1=data.table(factor=factor,week_id=1:54)
        fit=loess(factor~week_id,temp1, span=0.75)
        factor=predict(fit,temp1[["week_id"]])
        result[[i]]=cbind(data.table(week_id=0:53,factor=factor),temp[rep(1,54),name,with=F])
      }
      
      
    }
    
    final=rbindlist(result)
    final=data.table(client_id=rep(client_id,nrow(final)),final)
    ylim=range(final$factor)
    p=ggplot(final,aes(y=factor,x=week_id))+geom_line()+
      facet_wrap(eval(parse(text=paste("~",paste(name,collapse = "+"),sep=""))),scales="free",ncol=3)+
      labs(x="week_id",y="factor")+scale_y_continuous(limits=ylim)+
      #stat_smooth(method="loess",span = 0.75,size=1,color="darkred",fill="#FFFF99")+
      theme(plot.title = element_text(size = 20,face="bold",vjust=1),axis.title=element_text(face="bold",size=14))
    print(p)
    temp=final
    if (db.usage){
      # check new col which dosen't exit in db
      col.exist=dbGetQuery(conn,"show columns from opt_modelinput_season")[,1]
      index=!(names(temp) %in% col.exist)
      if (sum(index)!=0){
        col.new=names(temp)[index]
        for (j in 1:length(col.new)){
          print(paste("Note: New Column ",col.new[j]," Inserted",sep=""))
          query=paste(paste(col.new[j]," int null default null",sep = ""))
          dbGetQuery(conn,paste("alter table opt_modelinput_season add column ",query,sep=""))
        }
      }
      # upload curve to db
      dbGetQuery(conn,paste("delete from opt_modelinput_season where client_id=",client_id,sep=""))
      dbWriteTable(conn,"opt_modelinput_season",temp,append=T,row.names = F,header=F)
    }else write.csv(temp,"opt_modelinput_season.csv",row.names=F,na="NULL")
  }
  
  # cps file
  print("Note: Creating CPS Table")
  temp.dim=strsplit(adm.setup[attribute=="bdgt_dim"]$value,",")[[1]]
  temp.dim=temp.dim[!temp.dim %in% c("week_id","month_id")]
  temp.dim=unlist(strsplit(temp.dim,"_id"))
  if (db.usage==F) temp.dim=c(paste(temp.dim,"_id",sep=""),paste(temp.dim,"_name",sep=""),"curvegroup_id") else
    temp.dim=c(paste(temp.dim,"_id",sep=""),"curvegroup_id") 
  temp.table=ex.curve[!duplicated(bdgt_id)][,c(temp.dim,"bdgt_id","cpp"),with=F]
  ex.cps=temp.table
  setnames(ex.cps,"cpp","cps")
  temp=data.table(client_id=client_id,ex.cps)
  if (db.usage){
    # check new col which dosen't exit in db
    col.exist=dbGetQuery(conn,"show columns from opt_input_cps")[,1]
    index=!(names(temp[,!"client_id",with=F]) %in% col.exist)
    if (sum(index)!=0){
      col.new=names(temp)[index]
      for (j in 1:length(col.new)){
        print(paste("Note: New Column ",col.new[j]," Inserted",sep=""))
        query=paste(paste(col.new[j]," int null default null",sep = ""))
        dbGetQuery(conn,paste("alter table opt_input_cps add column ",query,sep=""))
        # for userinput table
        dbGetQuery(conn,paste("alter table opt_userinput_cps add column ",query,sep=""))
        # for cstr save table
        dbGetQuery(conn,paste("alter table ","opt_cps_save"," add column ",query,sep=""))
      }
    }
    # upload curve to db
    dbGetQuery(conn,paste("delete from opt_input_cps where client_id=",client_id,sep=""))
    dbWriteTable(conn,"opt_input_cps",temp,append=T,row.names = F,header=F)
  }else write.csv(temp,"opt_input_cps.csv",row.names=F,na="NULL")
  
  # modelinput output and bdgt table
  print("Note: Creating Output relalted Tables")
  bdgt=data.table(client_id=client_id,bdgt_dim=adm.setup[attribute=="bdgt_dim"]$value)
  adm.output=fread("adm_output.csv",na.strings="NULL",colClasses =list(character="filter"))
  output=data.table(client_id=rep(client_id,nrow(adm.output)),adm.output)
  if (db.usage){
    dbGetQuery(conn,paste("delete from opt_modelinput_bdgt where client_id=",client_id,sep=""))
    dbWriteTable(conn,"opt_modelinput_bdgt",bdgt,append=T,row.names = F,header=F)
    
    dbGetQuery(conn,paste("delete from opt_modelinput_output where client_id=",client_id,sep=""))
    dbWriteTable(conn,"opt_modelinput_output",output,append=T,row.names = F,header=F)
  }else{
    write.csv(bdgt,"opt_modelinput_bdgt.csv",row.names=F,na="NULL")
    write.csv(ex.output,"opt_modelinput_output.csv",row.names=F,na="NULL")
  }
  
  
  # other input tables from input.list
  print("Note: Creating Other Input Tables")
  if (db.usage){
    for (i in 1:length(input.list)){
      #i=1
      temp=fread(paste("adm_",input.list[i],".csv",sep=""))[,"label",with=F]
      if (nrow(temp)!=0){
        # check talbe existing or not first, then insert into it
        tb.name=paste("opt_label_",input.list[i],sep="")
        value=dbGetQuery(conn,paste("select label from ",tb.name,sep=""))[,1]
        index=!(temp$label %in% value)
        temp=temp[index]
        if (nrow(temp)!=0) {
          print(paste("Note: New labels Inserted in ",tb.name,sep=""))
          dbWriteTable(conn,tb.name,temp,append=T,row.names = F,header=F)
        }
        # fetch match table
        temp1=fread(paste("adm_",input.list[i],".csv",sep=""))
        temp=data.table(dbGetQuery(conn,paste("select * from ",tb.name,sep="")))
        temp=data.table(merge.data.frame(temp1,temp,by="label",all.x=T,sort=F))
        temp$client_id=rep(client_id,nrow(temp))
        temp=temp[,!"label",with=F]
        setnames(temp,"id",id.list[i])
        dbGetQuery(conn,paste("delete from ",paste("opt_input_",input.list[i],sep="")," where client_id=",client_id,sep=""))
        dbWriteTable(conn,paste("opt_input_",input.list[i],sep=""),temp,append=T,row.names = F,header=F)
      }
    }
  }
  
  # clv table
  print("Note: Creating CLV Table")
  data=fread("adm_clv.csv")
  if (db.usage){
    # convert label to id
    dim=names(data)[grepl("_name",names(data))]
    for (i in 1:length(dim)){
      temp.dim=dim[i]
      match=dbGetQuery(conn,paste("select * from opt_label_",strsplit(temp.dim,"_name")[[1]],sep=""))
      setnames(match,c("label","id"),c(temp.dim,paste(strsplit(temp.dim,"_name")[[1]],"_id",sep="")))         
      data=merge(data,match,by=temp.dim)
      data=data[,!temp.dim,with=F]
    }
    temp=data.table(client_id=rep(client_id,nrow(data)),data)
    # update db
    dbGetQuery(conn,paste("delete from opt_modelinput_clv where client_id=",client_id,sep=""))
    dbWriteTable(conn,"opt_modelinput_clv",temp,append=T,row.names = F,header=F)
  }else{
    # convert label to id
    dim=names(data)[grepl("_name",names(data))]
    for (i in 1:length(dim)){
      temp.dim=dim[i]
      match=unique(ex.curve[,c(temp.dim,id.dim),with=F])
      setnames(match,c("label","id"),c(temp.dim,paste(strsplit(temp.dim,"_name")[[1]],"_id",sep="")))         
      data=merge(data,match,by=temp.dim)
    }
    temp=data.table(client_id=rep(client_id,nrow(data)),data)
    write.csv(temp,"opt_modelinput_clv.csv",row.names = F)
  }
  
  # freq capping
  if ("adm_freq_cap.csv" %in% list.files(getwd())){
    print("Note: Creating Freq Cap Table")
    # merge id to curve data
    data=fread("adm_freq_cap.csv")
    dim=names(data)
    index=grepl("_name",dim)
    dim=dim[index]
    for (i in 1:length(dim)){
      # convert field type all to character
      if (!is.character(data[[dim[i]]])) data[[dim[i]]]=as.character(data[[dim[i]]])
      # fetch match table
      temp=data.table(dbGetQuery(conn,paste("select * from opt_label_",strsplit(dim[i],"_")[[1]][1],sep="")))
      # merge
      setnames(temp,names(temp),paste(strsplit(dim[i],"_")[[1]][1],c("id","name"),sep="_"))
      data=merge(data,temp,by=dim[i],all.x=T)
    }
    data=data[,!dim,with=F]
    temp=data.table(client_id=rep(client_id,nrow(data)),data)
    dbGetQuery(conn,paste("delete from opt_modelinput_freq_cap where client_id=",client_id,sep=""))
    dbWriteTable(conn,"opt_modelinput_freq_cap",temp,append=T,row.names = F,header=F)
  }
  
  
  # # multi-goal setup
  # print("Note: Creating Multi-goal Table")
  # temp.dim=ex.dim$multigoal[ex.dim$multigoal!=0]
  # temp.dim=paste(temp.dim,"_id",sep="")
  # temp.table=data.table(matrix(ncol=length(temp.dim)+3))
  # setnames(temp.table,names(temp.table),c("goal","check","goal_name",temp.dim))
  # temp.table=data.table(client_id=ex.dim$client_id[1],temp.table)
  # if (db.usage==F) write.csv(temp.table[-1],"opt_input_multigoal.csv",row.names = F,na="NULL")
  
  # Nationl-Local 
  if (1 %in% unique(ex.curve$is_national)) {
    if (db.usage){
      # merge id to curve data
      ex.national=fread("adm_national.csv")
      dim=names(ex.national)
      index=grepl("_name",dim)
      dim=dim[index]
      for (i in 1:length(dim)){
        # convert field type all to character
        if (!is.character(ex.national[[dim[i]]])) ex.national[[dim[i]]]=as.character(ex.national[[dim[i]]])
        # fetch match table
        temp=data.table(dbGetQuery(conn,paste("select * from opt_label_",strsplit(dim[i],"_")[[1]][1],sep="")))
        # merge
        setnames(temp,names(temp),paste(strsplit(dim[i],"_")[[1]][1],c("id","name"),sep="_"))
        ex.national=merge(ex.national,temp,by=dim[i],all.x=T)
      }
      # add in client id
      ex.national=data.table(client_id=rep(client_id,nrow(ex.national)),ex.national)
      # create bdgt_id
      bdgt_dim=strsplit(adm.setup[attribute=="bdgt_dim"]$value,",")[[1]]
      bdgt_dim=bdgt_dim[!bdgt_dim %in% c("week_id","month_id")]
      ex.national$bdgt_id=do.call(paste, c(ex.national[,bdgt_dim,with=F], sep = "_"))
      # delete name column
      index=!grepl("name",names(ex.national))
      temp=ex.national[,names(ex.national)[index],with=F]
      # check new col which dosen't exit in db
      col.exist=dbGetQuery(conn,"show columns from opt_modelinput_national_setup")[,1]
      index=!(names(temp) %in% col.exist)
      if (sum(index)!=0){
        col.new=names(temp)[index]
        for (j in 1:length(col.new)){
          print(paste("Note: New Column ",col.new[j]," Inserted",sep=""))
          query=paste(paste(col.new[j]," int null default null",sep = ""))
          dbGetQuery(conn,paste("alter table opt_modelinput_national_setup add column ",query,sep=""))
        }
      }
      
      # upload national to db
      dbGetQuery(conn,paste("delete from opt_modelinput_national_setup where client_id=",client_id,sep=""))
      dbWriteTable(conn,"opt_modelinput_national_setup",temp,append=T,row.names = F,header=F)
    }else{
      ex.national=fread("adm_national.csv")
    }
  }
  
}
