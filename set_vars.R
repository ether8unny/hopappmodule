#function for dummy inventory
make_inv_db <- function(){ 
  load("~/hopappmodule/data/inv_classes")
  load("~/hopappmodule/data/prod_list")
  load("~/hopappmodule/data/prod_num_list")
  
  inv_qty <- data.table(prod_name = prod_list, prod_num = prod_num_list)
  inv_qty[,eval(inv_classes[1]) := as.integer(3)]
  inv_qty[,eval(inv_classes[2:length(inv_classes)]) := as.integer(0)]
  write.csv(inv_qty,file = "~/hopappmodule/data/inv_qty.csv")
  
  save(inv_qty,file = "~/hopappmodule/data/inv_qty")
  
  return(inv_qty)
}
init_timeline <- function(){
  
}
make_sales_log <- function(){
  salesbydatebynumbers <- as.data.table(read.csv("~/hopappmodule/data/salesbydatebynumbers.csv",header = TRUE, stringsAsFactors = FALSE))
  salesbydatebynumbers[,Date:=as.Date(Date,"%m/%d/%Y")]
  tmp <- as.data.table(lapply(salesbydatebynumbers, function(x) stri_replace_all_fixed(x,"(","-")))
  tmp <- as.data.table(lapply(tmp, function(x) stri_replace_all_fixed(x,")","")))
  tmp <- as.data.table(lapply(tmp[,4:ncol(tmp)], function(x) as.numeric(x)))
  sales_log <- data.table(salesbydatebynumbers[,1:3],tmp)
  rm(tmp,salesbydatebynumbers)
  write.csv(sales_log,file = "~/hopappmodule/data/sales_log.csv")
  data_con <- antidb()
  
  save(sales_log,file = "~/hopappmodule/data/sales_log")
  dbWriteTable(data_con, "sales_log", sales_log, overwrite=TRUE)
  dbDisconnect(data_con)
  
}

load_list <- c("full_clip_prod_dt","inv_qty","periods_by","periods_list","inv_classes","prod_list
               ","comp_info","comp_col_names","sales_log","comp_info_names","proc_list","tl_events_list",
               "tl_range_list")
if(file.mtime("~/hopappmodule/data/comp_info") < file.mtime("~/hopappmodule/data/compinfo2.csv")){
  load("~/hopappmodule/data/comp_col_names")
  comp_info <- as.data.table((read.csv("~/hopappmodule/data/compinfo2.csv",header = TRUE,stringsAsFactors=FALSE)),keep.rownames = FALSE)
  names(comp_info) <- comp_col_names
  comp_info[,main_phone:= character()]
  comp_info[,contact_name := character()]
  comp_info[,contact_phone := character()]#
  comp_info[,contact_email := character()]
  comp_info[,lon := numeric()]
  comp_info[,lat := numeric()]
  for(i in 1: nrow(comp_info)){
    if(is.na(comp_info[i,lon])){
      tmp_df <- geocode(unlist(comp_info[i,3]),override_limit = TRUE,output = "latlon")
      if(!is.na(tmp_df$lon)){
        comp_info$lon[i] <- tmp_df$lon
        comp_info$lat[i] <- tmp_df$lat
        
        
      }
    }
  }
  write.csv(comp_info,file = "~/hopappmodule/data/comp_info.csv")
  
  save(comp_info,file = "~/hopappmodule/data/comp_info")
  
  print("comp_info file updated from new download file and saved.")
}else{
  print("comp_info file up to date.")
}
if(file.mtime("~/hopappmodule/data/sales_log") < file.mtime("~/hopappmodule/data/salesbydatebynumbers.csv")){
  salesbydatebynumbers <- as.data.table((read.csv("~/hopappmodule/data/salesbydatebynumbers.csv",header = TRUE,stringsAsFactors=FALSE)),keep.rownames = FALSE)
  salesbydatebynumbers[,Date:=as.Date(Date,"%m/%d/%Y")]
  tmp <- as.data.table(lapply(salesbydatebynumbers, function(x) stri_replace_all_fixed(x,"(","-")))
  tmp <- as.data.table(lapply(tmp, function(x) stri_replace_all_fixed(x,")","")))
  tmp <- as.data.table(lapply(tmp[,3:length(salesbydatebynumbers)], function(x) as.numeric(x)))
  sales_log <- data.table(salesbydatebynumbers[,1:2],tmp)
  rm(salesbydatebynumbers,tmp)
  
  data_con <- antidb()
  
  dbWriteTable(data_con, "sales_log", sales_log,overwrite=TRUE)
  dbDisconnect(data_con)
  write.csv(sales_log,file = "~/hopappmodule/data/sales_log.csv")
  save(sales_log,file = "~/hopappmodule/data/sales_log")
  print("sales_log file updated from new download file and saved.")
}else{
  print("sales_log file up to date.")
}


for(i in 1:length(load_list)){
 # if(exists(load_list[i])){
  #    print(paste(load_list[i]," already exists. removing.",sep=""))
   # rm(list = load_list[i])
  #  }
      if(file.exists(paste("~/hopappmodule/data/",load_list[i],sep=""))){
        load(paste("~/hopappmodule/data/",load_list[i],sep=""))
        if(exists(load_list[i])){
          print(paste(load_list[i]," loaded from file.",sep=""))
        }
      
    }

}

#variables
