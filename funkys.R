do_geocode <- function(tmp_txt){
  
  print(tmp_txt)   
  map_coords <- geocode(tmp_txt, source = "google")
  map_coords <- na.omit(map_coords)
 
  print("map_coo")
  print(map_coords)
  return(map_coords)
  
}
make_summary_card <- function(curr_sales_card){
  beer_summary <- curr_sales_card[,sum(prod_qty),by = prod_name]
  names(beer_summary)[2] <- "qty"
  ttl_qty <- beer_summary[,sum(qty)]
  last_sl <- as.Date(curr_sales_card[1,Date])
  time_since_lst_sl <- as.numeric(Sys.Date() - as.Date(last_sl))
  first_sl <- as.Date(curr_sales_card[nrow(curr_sales_card),Date])
  lst_prod <- curr_sales_card[1,prod_name]
  summary_card <- list(beer_summary = beer_summary,ttl_qty=ttl_qty,last_sl = last_sl,time_since_lst_sl = time_since_lst_sl,lst_prod = lst_prod,first_sl = first_sl)
  return(summary_card)
}
make_last_sales_data <- function(m_a_t,a_r,dat,narrower,card_columns){
  all_dates <- unique(setDT(sales_log)[, .SD[which.max(Date)], by="Customer.Num"])
  print(today() - a_r)
  this <- all_dates[which(all_dates$Date < as.Date(today() - m_a_t) & all_dates$Date > as.Date(today()  - m_a_t - a_r)),]
  print("this")
   print(head(this))
   print(narrower)
   print(dat)
  that <- comp_info[which(comp_info[,dat,with=FALSE] ==  narrower),c(card_columns,dat),with=FALSE]
  print("this shit again")
  print(that)
  stps <- nrow(this)
  
  this_Company <- this$Company
  this_Customer.Num <- this$Customer.Num
  this_Date <- this$Date
  this_prods <-  unlist(lapply(1:stps, function(x) names(this[x,which(colSums(this[x,4:ncol(this)]) >0)][1]) ))
 # print("this_prods")
 # print(this_prods)
  this_s_card <- "sales_card" #unlist(lapply(1:stps, function(x) as.numeric(this[x,which(colSums(this[x,4:ncol(this)]) >0) - 3,with=FALSE][1])))

  this_other <- data.table(Company = this_Company, Customer.Num = this_Customer.Num, Date = date(this_Date), prod_name =this_prods, s_card = this_s_card)
  that_and_this <- merge(this_other,that,by="Customer.Num")
  print("tnt")
  print(that_and_this)
 # print("other")
  #print(this_other)
  
 # print(this_other)
  
  return(that_and_this)
}
make_graphing_card <- function(gc_cnum_dt){
  gc_lon <- comp_info$lon[which(comp_info$Customer.Num %in% gc_cnum_dt$Customer.Num)] 
  gc_lat <- comp_info$lat[which(comp_info$Customer.Num %in% gc_cnum_dt$Customer.Num)] 
  gc_cnum_dt[,lon := gc_lon]
  gc_cnum_dt[,lat := gc_lat]
  rnumbs <- nrow(gc_cnum_dt)
  age_list <- unlist(lapply(1:rnumbs, function(x) round(365/as.integer(today() - gc_cnum_dt$Date[x]))))
  print(age_list)
  
  
  
  print(age_list)
  gc_cnum_dt[,group := age_list]
  
  print(gc_cnum_dt)
  return(gc_cnum_dt)
  }
make_sales_card <- function(sc_cnum){
  sales_card <- NULL
  tmp_nim <- ncol(sales_log)
  tmpdt <- sales_log[which(sales_log$Customer.Num == sc_cnum & rowSums(sales_log[,4:tmp_nim]) > 0),]
  print(tmpdt)
  dead_cols <- 3
  nutmpnum <- ncol(tmpdt)
  tmp2 <- unique(unlist(lapply(1:nrow(tmpdt), function(x) tmpdt[,which(tmpdt[x,] > 0)])))
  print("tm,p2")
  print(tmp2)
  print("adjtmp2")
  tmp2 <- tmp2[4:length(tmp2)]
  print(tmp2)
        
  nmsthing <- as.data.table(tmpdt[,1:3])
  print("nmsthing")
  print(nmsthing)
  nmsthing[,prod_name := names(tmpdt[,tmp2,with=FALSE])]
  print("nmsthing")
  print(nmsthing)
  sold_qty <- unlist(lapply(1:length(tmp2), function(x) tmpdt[,tmp2[x],with = FALSE]))
  #print(sold_qty)
  sold_qty <- sold_qty[which(sold_qty > 0)]
  #print(sold_qty)
  nmsthing[,sold_qty := sold_qty]
  print(tmpdt[,tmp2,with=FALSE])
  print("nmsthing")
  print(nmsthing)
  sales_card <- nmsthing
  return(sales_card)
  
  
}
make_client_card <- function(sc_cnum){
  ccnim <- ncol(sales_log)
  client_card <- sales_log[which(sales_log$Customer.Num == sc_cnum),]
  
  print(client_card)
  return(client_card)
}
antidb <- function(){
  data_con <- dbConnect(MySQL(),user= "remote", password = "r3m0t3", host = "75.142.162.88",port = 3306, dbname = "hopappmodule")
  return(data_con)
}
data_saver <- function(obj_name, data_set){

data_con <- antidb()
dbWriteTable(data_con, obj_name, data_set,overwrite=TRUE)


dbDisconnect(data_con)
}



add_prod_Modal <- function(){
  modalDialog(title = "add to production",
              tagList(
                selectizeInput("inv_add_prod","product(s) to add; up to 5",
                               choices = unique(inv_qty[,1]),multiple=TRUE),
                            
                               
                                  uiOutput("add_numerics")
                                  
                                
                              
              ),
                footer = tagList(
                  actionButton("butt_id2", "Confirm"),
                  modalButton("Cancel")
                  
                )
              )
}

add_to_Modal <- function(aa,bb,tbl){
  sel_cell <- tbl[aa,bb, with = FALSE]
  title_nm <- tbl[aa,1, with = FALSE]
  footer_nm <- names(sel_cell)
  col_lngth <- length(tbl)-1
  col_nm <- names(tbl[,4:col_lngth])
  modalDialog(title= title_nm,
              tagList(
                numericInput("num_2_add","Units",value = 0,min = 1 )
                ),
              footer = tagList(
                tags$b(footer_nm),
                actionButton("do_add_to", "Submit"),
                modalButton("Cancel")
                
              ),
              size = c("s")
              
              
  )
}

move_Modal <- function(aa,bb,tbl){
  sel_cell <- tbl[aa,bb, with = FALSE]
  title_nm <- tbl[aa,1, with = FALSE]
  footer_nm <- names(sel_cell)
  col_lngth <- length(tbl)-1
  col_nm <- names(tbl[,4:col_lngth])
  modalDialog(title= title_nm,
              tagList(
              numericInput("num_2_mv","Units",value = 0,min = 0, max = sel_cell ),
              selectInput("move_2","Destination",choices = col_nm[which(col_nm != footer_nm)])
              ),
              footer = tagList(
                tags$b(footer_nm),
                actionButton("do_inv_mov", "Submit"),
                modalButton("Cancel")
                
              ),
              size = c("s")
              
    
  )
}
confirmModal <- function(msg_2_disp,butt_id){
  modalDialog(
    title = "Confirm",
    tagList(tags$b(msg_2_disp))
    
    ,
    footer = (
      tagList(
        actionButton(butt_id, "Confirm"),
        modalButton("Cancel")
      )
    )
  )
}
editModal <- function(aa,bb,tbl, failed = FALSE) {
 sel_cell <- tbl[aa,bb, with = FALSE]
  print(str(tbl[aa,1, with = FALSE]))
  title_nm <- tbl[aa,1, with = FALSE]
  footer_nm <- names(sel_cell)
  print(str(names(sel_cell)))
  modalDialog(title= title_nm,
    
    
    if (failed)
      div(tags$b("fuck off", style = "color: red;")),
    if(names(sel_cell) %in% c("prod_num","prod_name")){
     tags$div(actionButton("prod_det", tags$b("Product Info", style = "color: blue;"))
      #footer = NULL#,
       # modalButton("Cancel")
        
      ) 
    }else{
    if(names(sel_cell) == "production"){
      tagList(
        tags$div(actionButton("keg_det", tags$b("Keg Info", style = "color: blue;")),
        actionButton("add_to", tags$b("Add New", style = "color: red;")),
        actionButton("move_from", tags$b("Move", style = "color: green;")))
        )
    }else{
     tagList(
        tags$div(actionButton("keg_det", tags$b("Keg Info", style = "color: blue;")),
        actionButton("move_from", tags$b("Move", style = "color: green;")))
      )
    
    }},footer = 
      tagList(
        tags$b(footer_nm),
        modalButton("Cancel")
        
      ),
        size = c("s")
  )
}
import_comp_info <- function(filename,filepath,comp_info_current){
path_plus_file <- paste0(filepath,filename,sep="")  
comp_i <- as.data.table((read.csv(path_plus_file,header = TRUE,stringsAsFactors=FALSE)),keep.rownames = FALSE)
#names(comp_i) <- comp_col_names
comp_i[,main_phone:= character()]
comp_i[,contact_name := character()]
comp_i[,contact_phone := character()]#
comp_i[,contact_email := character()]
comp_i[,lon := numeric()]
comp_i[,lat := numeric()]
comp_i[,s_card := "sales_card"]
for(i in 1: nrow(comp_i)){
  if(is.na(comp_i[i,lon])){
    tmp_df <- geocode(unlist(comp_i[i,3]),override_limit = TRUE,output = "latlon")
    if(!is.na(tmp_df$lon)){
      comp_i$lon[i] <- tmp_df$lon
      comp_i$lat[i] <- tmp_df$lat
      
      
    }
  }
}
add_on_comp_i <- comp_i[which(!comp_i$Customer.Num %in% comp_info_current$Customer.Num),]
return_comp_i <- rbind(comp_info_current,add_on_comp_i)
write.csv(return_comp_i,file = "~/hopappmodule/data/comp_infotest.csv")

save(return_comp_i,file = "~/hopappmodule/data/comp_infotest")
print("comp_info file updated from new download file and saved.")
return(return_comp_i)
}