#get sales by cust num
tmp1009 <- sales_log[which(sales_log$Customer.Num == 1009),]
#extract list of sold products
sold_product <- tmp1009[,which(colSums(tmp1009[] > 0) > 0)]
sold_product <- sold_product[3:length(sold_product)]
prod_nums <- names(sold_product)
prod_nums <- stri_replace_all_fixed(prod_nums,"X","")


tmp_lst <- inv_qty[which(!inv_qty$prod_name %in% product_num_list$prod_name),c("prod_name","prod_num"),with=FALSE]
product_num_list <- full_clip_prod_dt[,prod_num_full_clip,prod_name]
names(product_num_list) <- c("prod_name","prod_num")
product_number_list <- rbindlist(list(product_num_list,tmp_lst))

sales_card_1009 <- data.table(cust_num = tmp1009[2,Customer.Num], date = tmp1009[2,Date], products_sold = which(tmp1009[,sold_product,with=FALSE] > 0) )
product_number_list$prod_name[which(product_number_list$prod_num %in% prod_nums)]

make_sales_card <- function(cust_num){
  sales_card <- NULL
  tmpdt <- sales_log[which(sales_log$Customer.Num == cust_num & rowSums(sales_log[,3:47] > 0)),]
  s_p <- which(colSums(tmpdt[] > 0)>0)
  tmpdt <- tmpdt[,s_p,with=FALSE]
  tmp_s_p <- length(s_p)
  s_p <- s_p[3:tmp_s_p]
 
  print(tmpdt)
  stps <- nrow(tmpdt)
  for (i in 1:stps){
    dat_ret <- tmpdt[i,]
 #  print("datret")
 #   print(dat_ret)
  #  print(dat_ret$Date)
    tmp_d_r <- dat_ret[,3:ncol(dat_ret),with=FALSE]
   # print(tmp_d_r)
    prods <- names(tmp_d_r[,which(colSums(tmp_d_r[1,]) >0),with=FALSE])
    prods <- stri_replace_all_fixed(prods,"X","")
  #  print(prods)
    amts <- unlist(tmp_d_r[,which(colSums(tmp_d_r[1,]) >0),with=FALSE])
    nms <- product_number_list$prod_name[which(product_number_list$prod_num %in% prods)]
   #print(amts)
  #  print("nms")
  #  print(nms)
    if(i == 1){
      sales_card <- data.table( Date = dat_ret$Date, Customer.Num = dat_ret$Customer.Num, prod_name = nms, prod_qty = amts)
    }else{
      tmp_card <- data.table( Date = dat_ret$Date, Customer.Num = dat_ret$Customer.Num, prod_name = nms, prod_qty = amts)
      sales_card <- rbindlist(list(sales_card,tmp_card))
    }
  }
    return(sales_card)
    
    
  }
    for(i in 1:967){
      
    }

make_summary_card <- function(curr_sales_card){
  beer_summary <- curr_sales_card[,sum(prod_qty),by = prod_name]
  names(beer_summary)[2] <- "qty"
  ttl_qty <- beer_summary[,sum(qty)]
  last_sl <- curr_sales_card[1,Date]
  time_since_lst_sl <- as.numeric(Sys.Date() - as.Date(last_sl))
  first_sl <- curr_sales_card[nrow(curr_sales_card),Date]
  lst_prod <- curr_sales_card[1,prod_name]
  summary_card <- list(beer_summary = beer_summary,ttl_qty=ttl_qty,last_sl = last_sl,time_since_lst_sl = time_since_lst_sl,lst_prod = lst_prod,first_sl = first_sl)
  return(summary_card)
}

make_client_details_card <- function(){
  
}
}