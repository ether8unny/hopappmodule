#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("XML")
library("tm")
library("tau")
library("stringr")
library("stringi")
library("shinyjs")
library("shinyBS")
library("shiny")
library("selectr")
library("rvest")
library("RSelenium")
library("RMySQL")
library("NLP")
library("lubridate")
library("ggplot2")
library("dplyr")
library("DBI")
library("data.table")
library("curl")
library("htmltools")
library("htmlwidgets")
library("ggmap")
library("compositions")
library("leaflet")
library("metricsgraphics")
library("timevis")
library("openxlsx")

source("set_vars.R")
source("selec_module.R")
source("funkys.R")
# Define UI for application that manages to do shit

cust_list <- comp_info$cust_name
cnst_choices <- names(comp_info)
names(cnst_choices) <- comp_info_names
graph_by_list <- c("day","week","month","quarter", "year")
periods_list <- c("custom","current week","previous week","previous two weeks","current month","previous month","current quarter","previous quarter","current year","previous year","rolling 6 months","rolling 12 months","rolling 24 months")
peri_by_day <-     c(1,1,1,1,1,1,1,1,1,1,1,1,1)
peri_by_week <-    c(1,1,1,1,1,1,1,1,1,1,1,1,1)
peri_by_month <-   c(1,0,0,0,1,1,1,1,1,1,1,1,1)
peri_by_quarter <- c(1,0,0,0,0,0,1,1,1,1,1,1,1)
peri_by_year <-    c(1,0,0,0,0,0,0,0,1,1,0,1,1)

hop_geo_code <- data.table(lon = -97.3247, lat = 32.74158)
register_google("AIzaSyAS7BgeS9BWMLnLSvYcqLZMHR0Zc-tkGJw")
useShinyjs()
#proc_list <- data.table(start = 0,content = "template", end = NA, type= "point",id = 0, group = "default_group")
ui <- fluidPage(
#  useShinyalert(),
  
  uiOutput("base_page")
             
  )
  


server <- function(input, output, session) {
  
  main_pan_id <- reactiveValues(inv_main = "tap_inv", dat_main = NULL)
  side_pan_id <- reactiveValues(inv_side = "side_tap_inv", dat_side = "side_dat")
  sel_choices <- reactiveValues(tap_inv = "c(inv_classes[4],inv_classes[8],inv_classes[9],inv_classes[2],inv_classes[5])")
  to_ <- reactiveValues(col = NULL, from = NULL, qty = 0, name = NULL, edit = NULL)
  db_ <- reactiveValues(inv = inv_qty, comp = comp_info,tmp = NULL)
  uiparts_ <- reactiveValues(active_page = "geo_data", change_active = FALSE, new_active = NULL )
  map_ <- reactiveValues(mp_ls = NULL, mp_nm = NULL, mlon = NULL, mlat = NULL, mzoom = NULL, mdata = NULL)
  states <- reactiveValues(change_steps_cur = 0, change_steps_last = 0)
  dt <- reactiveValues(prod_bucket = NULL, cust_bucket = NULL, rng_o_dates = c(today() - wday(today()) - 1,today()),proc_list = proc_list,
                       peri = periods_list[which(peri_by_day == 1)],proc_title = NULL, geo_bucket = NULL, cnst_state = 0,tran = NULL,prod_col_nm = NULL, t_r_d = NULL, ret_data = NULL)
  starting <- reactiveValues(proc_card = NULL)
  curr_proc_card <- reactiveVal()
  proc_title <- reactiveVal()
  curr <- reactiveValues(card_columns = c("Customer.Num","lon","lat"),dat= "On.Off.Premise",da_narrower ="On Premise",graphing_card = NULL, client_card = NULL,a_r = 35,m_a_t = 91, sales_card = NULL, card_name = NULL,sales_card_lst = NULL, card_name_lst = NULL, s_c_summary = NULL, m_l_s_d = NULL)
  choices <- reactiveValues(product_names = names(sales_log[,4:ncol(sales_log)]),da_narrower = comp_info[,On.Off.Premise], comp_choices = c("On.Off.Premise","Postal.Code","Salesman.Assigned","County","City","Deliveryman.Assigned"))
  output$base_page <- renderUI({
    
    tagList(
      navbarPage("Hop Fusion FTW", 
                 # tabPanel("Graphing",  value = "graphing"), # uiOutput("graphing")),
                 navbarMenu("Sales Data",
                            tabPanel("Mapping",value ="geo_data",uiOutput("geo_data")),
                            "----",
                            tabPanel("Graphing",value ="graph_data",uiOutput("graph_data")),
                            "----",
                            tabPanel("Tabling",value ="data_tbl_data",uiOutput("data_tbl_data"))
                            ),
                 navbarMenu("Data Inquiry",
                            tabPanel("Clients",value ="client_dat",uiOutput("client_dat")),
                            "----",
                            tabPanel("Distro",value ="dist_dat",uiOutput("dist_dat")),
                            "----",
                            tabPanel("Product",value ="prod_dat",uiOutput("prod_dat")),
                            "----",
                            tabPanel("Last Purchase",value ="last_dat",uiOutput("last_dat"))
                            
                 ),
                 tabPanel("Inventory", value ="inventory",uiOutput("inventory")),
                 navbarMenu("Production",
                            tabPanel("Time-line",
                                     value = "time_line",uiOutput("time_line_out")),
                            "----",
                            tabPanel("Recipes",
                                     value = "recipes",uiOutput("recipes")),
                            "----",
                            tabPanel("Add Steps",
                                     value = "add_steps",uiOutput("add_steps"))),
                 id = "active_page" )
    )
  })
  output$add_steps <- renderUI({
    tagList(
      fluidRow(
        column(width = 4,
               tags$div(id="a_s_1_1", uiOutput("process_name"))
        ),
        column(width = 3,
               tags$div(id="a_s_1_2", numericInput("ttl_lngth","total number of days",value = 14, min = 0, step = 1))),
        column(width = 3,
               tags$div(id="a_s_1_3", selectInput("all_proc_lst","All processes", choices = c("select process",uni_grp(),"new process"))))
               
      ),
      fluidRow(
        column(width = 4,
               tags$div(id="a_s_2_1")
        )
      ),
      fluidRow(
        timevisOutput("sample_tl")
        ),
      fluidRow(
        column(width = 3,
                      tags$div(id="a_s_4_1", selectInput("add_event_tl","Add event", choices = c("select event",tl_events_list,"new event")))
      ),
      column(width = 2,
             tags$div(id="a_s_4_2", numericInput("start_event_tl","Event day", value = 1, min = 0, step = 1))
      ),
        column(width = 3,
             tags$div(id="a_s_4_3", selectInput("add_range_tl","Add event range", choices = c("select ranged",tl_range_list,"new ranged"))
      )),
      
      column(width = 2,
             tags$div(id="a_s_4_4", numericInput("start_range_tl","day start",value = 0, min = 0, step = 1))
     ),
      column(width = 2,
             tags$div(id="a_s_4_4", numericInput("end_range_tl","day end",value = 1, min = 0, step = 1)))
      
        ),
      fluidRow(
        column(width = 2,
               tags$div(id="a_s_5_1",actionButton("event_submit","Add Event"))
               ),
        column(width=3,
               tags$div(id="a_s_5_2")
               ),
        column(width = 2,
               tags$div(id="a_s_5_3",actionButton("range_submit","Add Range"))
        )
      ),
     fluidRow(
       column(width = 8,
              tags$div(id="a_s_6_1",DT::DTOutput("step_data"))
              ),
       column(width = 2,
              tags$div(id="a_s_6_2", actionButton("apply_step","Apply and Save"))),
       column(width = 2,
              tags$div(id="a_s_6_3", actionButton("delete_proc","Delete Process")))
     )
    )
  })
  
  uni_grp <- reactive({
    unique(dt$proc_list$group)
  })
  output$time_vis_out <- renderUI({
    timevisOutput("sample_tl")
  })
  observeEvent(input$all_proc_lst,{
    
    if(!is.null(input$all_proc_lst) & input$all_proc_lst != "select process"){
      if(input$all_proc_lst == "new process"){
        curr_proc_card(NULL)
        }else{
        print(dt$proc_list)
        print(which(dt$proc_list$group ==  input$all_proc_lst))
        starting$proc_card <- dt$proc_list[which(dt$proc_list$group == input$all_proc_lst),]
        curr_proc_card(starting$proc_card)
        print("curr 1")
        print(curr_proc_card()$group[[1]])
        proc_title(curr_proc_card()$group[[1]])
      }
    }
  })
  output$process_name <- renderUI({
   # if(!is.null(input$all_proc_lst) & input$all_proc_lst != "select process"){
    #  if(input$all_proc_lst == "new process"){
    #  textInput("beer_nm","name of beer")
    #  }else{
     #   tagList(
     #     tags$h1(textOutput("process_title"))
      #  )
    #  }
   # }
  })
  observeEvent(input$all_proc_lst, {
    removeUI(
      selector = "#tit"
    )
    if(input$all_proc_lst == "new process"){
    insertUI(
      selector = "#a_s_1_1",
      where = "afterEnd",
      ui = tags$div(id = "tit",textInput("beer_nm","name of beer"))
    )
    }else{
      insertUI(
        selector = "#a_s_1_1",
        where = "afterEnd",
        ui = tags$div(id = "tit",tags$h1(textOutput("process_title")))
      )
    }
  })
  output$process_title <- renderText({
    print(curr_proc_card()$group[[1]])
   
    print("process")
   proc_title(curr_proc_card()$group[[1]])
 #  print(str(input$all_proc_lst))
 # proc_title()
   
   return(proc_title())
    
  })
   
  observeEvent(input$beer_nm,{
    curr_proc_card(NULL)
    print("bee name")
    print(input$beer_nm)
   #proc_title(input$beer_nm) 
    curr_proc_card(data.table(start = 0,content = paste0("total production time ",input$ttl_lngth," days"), end = 0+input$ttl_lngth, type= "background",id = 0, group = input$beer_nm))
    proc_title(curr_proc_card()$group[[1]])
  })
  observeEvent(input$event_submit,{
    if(input$add_event_tl != "select event"){
      if(input$add_event_tl == "new event"){
        showModal(add_event_Modal())
      }else{
      if(is.null(curr_proc_card())){
        curr_proc_card(data.table(start = input$start_event_tl,content = input$add_event_tl, end = NA, type= "point",id = 1, group =proc_title()))
    }else{
      print("else")
      print(curr_proc_card())
      mkr <- nrow(curr_proc_card())
      
      curr_proc_card(rbindlist(list(curr_proc_card(),data.table(start = input$start_event_tl,content = input$add_event_tl, end = NA, type= "point",id = curr_proc_card()$id[mkr]+1, group =proc_title()))))
    }
      }
    }
  })
  observeEvent(input$range_submit,{
    if(input$add_range_tl != "select ranged"){
      if(input$add_range_tl == "new ranged"){
        showModal(add_range_Modal())
      }else{
    if(is.null(curr_proc_card())){
      curr_proc_card(data.table(start = input$start_range_tl,content = input$add_range_tl, end = input$end_range_tl, type= "range",id = 1, group =proc_title()))
    }else{
      mkr <- nrow(curr_proc_card())
      curr_proc_card(rbindlist(list(curr_proc_card(),data.table(start = input$start_range_tl,content = input$add_range_tl, end = input$end_range_tl, type= "range",id = curr_proc_card()$id[mkr]+1, group =proc_title()))))
    }
      }
    }
  })
  s_c_Modal <- function(card_name){
    s_c_p <- make_sales_card(card_name)
    curr$sales_card <- s_c_p
    print(str(curr$sales_card))
    modalDialog(title ="view sales",
                tagList(
                  tabsetPanel(id = "s_c_pnl", type = "pills",
                              tabPanel(title = "Table",value = "s_c_table",
                                       DT::DTOutput("s_c_dt")
                                       ),
                              tabPanel(title = "Plot",value = "s_c_plot",
                                       plotOutput("imdumb")
                                       ),
                              tabPanel(title = "Summary", value = "s_c_summary",
                                       uiOutput("s_c_summ")
                                       )
                  )
                  ),
                footer = tagList(
                 # actionButton("add_new_evnt", "Confirm"),
                  modalButton("Cancel")
                  
                ),
                size="l"
    )
    
  }
  output$s_c_summ <- renderUI({
    curr$s_c_summary <- make_summary_card(curr$sales_card)
    print(str(curr$s_c_summary))
    tagList(
    fluidRow(
      column(width = 4, tags$b("Date of last sale"), textOutput("last_sale_date")
      ),
      column(width = 2, textOutput("last_sale_days")
      ),
      column(width = 4, textOutput("last_sale_prod")
      )
    ),
    fluidRow(
      column(width= 4, textOutput("first_sale_date")
      ),
      column(width = 2
      ),
      column(width = 4, textOutput("first_sale_prod")
      )
    ),
    fluidRow(
      column(width = 10, tableOutput("beer_summary")
      )
    )
    )
  })
  output$last_sale_date <- renderText({
    
     as.Date(curr$s_c_summary$last_sl,"%m/%d/%y")
    
    
  })
  output$imdumb <- renderPlot({
    #png(filename="sales_card_image.png", width=600, height=600)
    
    sales_card_image <- ggplot(data = curr$sales_card, aes(x=Date, y=sold_qty), fig.height = 20, fig.width = 20) + geom_point(aes(color = prod_name))
  #  dev.off()
    return(sales_card_image)
  })
  output$s_c_dt <- renderDT({
    curr$sales_card
  })
  add_event_Modal <- function(){
    
    modalDialog(title ="new event choice",
                tagList(
                  textInput("new_evnt_nm","title")
                ),
                footer = tagList(
                  actionButton("add_new_evnt", "Confirm"),
                  modalButton("Cancel")
                  
                )
    )
  }
  observeEvent(input$add_new_evnt,{
    removeModal()
    
    })
  add_range_Modal <- function(){
    modalDialog(title ="new event choice",
                tagList(
                  textInput("new_rnge_nm","title")
                ),
                footer = tagList(
                  actionButton("add_new_rnge", "Confirm"),
                  modalButton("Cancel")
                  
                )
    )
  }
  output$step_data <- DT::renderDT({
   
     curr_proc_card()
     
    })
  
  new_sample_tl <- reactive({
    if(!is.null(input$beer_nm)){
      dt$ret_data <- data.table(start = 0,content = paste0("total production time ",input$ttl_lngth," days"), end = 0+input$ttl_lngth, type= "background",id = 0, group = input$beer_nm)}
    else{
      dt$ret_data <- NULL
    }
    
        
      
    return(dt$ret_data)
  })
  exist_sample_tl <- reactive({
    curr_proc_card(rbindlist(list(dt$ret_data,curr_proc_card)))
    return(curr_proc_card())
  })
  observeEvent(input$delete_proc,{
    proc_list <- dt$proc_list[!dt$proc_list$group == curr_proc_card()$group[1]]
    print("testdelete")
    print(proc_list)
    data_con <- antidb()
    dbWriteTable(data_con, "proc_list", proc_list,overwrite=TRUE)
    write.csv(proc_list,file = "~/hopappmodule/data/proc_list.csv")
    
    
    save(proc_list,file = "~/hopappmodule/data/proc_list")
    dbDisconnect(data_con)
    dt$proc_list <<- proc_list
    curr_proc_card(NULL)
  })
  observeEvent(input$apply_step,{
    tempshit <- curr_proc_card()[which(!curr_proc_card()$id %in% starting$proc_card$id),]
    print("temposhit")
    print(tempshit)
    #addItems("sample_tl",curr_proc_card())
    new_proc <-  tempshit
    if(dt$proc_list$content[1] == "template"){
      data_con <- antidb()
     proc_list <- new_proc
     dbWriteTable(data_con, "proc_list", proc_list,overwrite=TRUE)
    }else{
      data_con <- antidb()
      proc_list <- rbindlist(list(dt$proc_list,new_proc))
      dbWriteTable(data_con, "proc_list", proc_list,overwrite=TRUE)
    }
    

    write.csv(proc_list,file = "~/hopappmodule/data/proc_list.csv")
    
    
    save(proc_list,file = "~/hopappmodule/data/proc_list")
    dbDisconnect(data_con)
    dt$proc_list <<- proc_list
    curr_proc_card(NULL)
    print(new_proc)
    print(proc_list)
    #dt$t_r_d <- NULL
   # dt$ret_data <- NULL
  })
  output$sample_tl <- renderTimevis({
    #dt$ret_data <- new_sample_tl()
  #if(!is.null(dt$ret_data)){
   #   timevis(dt$ret_data, groups = data.table(id = input$beer_nm, content = input$beer_nm))
  #}else{
   tl_obj <- process_timeline()
  #}
  
  })
  process_timeline <- eventReactive(curr_proc_card(),{
    print(curr_proc_card())
    timevis(curr_proc_card(),groups = data.table(id = curr_proc_card()$group[[1]], content = curr_proc_card()$group[[1]]))
    
  })
  output$data_tbl_data <- renderUI({
  })
  output$graph_data <- renderUI({
  })
 output$geo_data <- renderUI({
    tagList(
      tags$div(id="tab_ins"),
      fluidRow(
        column(width = 4,
               tags$div(id="one_one",
               actionButton("add_prod","Products",width="90%"))
        ),
        tags$div(class = "span",id = "well_panel1",
                 column(width = 8,
                    wellPanel(
                      column(width=3,
                        tags$div(id="one_three",
                          selectInput("period_selection","period : ", choices= if(is.null(input$date_by)){dt$peri}else{peri_build()},selected=if(is.null(input$period_selection)){"custom"}else{input$period_selection}))

                      ),
                      column(width = 6,
                        tags$div(id="one_four",
                          dateRangeInput("geo_date_range","date range :", start = dt$rng_o_dates[1],end = dt$rng_o_dates[2],format= "mm-dd-yyyy", width = '450px'))#,min = make_range()[1],max = make_range()[2])
                      ),
                      column(width = 3,
                        tags$div(id="one_five",
                          selectizeInput("date_by",label = "date units :",choices=graph_by_list, 
                                       selected=if(is.null(input$date_by)){"day"}else{input$date_by}))
                      ),
                    style = "height: 85px;"
                    )
        ))
        
      ),
      fluidRow(
        column(width=3,
               tags$div(id="two_one",
                        selectInput("geo_geo_sel","constraints",
                                       choices = c("choose one",cnst_choices),multiple=FALSE,selected = "choose one"))
               
                ),
        column(width=5,
               tags$div(id="two_two",sliderInput("zooms", "Zoom",
                                                 min = -8, max = 8, value = 0))
        ),
        column(width=4,
               tags$div(id="two_three",
                        actionButton("make_graph","process data", width = "90%"))
               
        ),
        style = "height: 85px;"
      ),
      fluidRow(
        column(width=3,
               tags$div(id="three_one")
        ),#print(input$geo_geo_sel),
        column(width=3,
               tags$div(id="three_two")#,
                     #   tableOutput("prod_sel_andor"))
        ),
        column(width=3,
               tags$div(id="three_three")#,
                     #   tableOutput("cust_sel_tbl")
              # )
        ),
        column(width=3,
               tags$div(id="three_four")
        )
      ),
      fluidRow(
        column(width=2,
               tags$div(id="four_one",
                        tableOutput("prod_sel_andor"))
        ),
        column(width=8,
               tags$div(id="four_two",
                        plotOutput("its_a_map"#,
                                  # hover = hoverOpts(
                                  #   id = "plot_hover"
                                   ))
                        #print(input$plot_hover))
        ),
        column(width=2,
               tags$div(id="four_three",
                        tableOutput("cust_sel_tbl")
               )
        )
      ),
      fluidRow(
        column(width = 2,
               tags$div(id="five_one")
        ),
        column(width = 8,
               tags$div(id="five_two"
                        )
      
      )
                 
      )
    )
    
  })
 prod_sel_Modal <- function(){
   modalDialog(title ="pick products",
               tagList(
                 selectizeInput("geo_prod_sel","product choice : ",choices= prod_list, multiple = TRUE, selected = dt$prod_bucket,width="90%")
               ),
               footer = tagList(
                 print("geoprodsel"),
                 if(!is.null(input$geo_prod_sel)){
                   print(input$geo_prod_sel)},
                 actionButton("butt_id5", "Confirm"),
                 modalButton("Cancel")
                 
               )
   )
 }
  observeEvent(input$butt_id3,{
    removeModal()
    showModal(pick_cnst_Modal())
  })
  
  observeEvent(input$add_prod, {  #product
    showModal(prod_sel_Modal())
    }
    )
    
  observeEvent(input$geo_prod_sel,{
    dt$prod_bucket <- input$geo_prod_sel
  })
  observeEvent(input$add_cnst, { #geographic
    showModal(cnst_Modal())
    
  })
  observeEvent(input$cnst_data,{
   dt$cust_bucket <- input$cnst_data
    })
  pick_cnst_Modal <- function(){
    print(input$geo_geo_sel)
    dt$cnst_pass <- input$geo_geo_sel
    lbl <- names(which(cnst_choices ==  dt$cnst_pass))
    chcs <- make_cnst_list()
    modalDialog(title ="pick constraint",
                tagList(
                  selectizeInput("cnst_data",lbl,choices= chcs, multiple = TRUE, selected = dt$cust_bucket,width="90%")
                  
                ),
                footer = tagList(
                  print(input$cnst_data),
                  modalButton("Cancel")
                  
                )
    )
    
  }
 
  observeEvent(input$geo_geo_sel,{
    if(input$geo_geo_sel != "choose one"){
      showModal(pick_cnst_Modal())
    }
  })
  make_cnst_list <- eventReactive(input$geo_geo_sel,{
    tmp_choice <- unlist(unique((db_$comp[,input$geo_geo_sel, with=FALSE])))
    tmp_choice <- unname(tmp_choice)
    tmp_choice <- sort(tmp_choice)

    return(tmp_choice)
  })
    observeEvent(input$period_selection,{
      dt$rng_o_dates <- switch(input$period_selection,
                                "custom" = c(as.Date("2016-11-24"),today()),
                                "current week" = c(today() - (wday(today()) - 1),today()),
                                "previous week" = c((today() - (wday(today()) - 1) -7),(today() - (wday(today()) - 1) - 1)),
                                "previous two weeks" = c(today() - (wday(today()) - 1) -14,today() - (wday(today()) - 1) -1),
                                "current month"= c(rollback(today(),roll_to_first = TRUE), today()),
                                "previous month" = c(rollback(rollback(today()),roll_to_first = TRUE),rollback(today())),
                                "current quarter"= c(today() - (qday(today() -1)), today()),
                                "previous quarter" = c(today() - (qday(today())) -qday(today() - (qday(today())))+1,today() - (qday(today()))),
                                "current year" = c(as.Date(paste(year(today()),"-01-01",sep=""),"%Y-%m-%d"),today()),
                                "previous year" = c(as.Date(paste(year(today())-1,"-01-01",sep=""),"%Y-%m-%d"),as.Date(paste(year(today())-1,"-12-31",sep=""),"%Y-%m-%d")),
                                "rolling 6 months" = c(today() - 182,today()),
                                "rolling 12 months"= c(today() - 365,today()),
                                "rolling 24 months" = c(today() - 730,today())
      )
      print("temp")
      print(dt$rng_o_dates)
      return(dt$rng_o_dates)
      
    })
    peri_build <- eventReactive(input$date_by,{
      perid <- switch(input$date_by,
                "day" = periods_list[which(peri_by_day == 1)],
                "week" = periods_list[which(peri_by_week == 1)],
                "month" = periods_list[which(peri_by_month == 1)],
                "quarter" = periods_list[which(peri_by_quarter == 1)],
                "year" = periods_list[which(peri_by_year == 1)]
      )
      return(perid)
    })
      observeEvent(input$date_by,{
                  dt$peri <- peri_build()
                  
    })
  
  output$prod_sel_andor <- renderTable(make_andor())
  output$cust_sel_tbl <- renderTable(make_cust())
  make_andor <- eventReactive(dt$prod_bucket,{
    if(!is.null(input$add_prod)){
    prod_andor <- data.table(product = dt$prod_bucket)
    return(prod_andor)
    }
    
  })
  make_cust <- eventReactive(dt$cust_bucket,{
    if(!is.null(dt$cust_bucket)){
      cust_tbl <- data.table(constraint = dt$cust_bucket)
      print(cust_tbl)
      return(cust_tbl)
    }
  })
  trig <- 0
  make_gc_coords <- function(curr_graphing_card){
    gc_center_coords = c(sum(curr_graphing_card[,lon])/length(curr_graphing_card[,lon]),sum(curr_graphing_card[,lat]/length(curr_graphing_card[,lat])))
    gc_adj_coords <- data.table(lon = c(max(curr_graphing_card[,lon]) + .65, min(curr_graphing_card[,lon]) - .65),
                             lat = c(max(curr_graphing_card[,lat]) + .05, min(curr_graphing_card[,lat]) - .05))
    print("center coords")
    print(curr_graphing_card)
    gc_bounding_box <- make_bbox(lon,lat,data = gc_adj_coords,f=.10)
    print("gc_bounding")
    print(gc_bounding_box)
    gc_zoom_lvl <- calc_zoom(gc_bounding_box)
    gc_coords_info <- c(gc_zoom_lvl,gc_center_coords)
    return(gc_coords_info)
  }
  #observeEvent(input$go_graph,{
    
    
 # })
  graph_Modal <- function(){
    modalDialog(title = "Map",
                tagList(
                  plotOutput("its_a_map_ls")
                  ),
                footer = modalButton("Dismiss"),
                size = "l")
  }
  output$graph_ls <- renderUI({
    
  })
  output$its_a_map_ls <- renderPlot({
    
    
      map_plotted_ls <- get_googlemap(key="AIzaSyAS7BgeS9BWMLnLSvYcqLZMHR0Zc-tkGJw",center = c(lon = map_$mlon, lat = map_$mlat),size = c(480, 480), scale = 2, zoom = map_$mzoom) %>% ggmap(extent="panel") + geom_point(data = curr$graphing_card,aes(alpha = 0.05, color=-group, size =.1))
    
    return(map_plotted_ls)
  },height = 800, width = 800)
  observeEvent(input$make_graph,{
        map_$mp_nm <- make_map_coords()
        print(map_$mp_nm)
        map_$mlon <- as.numeric(map_$mp_nm[2])
        map_$mlat <- as.numeric(map_$mp_nm[3])
        map_$bbox <- map_$mp_nm[c(2:5)]
        map_$mzoom <- as.integer(map_$mp_nm[1])
        map_$made_data <- make_data_for()
        map_$made_data <- as.data.table(map_$made_data)
        trig <- 1
        #plot_things()
  })
  output$plotter <- renderUI({
    tagList(
      wellPanel(
        plotOutput("its_a_map",
                   hover = hoverOpts(
                     id = "plot_hover"
                   ))
        )
    )
  })

  
  
  make_map_coords <- eventReactive(input$make_graph,{
    if(!is.null(dt$cust_bucket)){
    map_coords <- do_geocode(dt$cust_bucket)
    center_coords = c(sum(map_coords[,1])/length(map_coords[,1]),sum(map_coords[,2]/length(map_coords[,2])))
    adj_coords <- data.table(lon = c(max(map_coords[,1]) + .65, min(map_coords[,1]) - .65),
                             lat = c(max(map_coords[,2]) + .05, min(map_coords[,2]) - .05))
    print("center coords")
    print(map_coords)
    bounding_box <- make_bbox(lon,lat,data = adj_coords,f=.10)
    print("bounding")
    print(bounding_box)
    zoom_lvl <- calc_zoom(bounding_box)
    coords_info <- c(zoom_lvl,center_coords)
    return(coords_info)
    }
  })
  output$its_a_map <- renderPlot({
    if(is.null(map_$bbox)){
      center <- as.numeric(hop_geo_code)
      map_plotted <- get_googlemap(key="AIzaSyAS7BgeS9BWMLnLSvYcqLZMHR0Zc-tkGJw",center = center,size = c(480,480), scale = 2, zoom = 14+input$zooms) %>% ggmap(extent = "panel") + geom_point(data = hop_geo_code, size = 3, color = "blue")
     }else{
    map_plotted <- get_googlemap(key="AIzaSyAS7BgeS9BWMLnLSvYcqLZMHR0Zc-tkGJw",center = c(lon = map_$mlon, lat = map_$mlat),size = c(480, 480), scale = 2, zoom = map_$mzoom+input$zooms) %>% ggmap(extent="panel") + geom_point(data = map_$made_data,aes(alpha = 0.05, color=prod, size = case_equiv/100))
      }
    return(map_plotted)
  },height = 800, width = 800)
  
  make_products_list <- eventReactive(input$make_graph,{
    product_list <- full_clip_prod_dt[which(full_clip_prod_dt$prod_name %in% dt$prod_bucket),prod_num_full_clip]
    return(product_list)
  })
  make_data_for <- eventReactive(input$make_graph,{
    print(input$geo_date_range)
    date_const <- sales_log[which(sales_log[,Date] >= input$geo_date_range[1] & sales_log[,Date] <= input$geo_date_range[2]),]
    
    product_list <-  make_products_list()
    dt$prod_col_nm <- unlist(lapply(1:length(product_list), function(x) paste("X",product_list[x],sep="")))
    print(dt$prod_col_nm)
    
    vect_lst <- c("Customer.Num",dt$prod_col_nm)
    print("vset")
    print(vect_lst)
    prod_const <- date_const[,vect_lst,with=FALSE]
    for(i in 1:length(product_list)){
      u <- i + 1
      tmp_lst <- which(prod_const[,u,with=FALSE] > 0)
      tmp_dt <- prod_const[tmp_lst,c(1,u),with=FALSE]
      tmp_lst2 <- comp_info[which(as.character(unlist(comp_info[,dt$cnst_pass,with=FALSE])) %in% dt$cust_bucket),]
      tmp_dt1 <- tmp_dt[which(as.character(unlist(tmp_dt$Customer.Num)) %in% as.character(unlist(tmp_lst2$cust_num))),]
      tmp_dt2 <- tmp_dt1[, c(.N, lapply(.SD, sum)), by="Customer.Num"]
      tmp_dt2[,prod := names(tmp_dt2[,3])]
      setnames(tmp_dt2,old=dt$prod_col_nm[i],new="case_equiv")
      for(k in 1: length(tmp_dt2$Customer.Num)){
        tmp_dt2$lon[k] <- as.numeric(unlist(tmp_lst2[which(tmp_lst2[,cust_num]==tmp_dt2[k,Customer.Num]),c("lon")]))
      }
      for(p in 1: length(tmp_dt2$Customer.Num)){
        tmp_dt2$lat[p] <- as.numeric(unlist(tmp_lst2[which(tmp_lst2[,cust_num]==tmp_dt2[p,Customer.Num]),c("lat")]))
        }
      if(i == 1){
        tmp_dt3 <- tmp_dt2
      }else{
        tmp_dt3 <- bind_rows(tmp_dt2,tmp_dt3)
      }
      if(i == length(product_list)){
        final_dt <- tmp_dt3
      }
    }
    save(final_dt,file="~/hopappmodule/data/final_dt")
    print(final_dt)
    return(final_dt)
    dbDisconnect(data_con)
    
  })
  output$search_by <- renderUI({
    
    # get the col names of f and assign them to a list
    cols = mapply(list, names(comp_info))
    names(cols) <- comp_col_names
    print(names(cols))
    # render column group checkbox ui after loading the data
    tagList(
      column(2,
             selectizeInput("opt_one","Search by",choices = cols, selected = cols[2])
      ),
      column(3,
             selectizeInput("show_one","Columns",choices = cols[2:length(cols)], multiple = TRUE, selected = cols[c(2,3,6,9,17)])
      )
    )
    
  })
  
  output$last_dat <- renderUI({
    
    
    tagList(
      uiOutput("last_sl_inputs"),
      uiOutput("button_embed"),
      uiOutput("last_sl_table")
    #print(curr$m_l_s_d)
    
    )
    
  })
  init_mlsd <- eventReactive(input$active_page,{
    curr$m_l_s_d <- make_last_sales_data(curr$m_a_t,curr$a_r,curr$dat,curr$da_narrower,curr$card_columns)
    
  })
  output$last_sl_table <- renderUI({
    if(is.null(input$make_it_so)){
      init_mlsd()
    }
    tagList(
    fluidRow(
      
        
        
        DT::DTOutput("mlsd_table")
      
      
    ),
    
    uiOutput("graph_ls")
    )
  })
  output$button_embed <- renderUI({
    fluidRow(
      column(width = 3,
             actionButton("make_it_so","Update Table")),
      column(width=3,
             actionButton("go_graph","Garph Table")),
      column(width=3,
             actionButton("save_excel","Save this bitch"))
    )
  })
  
  observeEvent(input$save_excel,{
    showModal(file_save_Modal())
    
  })
  observeEvent(input$save_file_modal,{
    write.xlsx(curr$m_l_s_d, file = input$file_name_save, colNames = TRUE, borders = "columns", overwrite=TRUE)
    
  })
  file_save_Modal <- function(){
    modalDialog(title = "Save to excel format",
                tagList(
                nm_var <- paste("~/hopappmodule/exportedfiles/aged_account_",as.character(Sys.Date()),".xlsx",sep=""),
                textInput("file_name_save","Name of file",value = nm_var),
                actionButton("save_file_modal","Save File")
                ),
                footer = modalButton("Dismiss"),
                size = "l")
  }
  observeEvent(input$go_graph,{
    curr$graphing_card <- make_graphing_card(curr$m_l_s_d)
    map_$mp_ls <- make_gc_coords(curr$graphing_card)
    print(map_$mp_ls)
    map_$mlon <- as.numeric(map_$mp_ls[2])
    map_$mlat <- as.numeric(map_$mp_ls[3])
    #map_$bbox <- map_$mp_ls[c(2:5)]
    map_$mzoom <- as.integer(map_$mp_ls[1])
    #uiOutput("graph_ls")
    showModal(graph_Modal())
  })
  observeEvent(input$max_age_threshhold,{
    curr$m_a_t <- input$max_age_threshhold
  })
  observeEvent(input$aged_range,{
    curr$a_r <- input$aged_range
  })
  observeEvent(input$dormant_account_constraints,{
    curr$dat <- input$dormant_account_constraints
  })
  observeEvent(input$da_narrower,{
    curr$da_narrower <- input$da_narrower
  })
  observeEvent(input$make_it_so,{
    #init_mlsd()
    curr$m_l_s_d <- make_last_sales_data(curr$m_a_t,curr$a_r,curr$dat,curr$da_narrower,curr$card_columns)
  })
  
output$m_a_thresh <- renderUI({
  column(width = 3,
         numericInput("max_age_threshhold","most recent ",min =7, max = 735, step = 7,value = if(is.null(input$max_age_threshhold)){curr$m_a_t}else{input$max_age_threshhold}
         )#if(!is.null(input$max_age_threshhold)){input$max_age_threshhold}else{91})
         #           sliderInput("max_age_threshhold","Max Number of Days",min = 
         #                         if(is.null(input$aged_range)){
         #                           14}else{ input$aged_range + 7},max = 735,
         #                       step = 7, value = 182)
  )
})
output$a_rhresh <- renderUI({
  column(width = 3,
         isolate(numericInput("aged_range","range of scope",min =14,max = 721, step = 7,value =if(is.null(input$aged_range)){curr$a_r}else{input$aged_range}))
         
         
         
         #           sliderInput("aged_range","Min Number of Days",max = 
         #                         if(!is.null(input$max_age_threshhold)){
         #                          728}else{ input$max_age_threshhold - 7},min = 7,
         #                      step = 7, value = 91)
  )
})
 output$last_sl_inputs <- renderUI({
   tagList(
   fluidRow(
     uiOutput("m_a_thresh")
     ,
     uiOutput("a_rhresh")
     
     ,
     column(width = 3,
            selectizeInput("dormant_account_constraints","Narrow search by",
                           choices = c(choices$comp_choices), 
                           selected=if(is.null(input$dormant_account_constraints)){curr$dat}else{input$dormant_account_constraints}
            )),
     column(width = 3,
            selectizeInput("da_narrower","Narrow Values",choices = choices$da_narrower,multiple=TRUE, selected=if(is.null(input$da_narrower)){curr$da_narrower}else{input$da_narrower},width='90%')
     )
   ),
   fluidRow(
     ))
 })
 observeEvent(input$dormant_account_constraints, {
   choices$da_narrower <- unique(comp_info[,input$dormant_account_constraints,with=FALSE])
  })#keep
 
  output$mlsd_table <- renderDT(
    
    curr$m_l_s_d,selection=list(mode="single", target="cell"),
    server = FALSE,
    rownames=FALSE,
    editable=TRUE
  )
  observeEvent(input$mlsd_table_cells_selected,{
    if(length(input$mlsd_table_cells_selected) != 0){
      str(input$mlsd_table_cells_selected)
      print("cher")
      cc1 <- input$mlsd_table_cells_selected[1,1]
      cc2 <- input$mlsd_table_cells_selected[1,2]+1
      print(cc1)
      print(cc2)
      such <- curr$m_l_s_d
      
      stch <- names(such[cc1,cc2,with=FALSE])
      
      print(stch)
      cc_num <- such[cc1,Customer.Num]
      print("ccnum")
      print(cc_num)
      
     
      #print(str(curr$card_name_lst))
      curr$card_name_lst <- as.character(unlist(cc_num))
      if(stch == "s_card"){
        #curr$sales_card_lst <- make_sales_card(curr$card_name_lst)
        #print(curr$sales_card_lst)
        
        showModal(s_c_Modal(curr$card_name_lst))
      }else{
        if(stch == "Customer.Num" | stch == "Company"){
          curr$client_card <- make_client_card(curr$card_name_lst)
          print(curr$client_card)
        }
      }
      
    }
  })
  output$client_dat <- renderUI({
   
    tagList(
                  fluidRow(
                            uiOutput("search_by"),
                            column(7,
                            selectizeInput("opt_one_sel","Values",choices = NULL,multiple=TRUE, selected=NULL,width='90%')
                           )
        ),
       # mainPanel(width = 9,
                  fluidRow(
                    column(4),
                    column(4,
                        h1("Client Data")
                    )
                  ),
                  #print(),
                  fluidRow(
                    DT::DTOutput("search_output")
                    )
                  
                            
       # )#)
    )
  })#kep
  observeEvent(input$opt_one, {
    dt$cols <-  mapply(list, db_$comp[,input$opt_one,with=FALSE])
    dt$uni <- unlist(unique(dt$cols))
    if (is.null(dt$uni)) dt$uni <- ""
    # update input$vars for pivottable tab
    print("observerEvent")
    print(dt$uni)
    if(is.null(input$opt_one)) { 
      updateSelectizeInput(session, "opt_one_sel", "Values", choices = "", selected = NULL)  
    }
    else {
      updateSelectizeInput(session, "opt_one_sel", "Values", choices = dt$uni, selected = NULL)
    }
  }, ignoreNULL = FALSE)#keep
    
  
  
coord_one <- eventReactive(input$search_output_cells_selected,{

  cord_1 <- as.integer(unlist(input$search_output_cells_selected))[1]
  return(cord_1)
})        
observeEvent(input$search_output_cells_selected,{
  if(length(input$search_output_cells_selected) != 0){
    str(input$search_output_cells_selected)
    print("cher")
    c1 <- input$search_output_cells_selected[1,1]
    c2 <- input$search_output_cells_selected[1,2]+1
    print(c1)
    print(c2)
    srch <- search_dt()
    print("srch")
    print(srch)
    c_num <- srch[c1,Customer.Num]
    print("cnum")
    print(c_num)
    srch <- srch[c1,c2,with=FALSE]
    
    print(srch)
    curr$card_name <- as.character(unlist(c_num))
    grp <- length(grep("sales_card",srch,value=TRUE))
  print(str(curr$card_name))
    if(length(grep("sales_card",srch))>0){
      #curr$sales_card <- make_sales_card(curr$card_name)
      #print(curr$sales_card)
    showModal(s_c_Modal(curr$card_name))
    }
  
  }
})
get_col_name <- eventReactive(input$search_output_cells_selected,{
  col_name <- names(search_dt()[coord_one(),coord_two(),with=FALSE])
  return(col_name)
})
  output$print2 <- renderPrint(db_$comp[which(db_$comp[,1] == unlist(search_dt()[coord_one(),1,with=FALSE])),get_col_name(),with=FALSE]) #[as.integer(unlist(input$search_output_cells_selected))[1],as.integer(unlist(input$search_output_cells_selected))[2]+1,with=FALSE])
  #output$print2 <- renderPrint(print(as.integer(unlist(input$search_output_cells_selected))[2]))  
  output$search_output <- DT::renderDT(search_dt(),selection=list(mode="single", target="cell"),
                                              server = FALSE,
                                              rownames=FALSE,
                                              editable=TRUE
    
  )
  proxy = dataTableProxy('search_output')
  
  observeEvent(input$search_output_cell_edit, {
    info = input$search_output_cell_edit
    str(info)
    i = info$row
    j = info$col +1
    v = info$value
   
 #   save(db_$comp,file = "~/hopappmodule/data/comp_info")
 db_$tmp <- search_dt()
 if(is.integer(db_$tmp[i, j,with=FALSE])){
   db_$tmp[i, j] <- as.integer(v) 
  # print("int")
 #  print(db_$tmp[i, j,with=FALSE])
 }else{
   db_$tmp[i, j] <- as.character(v) 
 #  print("char")
 #  print(db_$tmp[i, j,with=FALSE])
 }

    fir_co <- which(db_$comp[,1] == unlist(db_$tmp[i, 1,with=FALSE]))
    sec_co <- names(db_$tmp[i, j,with=FALSE])
   
 if(is.integer(db_$comp[fir_co,sec_co,with=FALSE])){
    db_$comp[fir_co,sec_co] <- as.integer(v) 
    #  print("int")
    #  print(db_$comp[fir_co,sec_co,with=FALSE])
 }else{
      db_$comp[fir_co,sec_co] <- as.character(v) 
   #   print("char")
   #   print(db_$comp[fir_co,sec_co,with=FALSE])
 }  
    
    print("after")
    print(db_$comp[fir_co,sec_co,with=FALSE])
    comp_info <<- db_$comp
 data_con <- antidb()
    
    write.csv(comp_info,file = "~/hopappmodule/data/comp_info.csv")
    dbWriteTable(data_con, "comp_info", comp_info,overwrite=TRUE)
    
    save(comp_info,file = "~/hopappmodule/data/comp_info")
    dbDisconnect(data_con)
    
})
 search_dt <-  eventReactive(input$opt_one_sel,{
    if(!is.null(input$opt_one_sel)){
      tmpwhich <- c(0)
      
      for(i in 1: length(input$opt_one_sel)){
        tmpwhich <- c(tmpwhich,which(db_$comp[,input$opt_one,with=FALSE] == input$opt_one_sel[i]))
        print(tmpwhich)
        print("tmp")
        
      }
      col_s <- c("Customer.Num",input$show_one)
        scoop <- db_$comp[tmpwhich[1:length(tmpwhich)],col_s,with=FALSE]
    #    scoop <- db_$comp[which(db_$comp[,input$opt_one,with=FALSE] == input$opt_one_sel[i]tmpwhich[1:length(tmpwhich)],col_s,with=FALSE]
        
        print(scoop)
        
        return(scoop)
      
    }
  })
  observeEvent(input$dat_side_panel,{
    main_pan_id$dat_main <- input$dat_side_panel
    side_pan_id$dat_side <- paste("side_",input$dat_side_panel,sep="")
    
  })  
  #start inventory secvtion
  obj_id <- reactiveValues(act_butt1 = "inv_adj", act_butt1_lbl = "Move Inv", full_toggle = "full_off",full_toggle_lbl = "Show All")
  dt_inv <- reactiveValues(tbl = NULL)
  
  output$inventory <- renderUI({
    tagList(
        
                    tags$div(id = "row_one",
                             fluidRow(
                              column(width = 2,
                                     actionButton("full_on","hide 0 totals",width = "90%")),
                              column(width = 2,
                                     actionButton("add_prodb","add production",width = "90%")),
                              column(width = 2,
                                     actionButton("move_multib","move multi",width = "90%"))
                             )
                    ),
                    fluidRow(
                      uiOutput("dt_ui")
                    )#,
                      #uiOutput(main_pan_id$inv_main)
                    
          
        )
})
  output$add_numerics <- renderUI({
    if(!is.null(input$inv_add_prod)){
      if(length(input$inv_add_prod) < 6 ){
      lapply(1:length(input$inv_add_prod),function(i){
      numericInput(inputId = paste0("add_p_",i),paste0(input$inv_add_prod[i]), value = 0)
      })
      }else{
        "Too many selections"
      }
      }
  })
  observeEvent(input$add_prodb,{
    showModal(add_prod_Modal())
  })
  observeEvent(input$move_multib,{
    
  })
  observeEvent(input$inv_tbl_cells_selected,{
    if(length(input$inv_tbl_cells_selected) != 0){
    str(input$inv_tbl_cells_selected)
    print("sucher")
    dt_inv$aa <- input$inv_tbl_cells_selected[1,1]
    dt_inv$bb <- input$inv_tbl_cells_selected[1,2]
   
    showModal(editModal(dt_inv$aa,dt_inv$bb, dt_inv$tbl))
    }
  })
  observeEvent(input$do_add_to,{
    removeModal()
    showModal(confirmModal("Confirm add to production","conf_add_to"))
  })
  observeEvent(input$do_inv_mov,{
    removeModal()
    showModal(confirmModal("Confirm inventory move","conf_inv_mv"))
  })
  observeEvent(input$conf_add_to,{
    removeModal()
    cel_sell_row <- dt_inv$tbl[dt_inv$aa,1,with=FALSE]
    row_nm <- which(unlist(inv_qty[,1]) %in% unlist(cel_sell_row))
    print(row_nm)
    inv_qty[row_nm,3] <<- inv_qty[row_nm,3] + input$num_2_add
    inv_qty$totals <- unlist(lapply(1:nr, function(x) sum(inv_qty[x,3:11])))
    dt_inv$tbl <<- inv_qty
    save(inv_qty, file = "~/hopappmodule/data/inv_qty")
    print("inv_qty saved")
    data_saver("inv_qty", inv_qty)
    if(obj_id$full_toggle == "full_off"){
      obj_id$full_toggle <- "full_on"
      set_table()
      print("off")
    }else{
      if(obj_id$full_toggle == "full_on"){
        obj_id$full_toggle <- "full_off"
        set_table()
        print("on")
      }
    }
  })
  observeEvent(input$conf_inv_mv,{
    removeModal()
    cel_sell <- dt_inv$tbl[dt_inv$aa,dt_inv$bb,with=FALSE]
    cel_sell_row <- dt_inv$tbl[dt_inv$aa,1,with=FALSE]
    col_nm_fm <- names(cel_sell)
    col_nm_to <- input$move_2
    print("cel")
    print(cel_sell)
    print(col_nm_fm)
    print(col_nm_to)
    
    row_nm <- which(unlist(inv_qty[,1]) %in% unlist(cel_sell_row))
    print(row_nm)
    
    inv_qty[row_nm,col_nm_to] <<- inv_qty[row_nm,col_nm_to, with = FALSE] + input$num_2_mv
    
    inv_qty[row_nm,col_nm_fm] <<- inv_qty[row_nm,col_nm_fm, with = FALSE] - input$num_2_mv
    dt_inv$tbl <<- inv_qty
    save(inv_qty, file = "~/hopappmodule/data/inv_qty")
    print("inv_qty saved")
    data_saver("inv_qty", inv_qty)
    if(obj_id$full_toggle == "full_off"){
      obj_id$full_toggle <- "full_on"
      set_table()
      print("off")
    }else{
    if(obj_id$full_toggle == "full_on"){
      obj_id$full_toggle <- "full_off"
      set_table()
      print("on")
    }
    }
  })
  observeEvent(input$move_from,{
    removeModal()
    showModal(move_Modal(dt_inv$aa,dt_inv$bb, dt_inv$tbl))
  })
  
  observeEvent(input$add_to,{
    removeModal()
    showModal(add_to_Modal(dt_inv$aa,dt_inv$bb, dt_inv$tbl))
  })
  observeEvent(input$prod_det,{
    
  })
  observeEvent(input$keg_det,{
    
  })
  observeEvent(input$full_on,{
    if(obj_id$full_toggle == "full_off"){
      obj_id$full_toggle <- "full_on"
      set_table()
    }else{
      if(obj_id$full_toggle == "full_on"){
        obj_id$full_toggle <- "full_off"
        set_table()
        }
    }
  })
  output$dt_ui <- renderUI({
    set_table()
    DT::DTOutput("inv_tbl")
    
  })
  set_table <- reactive({
    nr <- nrow(inv_qty)
    inv_qty$totals <- unlist(lapply(1:nr, function(x) sum(inv_qty[x,3:11])))
    if(obj_id$full_toggle == "full_off"){
      
      dt_inv$tbl <<- inv_qty[which(inv_qty$totals > 0),]
      updateActionButton(session,"full_on",label = "show all")
    }else{
      if(obj_id$full_toggle == "full_on"){
        
        dt_inv$tbl <<- inv_qty
        updateActionButton(session,"full_on",label = "hide 0 totals")
        
      }
    }
    return(dt_inv$tbl)
  }
                             )
  

 output$inv_tbl <- DT::renderDT(set_table(), options = list(pageLength = 50,rownames = inv_qty[,1]), selection=list(mode="single", target="cell"),
 server = FALSE,editable=TRUE
 )
 
}
#end inventory section

# Run the application 
shinyApp(ui = ui, server = server)

