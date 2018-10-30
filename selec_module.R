inv_movementUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("controls"))
}

inv_movement <- function(input, output, session,from_choices, prod_choices,sel_choices) {
  output$controls <- renderUI({
    ns <- session$ns
    tagList(
    selectizeInput(ns("move_from"),"Move from",choices= from_choices, selected=if(!is.null(input$move_from)){input$move_from}else{from_choices[1]}),
    selectizeInput(ns("inv_to_move"),"Inventory",choices= prod_choices, selected=if(!is.null(input$inv_to_move)){input$inv_to_move}else{prod_choices[1]}),
    numericInput(ns("qty_to_move"),"Quantity",value = input$qty_to_move ,min=0, step = 1),
    selectizeInput(ns("move_to"),"Move to",choices= sel_choices, selected=if(!is.null(input$move_to)){input$move_to}else{sel_choices[1]})
    
    )
  })
  return(reactive(c(input$move_from,input$inv_to_move,input$move_to,input$qty_to_move)))
}