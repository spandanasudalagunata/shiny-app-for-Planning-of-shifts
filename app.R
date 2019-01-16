library(shiny)
library(lpSolveAPI)
library(shinyWidgets)
library(shinydashboard)
ui <- fluidPage(
  setBackgroundColor(color = c("#F7FBFF", "#2171B5"),
                     gradient = "radial",
                     direction = c("top", "left")),
  titlePanel("Planning of shifts through Linear programming"),
  br(),
  h4("This shiny app is useful for planning the shifts of employees.
     The shifts are consecutive and circular within a 24 hours time frame. 
     This app uses linear programming for planning the shifts."),
  br(),
  
  h5("HELP CONTENT: The user must input the shift, enter the number of employees per shift and click
     on the ADD SHIFT button. Repeat these steps for each shift required as input to plan the shifts. 
     Once the inputs are given click on the GET SOLUTION button."), 
  br(),
  h5("Please input the shift(example-S1)"),
  textInput(
    inputId = "shiftId",
    label = "The shifts"
  ),
  numericInput(
    inputId = "n",
    "Enter the Number of employees per shift",
    value = NULL,
    min = 1,
    max = 100
  ),

  actionButton(inputId = "add", "ADD SHIFT",style = "color: white;
                           background-color: blue"),
  actionButton(inputId = "solve", label = "GET SOLUTION",style = "color: white;
                           background-color: green"),
  h5("Added shifts"),
  verbatimTextOutput('shifts'),
  br(),
  h5("Added no.of employees per shift"),
  verbatimTextOutput("employees"),
h5("Optimal Solution"),

tableOutput("table"),
h5("Objective Function value"),

textOutput("objective")  
  
)

server <- function(input, output) {
  myValues <- reactiveValues()
  
  observe({
    if(input$add > 0){
      myValues$shifts <- c(isolate(myValues$shifts), isolate(input$shiftId))
      myValues$employees <- c(isolate(myValues$employees), isolate(input$n))
    }
  })
  
  output$shifts<-renderPrint({
    myValues$shifts
    
  })
  
  output$employees<-renderPrint({
    myValues$employees
  })

  
  lp_prob <- observeEvent(input$solve, {
    
    
    shift_df <- data.frame(
      shifts = myValues$shifts,
      num_emp = myValues$employees
    )
    
    num_dvs <- length(shift_df$shifts)
    
    lp <- make.lp(0, num_dvs)
    
    for (i in 1:num_dvs) {
      if (i == 1) {
        
        add.constraint(lp, rep(1, 2), ">=", rhs = shift_df$num_emp[[i]], indices = c(num_dvs, i)) 
      }
      else{
        
        add.constraint(lp, rep(1, 2), ">=", rhs = shift_df$num_emp[[i]], indices = c(i-1, i)) 
      }
    }
    set.type(lp, c(1:num_dvs), "integer")
    
    #set objective coefficients
    set.objfn(lp, rep(1, num_dvs))
    
    #set objective direction
    lp.control(lp, sense = 'min')
    
    #I in order to be able to visually check the model, I find it useful to write the model to a text file
    solve(lp)
    
    print(lp)
    
    print(get.objective(lp))
    
    print(get.constraints(lp))
    
    print(get.variables(lp))
    
    output$table <- renderTable({
     data.frame(
       Shifts = myValues$shifts,
       Number_of_Employees = get.variables(lp)
       
        ) 
    })
    
    output$objective <- renderText(print(get.objective(lp)))
  })
  
  
}


shinyApp(ui = ui, server = server)
