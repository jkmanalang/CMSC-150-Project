#***********************************************************
#
# This is a script that performs Quadratic Spline
# Interpolation and Simplex Method
#
# @author John Kenneth Manalang
# @created_date  2020-12-16  11:45
#
#***********************************************************

source("ManalangEx08.R")
source("ManalangEx09.R")

library(shiny)
library(shinyMatrix)

# Defining UI
ui <- fluidPage( 
  titlePanel("CMSC 150 Exercise 10"),  hr(), br(),
  
  tabsetPanel( 
    # ------------ UI for exer 8 ---------------
    tabPanel(("Quadratic Spline Interpolation"), br(),br(),br(),
             
             fluidRow(
               column(4, offset = 1,
                      actionButton("add", "Add point"),
                      actionButton("remove", "Remove point"),
                      
                      div(id = "points",
                          style = "border: 1px solid silver;")     
               ),
               column(5,
                      h3("Estimated y value:"),
                      verbatimTextOutput("resultQSI", placeholder = TRUE)
               ),
               column(5,
                      plotOutput("graph")
               )
             ), 
             br(),
             fluidRow( 
               column(2, offset = 1,
                      numericInput("estimate", h5("estimate="), value = 0, width = 80)
                      ),
               column(6, offset = 2,
                      h3("Functions per interval:"),
                      htmlOutput("functions", placeholder = TRUE)
               )
             ),
             br(), br(),
             fluidRow(
               column(3, offset = 1,
                      actionButton("solve", "Solve QSI")
               )
             )
             
    ),
    # ------------ UI for exer 9 ---------------
    tabPanel(("Simplex Method"), br(),br(),br(),
             fluidRow(
               column(3, offset = 1,
                      numericInput("variables", h5("Variables="), value = 0, width = 80),
                      numericInput("constraints", h5("Constraints="), value = 0, width = 80), br(),br(),
                      actionButton("create", "Input Values")
               ),
               column(5,
                      fluidRow(
                        column(6,
                               selectInput("type", label = h5("Type:"), 
                                             choices = list("Maximization" = TRUE, "Minimization" = FALSE),
                                             selected = TRUE)
                               ),
                        column(6,
                               selectInput("shippingProb", label = h5("Exer 10 Word Problem:"), 
                                             choices = list("Yes" = TRUE, "No" = FALSE),
                                             selected = FALSE)
                               )
                      ),
                      
                      h3("Result:"),
                      verbatimTextOutput("resultSimplex", placeholder = TRUE)
               )
             ),
             
             br(),br(),br(),
             div(id = "tableau"),
             div(id = "forSolveButton"), br(),br(),
             div(id = "resultingTableauHeader"), br(),
             div(id = "resultingTableau"), br(),br(),
             div(id = "solutionSetHeader"), br(),
             div(id = "solutionSet"), br(), br(),
             div(id = "exerWordProbHeader"), br(),
             div(id = "exerWordProb")
    )
  )
  
)

# Defining server logic
server <- function(input, output, session) {
  #----------- for exer 8 ---------------------
  values <- reactiveValues(num_points = 0) 
  # add button
  observeEvent(input$add, ignoreNULL = FALSE, ignoreInit = TRUE, {
    
    values$num_points <- values$num_points + 1
    num <- values$num_points
    
    # insert numericInput UI
    insertUI( immediate = TRUE,
              selector = "#points", where = "beforeEnd", 
              splitLayout(
                cellWidths = c("50%","50%"), 
                cellArgs = list(style = "padding: 5px"),
                id = paste0("point", num),
                
                numericInput(paste0("x",num), h5(paste0("x",num,"=")), value = 0, width = 80),
                numericInput(paste0("y",num), h5(paste0("y",num,"=")), value = 0, width = 80)
              ))
    
  })
  
  # remove button
  observeEvent(input$remove, {
    num <- values$num_points

    removeUI(selector = paste0("#point", num))
    values$num_points <- values$num_points - 1
    
    if(values$num_points == 0){
      # resetting the graph
      x = c(0)
      y = c(0)
      output$graph <- renderPlot({
        plot(x, y, col = "red", main = "Knots")
      })
    }

  })
  
  # solve button
  observeEvent(input$solve, {
    # getting the points from user inputs
    x = c()
    y = c()
    estimate = NULL
    for(i in 0:values$num_points){
      x <- append(x, input[[paste0("x", i)]])
      y <- append(y, input[[paste0("y", i)]])
    }
    
    data = list(x, y)
    
    # checking if points is more than 2
    if(values$num_points > 2){
      output$resultQSI <- renderText(NULL)
      
      # checking if the given values are sorted
      if(is.sorted(data)){
        poly = poly.qsi(data, input$estimate)
        output$resultQSI <- renderText(poly[[2]])
        estimate = poly[[2]]
        
        # getting functions per interval
        if(!is.na(estimate)){
          output$functions <- renderUI({
            textfunction = ""
            
            # converting the function into string
            for(i in 1:((length(x))-2)){
              textfunction = paste(textfunction, "Interval ", sep = "")
              textfunction = paste(textfunction, i, sep = "")
              textfunction = paste(textfunction, ": ", sep = "")
              
              polyFuncs = deparse(poly[[1]][i])[2]
              
              textfunction = paste(textfunction, "function(x) ", sep = "")
              textfunction = paste(textfunction, polyFuncs, sep = "")
              textfunction = paste(textfunction, "", sep = "<br/><br/>")

            }
            HTML(paste(textfunction, sep = '<br/>'))
          })
        }
        
      } else {
        output$resultQSI <- renderText("Please manually sort your x values in ascending order")
      }
      
    }else{
      output$resultQSI <- renderText("Please insert more than two data points")
    }
    
    # plotting the estimate
    if(!is.null(estimate)){
      x <- append(x, input$estimate)
      y <- append(y, estimate)
    } 
    if (length(x) != 0){
      output$graph <- renderPlot({
        plot(x, y, col = "red", main = "Knots")
      })
    }
  })
  
  #----------- for exer 8 ---------------------
  
  #----------- for exer 9 ---------------------
  
  observeEvent(input$create, {
    # ensuring that the variable and constraints counts are more than 1
    if(input$variables > 1 && input$constraints > 1){
      
      rowCount = (input$constraints) +1
      colCount = (input$variables) +1
      
      rowName = character(rowCount)
      colName = character(colCount)
      
      # generating row and column names
      for(i in 1:(rowCount-1)){
        rowName[i] <- paste0("Constraint ", i)
      }
      rowName[rowCount] <- "Objective Function"
      
      for(i in 1:(colCount-1)){
        colName[i] <- paste0("x", i)
      }
      colName[colCount] <- "RHS"
      
      # creating the matrix where user will input values
      m <- matrix(0, rowCount, colCount, dimnames = list(rowName, colName))
      insertUI(
        immediate = TRUE, selector = "#tableau", where = "beforeEnd",
        matrixInput(
          "mat", value = m, class = "numeric"
        )
      )
      
      # creating the compute button
      insertUI(
        immediate = TRUE, selector = "#forSolveButton", where = "beforeEnd",
        actionButton("compute", "Compute")
      )
      removeUI(selector = "#create")
      
    } else {
      output$resultSimplex <- renderText("variable and constraint must be both more than 1")
    }
  })
  
  # compute button
  observeEvent(input$compute, {
    # computing using simplex method
    tableau = formTableau(input$mat, input$type)
    result = simplex(tableau, input$type, input$shippingProb)
    
    # checking if there is a feasible solution
    if(!is.na(result[[2]][length(result[[2]])])){
      output$resultSimplex <- renderText(result[[2]][length(result[[2]])])
    } else {
      output$resultSimplex <- renderText("No Feasible Solution")
    }
    
    # for resulting tableau
    insertUI(
      immediate = TRUE, selector = "#resultingTableauHeader", where = "beforeEnd",
      h3("Resulting Tableau")
    )
    insertUI(
      immediate = TRUE, selector = "#resultingTableau", where = "beforeEnd",
      output$resultingTabl <- renderTable(result[[1]], rownames = TRUE, width = "auto", colnames = TRUE, spacing = "l")
    )
    
    # for solution set
    insertUI(
      immediate = TRUE, selector = "#solutionSetHeader", where = "beforeEnd",
      h3("Solution Set")
    )
    insertUI(
      immediate = TRUE, selector = "#solutionSet", where = "beforeEnd",
      output$solutionSet <- renderTable(result[[2]], width = "auto", colnames = TRUE, spacing = "l")
    )

    if(input$shippingProb == TRUE){
      # for matrix specific to exer 9 word problem
      insertUI(
        immediate = TRUE, selector = "#exerWordProbHeader", where = "beforeEnd",
        h3("Items shipped from a plant to a warehouse")
      )
      insertUI(
        immediate = TRUE, selector = "#exerWordProb", where = "beforeEnd",
        output$exerWordProb <- renderTable(result[[4]], rownames = TRUE, width = "auto", colnames = TRUE, spacing = "l")
      )
    }

    removeUI(selector = "#compute")
  })
  
  #----------- for exer 9 ---------------------
  
}

# Run the app ----
shinyApp(ui = ui, server = server)