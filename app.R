# Package yang mungkin digunakan
# library(shiny)
# library(shinydashboard)
# library(shinydashboardPlus)
# library(RPostgreSQL)
# library(DBI)
# library(DT)
# # library(bs4Dash)
# library(dplyr)
# library(recipes)
library(shinythemes)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(recipes)
data(mtcars)
#=========================Used Dataset=========================#
# Define df1
tabel01 <- data(women)
tabel02 <- data(mtcars)
variables=c("mpg","cyl","disp","hp","drat","wt","qsec","vs")
#============================Interface===========================#
# Define UI
# ui <- fluidPage(
#   navbarPage(
#     "Dashboard Kami hehe",
#     tabPanel(
#       "Regression",
#       tabName = "regression",
#       selectInput(inputId = "dependent",
#                   label = "Dependent Variables",
#                   choices = as.list(variables)),
#       uiOutput("indep"),
#       verbatimTextOutput(outputId = "RegOut")
#     )
#   )
# )


ui <- shinyUI(
  fluidPage(
    titlePanel("Linear Regression"),
    sidebarLayout(
      # sidebar
      sidebarPanel(
        # sidebar
        selectInput(inputId = "data", "Enter Data", 
                    choices = c("Example Datasets", "Upload File", "Enter Your Own")),
        # ----- Input Mandiri
        conditionalPanel(
          condition = "input.data == 'Enter Your Own'",
          textOutput(outputId = "caption")
        ),
        checkboxInput(inputId = "show_sum", label = "Show Summary"),
        conditionalPanel(
          condition = "input.show_sum == TRUE",
          verbatimTextOutput("sum")
        ),
        # Plot Options
        h4("Plot Options"),
        checkboxInput(inputId = "smooth", label = "Smooth Trend"),
        conditionalPanel(
          condition = "input.smooth == TRUE",
          sliderInput(inputId = "slider.smooth", label = "Degree of Smoothness", min = 0.5, max = 2, value = 0.8)
        ),
        checkboxInput(inputId = "2", label = "Regression Line"),
        checkboxInput(inputId = "2", label = "Click to Remove Points"),
        checkboxInput(inputId = "2", label = "Drag Points"),
        checkboxInput(inputId = "2", label = "Select Variable(s) for Hover Info"),
        checkboxInput(inputId = "2", label = "Title & Subtitle"),
        checkboxInput(inputId = "2", label = "Regression Line"),
        h4("Regression Options"),
        checkboxInput(inputId = "", label = "Find Predicted Value"),
        checkboxInput(inputId = "", label = "Show Residuals on Plot"),
        checkboxInput(inputId = "", label = "Show Standard Errors & P-Values"),
        checkboxInput(inputId = "", label = "Confidence Intervals for Slope"),
        checkboxInput(inputId = "", label = "Confidence/Prediction Interval"),
        checkboxInput(inputId = "", label = "ANOVA Table"),
        conditionalPanel(
          condition = "input.data == 'Input Mandiri'",
          sliderInput(inputId = "slider.x", label = "Atur rentang X", min = -100, max = 100, value = c(-10, 10)),
          sliderInput(inputId = "slider.y", label = "Atur rentang Y", min = -100, max = 100, value = c(-10, 10))
        )
      ),
      # main
      mainPanel(
        tabsetPanel(
          #tab 1
          tabPanel(
            #plot
            title = "Plot",
            plotlyOutput(outputId = "plot", width = "100%", height = "100%"),
            #tabel
            tableOutput(outputId = "table")
          ),
          tabPanel(
            # information
            title = "Information",
            textOutput(outputId = "info")
          )
        )
      )
    )
  )
)







#==========================Back - End===========================#
# Define server logic required to draw a histogram
server <- #============================= SERVER (back-end) ==============================#
  function(input, output){
    
    #----------DATA-----------#
    data <- reactive({
      req(input$data)
      if(input$data == "Example Datasets") {
        generateRandomData(n = input$slider.n, type = input$data, s = "sedang", slope = rnorm(1, mean = 0, sd = 5))
      } else if (input$data == "Upload File" || input$data == "Dataset Kuadratik") {
        generateRandomData(n = input$slider.n, type = input$data, s = input$spread, slope = input$slope)
      } else {
        click <- event_data("plotly_click", source = "inputPlot")
        data.frame(x = click$x, y = click$y)
      }
    })
    
    
    
    
    output$indep <- renderUI({
      selectInput(inputId = "indep", label = "Independent Variables", 
                  multiple = FALSE, choices = as.list(AttributeChoices[AttributeChoices!= input$dependent]), selected = AttributeChoices[1])
    })
    
    formula <- reactive({
      req(input$indep)
      mtcars %>%
        recipe() %>%
        update_role(!!!input$dependent, new_role = "outcome") %>%
        update_role(!!!input$indep, new_role = "predictor") %>%
        prep() %>% 
        formula()
    })
    
    lm_reg <- reactive(
      lm(formula(),data = mtcars)
    )
    
    output$RegOut = renderPrint({
      summary(lm_reg())
    })
    
    output$sum <- renderPrint({
      summary(data)
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)
