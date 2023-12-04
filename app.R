library(shinythemes)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
###############################################################################
#                               USER INTERFACE                                #  
###############################################################################
ui <- shinyUI(
  navbarPage(
    title = "LINEAR REGRESSION",
    #~~~~~~~~~~~~~~~~~~~~~~~~~Tab 1~~~~~~~~~~~~~~~~~~~~~~~~~#
    tabPanel(
      title = "Fit Linear Regression Model",
      sidebarLayout(
        #-------------Sidebar Panel inside Tab 1------------#
        sidebarPanel(
          # ----- Data Source Selection ----- #
          selectInput(inputId = "data", "Enter Data", 
                      choices = c('Example Datasets', 
                                  'Upload File', 
                                  'Enter Your Own'),
                      multiple = FALSE,
                      selected = "Example Datasets"),
          # --- [Cond 1] Select: Example Datasets
          conditionalPanel(
            condition = "input.data == 'Example Datasets'",
            selectInput(inputId = 'select_example',
                        label = "Dataset Choice",
                        choices = list("cars",
                                       "women",
                                       "rock",
                                       "pressure"),
                        multiple = FALSE)
          ),
          # --- [Cond 2] Select: Upload File
          conditionalPanel(
            condition = "input.data == 'Upload File'",
            fileInput(inputId = 'chosen_file', 
                      label = 'Choose CSV File',
                      accept = c('text/csv',
                                 'text/comma-separated-values,text/plain',
                                 '.csv')
            )
          ),
          # --- [Cond 3] Select: Enter Your Own
          conditionalPanel(condition = "input.data == 'Enter Your Own'",
                           hotable("hot")   # INI KAYAKNYA ADA YG SALAH
                           
          )
        ),
        mainPanel(
          # --- Descriptive Statistics --- #
          fluidRow(
          ),
          # --- Scatter Plot --- #
          fluidRow(
          ),
          # --- Linear Regression Equations --- #
          fluidRow(
          ),
          # --- Model Summary --- #
          fluidRow(
          )
        )
      )
    ),
    #~~~~~~~~~~~~~~~~~~~~~~~~~Tab 2~~~~~~~~~~~~~~~~~~~~~~~~~#
    tabPanel(
      title = "Fitted Values & Residual Analysis",
      # Layout of Tab 2
      sidebarLayout(
        #-------------Sidebar Panel inside Tab 2------------#
        sidebarPanel(
          checkboxGroupButtons(inputId = "show", 
                               label = "Show column:", 
                               choices = c("Column 1", "Column 2"))
        ),
        mainPanel(
          fluidRow(
            column(6,
                   uiOutput("col1")),
            column(6,
                   uiOutput("col2"))
          )
        )
      )
    )
  )
)
###############################################################################
#                                    SERVER                                   #  
###############################################################################
server <- 
  function(input, output){
    #~~~~~~~~~~~~~~~~~~~~~~Used Dataset~~~~~~~~~~~~~~~~~~~~~#
    
    myData <- reactive({
      req(input$data)
      # ---------- Example Dataset ---------- #
      if(input$data == "Example Datasets"){
        chosendata <- reactive({
          switch(input$select_example,
                 "cars" = mtcars,
                 "women" = women, 
                 "rock" = rock,
                 "pressure" = pressure)})
        return(as.data.frame(chosendata()))
      }
      # ---------- Upload the Dataset ---------- #
      else if(input$data == "Upload File"){
        filedata <- reactive({
          inFile <- input$chosen_file
          if (is.null(inFile)) return(NULL)
          readData <- read.csv(inFile$datapath, header = TRUE)
        })
        return(as.data.frame(readData))
      }
      # ---------- Manually Enter Dataset ---------- #
      else if (input$data == "Enter Your Own"){
        # Initiate the Table
        temp_tbl <- setNames(data.frame(matrix(ncol = 2, nrow = 10)),
                             c(get(input$dv), get(input$iv))) #JANGAN LUPA DEFINE DV & IV JADI ELSE = X & Y DI AKHIR
        init_tbl <- reactive({temp_tbl})
        # Table Changes
        edit_tbl <- reactive({
          if(is.null(input$hot)){return(init_tbl())}
          else if(!identical(init_tbl(), input$hot)){
            # hot.to.df will convert the updated table into df
            as.data.frame(hot.to.df(input$hot))
          }
        })
        output$hot <- renderHotable({edit_tbl()}, readOnly = F)
        return(edit_tbl())
      }
    })
    
    output$show_tbl = renderTable(myData())
    
    #     # ---------- Example Dataset ---------- #
    #     example_df <- reactive({
    #       input_data <- switch(input$select_example,
    #                            "cars" = mtcars,
    #                            "women" = women,  
    #                            "rock" = rock,
    # "pressure" = pressure
    #              )
    #     })
    #     # ----- [output] Example Dataset
    #     output$ex_tbl = DT::renderDataTable(example_df())
    #     
    #     # data view 
    #     # output$view <- renderTable({
    #     #   head(datasetInput(), n = input$obs)
    #     # })
    #     
    #     # ---------- Upload the Dataset ---------- #
    #     myData <- reactive({
    #       inFile <- input$datfile
    #       if (is.null(inFile)) return(NULL)
    #       data <- read.csv(inFile$datapath, header = TRUE)
    #       data
    #     })
    #     # ----- [output] Uploaded Dataset
    #     output$contents <- DT::renderDataTable({
    #       DT::datatable(myData())       
    #     })
    #     
    #     # ---------- Manually Enter Dataset ---------- #
    #     df_create <- setNames(data.frame(matrix(ncol = 2, nrow = 10)),
    #                           c(get(input$iv), get(input$dv)))
    #     # Initiate the Table
    #     init_df <- reactive({df_create})
    #     # Trigger the Change
    #     Trigger_orders <- reactive({
    #       if(is.null(input$hotable1)){return(init_df())}
    #       else if(!identical(init_df(),input$hotable1)){
    #         # hot.to.df function will convert your updated table into the dataframe
    #         as.data.frame(hot.to.df(input$hotable1))
    #       }
    #     })
    #     output$hotable1 <- renderHotable({Trigger_orders()}, readOnly = F)
    #     # ----- [output] Entered Dataset
    #     output$own_tbl = DT::renderDataTable(Trigger_orders())
    
    ###################################################################
    #~~~~~~~~~~~~~~~~~~~~~~All Input Informations~~~~~~~~~~~~~~~~~~~~~#
    ###################################################################
    # ---------- Independent Variable ---------- #
    output$iv <- renderUI({
      if(req(input$data) != "Enter Your Own"){
        selectInput(inputId = 'iv', label = h5('Independent Variable'), 
                    choices = names(output$show_tbl))
      } else if(req(input$data) == "Enter Your Own"){
        textInput(inputId = 'iv',
                  label = h5('Write Your Independent Variable'),
                  value = "X")
      }
    })
    # ---------- Dependent Variable ---------- #
    output$dv <- renderUI({
      if(req(input$data) != "Enter Your Own"){
        selectInput(inputId = 'dv', label = h5('Dependent Variable'), 
                    choices = names(output$show_tbl))
      } else if(input$data == "Enter Your Own"){
        textInput(inputId = 'dv',
                  label = h5('Write Your Dependent Variable'),
                  value = "Y")
      }
    })
    
    
    
    # # --- Example Dataset
    # output$ex_iv = renderUI({
    #   selectInput(inputId = 'sel_iv', label = h4('Independent Variable'), 
    #               choices = names(example_df()))
    # })
    # # --- Uploaded Dataset
    # output$upl_iv = renderUI({
    #   selectInput(inputId = 'upl_iv', label = h4('Independent Variable'), 
    #               choices = names(myData()))
    # })
    # 
    # # --- Manually Entered Dataset
    # output$ent_iv = renderUI({
    #   textInput(inputId = 'ent_iv',
    #             label = h4('Write Your Independent Variable'),
    #             value = "X")
    # })
    
    # # ---------- Dependent Variable ---------- #
    # # --- Example Dataset
    # output$sel_dv = renderUI({
    #   selectInput(inputId = 'sel_dv', label = h4('Dependent Variable'), 
    #               choices = names(example_df()))
    # })
    # # --- Uploaded Dataset
    # output$upl_dv = renderUI({
    #   selectInput(inputId = 'upl_dv', label = h4('Dependent Variable'), 
    #               choices = names(myData()))
    # })
    # 
    # # --- Manually Entered Dataset
    # output$ent_dv = renderUI({
    #   textInput(inputId = 'ent_dv',
    #             label = h4('Write Your Dependent Variable'),
    #             value = "Y")
    # })
    
    # ---------- Regression Information ---------- #
    # --- Regression Formula
    regFormula <- reactive({
      as.formula(paste(input$dv, '~', input$iv))
    })
    
    # --- Linear Model
    model <- reactive({
      lm(regFormula(), data = myData())
    })
    
    
    # formula <- reactive({
    #   req(input$indep)
    #   mtcars %>%
    #     recipe() %>%
    #     update_role(!!!input$dependent, new_role = "outcome") %>%
    #     update_role(!!!input$indep, new_role = "predictor") %>%
    #     prep() %>% 
    #     formula()
    # })
    ###################################################################
    #~~~~~~~~~~~~~~~~~~~~~~~All Output in Sidebar~~~~~~~~~~~~~~~~~~~~~#
    ###################################################################
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~Predicted Value~~~~~~~~~~~~~~~~~~~~~#
    
    pred <- reactive({
      predict(model,myData())
    })
    
    output$Pred <- renderPrint(pred())
    
    #~~~~~~~~~~~~~~~Confidence Interval for Slope~~~~~~~~~~~~~~#
    
    #~~~~~~~~~~~~~~~Confidence/Prediction Interval~~~~~~~~~~~~~~#
    
    
    ###################################################################
    #~~~~~~~~~~~~~~~~~~~~~All Output in Main Panel~~~~~~~~~~~~~~~~~~~~#
    ###################################################################
    
    
    
    #########################~~~~~~Tab 1~~~~~~#########################
    
    #~~~~~~~~~~~~~~~~~~~~~~Descriptive Statistics~~~~~~~~~~~~~~~~~~~~~#
    
    # ---------- Summary Statistics ---------- #
    output$summary <- renderPrint({
      summary(cbind(myData()[input$dv], myData()[input$iv]))
    })
    
    #rbind(summary(women[,1]),summary(women[,2]))
    
    #~~~~~~~~~~~~~~~~~~~~~~Scatter Plot~~~~~~~~~~~~~~~~~~~~~#
    
    # RegressionPlot <- function(
    # data, x_var, y_var, smoothness, 
    # show_reg_line, show_smooth_line, show_residuals){
    #   # -- model regresi
    #   lm_model <- lm(formula = paste(y_var, "~", x_var), data = data)
    #   
    #   # -- ggplot regresi
    #   p <- ggplot(data, aes_string(x = x_var, y = y_var)) + geom_point() + 
    #     theme_minimal()
    #   
    #   # -- ggplot regression line = T
    #   if (show_reg_line) {
    #     p <- p + geom_smooth(method = "lm", se = FALSE, color = "#5959b8")
    #   }
    #   
    #   # -- ggplot smooth line = T
    #   if (show_smooth_line) {
    #     p <- p + geom_smooth(method = "loess", se = FALSE, color = "#92fb51", 
    #                          span = smoothness)
    #   }
    #   
    #   # -- ggplot show residuals = T
    #   if (show_residuals) {
    #     residuals <- residuals(lm_model)
    #     data$residuals <- residuals
    #     p <- p + geom_segment(aes(xend = data[[x_var]], 
    #                               yend = predict(lm_model)), color = "#f7f02b")
    #   }
    #   
    #   ## -- konversi ke plotly
    #   plotly_output <- ggplotly(p)
    #   return(plotly_output)
    # }
    
    # ----- plot
    # output$plot <- renderPlotly({
    #   if(input$data != "Input Mandiri"){
    #     if(input$zsc == TRUE) {
    #       x <- scale(data()$x)
    #       y <- scale(data()$y)
    #       data2 <- data.frame(x=x, y=y)
    #       RegressionPlot(data = data2, x_var = "x", y_var = "y", 
    #                      smoothness = input$slider.smooth, 
    #                      show_reg_line = input$regline, 
    #                      show_smooth_line = input$smt, 
    #                      show_residuals = input$res)
    #     } else {
    #       RegressionPlot(data = data(), x_var = "x", y_var = "y", 
    #                      smoothness = input$slider.smooth, 
    #                      show_reg_line = input$reg, 
    #                      show_smooth_line = input$smt, 
    #                      show_residuals = input$res)
    #     }
    #   } else {
    #     p <- ggplot() + xlim(input$slider.x[1], input$slider.x[2]) + 
    #       ylim(input$slider.y[1], input$slider.y[2])
    #     ggplotly(p, source = "inputPlot")
    #   }
    # })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~Regression Equations~~~~~~~~~~~~~~~~~~~~~#
    
    
    #~~~~~~~~~~~~~~~~~~~~~~Model Summary~~~~~~~~~~~~~~~~~~~~~#
    
    #~~~~~~~~~~~~~~~~~~~~~~ANOVA Table~~~~~~~~~~~~~~~~~~~~~#
    
    
    #########################~~~~~~Tab 2~~~~~~#########################
    
    #~~~~~~~~~~~~~~~~~~~~~~Residual Table~~~~~~~~~~~~~~~~~~~~~#
    
    #~~~~~~~~~~~~~~~~~~~~~~Residual Plot~~~~~~~~~~~~~~~~~~~~~#
    
    #~~~~~~~~~~~~~~~~~~~~~~Histogram/Boxplot of Residuals~~~~~~~~~~~~~~~~~~~~~#
    
    # residuals
    # output$residuals_hist <- renderPlot({
    #   hist(model()$residuals, main = paste(input$dv, '~', input$iv), xlab = 'Residuals') 
    # })
    
    # output$distPlot_dv <- renderPlot({
    #   x    <- datasetInput()[,input$dv]  
    #   bins <- seq(min(x), max(x), length.out = input$bins_dv + 1)
    #   hist(x, breaks = bins, col = 'darkgray', border = 'white', main = 'Dependent Variable', xlab = input$dv)
    # })
    
    
    # output$residuals_scatter <- renderPlot({
    #   plot(model()$residuals ~ datasetInput()[,input$iv], xlab = input$iv, ylab = 'Residuals')
    #   abline(h = 0, lty = 3) 
    # })
    # 
    # output$residuals_qqline <- renderPlot({
    #   qqnorm(model()$residuals)
    #   qqline(model()$residuals)
    # })
    # 
    # output$distPlot_iv <- renderPlot({
    #   x    <- datasetInput()[,input$iv]  
    #   bins <- seq(min(x), max(x), length.out = input$bins_iv + 1)
    #   hist(x, breaks = bins, col = 'darkgray', border = 'white', main = 'Independent Variable', xlab = input$iv)
    # })
    
    # scatter plot 
    # output$scatter <- renderPlot({
    #   plot(datasetInput()[,input$iv], datasetInput()[,input$dv],
    #        xlab = input$iv, ylab = input$dv,  main = "Scatter Plot of Independent and Dependent Variables", pch = 16, 
    #        col = "black", cex = 1) 
    #   
    #   abline(lm(datasetInput()[,input$dv]~datasetInput()[,input$iv]), col="grey", lwd = 2) 
    # })
    
    # correlation matrix
    # output$corr <- renderGvis({
    #   d <- datasetInput()[,sapply(datasetInput(),is.integer)|sapply(datasetInput(),is.numeric)] 
    #   cor <- as.data.frame(round(cor(d), 2))
    #   cor <- cbind(Variables = rownames(cor), cor)
    #   gvisTable(cor) 
    # })
    
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~Tab 1~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~Tab 2~~~~~~~~~~~~~~~~~~~~~~~~~#
    
  }


shinyApp(ui, server)
