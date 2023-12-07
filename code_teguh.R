library(shinythemes)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(shinysky)
library(DT)
# library(dplyr)
library(tidyverse)
library(rhandsontable)
library(data.table)
library(googleVis)
library(plotly)
library(ggpmisc)
###############################################################################
#                               USER INTERFACE                                #  
###############################################################################
ui <- fluidPage(
  navbarPage(
    title = a(tags$b("REGRESI LINIER")),
    #~~~~~~~~~~~~~~~~~~~~~~~~~Tab 1~~~~~~~~~~~~~~~~~~~~~~~~~#
    tabPanel(
      title = "Pemodelan Regresi Linier",
      sidebarLayout(
        #-------------Sidebar Panel inside Tab 1------------#
        sidebarPanel(
          # ----- Data Source Selection ----- #
          fluidRow(
            column(6, selectInput(inputId = "data", label = "Pilih Data",
                                  choices = c("Dataset Contoh", 
                                              "Unggah Dataset", 
                                              "Masukan Mandiri"))
            ),
            # --- [Cond 1] Select: Example Datasets
            column(6, conditionalPanel(
              condition = "input.data == 'Dataset Contoh'",
              selectInput(inputId = 'select_example',
                          label = "Pilihan Dataset",
                          choices = c("Otomotif Mobil",
                                      "Tinggi & Berat Perempuan",
                                      "Karakteristik Batu",
                                      "Temperatur & Tekanan"))
            )),
            # --- [Cond 2] Select: Upload File
            column(6, conditionalPanel(
              condition = "input.data == 'Unggah Dataset'",
              fileInput(inputId = 'chosen_file', 
                        label = 'Choose CSV File',
                        accept = c('text/csv',
                                   'text/comma-separated-values,text/plain',
                                   '.csv'))
            ))
          ),
          # --- [Cond 3] Select: Enter Your Own
          fluidRow(
            column(12, conditionalPanel(
              condition = "input.data == 'Masukan Mandiri'",
              rHandsontableOutput(outputId = "tabelle"),
              actionButton("save",label = "Save Data")
            ))
          ),
          # ------ Display Table ------ #
          fluidRow(
            DT::dataTableOutput("show_tbl", width = "100%")
          ),
          # ------ Set Used Variables ------ #
          fluidRow(
            column(6, uiOutput('iv')), # Set X-Variable
            column(6, uiOutput('dv'))  # Set X-Variable
          ),
          # ------ Show Prediction Results ------ #
          # fluidRow(uiOutput("Pred")), # Ini belum ada inputnya
          # ---------- Plot Options ---------- #
          fluidRow(
            h5("Pilihan Plot"),
            # --- Smooth Trend
            checkboxInput(inputId = "smooth", label = "Tren Pemulusan"),
            # --- [Cond] Selected
            conditionalPanel(
              condition = "input.smooth == TRUE",
              sliderInput(inputId = "slider.smooth", label = "Derajat Pemulusan", min = 0.5, max = 2, value = 0.8)
            ),
            # --- Regression Line
            checkboxInput(inputId = "regline", label = "Garis Regresi"),
            # --- Remove Points            !!!!!!!!!!!!!!! BELUM TAU CARANYA !!!!!!!!!!!!!!!!!
            checkboxInput(inputId = "p3", label = "Klik untuk Menghapus Titik"),
            
            
            # --- Drag Points               !!!!!!!!!!!!!!! BELUM TAU CARANYA !!!!!!!!!!!!!!!!!
            checkboxInput(inputId = "p4", label = "Menggeser Titik"),
            
            
            # --- Variable Shown when Hover   !!!!!!!!!!!!!!! BELUM TAU CARANYA !!!!!!!!!!!!!!!!!
            checkboxInput(inputId = "p5", label = "Pilih Peubah untuk Info Hover"),
            
            
            # --- Title & Subtitle             !!!!!!!!!!!!!!! BELUM TAU CARANYA !!!!!!!!!!!!!!!!!
            checkboxInput(inputId = "p6", label = "Judul & Subjudul"),
            
            
            # --- Smooth Trend           !!!!!!!!!!!!!!! BELUM TAU CARANYA !!!!!!!!!!!!!!!!!
            checkboxInput(inputId = "p7", label = "Label Axis")
          ),
          
          # ---------- Regression Options ---------- #
          fluidRow(
            h5("Pilihan Regresi"),
            # --- Predicted Value
            checkboxInput(inputId = "r1", label = "Hitung Nilai Prediksi"),
            # --- [Cond] Selected
            
            checkboxInput(inputId = "r2", label = "Tampilkan Sisaan pada Plot"),
            # --- [Cond] Selected
            
            checkboxInput(inputId = "r3", label = "Tampilkan Salah Baku dan P-Value"),
            # --- [Cond] Selected
            
            checkboxInput(inputId = "r4", label = "Selang Kepercayaan untuk Slope"),
            # --- [Cond] Selected
            
            checkboxInput(inputId = "r5", label = "Selang Kepercayaan untuk Prediksi"),
            # --- [Cond] Selected
            
            checkboxInput(inputId = "r6", label = "Tabel ANOVA")
            # --- [Cond] Selected
            # conditionalPanel()
          )
        ),
        #--------------Main Panel inside Tab 1--------------# 
        mainPanel(
          # --- Descriptive Statistics --- #
          fluidRow(
            column(12, verbatimTextOutput("summary"))
          ),
          # --- Scatter Plot --- #
          fluidRow(
            plotlyOutput(outputId = "scatter")
          ),
          # --- Linear Regression Equations --- #
          fluidRow(
            column(12, verbatimTextOutput("model"))
          ),
          # --- Model Summary (CORRELATION DULU SEMENTARA) --- #
          fluidRow(
            htmlOutput("corr"),
            HTML('</br> </br>')
          )
        )
      )
    ),
    #~~~~~~~~~~~~~~~~~~~~~~~~~Tab 2~~~~~~~~~~~~~~~~~~~~~~~~~#
    tabPanel(
      title = "Analisis Sisaan dan Nilai Prediksi",
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
  function(input, output, session){
    ###################################################################
    #~~~~~~~~~~~Conditional Datasets & Personalized Outputs~~~~~~~~~~~#
    ###################################################################
    myData <- reactive({
      req(input$data)
      # ---------- Example Dataset ---------- #
      if(input$data == "Dataset Contoh"){
        chosendata <- reactive({
          switch(input$select_example,
                 "Otomotif Mobil" = mtcars,
                 "Tinggi Berat Perempuan" = women, 
                 "Karakteristik Batu" = rock,
                 "Temperatur & Tekanan" = pressure)
        })
        output$scatter <- renderPlotly({
          ggplotly(ggplot(data=myData(), aes(x = myData()[,input$iv], y = myData()[,input$dv]))+ stat_poly_line()+ stat_poly_eq(use_label(c("eq", "R2"))) +
                     geom_point() +labs(title="Diagram Pencar dan Garis Regresi",
                                       x ="Peubah Penjelas", y = "Peubah Respon"))})
        return(as.data.frame(chosendata()))
      }
      # ---------- Upload the Dataset ---------- #
      else if(input$data == "Unggah Dataset"){
        filedata <- reactive({
          inFile <- input$chosen_file
          ext <- tools::file_ext(inFile$datapath)
          req(inFile)
          validate(need(ext =="csv", "Silahkan unggah berkas csv"))
          readData <- read.csv(inFile$datapath, header = TRUE)
          readData
        })
        output$scatter <- renderPlotly({
          ggplotly(ggplot(data=myData(), aes(x = myData()[,input$iv], y = myData()[,input$dv]))+ stat_poly_line()+ stat_poly_eq(use_label(c("eq", "R2"))) +
                     geom_point() +labs(title="Diagram Pencar dan Garis Regresi",
                                        x ="Peubah Penjelas", y = "Peubah Respon"))})
        return(as.data.frame(chosendata()))
      }
      # ---------- Manually Enter Dataset ---------- #
      else if(input$data == "Masukan Mandiri"){
        daten <- data.table(
          X = vector(mode = "character", length = 10), 
          Y = vector(mode = "character", length = 10))
        data.in <- reactiveValues(values = daten)
        output$tabelle <- renderRHandsontable({
          rhandsontable(data.in$values)
        })
        observeEvent(eventExpr = input$tabelle, {
          data.in$values <- hot_to_r(input$tabelle)
          output$scatter <- renderPlot({
            req(input$save)
            if(!is.null(tryCatch(plot(data.in$values), error = function(e){})))
            {plot(data.in$values)}
          })
        })
      }
    })
    
   # output$scatter <- renderPlotly({
   #   ggplotly(RegressionPlot(data=myData(), x_var = myData()[,input$iv], y_var = myData()[,input$dv]))})
   
    
    output$show_tbl = DT::renderDataTable(myData(),
                                          options = list(scrollX = TRUE))
    
    
    ###################################################################
    #~~~~~~~~~~~~~~~~~~~~~~~Global Informations~~~~~~~~~~~~~~~~~~~~~~~#
    ###################################################################
    # ---------- Independent Variable ---------- #
    output$iv <- renderUI({
      if(req(input$data) != "Masukan Mandiri"){
        selectInput(inputId = 'iv', label = h5('Peubah Penjelas'), 
                    choices = names(myData()))
      } else if(req(input$data) == "Masukan Mandiri"){
        textInput(inputId = 'iv',
                  label = h5('Pilih Peubah Penjelas'),
                  value = "X",
                  placeholder = "Enter text...")
        actionButton("subx", "submitx")
      }
    })
    # ---------- Dependent Variable ---------- #
    output$dv <- renderUI({
      if(req(input$data) != "Masukan Mandiri"){
        selectInput(inputId = 'dv', label = h5('Peubah Respon'), 
                    choices = names(myData()))
      } else if(input$data == "Masukan Mandiri"){
        textInput(inputId = 'dv',
                  label = h5('Pilih Peubah Respon'),
                  value = "Y",
                  placeholder = "Enter text...")
        actionButton("suby", "submity")
      }
    })
    
    
    # --- Model Summary
    ###################################################################
    #~~~~~~~~~~~~~~~~~~~~~~~All Output in Sidebar~~~~~~~~~~~~~~~~~~~~~#
    ###################################################################
    #~~~~~~~~~~~~~~~~~~~~~~Predicted Value~~~~~~~~~~~~~~~~~~~~~#
    # pred <- reactive({
    #   predict(model,myData())
    # })
    
    # output$Pred <- renderPrint(pred())
    
    #~~~~~~~~~~~~~~~Confidence Interval for Slope~~~~~~~~~~~~~~#
    
    #~~~~~~~~~~~~~~~Confidence/Prediction Interval~~~~~~~~~~~~~~#
    
    
    ###################################################################
    #~~~~~~~~~~~~~~~~~~~~~All Output in Main Panel~~~~~~~~~~~~~~~~~~~~#
    ###################################################################
    
    
    #########################~~~~~~Tab 1~~~~~~#########################
    #~~~~~~~~~~~~~~~Statistics Descriptive~~~~~~~~~~~~~~#
    
    output$summary <- renderPrint({
      summary(myData())
    })
    
    # output$summary <- renderPrint({
    #   rbind(summary(women[,1]),summary(women[,2]))
    # })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~Interactive Scatterplot~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
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
    
    
    
    #~~~~~~~~~~~~~~~Informations Related to Regression~~~~~~~~~~~~~~#
    # ---------- Regression Formula ---------- #
    regFormula <- reactive({
      as.formula(paste(input$dv, '~', input$iv))
    })
    # ---------- Linear Model ---------- #
    model <- reactive({
      lm(regFormula(), data = myData())
    })
    # ---------- Summary Model ---------- #
    output$model <- renderPrint({
      summary(model())
    })
    
    # ------------ Correlation ---------- #
    output$corr <- renderGvis({
      d <- myData()[,sapply(myData(),is.integer)|sapply(myData(),is.numeric)] 
      cor <- as.data.frame(round(cor(d), 2))
      cor <- cbind(Variables = rownames(cor), cor)
      gvisTable(cor)
    })
    
    
    
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