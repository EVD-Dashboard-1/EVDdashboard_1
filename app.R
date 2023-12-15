library(shinythemes)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(rhandsontable)
library(data.table)
library(googleVis)
library(MASS)
library(plotly)
library(bslib)
library(shinyBS)
###############################################################################
#                               USER INTERFACE                                #  
###############################################################################
ui <- shinyUI(
  fluidPage(
    withMathJax(), 
    includeCSS(path = "www/css/styles.css"),
    # --- Style Browser Scale --- #
    tags$head(
      tags$style("
              body {
    -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
    zoom: 0.95; /* Other non-webkit browsers */
    zoom: 95%; /* Webkit browsers */}
              "),
      tags$style(HTML("
      .table>tbody>tr>td, .table>tbody>tr>th, .table>tfoot>tr>td, .table>tfoot>tr>th, .table>thead>tr>td, .table>thead>tr>th {
        padding: 8px;
        line-height: 1.42857143;
        vertical-align: top;
        border-top: 3px black; 
      }
    ")),
      withMathJax(),
      tags$head(
        tags$style(
          HTML(
            ".MathJax {
            font-size: 2em !important;
          }"
          )
        )
      )
    ),
    navbarPage(
      title = a(tags$b("REGRESI LINIER")),
      #~~~~~~~~~~~~~~~~~~~~~~~~~Tab 1~~~~~~~~~~~~~~~~~~~~~~~~~#
      tabPanel(
        "Mengenal Analisis Regresi",
        tabName = "pengenalan",
        mainPanel(
          img(src = "1.png", width = "120%")
        )
      ),
      #~~~~~~~~~~~~~~~~~~~~~~~~~Tab 2~~~~~~~~~~~~~~~~~~~~~~~~~#
      tabPanel(
        title = "Pemodelan Regresi Linier",
        sidebarLayout(
          #-------------Sidebar Panel inside Tab 1------------#
          sidebarPanel(
            fluidRow(
              column(6, selectInput(inputId = "data", label = "Pilih Data",
                                    choices = c("Dataset Contoh", 
                                                "Unggah Dataset"))
              ),
              # --- [Cond 1] Select: Example Datasets
              column(6, conditionalPanel(
                condition = "input.data == 'Dataset Contoh'",
                selectInput(inputId = 'select_example',
                            label = "Pilihan Dataset",
                            choices = c("Otomotif Mobil",
                                        "Karakteristik Batu",
                                        "Temperatur & Tekanan"))
              )),
              # --- [Cond 2] Select: Upload File
              column(6, conditionalPanel(
                condition = "input.data == 'Unggah Dataset'",
                fileInput(inputId = 'chosen_file', 
                          label = 'Pilih File csv',
                          accept = c('text/csv',
                                     'text/comma-separated-values,text/plain',
                                     '.csv')),
                bsTooltip(id = "chosen_file", 
                          title = "Here is some text with your instructions")
              ))
            ),
            # ------ Display Table ------ #
            fluidRow(
              card(
                card_header(h5(HTML("<b>Tabel Data yang Digunakan</b>"), 
                               style="text-align:center")),
                height = 380,
                style = "resize:vertical;",
                card_body(
                  max_height = 380,
                  tableOutput("show_tbl")
                )
              )
            ),
            # ------ Statistics Descriptive ------ #
            fluidRow(
              h5("Tampilkan Hasil"),
              column(6, 
                     # ------- Statistika Deskriptif
                     checkboxInput(inputId = "summary", label = "Statistik Deskriptif"),
                     # ------- ANOVA Table
                     checkboxInput(inputId = "ro_ano", label = "Tabel ANOVA")),
              column(6,
                     # ------- Scatter Plot
                     checkboxInput(inputId = "scatter", label = "Scatterplot X vs. Y"),
                     # ------- ANOVA Table
                     checkboxInput(inputId = "corr", label = "Korelasi Antar Peubah"))
              
            ),
            # ------ Set Used Variables ------ #
            fluidRow(
              column(6, uiOutput('iv')), # Set X-Variable
              column(6, uiOutput('dv'))  # Set X-Variable
            ),
            # ---------- Plot Options ---------- #
            fluidRow(
              h5("Pengaturan Plot"),
              # --- Smooth Trend
              checkboxInput(inputId = "po_smo", label = "Tren Pemulusan"),
              # --- [Cond] Selected
              conditionalPanel(
                condition = "input.po_smo == true",
                sliderInput(inputId = "smooth", label = h5("Derajat pemulusan"), 
                            min = 0, max = 1, value = 0.5)),
              # --- Regression Line
              checkboxInput(inputId = "po_reg", label = "Garis Regresi"),
              # ------- Show Residuals
              checkboxInput(inputId = "po_res", label = "Tampilkan Sisaan pada Plot"),
              # ------- CI for Plot
              checkboxInput(inputId = "po_ci", label = "Selang Kepercayaan Prediksi"),
              # --- [Cond] Selected
              conditionalPanel(
                condition = "input.ro_ci_plot == true",
                sliderInput(inputId = "ci_plot", label = h5("Tingkat Kepercayaan"), 
                            min = 0.90, max = 1, value = 0.95)
              ),
            )
          ),
          #-------------Main Panel inside Tab 1------------#
          mainPanel(
            # ------ Statistics Summary (Display) ------ #
            fluidRow(
              conditionalPanel(
                condition = "input.summary == true",
                h4("Statistik Deskriptif Seluruh Variabel"),
                verbatimTextOutput("show_sum")
              )
            ),
            # --- Interactive Scatter Plot --- #
            fluidRow(
              conditionalPanel(
                condition = "input.scatter == true",
                h4("Scatter Plot X vs. Y"),
                plotlyOutput(
                  outputId = "plot"
                )
              )
            ),
            # --- Linear Regression Equations --- #
            fluidRow(
              h4("Hasil Regresi dari R"),
              verbatimTextOutput("model")
            ),
            fluidRow(
              h4("Perhitungan Manual Model Regresi"),
              uiOutput("by_hand")
            ),
            fluidRow(
              # --- Korelasi --- #
              conditionalPanel(
                condition = "input.corr == true",
                h4("Korelasi antar Peubah"),
                htmlOutput("corr"),
                HTML('</br> </br>')
              )
            ),
            fluidRow(
              # --- Tabel ANOVA --- #
              conditionalPanel(
                condition = "input.ro_ano == true",
                h4("Tabel ANOVA"),
                htmlOutput("anova_table"),
                HTML('</br> </br>'))
            )
          )
        )
      ),
      #~~~~~~~~~~~~~~~~~~~~~~~~~Tab 3~~~~~~~~~~~~~~~~~~~~~~~~~#
      tabPanel(
        title = "Analisis Sisaan dan Nilai Prediksi",
        sidebarLayout(
          #-------------Sidebar Panel inside Tab 2------------#
          sidebarPanel(
            # ------ Display Table ------ #
            fluidRow(
              column(12, conditionalPanel(
                condition = "input.data == 'Enter Your Own'",
                rHandsontableOutput(outputId = "tabelle"),
                actionButton("save",label = "Save Data")
              ))
            ),
            fluidRow(
            ),
            # ------ Type of Residuals ------ #
            fluidRow(
              radioButtons(inputId = "res_type",
                           label = "Jenis Sisaan",
                           choices = c("Asli", "Hasil Standarisasi"))
            ),
            # ------ Plot Residuals ------ #
            fluidRow(
              radioButtons(inputId = "res_plot",
                           label = "Plot Sisaan",
                           choices = c("vs. Peubah Penjelas",
                                       "vs. Hasil Dugaan"))
            ),
            # ------ Plot Additional Option ------ #
            fluidRow(
              checkboxInput(inputId = "add_hist", label = "Histogram of Residuals"),
              checkboxInput(inputId = "add_box", label = "Boxplot of Residuals"),
              # ------- Predicted Value
              checkboxInput(inputId = "ro_pre", label = "Hitung Nilai Prediksi")
            )
          ),
          #--------------Main Panel inside Tab 2--------------# 
          mainPanel(
            fluidRow(
              h4("Plot Sisaan"),
              plotlyOutput(
                outputId = "residual_plot"
              )
            ),
            fluidRow(
              column(6,
                     conditionalPanel(
                       condition = "input.add_hist == true",
                       h4("Histogram Sisaan"),
                       plotlyOutput("hist_residual")
                     )),
              column(6,
                     conditionalPanel(
                       condition = "input.add_box == true",
                       h4("Boxplot Sisaan"),
                       plotlyOutput("boxplot_residual")
                     ))
            ),
            fluidRow(
              conditionalPanel(
                condition = "input.ro_pre == true",
                h4("Tabel Prediksi"),
                tableOutput("table_pred")
              )
            )
          )
        )
      ),
      #~~~~~~~~~~~~~~~~~~~~~~~~~Tab 3~~~~~~~~~~~~~~~~~~~~~~~~~#
      tabPanel(
        "Informasi Data dan Hasil Analisis",
        tabName = "informasi",
        mainPanel(
          includeMarkdown("www/informasi.md")
        )
      )
    ))
)

###############################################################################
#                                    SERVER                                   #  
###############################################################################
server <- 
  function(input,output, session){
    ###################################################################
    #~~~~~~~~~~~Conditional Datasets & Personalized Outputs~~~~~~~~~~~#
    ###################################################################
    # Reactive value for selected dataset
    chosendata <- reactive({
      switch(input$select_example,
             "Otomotif Mobil" = mtcars,
             "Karakteristik Batu" = rock,
             "Temperatur & Tekanan" = pressure)
    })
    
    filedata <- reactive({
      inFile <- input$chosen_file
      ext <- tools::file_ext(inFile$datapath)
      req(inFile)
      validate(need(ext == "csv", "Silakan upload csv file"))
      readData <- read.csv(inFile$datapath, header = TRUE, sep = c(';', '\t', ','))
      readData
    })
    
    myData <- reactive({
      req(input$data)
      if(input$data == "Dataset Contoh"){
        return(as.data.frame(chosendata()))
      }
      else if(input$data == "Unggah Dataset"){
        return(as.data.frame(filedata()))
      }
    })
    
    output$show_tbl = renderTable(myData(),
                                  options = list(scrollX = TRUE))
    
    ###################################################################
    #~~~~~~~~~~~~~~~~~~~~~~~Global Informations~~~~~~~~~~~~~~~~~~~~~~~#
    ###################################################################
    # ---------- Independent Variable ---------- #
    output$iv <- renderUI({
      selectInput(inputId = 'iv', label = h5('Peubah Penjelas (X)'), 
                  choices = colnames(myData()))
    })
    # ---------- Dependent Variable ---------- #
    observeEvent(c(myData(),input$iv), {
      freezeReactiveValue(input, "dv")
      updateSelectInput(session = session, inputId = "dv", 
                        choices = setdiff(colnames(myData()), input$iv))
    })
    
    output$dv <- renderUI({
      selectInput(inputId = 'dv', label = h5('Peubah Respon (Y)'), 
                  choices = colnames(myData()))
    })
    
    # -----Informations Related to Regression ----- # OKE
    # Observer to update the regression summary whenever variables change
    output$model <- renderPrint({
      lm_model <- lm(as.formula(paste0(input$dv, "~", input$iv)), data = myData())
      lm_model
    })
    
    output$by_hand <- renderUI({
      fit <- lm(as.formula(paste0(input$dv, "~", input$iv)), data = myData())
      withMathJax(
        paste0("\\(\\hat{\\beta}_1 = \\dfrac{\\big(\\sum^n_{i = 1} x_i y_i \\big) - n \\bar{x} \\bar{y}}{\\sum^n_{i = 1} (x_i - \\bar{x})^2} = \\) ", round(fit$coef[[2]], 3)),
        br(),
        paste0("\\(\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x} = \\) ", round(fit$coef[[1]], 3)),
        br(),
        br(),
        paste0("\\( \\Rightarrow y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = \\) ", round(fit$coef[[1]], 3), " + ", round(fit$coef[[2]], 3), "\\( x \\)")
      )
    })
    
    
    # ------------ Correlation ---------- # OKE
    output$corr <- renderGvis({
      d <- myData()[,sapply(myData(),is.integer)|sapply(myData(),is.numeric)] 
      cor <- as.data.frame(round(cor(d), 2))
      cor <- cbind(Variables = rownames(cor), cor)
      gvisTable(cor)
    })
    
    ###################################################################
    #~~~~~~~~~~~~~~~~~~~~~All Output in Main Panel~~~~~~~~~~~~~~~~~~~~#
    ###################################################################
    
    #~~~~~~~~~~~~~~~Statistics Descriptive~~~~~~~~~~~~~~# OKE
    output$show_sum <- renderPrint({
      summary(myData())
    })
    
    #~~~~~~~~~~~~~~~Interactive Scatterplot~~~~~~~~~~~~~~# OKE
    RegressionPlot <- function(
    data, x, y, show_reg_line, show_smooth_line, show_residuals, show_confint){
      # -- model regresi
      lm_model <- lm(formula = as.formula(paste0(y, "~", x)), 
                     data = data)
      
      # -- ggplot scatterplot
      p <- ggplot(data, aes_string(x = x, y = y)) + geom_point() +
        theme_minimal()
      
      # -- ggplot Regression Line = TRUE
      if (show_reg_line) {
        p <- p + geom_smooth(method = "lm", se = FALSE, color = "hotpink")
      }
      
      # -- ggplot Smooth Line == TRUE
      if (show_smooth_line) {
        p <- p + geom_smooth(method = "loess", se = FALSE, color = "skyblue",
                             span = input$smooth)
      }
      # -- ggplot Confidence Interval == TRUE
      if (show_confint) {
        p <- p + geom_smooth(method = "lm", se = TRUE,
                             level = input$ci_plot)
      }
      # -- ggplot show residuals = T
      if (show_residuals) {
        residuals <- residuals(lm_model)
        data$residuals <- residuals
        p <- p + geom_segment(aes(xend = data[,x],
                                  yend = predict(lm_model)), color = "green")
      }
      ## -- konversi ke plotly
      plotly_output <- ggplotly(p)
      return(plotly_output)
    }
    
    # Observer to update the scatterplot whenever variables change
    observeEvent(c(input$dv, input$iv, input$po_reg, input$po_smo,
                   input$smooth, input$ro_ci_plot, input$ci_plot, 
                   input$po_res), {
                     output$plot <- renderPlotly({
                       RegressionPlot(data = myData(),
                                      x = input$iv,
                                      y = input$dv,
                                      show_reg_line = input$po_reg,
                                      show_smooth_line = input$po_smo,
                                      show_residuals = input$po_res,
                                      show_confint = input$po_ci)
                     })
                   })
    
 
    #~~~~~~~~~~~~~~~Anaysis of Variance (ANOVA) Table~~~~~~~~~~~~~~#
    
    output$anova_table <- renderTable({
      # Fit linear regression model
      lm_model <- lm(paste(input$dv, "~", input$iv), data = myData())
      # Extract ANOVA table from the linear model
      anova_table <- anova(lm_model)
      # Return the ANOVA table
      anova_table
    })
    
    #~~~~~~~~~~~~~~~Predicted Value~~~~~~~~~~~~~~#
    
    predicted <- reactive({
      lm_model <- lm(as.formula(paste0(input$dv, "~", input$iv)), data = myData())
      pred <- predict(lm_model, myData())
      data_pred <- data.frame(actual = myData()[,input$dv], predicted = pred)
      data_pred
    })
    
    output$table_pred <- renderTable({
      as.data.frame(predicted())
    })
    
    #~~~~~~~~~~~~~~~Residual Value~~~~~~~~~~~~~~#
    residual <- reactive({
      lm_model <- lm(as.formula(paste0(input$dv, "~", input$iv)), data = myData())
      res <- residuals(lm_model)
      stdres <- studres(lm_model)
      pred <- predict(lm_model, myData())
      
      if (input$res_type == "Asli"){
        y <- res
      } else {
        y <- stdres
      }
      if (input$res_plot == "vs. Peubah Penjelas") {
        x <- myData()[, input$dv]
      } else {
        x <- pred
      }
      data.frame(x = x, y = y)
      
    })
    
    output$residual_plot <- renderPlotly({
      
      if (nrow(residual()) == 0) {
        dat <- data.frame(x = 0, y = 0)
      } else {
        dat <- residual()
      }
      
      ggplotly(createResidualPlot(dat, "x", "y"))
    })
    
    output$hist_residual <- renderPlotly({
      if (nrow(residual()) == 0) {
        res <- data.frame(err = 0)
      } else {
        lm_model <- lm(as.formula(paste0(input$dv, "~", input$iv)), data = myData())
        res <- data.frame(err = residuals(lm_model))
      }
      
      ggplotly(createHistogram(res, "err"))
    })
    
    output$boxplot_residual <- renderPlotly({
      if (nrow(residual()) == 0) {
        res <- data.frame(err = 0)
      } else {
        lm_model <- lm(as.formula(paste0(input$dv, "~", input$iv)), data = myData())
        res <- data.frame(err = residuals(lm_model))
      }
      
      ggplotly(createBoxplot(res, "err"))
    })
    
    output$markdown <- renderUI({
      HTML(markdown::markdownToHTML(knit('informasi', quiet = TRUE)))
    })
    
  }



### Helper

createResidualPlot <- function(data, x_var, y_var) {
  
  p <- ggplot(data, aes_string(x = x_var, y = y_var)) + geom_point(shape = 21, size = 2.5, stroke = 0.5, color = "black", fill = "#7d336df0") + theme_minimal() + geom_hline(yintercept = 0) 
  
  # if (show_smooth_line) {
  # 	p <- p + geom_smooth(method = "loess", se = FALSE, color = "#1ba0c1", span = smoothness) 
  # }
  
  return(p)
  
}

createHistogram <- function(data, var) {
  
  h <- ggplot(data, aes_string(x = var)) + geom_histogram(binwidth = 1, colour = 1, 
                                                          fill = "#f48194") + theme_minimal()
  
  return(h)
}

createBoxplot <- function(data, var) {
  b <- ggplot(data) + geom_boxplot(aes(x = var, y = factor(1))) + theme_minimal()
  
  return(b)
}

###############################################################################
#                                   Run App                                   #  
###############################################################################

shinyApp(ui, server)
