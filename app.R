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
library(bslib)
###############################################################################
#                               USER INTERFACE                                #  
###############################################################################
ui <- shinyUI(
  
  fluidPage(
    # --- Style Browser Scale --- #
    tags$head(
      tags$style("
              body {
    -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
    zoom: 0.8; /* Other non-webkit browsers */
    zoom: 80%; /* Webkit browsers */}
              "),
      tags$style(HTML("
      .table>tbody>tr>td, .table>tbody>tr>th, .table>tfoot>tr>td, .table>tfoot>tr>th, .table>thead>tr>td, .table>thead>tr>th {
        padding: 8px;
        line-height: 1.42857143;
        vertical-align: top;
        border-top: 3px black; 
      }
    "))
    ),
  navbarPage(
    title = a(tags$b("REGRESI LINIER")),
    #~~~~~~~~~~~~~~~~~~~~~~~~~Tab 1~~~~~~~~~~~~~~~~~~~~~~~~~#
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
                        label = 'Choose CSV File',
                        accept = c('text/csv',
                                   'text/comma-separated-values,text/plain',
                                   '.csv'))
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
            checkboxInput(inputId = "summary", label = "Statistik Deskriptif")
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
              sliderInput(inputId = "smooth", label = h5("Derajat pemulusan"), min = 0, max = 1, value = 0.5)),
            # --- Regression Line
            checkboxInput(inputId = "po_reg", label = "Garis Regresi"),
            # ------- Show Residuals
            checkboxInput(inputId = "po_res", label = "Tampilkan Sisaan pada Plot"),
            # ------- CI for Plot
            checkboxInput(inputId = "po_ci", label = "Selang Kepercayaan Prediksi"),
            # --- [Cond] Selected
            conditionalPanel(
              condition = "input.ro_ci_plot == true",
              sliderInput(inputId = "ci_plot", label = h5("Tingkat Kepercayaan"), min = 0.90, max = 1, value = 0.95)
            ),
            ),
          # ---------- Regression Options ---------- #
          fluidRow(
            h5("Pengaturan Regresi"),
            # ------- Predicted Value
            checkboxInput(inputId = "ro_pre", label = "Hitung Nilai Prediksi"),
            # --- [Cond] Selected

            # ------- Show Std. Error & P-Values
            checkboxInput(inputId = "ro_std", label = "Tampilkan Salah Baku dan P-Value"),
            # --- [Cond] Selected
            # ------- CI for Slope
            checkboxInput(inputId = "ro_ci", label = "Selang Kepercayaan Slope"),
            # --- [Cond] Selected
            conditionalPanel(
              condition = "input.ro_cisl == true",
              sliderInput(inputId = "slope", label = "Tingkat Kepercayaan", min = 0.90, max = 1, value = 0.95)
            ),
            # ------- ANOVA Table
            checkboxInput(inputId = "ro_ano", label = "Tabel ANOVA")
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
            h4("Scatter Plot X vs. Y"),
            plotlyOutput(
              outputId = "plot"
            )
          ),
          # --- Linear Regression Equations --- #
          fluidRow(
            h4("Rangkuman Model Regresi"),
            verbatimTextOutput("model")
          ),
          # --- Model Summary (CORRELATION DULU SEMENTARA) --- #
          fluidRow(
            column(6,
                   htmlOutput("corr"),
                   HTML('</br> </br>')),
            column(6,
                   conditionalPanel(
                     condition = "input.ro_ano ==true",
                     h4("Tabel ANOVA"),
                     htmlOutput("anova_table"),
                     HTML('</br> </br>')))
          )
        )
      )
    ),
    #~~~~~~~~~~~~~~~~~~~~~~~~~Tab 2~~~~~~~~~~~~~~~~~~~~~~~~~#
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
            checkboxInput(inputId = "add_box", label = "Boxplot of Residuals")
          )
        ),
        #--------------Main Panel inside Tab 2--------------# 
        mainPanel(
          fluidRow(
            plotlyOutput(
              outputId = "residual_plot"
            )
          ),
          fluidRow(
            column(6,
                   conditionalPanel(
                     condition = "input$add_hist == 'TRUE'",
                     )),
            column(6)
          )
        )
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
    myData <- reactive({
      req(input$data)
      # ---------- Example Dataset ---------- #
      if(input$data == "Dataset Contoh"){
        df <- reactive({
          switch(input$select_example,
                 "Otomotif Mobil" = mtcars,
                 "Karakteristik Batu" = rock,
                 "Temperatur & Tekanan" = pressure)
        })
      }
      # ---------- Upload the Dataset ---------- #
      else if(input$data == "Unggah Dataset"){
        df <- reactive({
          inFile <- input$chosen_file
          ext <- tools::file_ext(inFile$datapath)
          req(inFile)
          validate(need(ext =="csv", "Silahkan unggah berkas csv"))
          readData <- read.csv(inFile$datapath, header = TRUE)
          readData
        })
      }
      return(as.data.frame(df()))
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
        p <- p + geom_smooth(method = "lm", se = FALSE, color = "#5959b8")
      }

      # -- ggplot Smooth Line == TRUE
      if (show_smooth_line) {
        p <- p + geom_smooth(method = "loess", se = FALSE, color = "#92fb51",
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
                                  yend = predict(lm_model)), color = "red")
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
    
    #~~~~~~~~~~~~~~~~~~~~Residual Plot~~~~~~~~~~~~~~~~~#
    
    
    ## INI PLOT RESIDUAL OPSI 1. SEPAHAMKU HARUSNYA BISA SIH, DAN UDAH DI CEK DI SECARA TERPISAH
    # TAPI SETELAH DI CEK MASALAHNYA, DATA FRAME residuals_df KAYAKNYA TIDAK TERBACAA..#
    
    # ResidualPlot <- function(data,x,y){
    #   # -- model regresi
    #   lm_model <- lm(formula = as.formula(paste0(y, "~", x)),
    #                  data = data)
    # 
    #   # Create a data frame with predicted values and residuals
    #   residuals_df <- data.frame(
    #     Predictor = x,
    #     Predicted = predict(lm_model),
    #     Residuals = residuals(lm_model)
    #   )
    #   q <- ggplot(residuals_df, aes_string(x = Predictor, y = Residuals))
    #   + geom_point() + theme_minimal()
    # 
    # 
    # # -- Raw & vs. Predictor
    # if ("input$res_type == 'Asli'" && "input$res_plot == 'vs. Peubah Penjelas'"){
    #   q <- ggplot(residuals_df, aes_string(x = Predictor, y = Residuals))
    #   + geom_point() + theme_minimal()
    # }
    # 
    # # -- Standardized & vs. Predictor
    # if ("input$res_type == 'Hasil Standarisasi'" && "input$res_plot == 'vs. Peubah Penjelas'") {
    #   q <- ggplot(residuals_df, aes_string(x = scale(Predictor), y = scale(Residuals)))
    #   + geom_point() + theme_minimal()
    # }
    # 
    # # -- Raw & vs. Predicted
    # if ("input$res_type == 'Asli'" && "input$res_plot == 'vs. Hasil Dugaan'") {
    #   q <- ggplot(residuals_df, aes_string(x = Predicted, y = Residuals))
    #   + geom_point() + theme_minimal()
    # }
    # # -- Standardized & vs. Predicted
    # if ("input$res_type == 'Hasil Standarisasi'" && "input$res_plot == 'vs. Peubah Penjelas'") {
    #   q <- ggplot(residuals_df, aes_string(x = scale(Predicted), y = scale(Residuals)))
    #   + geom_point() + theme_minimal()
    # }
    # # -- konversi ke plotly
    #   plot_residu <- ggplotly(q)
    #   return(plot_residu)
    # }
    # 
    # observeEvent(c(input$dv, input$iv), {
    #                  output$res_plot <- renderPlotly({
    #                    ResidualPlot(data = myData(),
    #                                   x = input$iv,
    #                                   y = input$dv)
    #                  })
    #                })
    
    
    
    # ## KALAU DI BAWAH INI LEBIH LENGKAP. KALAU MASALAH KETERKAITAN DATA SUDAH BERES,
    # HARUSNYA INI BISA DIPAKAI. KALAU JALANIN YANG INI NANTI RESIDUAL PLOT, HISTOGRAM
    # DAN BOXPLOT BISA JALAN SEMUA#
    
    # Observer to update the residual plot whenever variables change
    observeEvent(c(input$dv, input$iv), {
      output$residual_plot <- renderPlot({
        # Fit linear regression model
        lm_model <- lm(paste(input$dv, "~", input$iv), data = myData())
        
        # Create a data frame with predicted values and residuals
        residuals_df <- data.frame(
          Predicted = predict(lm_model),
          Residuals = residuals(lm_model)
        )
        # Plot residuals
        ggplot(residuals_df, aes(x = Predicted, y = Residuals)) +
          geom_point() +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
          labs(title = "Residual Plot")
      })
      output$res_hist <- renderPlot({
        residuals <- residuals(lm_model())
        ggplot() +
          geom_histogram(aes(x = residuals), bins = 20, fill = "skyblue", color = "black") +
          labs(title = "Histogram of Residuals")
      })
      output$boxplot <- renderPlot({
        residuals <- residuals(lm_model())
        ggplot() +
          geom_boxplot(aes(y = residuals), fill = "lightgreen", color = "black") +
          labs(title = "Boxplot of Residuals")
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
    # pred <- reactive({
    #   predict(model,myData())
    # })
    
    # output$Pred <- renderPrint(pred())
        
}

###############################################################################
#                                   Run App                                   #  
###############################################################################

shinyApp(ui, server)
