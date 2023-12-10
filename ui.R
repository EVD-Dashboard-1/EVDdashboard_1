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
source("helper server.R")

#=========================Dataset Contoh=========================#
# Define df1
tabel01 <- data(women)
variables=c("mpg","cyl","disp","hp","drat","wt","qsec","vs")
#============================Interface===========================#
# Define UI for application that draws a histogram
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
      "Mengenal Analisis Regresi",
      tabName = "pengenalan",
      mainPanel(
        img(src = "1.png")
      )
    ),
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
            checkboxInput(inputId = "add_box", label = "Boxplot of Residuals"),
            # ------- Predicted Value
            checkboxInput(inputId = "ro_pre", label = "Hitung Nilai Prediksi")
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
                     condition = "input.add_hist == true",
                     plotlyOutput("hist_residual")
                   )),
            column(6,
                   conditionalPanel(
                     condition = "input.add_box == true",
                     plotlyOutput("boxplot_residual")
                   ))
          ),
          fluidRow(
            conditionalPanel(
              condition = "input.ro_pre == true",
              tableOutput("table_pred")
            )
          )
        )
      )
    ),
    tabPanel(
      "Informasi Data dan Hasil Analisis",
      tabName = "informasi",
      mainPanel(
        includeMarkdown("www/informasi.rmd")
      )
    ),
  )
  )
)