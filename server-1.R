#==========================Back - End===========================#
# Define server logic required to draw a histogram
server <- 
  function(input,output, session){
    ###################################################################
    #~~~~~~~~~~~Conditional Datasets & Personalized Outputs~~~~~~~~~~~#
    ###################################################################
    # Reactive value for selected dataset
    selected_df <- reactive({
      switch(input$select_example,
             "Otomotif Mobil" = mtcars,
             "Karakteristik Batu" = rock,
             "Temperatur & Tekanan" = pressure)
    })
    
    uploaded_df <- reactive({
      inFile <- input$chosen_file
      ext <- tools::file_ext(inFile$datapath)
      req(inFile)
      validate(need(ext =="csv", "Silahkan unggah berkas csv"))
      readData <- read.csv(inFile$datapath, header = TRUE)
      readData
    })
    
    myData <- reactive({
      req(input$data)
      # ---------- Example Dataset ---------- #
      if(input$data == "Dataset Contoh"){
        return(as.data.frame(selected_df()))
      }
      # ---------- Upload the Dataset ---------- #
      else if(input$data == "Unggah Dataset"){
        return(as.data.frame(uploaded_df()))  
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
    
    #   # Create a data frame with predicted values and residuals
    #   residuals_df <- data.frame(
    #     Predictor = x,
    #     Predicted = predict(lm_model),
    #     Residuals = residuals(lm_model)
    #   )
    #   q <- ggplot(residuals_df, aes_string(x = Predictor, y = Residuals))
    #   + geom_point() + theme_minimal()
    
    
    # # -- Raw & vs. Predictor
    # if ("input$res_type == 'Asli'" && "input$res_plot == 'vs. Peubah Penjelas'"){
    #   q <- ggplot(residuals_df, aes_string(x = Predictor, y = Residuals))
    #   + geom_point() + theme_minimal()
    # }
    
    # # -- Standardized & vs. Predictor
    # if ("input$res_type == 'Hasil Standarisasi'" && "input$res_plot == 'vs. Peubah Penjelas'") {
    #   q <- ggplot(residuals_df, aes_string(x = scale(Predictor), y = scale(Residuals)))
    #   + geom_point() + theme_minimal()
    # }
    
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
    # observeEvent(c(input$dv, input$iv), {
    #   output$residual_plot <- renderPlot({
    #     # Fit linear regression model
    #     lm_model <- lm(paste(input$dv, "~", input$iv), data = myData())
    
    #     # Create a data frame with predicted values and residuals
    #     residuals_df <- data.frame(
    #       Predicted = predict(lm_model),
    #       Residuals = residuals(lm_model)
    #     )
    #     # Plot residuals
    #     ggplot(residuals_df, aes(x = Predicted, y = Residuals)) +
    #       geom_point() +
    #       geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    #       labs(title = "Residual Plot")
    #   })
    #   output$res_hist <- renderPlot({
    #     residuals <- residuals(lm_model())
    #     ggplot() +
    #       geom_histogram(aes(x = residuals), bins = 20, fill = "skyblue", color = "black") +
    #       labs(title = "Histogram of Residuals")
    #   })
    #   output$boxplot <- renderPlot({
    #     residuals <- residuals(lm_model())
    #     ggplot() +
    #       geom_boxplot(aes(y = residuals), fill = "lightgreen", color = "black") +
    #       labs(title = "Boxplot of Residuals")
    #   })
    # })
    
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
    
  }

### Helper

#createResidualPlot <- function(data, x_var, y_var) {
  
#  p <- ggplot(data, aes_string(x = x_var, y = y_var)) + geom_point(shape = 21, size = 2.5, stroke = 0.5, color = "black", fill = "#7d336df0") + theme_minimal() + geom_hline(yintercept = 0) 
  
  # if (show_smooth_line) {
  # 	p <- p + geom_smooth(method = "loess", se = FALSE, color = "#1ba0c1", span = smoothness) 
  # }
  
#  return(p)
  
#}

#createHistogram <- function(data, var) {
  
#  h <- ggplot(data, aes_string(x = var)) + geom_histogram(binwidth = 2, colour = 1, fill = "#f48194") + theme_minimal()
  
#  return(h)
#

#createBoxplot <- function(data, var) {
#  b <- ggplot(data) + geom_boxplot(aes(x = var, y = factor(1))) + theme_minimal()
  
#  return(b)
#}