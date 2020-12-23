library(shiny)
library(e1071)
library(dplyr)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("A long-non-coding RNA based classifier to predict early risk of HCC recurrence"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv", "tsv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote,
                       row.names = 1)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    # if(input$disp == "head") {
    #   return(head(df))
    # }
    # else {
    #   return(df)
    # }

    if(TRUE){
      library(e1071)
      input_choose <- c(1:9)
      fix <- 9
      zhibiao_order <- input_choose
      
      svm_kernel <-"linear" 
      svm_type <- "C-classification"
      wts <- c(good = 1, bad = 2) 
      
      ## data loading -------------------------------------------------------------------------------------------------------------------------------
      all_data <- df
      data_svm <-all_data[]
      
      data_svm[, 10]<- factor(data_svm[, 10],levels = c(0,1), labels = c("good","bad"))
      
      train_svm <- data_svm[1:160,]
      label_train <- train_svm[1:160, 10]
      ### testing set------------------------------------------------------------------------------------------------------------------------------- 
      test_svm <- data_svm[162:dim(data_svm)[1], ]
      test_svm <- ((data_svm[162:dim(data_svm)[1], ] > 
                     matrix(nrow = length(162:dim(data_svm)[1]),
                            data = rep(x = c(data_svm[161, ] %>% unlist), times = length(162:dim(data_svm)[1])),
                            byrow = T)) + 0) %>% as.data.frame()
      
      
      func_muiti <- function(i) {
        
        svm_model <- svm(train_svm[, 1:9],label_train,type=svm_type,kernel=svm_kernel,class.weights=wts)
        predtemp1 <- predict(svm_model, test_svm[i, 1:9])
        
        res <- c(predtemp1)
        return(res)
      }
      
      results <- lapply(1:length(162:dim(data_svm)[1]), func_muiti)
      summary_performance <- do.call("rbind", results) %>% as.data.frame()
      summary_performance[summary_performance[, 1] == 2,] <- "High risk"
      summary_performance[summary_performance[, 1] == 1,] <- "Low risk"
      row.names(summary_performance) <- row.names(test_svm)
      colnames(summary_performance) <- "Predict result"
      summary_performance$patient <- row.names(summary_performance)
      
      
      return(summary_performance)
  }
  })
  
  
}

# Create Shiny app ----
shinyApp(ui, server)

