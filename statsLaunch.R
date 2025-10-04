library(tidyverse)
library(R6)
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(gridExtra)
library(ggpubr)
library(writexl)
library(magick)
library(readr)


setwd(here::here())


StatsObj <- R6Class("StatsObj",
                      public = list(
                        JobName = NULL,
                        groupUnique = 2,
                        groupTotal = 2,
                        groupNames = list(),
                        PathToFile = NULL,
                        InputType = "Default",
                        InputDF = data.frame(),
                        DFList = list(),
                        
                        initialize = function(JobName, PathToFile, InputType = NULL) {
                          
                          self$JobName = JobName
                          self$PathToFile = PathToFile 
                          if(!is.null(InputType)) {self$InputType <- InputType}
                        },
                        
                        ReadFile = function(SheetName = NULL) {
                          if (!is.null(SheetName) & str_ends(paste(self$PathToFile), ".xlsm|.xlsx")) {
                            self$InputDF = read_excel(path = paste(self$PathToFile), sheet = paste(SheetName), col_names = T) 
                            #self$DFList[[DFListIndex]] = self$InputDF
                            #return(self$InputDF)
                          } else if (str_ends(paste(self$PathToFile), ".csv")) {
                            self$InputDF = read_csv(paste(self$PathToFile), col_names = T)
                            #self$DFList[[DFListIndex]] = self$InputDF
                            #return(self$InputDF)
                          } else print("invalid input type")
                          
                          BuffDF = self$InputDF %>%
                            column_to_rownames(var = paste(names(self$InputDF[1]))) %>% 
                            t() %>% as.data.frame
                          view(BuffDF)
                          
                          self$groupUnique = length(as.list(unique(BuffDF[, 1])))
                          self$groupTotal = length(as.list(BuffDF[, 1]))
                          self$groupNames = as.list(unique(BuffDF[, 1]))
                        },
                        
                        CutSheet = function (StudyGroups, repetition_number) {
                          #conditions_total = repetition_number * length(StudyGroups); print("conditions_total"); print(conditions_total)
                          
                          # for (i in 1:self$groupUnique) {
                          #   
                          #   print("cutsheet rotation")
                          #   
                          #   BufferDF1 <- self$InputDF %>%
                          #     dplyr::select(c(1, (2 + ((i-1) * conditions_total)):((conditions_total +1) + ((i-1) * conditions_total))))  
                          #   
                          #   self$DFList[[i]] = BufferDF1
                          #   print("cutsheet rotation1 ")
                          # }
                          
                          self$DFList[[1]] = self$InputDF
                        }
                      )
)



replace_string_nan <- function(mat1) {
  mat1 <- as.matrix(mat1)
  mat1[mat1 == "NaN"] <- NA 
  mat_num <- matrix(as.numeric(mat1), nrow = nrow(mat1), ncol = ncol(mat1))
  non_na_vals <- mat_num[!is.na(mat_num)]
  if (length(non_na_vals) == 0) {
    stop("No non-NA values in matrix.")
  }
  
  min_val <- abs(min(non_na_vals))
  replacement <- min_val / 2
  mat_num[is.na(mat_num)] <- replacement
  return(mat_num)
}

replace_zeros <- function(mat) {
  non_zero_vals <- mat[mat != 0]
  
  if (length(non_zero_vals) == 0) {
    stop("Matrix has no non-zero values to compute a replacement.")
  }
  
  min_val <- min(non_zero_vals)
  replacement <- min_val / 2
  mat[mat == 0] <- replacement
  return(mat)
}

remove_high_rsd <- function(mat, percent = 10) {
  
  mat <- as.matrix(mat)
  if (!is.numeric(mat)) stop("Matrix must be numeric.")
  #rsd for each column
  rsd <- apply(mat, 2, function(x) {
    if (mean(x, na.rm = TRUE) == 0) return(Inf)
    sd(x, na.rm = TRUE) / abs(mean(x, na.rm = TRUE))
  })
  #cols to remove
  n_remove <- ceiling(length(rsd) * percent / 100)
  
  #smallest RSD value in top 10%
  cutoff <- sort(rsd, decreasing = TRUE)[n_remove]
  
  #keep columns with RSD ≤ cut-off
  keep_cols <- rsd <= cutoff
  filtered_mat <- mat[, keep_cols, drop = FALSE]
  list(
    filtered_matrix = filtered_mat,
    rsd_cutoff = cutoff,
    removed_columns = which(!keep_cols),
    rsd_values = rsd
  )
  return(filtered_mat)
}

ReadDF = function (Current_Job, index) {
  RawRead = Current_Job$DFList[[index]] #reads first row as cols
  RawNumMatrix = RawRead %>%  #transform read into only numeric value matrix for plotting
    slice(-c(1,2)) %>%
    dplyr::select(-1) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  mat = RawNumMatrix
  transponedmat = t(mat) 
  nohighrsd = remove_high_rsd(transponedmat)
  return(nohighrsd)
}

MatrixNormalization = function(RawNumMatrix, normalization_criteria, PCA_log_base) {
  
  source("analysis_utils.R", local = T)
  
  NormalizedMatrix = RawNumMatrix
  
  if (normalization_criteria[[1]] == T) { #norm by sum 
    
    NormalizedMatrix = apply(NormalizedMatrix, 1, SumNormal) #norm by sum
    
  } 
  
  if (normalization_criteria[[2]] == T) { #log transform
    
    min.val = min(abs(NormalizedMatrix[NormalizedMatrix!=0]))/10 #min value in sheet divided by 10
    NormalizedMatrix = apply(NormalizedMatrix, 2, LogTransform, min.val, PCA_log_base) #log transform base at input
    
    if(identical(normalization_criteria, c(F,T,F))) {
      
      return(NormalizedMatrix)
    }
    
  } 
  
  if (normalization_criteria[[3]] == T) { #autoscaling
    
    NormalizedMatrix = t(apply(NormalizedMatrix, 1, AutoScale)) #auto scaling applied to transposed matrix
    NormalizedMatrix = replace_string_nan(NormalizedMatrix)
    
    if(identical(normalization_criteria, c(F,T,T))) {
      
      return(NormalizedMatrix)
    }
    
  } 
  
  if(is.null(normalization_criteria) || identical(normalization_criteria, c(F,F,F)) || identical(normalization_criteria, c(F,F,T))) {
    
    return(NormalizedMatrix)
  }
  
  
  return(t(NormalizedMatrix))
}

FindsGridSize <- function(ListLength) {
  #closest biggest whole root of length of list
  #GridSizeBuff <- ceiling(sqrt(ListLength))
  GridSizeBuff = 1
  return(GridSizeBuff)
}



PerformPCA = function (Current_Job, repetition_number, normalization_criteria, log_base, titles) {
  
  source("Plotting_utils.R", local = T)
  
  Current_Job$ReadFile()
  StudyGroups = Current_Job$groupNames; #print("Current_Job$groupNames"); print(Current_Job$groupNames); print(length(Current_Job$groupNames))
  print("err1")
  Groups_for_scores = rep(StudyGroups, each = repetition_number); #print("Groups_for_scores: "); print(Groups_for_scores)
  
  print("err2")
  Current_Job$CutSheet(StudyGroups, repetition_number)
  #number of compartments
  groupNumVec = 1:length(Current_Job$DFList); #print(paste("group num vec: ", groupNumVec))
  
  plotlist = list()
  ScorePlotList = list()
  ScreePlotList = list()
  LoadingsPlotList = list()
  
  num_colors <- 130
  ColorsList_130 <- rgb(
    red = runif(num_colors, min = 0, max = 0.8),
    green = runif(num_colors, min = 0, max = 0.8),
    blue = runif(num_colors, min = 0, max = 0.8)
  )
  ColorSet = sample(ColorsList_130, length(StudyGroups), replace = FALSE)
  names(ColorSet) = paste(StudyGroups)
  print(ColorSet); print(class(ColorSet))
  
  GridNames = c("ScoreGrid.pdf", "ScreeGrid.pdf", "LoadingsGrid.pdf")
  
  names <- Current_Job$groupNames; print("names from object: "); #print(names);
  
  for (i in groupNumVec) {
    
    print(paste("main for loop rotation index: ", i))
    
    RawNumMatrixBuff = ReadDF(Current_Job, index = i)
    NormalizedMatrixBuff = MatrixNormalization(RawNumMatrixBuff, normalization_criteria, log_base)
    pcaBuff <- prcomp(NormalizedMatrixBuff, center = TRUE, scale. = FALSE)
    scoresBuff <- as.data.frame(pcaBuff$x)
    scoresBuff$Groups <- Groups_for_scores
    ScorePlotList[[i]] = DrawScorePlotBuff(scores = scoresBuff, ColorSet = ColorSet, title = paste(titles[i]), pcaBuff = pcaBuff)
    print("score plot done")
    ScreePlotList[[i]] = DrawScreePlotBuff(pcaBuff = pcaBuff, title = paste(titles[i]))
    print("scree plot done")
    LoadingsPlotList[[i]] = DrawLoadingsPlotBuff(pcaBuff = pcaBuff, title = paste(titles[i]))
    print("load plot done")
    
    #when for loop has gone through all organs, call Create Grid function
    if (i == tail(groupNumVec, n=1)) {
      PlotList_list = list(ScorePlotList, ScreePlotList, LoadingsPlotList)
      GridList1 = list()
      for (l in 1:3) {
        print("guhhherr45")
        #print(PlotList_list[[l]][[i]])
        #GridList1[[l]] = CreatePlotGrid(gridtitle = paste(GridNames[l]), PlotList_list = PlotList_list[[l]]) #this saves the grids
        pdf(paste(GridNames[l]), width = 22, height = 15)
        print(PlotList_list[[l]][[i]])
        dev.off()
        print("saved the grids successfully")
      }
    }
  }
  PlotList_list = list(ScorePlotList, ScreePlotList, LoadingsPlotList)
  return(PlotList_list)
}

placeholder_PlotLists = list(
  plot_list1 <- lapply(1:7, function(i) {
    ggplot(mtcars, aes(x = mpg, y = wt)) +
      geom_point(color = sample(colors(), 1)) +
      ggtitle(paste("Plot", i)) +
      theme_minimal()
  }),
  plot_list2 <- lapply(1:7, function(i) {
    ggplot(mtcars, aes(x = mpg, y = wt)) +
      geom_point(color = sample(colors(), 1)) +
      ggtitle(paste("Plot", i)) +
      theme_minimal()
  }),
  plot_list3 <- lapply(1:7, function(i) {
    ggplot(mtcars, aes(x = mpg, y = wt)) +
      geom_point(color = sample(colors(), 1)) +
      ggtitle(paste("Plot", i)) +
      theme_minimal()
  })
)




ui <- fluidPage(
  useShinyjs(),
  tags$style(HTML("
    
    body {
      background-color: #f0f4f8; /*soft blue-gray */
      color: #333;
    }

    .container-fluid {
      padding: 20px;
    }

    .btn {
      background-color: #1e88e5; /*vibrant blue*/
      color: white;
      border: none;
      border-radius: 8px;
      font-weight: bold;
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      transition: all 0.2s ease;
    }
    
    .btn:hover {
      background-color: #1565c0; /*darker blue on hover*/
      transform: translateY(-2px);
      box-shadow: 0 6px 8px rgba(0, 0, 0, 0.15);
    }
    
    .dataTables_wrapper .dt-buttons {
      float: right;
    }
    
    .dataTable th {
      background-color: #004d40 !important; /*dark teal header*/
      color: white !important;
      font-weight: bold;
      border-bottom: 2px solid #00251a;
    }
    
    .dataTable tr:nth-child(even) {
      background-color: #e8f5e9; /*light green even rows*/
    }
    
    .dataTable tr:nth-child(odd) {
      background-color: #ffffff; /*white odd rows*/
    }
    
    .dataTables_wrapper {
        border-radius: 8px;
        overflow: hidden;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
    }")),
  titlePanel(
    div("ez PCA", 
        h4("early dev version", style = "color: grey;"))
  ),
  fileInput(
    inputId = "filesTest",
    label = "select a .csv file according to the format",
    multiple = TRUE,
    placeholder = ".csv file",
    accept = ".csv"
  ),
  actionButton("stats_startPCA", "Start PCA"),
  textOutput("datapathstest"),
  numericInput("repetition_num", value = 4, "number of repetitions of each group:"),
  checkboxGroupInput("normalization_criteria", label = h3("select normalization criteria: "), 
                     choices = list("Normalization by Sum" = "sum", "Log Transformation" = "log",
                                    "Autoscaling" = "scale"), 
                     selected = 1),
  radioButtons(inputId = "log_base_input",
               label = "log base for log formation (2 or 10)", 
               choices = list( 
                 "log2" = 2, 
                 "log10" = 10 
               )),
  actionButton("prev_grid", "Previous Plot Grid"),
  actionButton("next_grid", "Next Plot Grid"),
  plotOutput(outputId = "PCA_grids", width = 900, height = 900)
)


server = function(input, output, session){
  
  plot_index_PCA = reactiveVal(1)
  PCA_grids = reactiveVal(placeholder_PlotLists)
  
  observeEvent(input$stats_startPCA, {
    req(input$filesTest)
    filepaths <- input$filesTest$datapath
    
    output$datapathstest = renderText({
      paste("plot title: ",filepaths)
      })
    
    Current_Job = StatsObj$new(JobName = "guragamesh", PathToFile = input$filesTest$datapath)
    
    normalization_criteria_vec =c (
      "sum"  %in% input$normalization_criteria,
      "log"  %in% input$normalization_criteria,
      "scale" %in% input$normalization_criteria
    )
    print("normalization_criteria_vec"); print(normalization_criteria_vec)
    
    PCA_plots_output = PerformPCA(Current_Job = Current_Job,
               repetition_number = input$repetition_num,
               normalization_criteria = normalization_criteria_vec,
               log_base = input$log_base_input, 
               titles = filepaths)
    
    output$PCA_grids = renderPlot({

      current_grids = PCA_plots_output
      GridSize = FindsGridSize(length(current_grids[[plot_index_PCA()]])) #; print("plotlist length: ") print(length(PlotList_list))

      print(
        grid.arrange(grobs = current_grids[[plot_index_PCA()]][1:length(current_grids[[plot_index_PCA()]])],
                     ncol = GridSize,
                     nrow = GridSize))

    })

  })
  
  observeEvent(input$prev_grid, {
    current_index_PCA <- plot_index_PCA()
    if (current_index_PCA > 1) {
      plot_index_PCA(current_index_PCA - 1)
    }
  })
  
  observeEvent(input$next_grid, {
    current_index_PCA <- plot_index_PCA()
    if (current_index_PCA < length(PCA_grids())) {
      plot_index_PCA(current_index_PCA + 1)
    }
  })
  
}

shinyApp(ui, server)
