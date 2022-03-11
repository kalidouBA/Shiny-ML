
library(shiny)
library(DBI)
library(dplyr)
library(openssl)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(sodium)
library(digest)
library(ggpubr)
library(tidyverse)
library(ggcorrplot)
library(shinyBS)


ui <- shinyUI(
  fluidPage(
  uiOutput("App_Panel")
  )
)

# shinydashboard UI
ui_db = fluidPage(
  setBackgroundImage(
    src = "home.jpg"
  ),
  dashboardPage(
   
    skin = "blue",
    
    dashboardHeader(title = "Machine Learning Labo"),
    
    dashboardSidebar(
      sidebarMenu(
        sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                          label = "Search..."),
        menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
        menuItem("Data Preparation", tabName = "datapreparation", icon = icon("wrench")),
        menuItem("Visualize", tabName = "Visualize", icon = icon("table")),
        menuItem("Analysis", tabName = "analysis", icon = icon("cogs"),
                 menuSubItem("Train & Validation",icon = icon("cog"), tabName = "trainvalidation"),
                 menuSubItem("Features",icon = icon("cog"), tabName = "features")
        ),
        menuItem("Algorithms", tabName = "analysis", icon = icon("cogs"),
                 menuSubItem("Algorithm for classification",icon = icon("cog"), tabName = "classification"),
                 menuSubItem("Algorithm for continuous",icon = icon("cog"), tabName = "continuous")
        ),
        menuItem("Results", tabName = "results", icon = icon("dashboard")),
        fluidRow(actionLink(inputId = "logout", "Logout",icon = icon("fa fa-sign-out")),
          hr()
        )
      )
      
    ),
    dashboardBody(
      background = "black",
      tabItems(
        # First tab content
        tabItem(tabName = "summary",
                fluidRow(
                  box(background = "black",title = "How To Use", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE, width = 8,
                      
                      h4("Step 1: Upload Dataset"),
                      h5("Ideally any csv or xlsx file is useable.  
                         It is recommended to perform cleaning and munging methods prior 
                         to the upload though. We intend to apply data munging/cleaning methods 
                         in this app in the near future."),
                      h4("Step 2: Analyze Data"),
                      h5("Current version allows the user to perform basic missing analysis."),
                      h4("Step 3: Choose Pre-processing Methods"),
                      h5("Basic K-Cross Validation Methods are applicable. "),
                      h4("Step 4: Choose Model"),
                      h5("*** Choose from a selection of machine learning models to run.  \n
                          *** Selected parameters for each corresponding model are available to tune and manipulate."),
                      h4("Step 5: Run Application"),
                      h5("Once the model(s) have been executed, the results and plots for each model 
                         can be viewed in the results tab for analysis."))),
                
                fluidRow(
                  box(background = "black",title = "Libraries/Dependencies",status = "primary", solidHeader = TRUE,
                      collapsible = TRUE, width = 8,
                      h4("- The caret package was used for the backend machine learning algorithms."),
                      h4("- Shiny Dashboard was used for the front end development."),
                      h4("- All models are difined in model.R file"),
                      h4("- The available models are:"),
                      h5("- No Model(NM)"),
                      h5("- Logistique Rgression (LR)"),
                      h5("- Naif Bayes (NB)"),
                      h5("- Decision Tree (DT)"),
                      h5("- k-nearest neighbors algorithm (KNN)"),
                      h5("- Support Vector Machine (SVM)"),
                      h5("- Random Forest (RF)"),
                      h5("- Gradient Boosting Machine (GBM)"),
                      h5("***  All models are run using Youden's optimization method to find the optimal classification threshold."),
                      h4("The default execution of the templates is done:"),
                      h5("***  Cross validation method with 10 learning/test blocks."),
                      h5("***  Selection of the optimal threshold (in terms of precision) of classification with the Youden method."),
                      h5("***  No test data on a population size smaller than 150.")
                      
                      ))),
        
        
        
        ######################################
        # Data Preparation Tab Contents
        ######################################
        
        # Second tab content
        tabItem(tabName = "datapreparation",
                            
                             tags$hr(),
                        sidebarLayout(
                             sidebarPanel(
                               fileInput('file1', 'Choose CSV File',
                                         accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                               
                               column(4,
                                      checkboxInput("header", "Header", TRUE),
                               ),
                               column(4,
                                      radioButtons("sep", "Separator",
                                                   choices = c(Comma = ",",
                                                               Semicolon = ";",
                                                               Tab = "\t"),
                                                   selected = ","),
                               ),
                               column(4,
                                      radioButtons("dec", "decimal",
                                                   choices = c(Comma = ",",
                                                               point = "."),
                                                   selected = ","),
                               ),
                               column(4,
                                      
                                      radioButtons("quote", "Quote",
                                                   choices = c(
                                                     "Double Quote" = '"'),
                                                   selected = '"'),
                               ),
                               hr(),
                               selectInput("inSelect",h6("Choose variable label:",style = "color:green"), choices=c("---")),
                               uiOutput("choose_label"),
                               radioButtons(inputId = "inCheckPositiveLabel", label = "Check the positive label",
                                                  choices = c("---")),
                               
                               hr(),
                               pickerInput(
                                 inputId = "choose_var_delete",
                                 label = h6("Select variables not interest in models:",style = "color:green"),
                                 choices = c("---"),
                                 multiple = TRUE
                               ),
                               verbatimTextOutput(outputId = "res_choose_var_delete"),
                               
                                 pickerInput(
                                 inputId = "choose_var_factor",
                                 label = h6("Select all factor variables:",style = "color:green"),
                                 choices = c("---"),
                                 multiple = TRUE
                               ),
                               actionButton("goButton", "Convert", class = "btn-success"),
                               
                               ),
                             tabPanel("preprocessing", br(),
                                      sidebarLayout(
                                        sidebarPanel(
                                          radioButtons(
                                            inputId = "PreData",
                                            label = "Choose the type of pre-processing for missing values",
                                            choices = c("View NA's" = 'view',
                                                        "Delete observations with missing values" = 'delete', 
                                                        "Imputation with mean" = 'mean', 
                                                        "Imputation with median" = 'median', 
                                                        "kNN Imputation" = 'kNN',
                                                        "Multivariate Imputation by Chained Equations" = 'multiImpu')), br(),
                                          actionButton("run", "Run"),),
                                        
                                        mainPanel( plotOutput("plotVarManq"),
                                                   verbatimTextOutput("number_NA"))
                                      ))
                             
                               
                             ),
        ),
        
        ######################################
        # Modeling Tab Contents
        ######################################
        
        
        
        ##################################################################################
        ####   Training/Splitting Tab Set Contents
        ##################################################################################
        
        tabItem(tabName = "Visualize",
                mainPanel(
                  
                  tabsetPanel(type = "tabs",
                              
                              
                              tabPanel("CorPlot", plotOutput("plotcorr")),
                              tabPanel("Summary", verbatimTextOutput("summary")),
                              tabPanel("plot relation", selectInput("varExpl", "Choose feature", c('---')),
                                       plotOutput("plotRelation"))
                  ),
                  inputPanel(
                    sliderInput("ncols", "Number of columns", 7, 20, 2),
                    sliderInput("nrows", "Number of rows", 10, 50, 6),
                    actionButton('btn_viewData',label = 'View Data',icon=icon('table'))
                  )
                ),
                bsModal('data',title = 'Dataset',trigger = 'btn_viewData',size = 'large',
                        tableOutput('rawdata'))
                
              
                
        ),
        tabItem(tabName = "trainvalidation",
                
                sidebarPanel( width = 5,
                  fluidRow(
                    h4("mode cross validation",style = "padding-top: 0;color:green"),
                            width = 7),
                                
                                fluidRow(          
                  
                                  awesomeRadio(inputId = "crossFoldTypeUI","",
                             choices = c("K-Fold CV"='cv',"Repeated KFold CV"="repeatedcv")),
                
                numericInput("foldsUI","Number of Folds(k)",5),
              
                hr(),
                conditionalPanel(condition="input.crossFoldTypeUI == repeatedcv",
                                 numericInput("repeatUI","Number of Repeats",5))
                
                
                ),
                uiOutput("CVTypeUI"),
                fluidRow(
                         radioButtons(inputId = "preprocessingUI","Pre-processing Type",
                                      choices = c('No Preprocessing'="",'PCA'="pca",'ICA'="ica"),
                                      selected = 'No Preprocessing'),
                
                uiOutput("ppUI"))
                ),
                sidebarPanel( width = 5,
                              fluidRow(
                                h4("split data",style = "padding-top: 0;color:green"),width = 7),
                              
                              numericInput("num", h3("Number split"), value = 70)
                )
                
        ),
        tabItem(tabName = "classification",sidebarPanel( width = 10,
               pickerInput(inputId = "modelSelect",label = "Select models to run", 
                                                                    choices=c("No Model (NM)" = 'NoMod',
                                                                               "Logistic Regression (LR)" = 'glm',
                                                                               "Naif Bayes (NB)" = 'nb',
                                                                               "Linear Discriminant Analyse (LDA)" = 'lda',
                                                                               "Support Vector Machine (SVM)" = 'svmRadial',
                                                                               "K-Nearest Neighbors (KNN)" = 'knn',
                                                                               "Random Forest (RF)" = 'cforest',
                                                                               "Decision Tree (DT)" = 'LMT',
                                                                               "XGBoost (GBM)" = 'gbm'),
                                                                    options = list(`actions-box` = TRUE),multiple = T)),
            
               actionButton('btn_launchModel',label = 'Train Models',
                            icon = icon('cogs'),
                            class='btn-success'),
        ),
        
        tabItem(tabName = "continuous",sidebarPanel( width = 10,
                                                         pickerInput(inputId = "modelSelectPredict",label = "Select models to run", 
                                                                     choices=c("Multiple Linear Regression Modelling" = 'regLin',
                                                                               "Regression Trees" = 'regTree'),
                                                                     options = list(`actions-box` = TRUE),multiple = T)),
                
                actionButton('btn_launchModel_reg',label = 'Train Models',
                             icon = icon('cogs'),
                             class='btn-success'),
        ),
        
        
        tabItem(tabName = "features",
                fluidPage(
                  useShinyjs(),
                  
                  div(
                    id = "choiceReset",
                  fluidRow(
                    column(6,actionLink(inputId = "createcategory",uiOutput("createcategories")))
                  ),
                verbatimTextOutput(outputId = "resSelect"),
                
                fluidRow( 
                  column(3, textInput("nameCat", "Name of category", 
                                   value = "")),
                          verbatimTextOutput("nameCatText")
                ),
                hr(),
                fluidRow(
                  column(3, actionLink(inputId = "create_categorie", label = "Create categorie",icon = icon("table"))),
                ))
                ),
                
                fluidRow(
                  column(3, h4("List of categories variables")),
                mainPanel(verbatimTextOutput(outputId = "valCat"),width = 10),
                actionButton("resetCat", "Reset", icon = icon("refresh"),class = "btn-success"),
                uiOutput("refresh_categories")
                ),
                
                hr(),
                awesomeRadio(
                  inputId = "Id044",
                  label = "Specifique choice", 
                  choices = c("Execute all model(s) selected for all single variable" = 'all_single', 
                              "Execute all model(s) selected for all single variable combinate variables" = 'all_single_all_combinate',
                              "Execute all model(s) selected for only combinates variables created" = 'only_combinate'),
                  selected = 'all_single'
                ),
                hr(),
                actionButton("all_single_selected", "Run the models on the selected variables",
                style="color: #fff; background-color: #e95420; border-color: #c34113;
                               border-radius: 10px; 
                               border-width: 2px"),
                verbatimTextOutput("selections")
                ),
        
        
                 
        
        ##################################################################################
        ####   Algorithm Tab Set Contents
        ##################################################################################
        
        tabItem(tabName = "results",
                
                  box(title = "No Model (NM)", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 8,
                      tabBox(
                        tabPanel("Results",tableOutput("NMtrainResultsUI")),
                        tabPanel("AUC Plot",plotOutput("NMfinalPlotUI")),
                        tabPanel("95% CI AUC",plotOutput("NMfinalPlotCIAUC")),
                        tabPanel("Importance Plot",plotOutput("NMfinalPlotImpUI"))
                      )
                  ),
                
                sidebarPanel(
                             
                             selectInput("dataset", "Choose results:",
                                         choices = c("No Model" = "GLMNo", "Logistic Regression"='GLM', 
                                                     "Naif Bayes" = 'nb',"Linear Discriminant Analyse" = 'lda',
                                                     "Support Vector Machine" = 'svm',"K-Nearest Neighbors" = 'knn',
                                                     "Random Forest" = 'rf',"Decision Tree" = 'dt',
                                                     "Gradient Boost" = 'gbm')),
                             
                             
                             downloadButton("downloadData", "Download")
                             
                ),
                  box(title = "Logistic Regression (LR)", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 8,
                      
                      tabBox(
                        tabPanel("Results",tableOutput("LRtrainResultsUI")),
                        tabPanel("AUC Plot",plotOutput("LRfinalPlotUI")),
                        tabPanel("95% CI AUC",plotOutput("LRfinalPlotCIAUC")),
                      tabPanel("Importance Plot",plotOutput("LRfinalPlotImpUI"))
                      )
                  ),
                  box(title = "Naif Bayes (NB)", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 8,
                      tabBox(
                        tabPanel(" Results",tableOutput("NBtrainResultsUI")),
                        tabPanel("AUC Plot",plotOutput("NBfinalPlotUI")),
                        tabPanel("95% CI AUC",plotOutput("NBfinalPlotCIAUC")),
                        tabPanel("Importance Plot",plotOutput("NBfinalPlotImpUI"))
                      )
                  ),
                  box(title = "Linear Discriminant Analyse (LDA)", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 8,
                      tabBox(
                        tabPanel("Train Results",tableOutput("LDAtrainResultsUI")),
                        tabPanel("AUC Plot",plotOutput("LDAfinalPlotUI")),
                        tabPanel("95% CI AUC",plotOutput("LDAfinalPlotCIAUC")),
                        tabPanel("Importance Plot",plotOutput("LDAfinalPlotImpUI"))
                        )
                  ),
                  box(title = "Support Vector Machine (SVM)", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 8,
                      tabBox(
                        tabPanel("Results",tableOutput("SVMtrainResultsUI")),
                        tabPanel("AUC Plot",plotOutput("SVMfinalPlotUI")),
                        tabPanel("95% CI AUC",plotOutput("SVMfinalPlotCIAUC")),
                        tabPanel("Importance Plot",plotOutput("SVMfinalPlotImpUI"))
                      )
                  ),
                  box(title = "K-Nearest Neighbors (KNN)", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 8,
                      tabBox(
                        tabPanel("Results",tableOutput("KNNtrainResultsUI")),
                        tabPanel("AUC Plot",plotOutput("KNNfinalPlotUI")),
                        tabPanel("95% CI AUC",plotOutput("KNNfinalPlotCIAUC")),
                        tabPanel("Importance Plot",plotOutput("KNNfinalPlotImpUI"))
                      )
                  ),
                  box(title = "Random Forest (RF)", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 8,
                      tabBox(
                        tabPanel("Results",tableOutput("RFtrainResultsUI")),
                        tabPanel("AUC Plot",plotOutput("RFfinalPlotUI")),
                        tabPanel("95% CI AUC",plotOutput("RFfinalPlotCIAUC")),
                        tabPanel("Importance Plot",plotOutput("RFfinalPlotImpUI"))
                      )
                  ),
                  box(title = "Decision Tree (DT)", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 8,
                      tabBox(
                        tabPanel("Results",tableOutput("DTtrainResultsUI")),
                        tabPanel("AUC Plot",plotOutput("DTfinalPlotUI")),
                        tabPanel("95% CI AUC",plotOutput("DTfinalPlotCIAUC")),
                        tabPanel("Importance Plot",plotOutput("DTfinalPlotImpUI"))
                      )
                 ),
                  box(title = "Gradient Boost (GBM)", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 8,
                      tabBox(
                        tabPanel("Results",tableOutput("GBMtrainResultsUI")),
                        tabPanel("AUC Plot",plotOutput("GBMfinalPlotUI")),
                        tabPanel("95% CI AUC",plotOutput("GBMfinalPlotCIAUC")),
                        tabPanel("Importance Plot ",plotOutput("GBMfinalPlotImpUI"))
                      )

                )
                ),
        
        tabItem(tabName = "Summary")
      ))
  ) 
  
  
)
