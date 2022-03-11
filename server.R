source("ui.R")
source("models.R")

suppressPackageStartupMessages(require(shiny))
suppressPackageStartupMessages(require(DBI))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(openssl))

#sudo apt-get install libssl-dev
suppressPackageStartupMessages(require(shinydashboard))
suppressPackageStartupMessages(require(shinyWidgets))
suppressPackageStartupMessages(require(DT))
suppressPackageStartupMessages(require(shinyjs))
suppressPackageStartupMessages(require(sodium)) 

#sudo apt-get install libsodium-dev
suppressPackageStartupMessages(require(digest))

# sudo apt-get install libxml2-dev
# sudo apt-get install libcurl4-openssl-dev libxml2-dev
# sudo apt-get install libcurl4-openssl-dev
# sudo apt-get install libcurl4-openssl-dev libxml2-dev
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(InformationValue))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(plotROC))

# sudo apt-get install r-base-dev default-jdk
# sudo R CMD javareconf
suppressPackageStartupMessages(require(xlsx))
suppressPackageStartupMessages(require(VIM))
suppressPackageStartupMessages(require(DMwR))
suppressPackageStartupMessages(require(mice))
suppressPackageStartupMessages(require(Hmisc))
suppressPackageStartupMessages(require(checkmate))
suppressPackageStartupMessages(require(RSQLite))


DB_NAME <- "data.sqlite"
TBL_USER_DATA <- "users"

DB_test_connect <- function(){
  db <- dbConnect(RSQLite::SQLite(), DB_NAME)
  
  print("#######################")
  print("Connected to Database sqlite")
  
  # If a user data table doesn't already exist, create one
  if(!(TBL_USER_DATA %in% dbListTables(db))){
    print("- Warning: No 'users' table found. Creating table...")
    df <- data.frame(ID = as.numeric(character()),
                     USER = character(),
                     HASH = character(),
                     stringsAsFactors = FALSE)
    dbWriteTable(db, TBL_USER_DATA, df)
  } 
  
  print("- Table exists.")
  print("#######################")
  
  dbDisconnect(db)
}

DB_upload_csv <- function(filename, tblname){
  db <- dbConnect(RSQLite::SQLite(), DB_NAME)
  
  df <- read.csv(file = filename, header = T, row.names = F, stringsAsFactors = F)
  
  dbWriteTable(db, tblname, df)
  
  dbDisconnect(db)
}

DB_get_user <- function(user){
  db <- dbConnect(RSQLite::SQLite(), DB_NAME)
  
  users_data <- dbReadTable(db, TBL_USER_DATA)
  
  hashusers_data <- filter(users_data, USER == user)
  
  dbDisconnect(db)
  
  return(hashusers_data)
}

DB_add_user <- function(usr, hsh){
	
  db <- dbConnect(RSQLite::SQLite(), DB_NAME)
  
  df <- dbReadTable(db, TBL_USER_DATA)
  
  q <- paste("INSERT INTO", TBL_USER_DATA, "(ID, USER, HASH) VALUEs (", paste("", nrow(df), ",", usr, ",", hsh, "", sep="'"), ")")
  
  dbSendQuery(db, q)
  
  suppressWarnings({dbDisconnect(db)})
  
}

# Initialisation de la base de donnnées ------

DB_test_connect()

shinyServer(function(input, output, session) {

  loggedIn <- reactiveVal(value = FALSE)

  user <- reactiveVal(value = NULL)
  
  login <- eventReactive(input$login, {
    
    user_data <- DB_get_user(input$username)
    
    if(nrow(user_data) > 0){ 
      if(md5(input$password) == user_data[1, "HASH"]){
        
        user(input$username)
        loggedIn(TRUE)
        
        return(TRUE)
      }
    }
    
    return(FALSE)
    
  })
  register_user <- eventReactive(input$register_user, {
    
    users_data <- DB_get_user(input$new_user)
    if (nchar(input$new_user)<3)
      return(span("username at least 3 characters please give a nother", style = "color:red"))

    else if(nchar(input$new_pw)<3)
      return(span("password at least 3 characters\nplease give a nother", style = "color:red"))
    else if(nrow(users_data) > 0 )
      return(span("User already exists", style = "color:red"))
      
      new_hash <- md5(input$new_pw)
      new_user <- input$new_user
      
      DB_add_user(new_user, new_hash)
      
      print("- New user added to database")
      return(span("New user registered", style = "color:green"))
     
    
  })
  
  output$register_status <- renderUI({
    if(input$register_user == 0){
      return(NULL)
    } else {
      register_user()
    }
  })
  output$login_status <- renderUI({
    if(input$login == 0){
      return(NULL)
    } else {
      if(!login()){
        return(span("The Username or Password is Incorrect", style = "color:red"))
      }
    }
  })
  

  
  observeEvent(input$create_login, {
    showModal(
      modalDialog(title = "Create New Login", size = "m", 
                textInput(inputId = "new_user", label = tagList(icon("user"),"Username")),
                passwordInput(inputId = "new_pw", label = tagList(icon("unlock-alt"),"Password")),
                actionButton(inputId = "register_user", label = "Create",style = "color: white; background-color:#3c8dbc;
                                 cursor: pointer;"),

                uiOutput("register_status")
                )
    )

    register_user()
    
  })
  observeEvent(input$logout, {
    user(NULL)
    loggedIn(FALSE)
    print("- User: logged out")
  })

  observe({
    skin = "blue"
    dsnames <- reactiveValues()
    
    data_set <- reactive({
      req(input$file1)
      inFile <- input$file1
      baseName = basename(inFile$datapath)
      baseNameSep = tail(strsplit(gsub('[.]', ' ', baseName),split=" ")[[1]],1)

      if(baseNameSep == 'csv') data_set<-read.csv(inFile$datapath, header=input$header,sep=input$sep, quote=input$quote,dec = input$dec)
      else if(baseNameSep == 'xlsx') data_set = read.xlsx(inFile$datapath,sheetIndex = 1,header = input$header)
      else 
        return(showNotification(
          h4("The file type cannot be read! Only CSV or SLSX files are accepted ",style = "color:red"),
          duration = 3
        ))
      
      })
    

   
    if(loggedIn()){
      
      
      nFolds = reactiveValues()
      nFolds = 5
      TyppeCross = reactiveValues()
      TyppeCross = "cv"
      mycols = c("red","blue")
     
      colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
      NewCat <- reactiveValues()
      NewCat$df <- list()
      Mcorr <- reactiveValues()
      output$App_Panel <- renderUI({
        fluidPage(
          fluidRow(
            strong(paste("username is", user(), "|")), actionLink(inputId = "logout", "Logout",icon = icon("fa fa-sign-out")), align = "right",
            hr()
          )
        )
        observe({
          asDouble <- function(x) as.double(as.character(x))
          factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],asDouble))
          req(input$file1)
          dsnames <- names(data_set())
          cb_options <- list()
          cb_options[dsnames] <- dsnames
          
          if(is.null(dsnames))
            return()
          
          updateSelectInput(session, "inSelect",
                            choices = dsnames)
          
          updatePickerInput(session = session, inputId = "choose_var_delete",
            choices = dsnames)
          
          updatePickerInput(
            session = session, inputId = "choose_var_factor",
            choices = dsnames
          )
          
        })
        
        
        output$resSelect <- renderPrint({
          input$createcategories
        })
        myValues <- reactiveValues()
        myValuesdListVect = reactiveValues()
        choose_label_var = reactiveValues()
        choose_var_delete = reactiveValues()
        choose_var_factor = reactiveValues()
        y = reactiveValues()
        X = reactiveValues()
        df = reactiveValues()
        selectedVra = reactiveValues()
        selectedVra$var = as.character(c())
        data_setTrNA = reactiveValues()
        data_setTrNA$df = NULL
        output$nameCatText <- renderText({sub(" ", "_",input$nameCat)})

        
        ########################################################
        #         Traitement des données manquantes            #
        ########################################################
        
        observe({
          myValues$dList <- input$createcategories
          myValuesdListVect$dList = dput(myValues$dList)
          choose_label_var<- input$inSelect
          choose_var_delete <- input$choose_var_delete
          choose_var_factor <- input$choose_var_factor
         
          dataset = reactiveValues()
          dataset$df = data_set()
          testsize <- reactiveValues()
          preProc = reactiveValues()
          val<-reactiveValues()
          val$txt<-""
          dataNoNa = reactiveValues()
          dataNoNa$df = data_set()
          choiceTraitNa = reactiveValues()
          choiceTraitNa$choice = NULL
          preProc = input$preprocessingUI
          choose_label_var = input$inSelect
          choose_var_delete = input$choose_var_delete
          nFolds = input$foldsUI
          TyppeCross = input$crossFoldTypeUI
          
          observe({
            req(input$goButton)
            df = data_set()
            df[choose_var_factor] <- lapply(df[choose_var_factor], factor)
            
            df[colnames(df)[!colnames(df) %in% choose_var_factor]] <- lapply(df[!colnames(df) %in% choose_var_factor], as.double) 
            
            
            if(input$inSelect!='---')
              
              if(is.factor(df[,input$inSelect]))
              {
                updateRadioButtons(session, "inCheckPositiveLabel",
                                   choices = levels(df[,input$inSelect]),
                                   selected = levels(df[,input$inSelect])[1])
              }
            updateSelectInput(session, "varExpl",
                              choices = names(dplyr::select_if(df[ , !Mcorr], is.numeric))
                              [!(names(dplyr::select_if(df[ , !Mcorr], is.numeric)) %in% c(input$inSelect))])
            
            output$plotRelation <- renderPlot({
              if(!is.null(input$inSelect) && !is.null(input$varExpl)){
                label = as.factor(df[,input$inSelect])
                ggplot(df, aes(df[,input$varExpl])) +
                  stat_ecdf(aes(color = label))+
                  scale_color_brewer(palette="Dark2")+ 
                  ggtitle("Empirical cumulative distribution") +
                  labs(fill = "Class")+
                  xlab(input$varExpl) +
                  ylab(input$inSelect)
                
              }
            })
          })
        
          Mcorr = names(dataset$df) %in% sub(" ", ",",choose_var_delete)
          print(Mcorr)
           if (input$PreData == 'delete') {
              dataNoNa$df <- dataset$df[ , !Mcorr] %>%
                filter(complete.cases(.))
              output$number_NA <- renderText({
                anyNA(dataNoNa$df)
              })
            }
         
          M <- stats::cor(dplyr::select_if(dataNoNa$df, is.numeric))
          
          output$plotcorr <- renderPlot({
            ggcorrplot(M,lab = TRUE,hc.order = TRUE)
          }) 
            
            output$summary <- renderPrint({
              summary(dataNoNa$df)
            })
            
            
            output$rawdata <- renderTable({
              if(input$nrows<nrow(dataset$df[ , !Mcorr])+1 && input$ncols<ncol(dataset$df[ , !Mcorr])+1)
                dataset$df[ , !Mcorr][1:input$nrows,1:input$ncols]
              else
                dataset$df[ , !Mcorr]
              },
             options = list(pageLength = 20,searching = FALSE))
            
            nameCat <- eventReactive(input$create_categorie, {
              return(as.character(sub(" ", "_",input$nameCat)))
            })
            varList <- eventReactive(input$create_categorie, {
              return(input$createcategories)
            })
          
        })
        
        observe({
          observeEvent(input$run, {
            req(data_set())
            data_setTrNA$df = data_set()
            Mcorr2 = names(data_setTrNA$df) %in% sub(" ", ",",input$choose_var_delete)
            data_setTrNA$df[input$choose_var_factor] <- lapply(data_setTrNA$df[input$choose_var_factor], factor)
            data_setTrNA$df = data_setTrNA$df[ , !Mcorr2]
            switch(input$PreData,
                   view={
                     print('view')
                     #############
                     # n_iter <- 50 
                     # 
                     # pb <- tkProgressBar(title = "Tk progress bar",      
                     #                     label = "Percentage completed", 
                     #                     min = 0,     
                     #                     max = n_iter, 
                     #                     initial = 0,  
                     #                     width = 300) 
                     # 
                     # for(i in 1:n_iter) {
                     #   
                     #   Sys.sleep(0.1) 
                     #   
                     #  
                     #   
                     #   pctg <- paste(round(i/n_iter *100, 0), "% completed")
                     #   setTkProgressBar(pb, i, label = pctg)
                     # }
                     # 
                     # close(pb)
                     ##########
                     output$plotVarManq <- renderPlot({
                       matrixplot(data_set())
                     })
                     output$number_NA <- renderText({
                       paste("\nNumber NA are there in dataset",length(which(is.na(data_set()))))
                       paste("\nNumber rows contain NA",length(which(!complete.cases(data_set()))))
                       paste("\nPercentage of variables is NA",summary(aggr(data_set())))
                       dim(data_setTrNA$df)
                     })
                     
                     
                   },
                   delete={
                     print('delete')
                     
                     data_setTrNA$df <- data_setTrNA$df[complete.cases(data_setTrNA$df), ]
                     output$plotVarManq <- renderPlot({
                       matrixplot(data_setTrNA$df)
                     })
                     output$number_NA <- renderText({
                       anyNA(data_setTrNA$df)
                       
                       dim(data_setTrNA$df)
                     })
                     
                   },
                   mean={
                     print('mean')
                     
                     for(i in colnames(data_setTrNA$df)){
                       data_setTrNA$df[,i] = impute(data_setTrNA$df[,i], mean) 
                     }
                     output$plotVarManq <- renderPlot({
                       matrixplot(data_setTrNA$df)
                     })
                     output$number_NA <- renderText({
                       anyNA(data_setTrNA$df)
                       dim(data_setTrNA$df)
                     })
                   },
                   median={
                     print('median')
                     
                     for(i in colnames(data_setTrNA$df)){
                       data_setTrNA$df[,i] = impute(data_setTrNA$df[,i], median) 
                     }
                     output$plotVarManq <- renderPlot({
                       matrixplot(data_setTrNA$df)
                     })
                     output$number_NA <- renderText({
                       anyNA(data_setTrNA$df)
                       dim(data_setTrNA$df)
                     })
                   },
                   kNN={
                     print('kNN')
                     data_setTrNA$df[colnames(dplyr::select_if(data_setTrNA$df, is.numeric))] <-  knnImputation(dplyr::select_if(data_setTrNA$df, is.numeric))
                     
                     output$plotVarManq <- renderPlot({
                       matrixplot(data_setTrNA$df)
                     })
                     output$number_NA <- renderText({
                       anyNA(data_setTrNA$df)
                       dim(data_setTrNA$df)
                     })
                     
                   },
                   multiImpu={
                     print('multiImpu')
                     miceMod <- mice(dplyr::select_if(data_setTrNA$df, is.numeric), method="rf") 
                     data_setTrNA$df <- complete(miceMod)  
                     output$number_NA <- renderText({
                       anyNA(data_setTrNA$df)
                     })
                     
                     output$plotVarManq <- renderPlot({
                       matrixplot(data_setTrNA$df)
                       
                       dim(data_setTrNA$df)
                     })
                     
                   },
                   {
                     print('default')
                   }
            )
            
            
          })
        })
        
        observe({output$createcategories = renderUI(
          if(input$inSelect != '---')
          multiInput(width = 1000,
                     inputId = "createcategories",
                     label = "Select all variables ",
                     choices = names(isolate(data_setTrNA$df[,!(names(data_setTrNA$df) %in% c(input$inSelect,input$choose_var_delete))])),
                     options = list(
                       enable_search = FALSE,
                       non_selected_header = "Choose between:",
                       selected_header = "You have selected:"
                     )
          )
          else multiInput(width = 1000,
                          inputId = "createcategories",
                          label = "Select all variables ",
                          choices = names(data_setTrNA$df),
                          options = list(
                            enable_search = FALSE,
                            non_selected_header = "Choose between:",
                            selected_header = "You have selected:"
                          )
          )
          )})
        nameCat <- eventReactive(input$create_categorie, {
          return(as.character(sub(" ", "_",input$nameCat)))
        })
        varList <- eventReactive(input$create_categorie, {
          return(input$createcategories)
        })
        
        observeEvent(input$create_categorie, {
          if(nchar(as.character(sub(" ", "_",input$nameCat)))>2 && length(input$createcategories)>1)
          {
            text = input$createcategories
            showModal( modalDialog(title = paste(paste(paste(paste("Create categorie :",
                                                                   as.character(sub(" ", "_",input$nameCat))),"{"),
                                                       paste(text, collapse=', ' )),"}"), size = "m", 
                                   actionButton(inputId = "Create_categorie_variable", label = "Create",
                                   style = "color: white; background-color:#3c8dbc;
                                 cursor: pointer;"),
                                   uiOutput("addCat")
                                   
            ))
          }
          else if(nchar(as.character(sub(" ", "_",input$nameCat)))>2 && length(input$createcategories)<2)
            return(showNotification(
            h4("At minimum two variables ",style = "color:red"),
            duration = 3
          ))
          else if(nchar(as.character(sub(" ", "_",input$nameCat)))<3 && length(input$createcategories)>1)return(showNotification(
            h4("Name categorie contains less than two characters",style = "color:red"),
            duration = 3
          ))
          else return(showNotification(
            h4("At minimum two variables and name categorie contains less than two characters",style = "color:red"),
            duration = 5
          ))
          
        })
        val<-reactiveValues()
        val$txt<-NULL
        
        create_cat <- eventReactive(input$Create_categorie_variable, {
          new<-paste("---", paste(paste(paste(paste("categorie :", as.character(sub(" ", "_",input$nameCat))),"{"),paste(varList(), collapse=', ' )),"}"))
          val$txt<-paste( val$txt,new,sep='\n')
          NewCat$df[[sub(" ", "_",input$nameCat)]] = c(varList())
          
   
          output$valCat <- renderText({
            req(input$Create_categorie_variable)
            str1 <- val$txt
          })
          
          reset("choiceReset")
          return(span("Create new categories", style = "color:green"))
        })
        output$addCat <- renderUI({
          create_cat()
        })
       
        observeEvent(input$resetCat, {
          val$txt<-NULL
          NewCat$df = list()
          output$valCat <- renderText({
            str1 <- "--- All categories is delete"
          })
        })
        
        observeEvent(input$all_single_selected, {
          updateSelectizeInput(session, "selectionsEnter", choices = names(isolate(data_set()[,!(names(data_set()) %in% c(input$inSelect,input$choose_var_delete))])), 
                               selected = input$selectionsEnter, server = TRUE)
          showModal(modalDialog(selectInput("selectionsEnter", "Parameters Selted", multiple = TRUE, choices = character(0))))
          
          
        })
        output$selections <- renderText(input$selectionsEnter)
        
        
        observeEvent(input$btn_launchModel, {

          
          if(input$crossFoldTypeUI == 'repeatedcv')
            trCtr <- trainControl(method = as.character(input$crossFoldTypeUI),
                                  number = 10 ,
                                  repeats = as.integer(input$repeatUI),
                                  classProbs = TRUE,
                                  savePredictions = TRUE)
          
          else trCtr <- trainControl(method = as.character(input$crossFoldTypeUI),
                                     number = as.integer(nFolds),
                                     savePredictions = TRUE, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
          
          
          
          
          
          if(is.null(input$inSelect)||input$inSelect=='---')
            return(NULL)
          
          y <- isolate(data_setTrNA$df[,input$inSelect])
          X <-  isolate(data_setTrNA$df[,!(names(data_setTrNA$df) %in% c(input$inSelect))])
          yi <- !is.na(y)
          Xi <- complete.cases(X)
          df2 <- cbind(y,X)[yi&Xi,]
          print(dim(df2))
          c <- class(df2$y)
          lvls <- length(unique(df2$y))
          if(lvls<10|(c!='numeric'&c!='integer')){
            modelType <<-'Classification'
            df2$y <- factor(df2$y)
          }
          
          set.seed(123)
          if (is.na(input$num) || input$num >100) {
            testsize = 0.75
          }else testsize <- input$num/100

          trainIndex <- createDataPartition(df2$y,
                                            p = testsize,
                                            list = FALSE,
                                            times = 1)
          if (length(df2)>150) 
            isolate({
              dataTrain <<- df2[ trainIndex,]
              dataTest  <<- df2[-trainIndex,]
            })
          
          else
            isolate({
              dataTrain <<- df2
              dataTest  <<- df2
            })
          
          # Creation de model
          popupModal <- function(failed = FALSE) {
            modalDialog(
              textInput("txt", "Write something"),
              if (failed)
                div(tags$b("You did not input anything", style = "color: red;")),
              
              footer = tagList(
                modalButton("Cancel"),
                actionButton("ok", "OK")
              )
            )
          }
          
          "
          Dans cette partie du code nous définissons les paramètres inclus dans les differents modèles 
          - Dans un premier temps nous prennons en compte l'exécution sur les seules variables sélectionnées par l'utilisateurs
          ainsi que toutes les combinaisons possibles de variables s'il en existe.
          
          -  Dans la deuximee option on prnd en compte que les singletons de variables
          -  Dans la troisième option on considère que les combinaison de variables s'il ne désire que les modèles soient exécurés 
          sur les seules combinaisons crées.
          
          -  Et en dernière possibilité tous les singletons associés aux variables combinées
          "
          
          selectedVra$var = append(selectedVra$var,input$selectionsEnter)
          if(length(selectedVra$var)>0){
            if(length(names(NewCat$df))>0) results  <- data.frame(Parms = c(selectedVra$var,names(NewCat$df)))
            else {results  <- data.frame(Parms = selectedVra$var)
            }
          }
          else if(input$Id044 == 'all_single')
            results <- data.frame( Parms = names(X))
          else if (input$Id044 == 'only_combinate')
            results <- data.frame( Parms = names(NewCat$df))
          else if (input$Id044 == 'all_single_all_combinate')
            results <- data.frame( Parms = c(names(X),names(NewCat$df)))
          
        
          
          for (i_model in input$modelSelect) {
            
            #input$inCheckPositiveLabel = 1
            
             if (i_model == 'NoMod'){
            print(input$inCheckPositiveLabel) 
            #print( levels(dataTrain$y)[levels(dataTrain$y)!=input$inCheckPositiveLabel]) 
               ktest = testModel(data = dataTrain,dataT = dataTest,i_model = i_model,grpeParams = results,
                         grpeNewCat = NewCat$df,positiveLabel = 1,trCtr = trCtr,
                         negative_label =  0)
               
               output$NMtrainResultsUI <- renderTable(ktest$res)
               
               if(ncol(ktest$ROCVALs)>2){
                 output$NMfinalPlotUI <- renderPlot({
                   ggplot(ktest$ROCVALs, aes(d = D, m = M, color = name)) + geom_roc(n.cuts = 0) + style_roc()
                 })
               }else{
                 output$NMfinalPlotUI <- renderPlot({
                   ggplot(ktest$ROCVALs, aes(d = D, m = M1)) + geom_roc()
                 })
               }
               
               #resultsAUCPlot
               output$NMfinalPlotCIAUC <- renderPlot({
                 ggplot(ktest$resultsAUCPlot, aes(Parms, AUC)) +
                   geom_line(aes(group = 1),size=1) +
                   geom_errorbar( aes(ymin = min_ci_auc, ymax = max_ci_auc),width = 0.2) +
                   geom_point(size = 2)+theme(axis.text.y = element_text(colour = 'black', size = 12,face = "bold"),
                                              axis.title.y = element_text(size = 12,
                                                                          hjust = 0.5, vjust = 0.2,face = "bold"),
                                              axis.text.x = element_text(colour = 'black', size = 12,face = "bold", angle = 90),
                                              axis.title.x = element_text(size = 12,face = "bold")) + 
                   scale_color_viridis_d()+
                 theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                                   vjust = 0.5, face = 'bold'))
               })
              

              # 
               if (!is.null(ktest$df.Impts)) {
                 output$NMfinalPlotImpUI <- renderPlot({
                     ggplot(ktest$df.Impts, aes(x = Parameters, y = Contribution)) +
                       geom_segment(
                         aes(x = Parameters, xend = Parameters, y = 0, yend = Contribution),
                         color = "lightgray"
                       ) +
                       geom_point(aes(color = Categories), size = 3) +
                       scale_color_viridis_d() +
                       theme_pubclean() +
                       rotate_x_text(45)+
                       theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                                         vjust = 0.5, face = 'bold'))

                   })
               }
             }

            # logistic regression
            else if (i_model == 'glm'){
               ktest2 = testModel(data = dataTrain,dataT = dataTest,i_model = i_model,grpeParams = results,
                                 grpeNewCat = NewCat$df,positiveLabel = 1,trCtr = trCtr,negative_label =  0)
               #NEGA = levels(dataTrain$y)[levels(dataTrain$y)!=input$inCheckPositiveLabel]

               output$LRtrainResultsUI <- renderTable(ktest2$res)
               if(ncol(ktest2$ROCVALs)>2){
                 output$LRfinalPlotUI <- renderPlot({
                   ggplot(ktest2$ROCVALs, aes(d = D, m = M, color = name)) + geom_roc(n.cuts = 0) + style_roc()
                 })
               }else{
                 output$LRfinalPlotUI <- renderPlot({
                   ggplot(ktest2$ROCVALs, aes(d = D, m = M1)) + geom_roc()
                 })
               }
               output$LRfinalPlotCIAUC <- renderPlot({
                 ggplot(ktest2$resultsAUCPlot, aes(Parms, AUC)) +
                   geom_line(aes(group = 1),size=1) +
                   geom_errorbar( aes(ymin = min_ci_auc, ymax = max_ci_auc),width = 0.2) +
                   geom_point(size = 2)+theme(axis.text.y = element_text(colour = 'black', size = 12,face = "bold"),
                                              axis.title.y = element_text(size = 12,
                                                                          hjust = 0.5, vjust = 0.2,face = "bold"),
                                              axis.text.x = element_text(colour = 'black', size = 12,face = "bold", angle = 90),
                                              axis.title.x = element_text(size = 12,face = "bold")) + 
                   theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                                     vjust = 0.5, face = 'bold'))
               })
               
               
               
               if (!is.null(ktest2$df.Impts)) {
                 
                 output$LRfinalPlotImpUI <- renderPlot({
                   
                   ggplot(ktest2$df.Impts, aes(x = Parameters, y = Contribution)) +
                     geom_segment(
                       aes(x = Parameters, xend = Parameters, y = 0, yend = Contribution),
                       color = "lightgray"
                     ) +
                     geom_point(aes(color = Categories), size = 3) +
                     scale_color_viridis_d() +
                     theme_pubclean() +
                     rotate_x_text(45)+
                     theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                                       vjust = 0.5, face = 'bold'))
                 })
                 } 
               }
             
             # Naive Bayes algorithm
             else if (i_model == 'nb'){
               ktest3 = testModel(data = dataTrain,dataT = dataTest,i_model = i_model,grpeParams = results,
                                  grpeNewCat = NewCat$df,positiveLabel = 1,trCtr = trCtr,negative_label = 0)
               output$NBtrainResultsUI <- renderTable(ktest3$res)
               if(ncol(ktest3$ROCVALs)>2){
                 output$NBfinalPlotUI <- renderPlot({
                   ggplot(ktest3$ROCVALs, aes(d = D, m = M, color = name)) + geom_roc(n.cuts = 0) + style_roc()
                 })
               }else{
                 output$NBfinalPlotUI <- renderPlot({
                   ggplot(ktest3$ROCVALs, aes(d = D, m = M1)) + geom_roc()
                 })
               }
               
               output$NBfinalPlotCIAUC <- renderPlot({
                 ggplot(ktest3$resultsAUCPlot, aes(Parms, AUC)) +
                   geom_line(aes(group = 1),size=1) +
                   geom_errorbar( aes(ymin = min_ci_auc, ymax = max_ci_auc),width = 0.2) +
                   geom_point(size = 2)+theme(axis.text.y = element_text(colour = 'black', size = 12,face = "bold"),
                                              axis.title.y = element_text(size = 12,
                                                                          hjust = 0.5, vjust = 0.2,face = "bold"),
                                              axis.text.x = element_text(colour = 'black', size = 12,face = "bold", angle = 90),
                                              axis.title.x = element_text(size = 12,face = "bold")) + 
                   theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                                     vjust = 0.5, face = 'bold'))
               })
               
               if (!is.null(ktest3$df.Impts)) {
                
                 output$NBfinalPlotImpUI <- renderPlot({
                   
                   ggplot(ktest3$df.Impts, aes(x = Parameters, y = Contribution)) +
                     geom_segment(
                       aes(x = Parameters, xend = Parameters, y = 0, yend = Contribution),
                       color = "lightgray"
                     ) +
                     geom_point(aes(color = Categories), size = 3) +
                     scale_color_viridis_d() +
                     theme_pubclean() +
                     rotate_x_text(45)+
                     theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                                       vjust = 0.5, face = 'bold'))
                   
                 })
               }
             }
             # Latent Dirichlet allocation
             else if (i_model == 'lda'){
               ktest4 = testModel(data = dataTrain,dataT = dataTest,i_model = i_model,grpeParams = results,
                                  grpeNewCat = NewCat$df,positiveLabel = 1,trCtr = trCtr,negative_label =  0)
               output$LDAtrainResultsUI <- renderTable(ktest4$res)
               
               if(ncol(ktest4$ROCVALs)>2){
                 output$LDAfinalPlotUI <- renderPlot({
                   ggplot(ktest4$ROCVALs, aes(d = D, m = M, color = name)) + geom_roc(n.cuts = 0) + style_roc()
                 })
               }else{
                 output$LDAfinalPlotUI <- renderPlot({
                   ggplot(ktest4$ROCVALs, aes(d = D, m = M1)) + geom_roc()
                 })
               }
               
               output$LDAfinalPlotCIAUC <- renderPlot({
                 ggplot(ktest4$resultsAUCPlot, aes(Parms, AUC)) +
                   geom_line(aes(group = 1),size=1) +
                   geom_errorbar( aes(ymin = min_ci_auc, ymax = max_ci_auc),width = 0.2) +
                   geom_point(size = 2)+theme(axis.text.y = element_text(colour = 'black', size = 12,face = "bold"),
                                              axis.title.y = element_text(size = 12,
                                                                          hjust = 0.5, vjust = 0.2,face = "bold"),
                                              axis.text.x = element_text(colour = 'black', size = 12,face = "bold", angle = 90),
                                              axis.title.x = element_text(size = 12,face = "bold")) + 
                   theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                                     vjust = 0.5, face = 'bold'))
               })
               
               if (!is.null(ktest4$df.Impts)) {
                 output$LDAfinalPlotImpUI <- renderPlot({
                   
                   ggplot(ktest4$df.Impts, aes(x = Parameters, y = Contribution)) +
                     geom_segment(
                       aes(x = Parameters, xend = Parameters, y = 0, yend = Contribution),
                       color = "lightgray"
                     ) +
                     geom_point(aes(color = Categories), size = 3) +
                     scale_color_viridis_d() +
                     theme_pubclean() +
                     rotate_x_text(45)+
                     theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                                       vjust = 0.5, face = 'bold'))
                   
                 })
               }
               }
             # k-nearest neighbors algorithme
             if (i_model == 'knn'){
               ktest5 = testModel(data = dataTrain,dataT = dataTest,i_model = i_model,grpeParams = results,
                                  grpeNewCat = NewCat$df,positiveLabel = 1,trCtr = trCtr,negative_label =  0)
               output$KNNtrainResultsUI <- renderTable(ktest5$res)
               if(ncol(ktest5$ROCVALs)>2){
                 output$KNNfinalPlotUI <- renderPlot({
                   ggplot(ktest5$ROCVALs, aes(d = D, m = M, color = name)) + geom_roc(n.cuts = 0) + style_roc()
                 })
               }else{
                 output$KNNfinalPlotUI <- renderPlot({
                   ggplot(ktest5$ROCVALs, aes(d = D, m = M1)) + geom_roc()
                 })
               }
               
               output$KNNfinalPlotCIAUC <- renderPlot({
                 ggplot(ktest5$resultsAUCPlot, aes(Parms, AUC)) +
                   geom_line(aes(group = 1),size=1) +
                   geom_errorbar( aes(ymin = min_ci_auc, ymax = max_ci_auc),width = 0.2) +
                   geom_point(size = 2)+theme(axis.text.y = element_text(colour = 'black', size = 12,face = "bold"),
                                              axis.title.y = element_text(size = 12,
                                                                          hjust = 0.5, vjust = 0.2,face = "bold"),
                                              axis.text.x = element_text(colour = 'black', size = 12,face = "bold", angle = 90),
                                              axis.title.x = element_text(size = 12,face = "bold")) + 
                   theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                                     vjust = 0.5, face = 'bold'))
               })
               
               if (!is.null(ktest5$df.Impts)) {
                 output$KNNfinalPlotImpUI <- renderPlot({
                   ggplot(ktest5$df.Impts, aes(x = Parameters, y = Contribution)) +
                     geom_segment(
                       aes(x = Parameters, xend = Parameters, y = 0, yend = Contribution),
                       color = "lightgray"
                     ) +
                     geom_point(aes(color = Categories), size = 3) +
                     scale_color_viridis_d() +
                     theme_pubclean() +
                     rotate_x_text(45)+
                     theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                                       vjust = 0.5, face = 'bold'))
                 })
               }
             }
           
 			
 			# Logistic model tree - arbre de décision
            
             else if (i_model == 'LMT'){
               ktest6 = testModel(data = dataTrain,dataT = dataTest,i_model = i_model,grpeParams = results,
                                  grpeNewCat = NewCat$df,positiveLabel = 1,trCtr = trCtr,negative_label =  0)
               output$DTtrainResultsUI <- renderTable(ktest6$res)
               if(ncol(ktest6$ROCVALs)>2){
                 output$DTfinalPlotUI <- renderPlot({
                   ggplot(ktest6$ROCVALs, aes(d = D, m = M, color = name)) + geom_roc(n.cuts = 0) + style_roc()
                 })
               }else{
                 output$DTfinalPlotUI <- renderPlot({
                   ggplot(ktest6$ROCVALs, aes(d = D, m = M1)) + geom_roc()
                 })
               }
               
               output$DTfinalPlotCIAUC <- renderPlot({
                 ggplot(ktest6$resultsAUCPlot, aes(Parms, AUC)) +
                   geom_line(aes(group = 1),size=1) +
                   geom_errorbar( aes(ymin = min_ci_auc, ymax = max_ci_auc),width = 0.2) +
                   geom_point(size = 2)+theme(axis.text.y = element_text(colour = 'black', size = 12,face = "bold"),
                                              axis.title.y = element_text(size = 12,
                                                                          hjust = 0.5, vjust = 0.2,face = "bold"),
                                              axis.text.x = element_text(colour = 'black', size = 12,face = "bold",angle = 90),
                                              axis.title.x = element_text(size = 12,face = "bold")) + 
                   theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                                     vjust = 0.5, face = 'bold'))
               })
               
               if (!is.null(ktest6$df.Impts)) {
                 output$DTfinalPlotImpUI <- renderPlot({
                   ggplot(ktest6$df.Impts, aes(x = Parameters, y = Contribution)) +
                     geom_segment(
                       aes(x = Parameters, xend = Parameters, y = 0, yend = Contribution),
                       color = "lightgray"
                     ) +
                     geom_point(aes(color = Categories), size = 3) +
                     scale_color_viridis_d() +
                     theme_pubclean() +
                     rotate_x_text(45)+
                     theme(strip.text.y = element_text(size = 11, hjust = 0.5, vjust = 0.5, face = 'bold'))
                 })
               }
             }
           
            
            # Gradient boosting 
            else if (i_model == 'gbm'){
              
                ktest7 = testModel(data = dataTrain,dataT = dataTest,i_model = i_model,grpeParams = results,
                                   grpeNewCat = NewCat$df,positiveLabel =1,trCtr = trCtr,negative_label =  0)
                output$GBMtrainResultsUI <- renderTable(ktest7$res)
                if(ncol(ktest7$ROCVALs)>2){
                  output$GBMfinalPlotUI <- renderPlot({
                    ggplot(ktest7$ROCVALs, aes(d = D, m = M, color = name)) + geom_roc(n.cuts = 0) + style_roc()
                  })
                }else{
                  output$GBMfinalPlotUI <- renderPlot({
                    ggplot(ktest7$ROCVALs, aes(d = D, m = M1)) + geom_roc()
                  })
                }
                output$GBMfinalPlotCIAUC <- renderPlot({
                    ggplot(ktest7$resultsAUCPlot, aes(Parms, AUC)) +
                      geom_line(aes(group = 1),size=1) +
                      geom_errorbar( aes(ymin = min_ci_auc, ymax = max_ci_auc),width = 0.2) +
                      geom_point(size = 2)+theme(axis.text.y = element_text(colour = 'black', size = 12,face = "bold"),
                                                 axis.title.y = element_text(size = 12,
                                                                             hjust = 0.5, vjust = 0.2,face = "bold",angle = 90),
                                                 axis.text.x = element_text(colour = 'black', size = 12,face = "bold"),
                                                 axis.title.x = element_text(size = 12,face = "bold")) +
                      theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                                        vjust = 0.5, face = 'bold'))
                  })
                if (!is.null(ktest7$df.Impts)) {
                  output$GBMfinalPlotImpUI <- renderPlot({
                    ggplot(ktest7$df.Impts, aes(x = Parameters, y = Contribution)) +
                      geom_segment(
                        aes(x = Parameters, xend = Parameters, y = 0, yend = Contribution),
                        color = "lightgray"
                      ) +
                      geom_point(aes(color = Categories), size = 3) +
                      scale_color_viridis_d() +
                      theme_pubclean() +
                      rotate_x_text(45)+
                      theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                                        vjust = 0.5, face = 'bold'))
                  })
                }
            }

             # Support vector machine
             else if (i_model == 'svmRadial')
             {
               ktest8 = testModel(data = dataTrain,dataT = dataTest,i_model = i_model,grpeParams = results,
                                  grpeNewCat = NewCat$df,positiveLabel = 1,trCtr = trCtr,negative_label =  0)
               output$SVMtrainResultsUI <- renderTable(ktest8$res)
               
               if(ncol(ktest8$ROCVALs)>2){
                 output$SVMfinalPlotUI <- renderPlot({
                   ggplot(ktest8$ROCVALs, aes(d = D, m = M, color = name)) + geom_roc(n.cuts = 0,increasing = FALSE) + style_roc()
                 })
               }else{
                 output$SVMfinalPlotUI <- renderPlot({
                   ggplot(ktest8$ROCVALs, aes(d = D, m = M1)) + geom_roc(increasing = FALSE)
                 })
               }
               
               output$SVMfinalPlotCIAUC <- renderPlot({
                 ggplot(ktest8$resultsAUCPlot, aes(Parms, AUC)) +
                   geom_line(aes(group = 1),size=1) +
                   geom_errorbar( aes(ymin = min_ci_auc, ymax = max_ci_auc),width = 0.2) +
                   geom_point(size = 2)+theme(axis.text.y = element_text(colour = 'black', size = 12,face = "bold"),
                                              axis.title.y = element_text(size = 12,
                                                                          hjust = 0.5, vjust = 0.2,face = "bold"),
                                              axis.text.x = element_text(colour = 'black', size = 12,face = "bold", angle = 90),
                                              axis.title.x = element_text(size = 12,face = "bold")) + 
                   theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                                     vjust = 0.5, face = 'bold'))
               })
               
               if (!is.null(ktest8$df.Impts)) {
                 
                 output$SVMfinalPlotImpUI <- renderPlot({
                   ggplot(ktest8$df.Impts, aes(x = Parameters, y = Contribution)) +
                     geom_segment(
                       aes(x = Parameters, xend = Parameters, y = 0, yend = Contribution),
                       color = "lightgray"
                     ) +
                     geom_point(aes(color = Categories), size = 3) +
                     scale_color_viridis_d() +
                     theme_pubclean() +
                     rotate_x_text(45)+
                     theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                                       vjust = 0.5, face = 'bold'))
                 })
               }
             }

            # Random Forest 

            else if (i_model == 'cforest'){
              ktest9 = testModel(data = dataTrain,dataT = dataTest,i_model = i_model,grpeParams = results,
                                 grpeNewCat = NewCat$df,positiveLabel = 1,trCtr = trCtr,
                                 negative_label =  0)
              output$RFtrainResultsUI <- renderTable(ktest9$res)
              if(ncol(ktest9$ROCVALs)>2){
                output$RFfinalPlotUI <- renderPlot({
                  ggplot(ktest9$ROCVALs, aes(d = D, m = M, color = name)) + geom_roc(n.cuts = 0) + style_roc()
                })
              }else{
                output$RFfinalPlotUI <- renderPlot({
                  ggplot(ktest9$ROCVALs, aes(d = D, m = M1)) + geom_roc()
                })
              }
              
              output$RFfinalPlotCIAUC<- renderPlot({
                ggplot(ktest9$resultsAUCPlot, aes(Parms, AUC)) +
                  geom_line(aes(group = 1),size=1) +
                  geom_errorbar( aes(ymin = min_ci_auc, ymax = max_ci_auc),width = 0.2) +
                  geom_point(size = 2)+theme(axis.text.y = element_text(colour = 'black', size = 12,face = "bold"),
                                             axis.title.y = element_text(size = 12,
                                                                         hjust = 0.5, vjust = 0.2,face = "bold"),
                                             axis.text.x = element_text(colour = 'black', size = 12,face = "bold", angle = 90),
                                             axis.title.x = element_text(size = 12,face = "bold")) + 
                  theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                                    vjust = 0.5, face = 'bold'))
              })

             if (!is.null(ktest9$df.Impts)) {
              
               output$RFfinalPlotImpUI <- renderPlot({

                 ggplot(ktest9$df.Impts, aes(x = Parameters, y = Contribution)) +
                   geom_segment(
                     aes(x = Parameters, xend = Parameters, y = 0, yend = Contribution),
                     color = "lightgray"
                   ) +
                   geom_point(aes(color = Categories), size = 3) +
                   scale_color_viridis_d() +
                   theme_pubclean() +
                   rotate_x_text(45)+
                   theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                                     vjust = 0.5, face = 'bold'))

               })
             }
            }
            
            # Affichage des résutats obtenus des differents models

            datasetInput <- reactive({
              switch(input$dataset,
                     "GLMNo" = ktest$res,
                     "GLM" = ktest2$res,
                     "nb" = ktest3$res,
                     "lda" = ktest4$res,
                     "svm" = ktest8$res,
                     "knn" = ktest5$res,
                     "rf" = ktest9$res,
                     "dt" = ktest6$res,
                     "gbm" = ktest7$res
                     )
            })
            
            # Télechargement des résutats 
            output$downloadData <- downloadHandler(
              filename = function() {
                paste(input$dataset, ".csv", sep = "")
              },
              content = function(file) {
                write.csv(datasetInput(), file, row.names = FALSE)
              }
            )

          }
        })
        shinyUI(ui_db)
        
      })
        
    } else {
      output$App_Panel <- renderUI({
        fluidPage(
          fluidRow(
            hr(),
            titlePanel(title = "Machine Learning App Shiny"), align = "center"
          ),
          fluidRow(
            column(4, offset = 4,
                   wellPanel(
                     tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                     textInput(inputId = "username", label = tagList(icon("user"), "Username")),
                     passwordInput(inputId = "password", label = tagList(icon("unlock-alt"), "Password")),
                     fluidRow(
                       column(4,offset = 4, align="center", actionButton(inputId = "login", label = "Login",style = "color: white; background-color:#3c8dbc;
                                 cursor: pointer;")),
                       br(),
                       br(),
                       div(
                       column(6, align = "right",actionLink(inputId = "create_login", label = "Create",icon = icon("user")),offset = 5)),
                       column(6, offset = 3, uiOutput(outputId = "login_status")
                       )
                     )
                   ))
            ))
      })
    } 
  })
})



