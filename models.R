suppressPackageStartupMessages(require(caret))
suppressPackageStartupMessages(require(rpart))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(rpart.plot) )
suppressPackageStartupMessages(require(RWeka))
suppressPackageStartupMessages(require(pROC))
suppressPackageStartupMessages(require(ROCR))
suppressPackageStartupMessages(require(kernlab))
suppressPackageStartupMessages(require(e1071))
suppressPackageStartupMessages(require(rminer))

"
Dans ce fichier nous définissons tout nos modèles que l'on souhaite rendre disponible sur notre plateforme
- Premièrement on créé notre base d'apprentissage et test sur les données d'entrées seon le split voulu par le user si le nombre d'observation est suppérieur à 150
- Puis on crée la vriable cible sur la base du choix de l'utilisateur en tenant compte de l'input de la label positive.
- On crée un dataframe contenant les contribution de chaque variable sur les modèles. Seules les combinaison de variables sont tenues en compte.
- Un dataframe aussi est créé pour stocké les probabilité d'appartenance d'une observation dans la classe positive. 
Ce dataframe est ensuite utilisé pour tracer les courbes de ROC associés.

"


testModel = function(data,dataT,i_model,grpeParams,grpeNewCat,positiveLabel,trCtr,negative_label) {

  if(i_model %in% c('glm','nb','lda','knn','cforest','LMT','svmRadial','gbm'))
  {
    levels(data[,"y"]) = levels(dataT[,"y"]) = list(YES = positiveLabel, NO = negative_label)
    positiveLabel = 'YES'
    negative_label = 'NO'
  }
  results 		= NULL
  min_ci_auc = AUC = max_ci_auc = KAPPA = DifferenceAUC = Precision = Specificity = Sensitivity = NPV = PPV = TP = FP = TN = FN = Concord = as.numeric(c())
  
  ROCVAL = NULL
  ROCVAL = data.frame(as.numeric(as.character(as.factor(ifelse(dataT[,"y"]== positiveLabel,1,0)))),stringsAsFactors=FALSE)
  
  
  colnames(ROCVAL) = "D"
  resPerf = data.frame( Parms = grpeParams$Parms)
  resultsAUC = data.frame( Parms = grpeParams$Parms)
  
  
  if (length(grpeNewCat)>0) {
    df.Impt <- data.frame(structure(
      rep(0,length(grpeNewCat)*(ncol(dataT)-1)), .Dim = c(length(grpeNewCat),ncol(dataT)-1), 
      .Dimnames = list(names(grpeNewCat),
                       colnames(dataT[ ,!(colnames(dataT) == "y")]))
    ))
  }
  else df.Impt = NULL
  
  for (i in grpeParams$Parms) {
    
    
    if(i %in% names(data))
    {
      dataTrain.ML 	= cbind(data["y"],data[i]) 
      dataTest.ML 	= cbind(dataT["y"],dataT[i]) 
    }
    else
    {
      dataTrain.ML 	= cbind(data["y"],data[grpeNewCat[[i]]])  
      dataTest.ML 	= cbind(dataT["y"],dataT[grpeNewCat[[i]]]) 
    }
    
    
    
    if(i_model == "NoMod"){
      dataTrain.ML.v  <- ifelse(dataTrain.ML[,"y"] == positiveLabel, 1, 0)
      print(dataTrain.ML.v)
      dataTest.ML.v  <- ifelse(dataTest.ML[,"y"] == positiveLabel, 1, 0)
      print(dataTest.ML.v )
      dataTrain.ML$y  <- factor(dataTrain.ML.v)
      dataTest.ML$y  <- factor(dataTest.ML.v)
      
      
      
      res.glm <- glm(y ~.,
                     family = binomial(),
                     data = dataTrain.ML)
      
      #prediction
      glm.pred <- predict.glm(res.glm, type = "response")
      
      
      #  Create struc of variable importants
      
      if( i %in% rownames(df.Impt)){
        varImpor = varImp (res.glm, scale = FALSE)
        df.Impt[i,rownames(varImpor)] = round(varImpor$Overall,2)
      }
      
      ROCVAL = cbind(ROCVAL,data.frame(glm.pred))
      colnames(ROCVAL) = c(colnames(ROCVAL)[-ncol(ROCVAL)],i)
      
      
      Youden = as.numeric(c())
      lab.predt <- function(t) as.vector(ifelse(glm.pred > t , 1,0))
      thres = seq(.1, .9, by = 0.01)
      for (threshold in thres){
        cm<-caret::confusionMatrix(as.factor(lab.predt(threshold)),as.factor(dataTest.ML.v),positive = "1")
        
        Sp = cm$byClass['Specificity']
        Ss = cm$byClass['Sensitivity']
        y = Sp+Ss-1
        Youden = append(Youden,y)
      }
      a = caret::confusionMatrix(as.factor(lab.predt(thres[which.max(Youden)])), as.factor(dataTest.ML.v),positive = "1")
      print(a)
      rpartROC = roc(y ~ predict.glm(res.glm, type = "response"), data = dataTrain.ML,levels = c(0, 1),direction = "<")
      
      
      Concord 		= append(Concord,round(Concordance(as.numeric(as.character(ifelse(dataTrain.ML$y == "1",1,0))), glm.pred)$Concordance,3))
      Specificity 	= append(Specificity,round(a$byClass['Specificity'],2))
      Precision 	= append(Precision,round(a$overall['Accuracy'],2))
      Sensitivity 	= append(Sensitivity,round(a$byClass['Sensitivity'],2))
      PPV 			= append(PPV,round(a$byClass['Pos Pred Value'],2))
      NPV 			= append(NPV,round(a$byClass['Neg Pred Value'],2))
      TP 			= append(TP,round(a$table[2,2],0))
      TN 			= append(TN,round(a$table[1,1],0))
      FP 			= append(FP,round(a$table[2,1],0))
      FN 			= append(FN,round(a$table[1,2],0))
      AUC 			= append(AUC,round(ci.auc(rpartROC)[2],2))
      KAPPA 		= append(KAPPA,round(a$overall["Kappa"],2))
      min_ci_auc 	= append(min_ci_auc,round(ci.auc(rpartROC)[1],2))
      max_ci_auc 	= append(max_ci_auc,round(ci.auc(rpartROC)[3],2))
    }
    
    
    
    else if(i_model %in% c('glm','nb','lda','knn','gbm')){
      
      
      set.seed(1)
      if (i_model == 'gbm') {
        
        fit <- train(dataTrain.ML[,-1], as.factor(dataTrain.ML[,"y"]), 
                     method='gbm', 
                     trControl=trCtr,  
                     metric = "ROC",
                     preProc = c("center", "scale"))
        print(fit)
        
      }
      else fit <- train(y ~.,
                        data      = dataTrain.ML,
                        method    = i_model  ,
                        trControl = trCtr)
      
      
      
      if( i %in% rownames(df.Impt)){
        
        
        if(i_model == 'gbm') 
        {
          summa = summary(fit)
          df.Impt[i,levels(summa$var)] = round(summa$rel.inf,2)
        }
        else if(i_model == 'glm') {
          varImpor = varImp (fit, scale = FALSE)
          df.Impt[i,rownames(varImpor$importance)] = round(varImpor$importance$Overall,2)   
        }
        else{
          varImpor = varImp (fit, scale = FALSE)
          df.Impt[i,rownames(varImpor$importance)] = round(varImpor$importance[,positiveLabel],2)   
        }
      }
      
      Youden = as.numeric(c())
      fitpred <- as.vector(predict(fit,dataTest.ML,type = "prob"))
      
      
      fitpredt <- function(t) as.factor(ifelse(fitpred[,positiveLabel] > t , positiveLabel,negative_label))
      
      
      
      ROCVAL = cbind(ROCVAL,data.frame(fitpred[,positiveLabel]))
      colnames(ROCVAL) = c(colnames(ROCVAL)[-ncol(ROCVAL)],i)
      print(ROCVAL)
      
      
      
      thres = seq(.1, .9, by = 0.01)
      for (threshold in thres){
        cm<-caret::confusionMatrix(as.factor(fitpredt(threshold)),as.factor(dataTrain.ML[,"y"]),positive = positiveLabel)
        
        Sp = cm$byClass['Specificity']
        Ss = cm$byClass['Sensitivity']
        youd = Sp+Ss-1
        Youden = append(Youden,youd)
      }
      
      Concord = append(Concord,round(Concordance(as.numeric(as.character(ifelse(dataTrain.ML[,"y"] == positiveLabel,1,0))), fitpred)$Concordance,2))
      
      
      
      a = caret::confusionMatrix(as.factor(fitpredt(thres[which.max(Youden)])), as.factor(dataTrain.ML[,"y"]),positive = positiveLabel)
      print(a)
      
      
      rpartROC <- roc(y ~ predict(fit, type = "prob")[,positiveLabel], data = dataTest.ML,levels = c(positiveLabel, negative_label),direction = ">")
      Specificity = append(Specificity,round(a$byClass['Specificity'],2))
      Precision = append(Precision,round(a$overall['Accuracy'],2))
      Sensitivity = append(Sensitivity,round(a$byClass['Sensitivity'],2))
      PPV = append(PPV,round(a$byClass['Pos Pred Value'],2))
      NPV = append(NPV,round(a$byClass['Neg Pred Value'],2))
      TP = append(TP,round(a$table[1,1],0))
      TN = append(TN,round(a$table[2,2],0))
      FP = append(FP,round(a$table[1,2],0))
      FN = append(FN,round(a$table[2,1],0))
      AUC = append(AUC,round(ci.auc(rpartROC)[2],2))
      KAPPA = append(KAPPA,round(a$overall["Kappa"],2))
      min_ci_auc = append(min_ci_auc,round(ci.auc(rpartROC)[1],2))
      max_ci_auc = append(max_ci_auc,round(ci.auc(rpartROC)[3],2))
    } 
    else if(i_model == 'cforest'){
      ctrl <- trainControl(method = "cv", number = 10, repeats = 3,
                           classProbs = TRUE, summaryFunction = twoClassSummary,
                           allowParallel = TRUE)
      set.seed(1)
      train.cf <- train(y ~ .,   
                        data=dataTrain.ML,
                        method="cforest",
                        trControl=ctrl) 
      cf.probs = predict(train.cf,dataTrain.ML,type="prob")
      
      ROCVAL = cbind(ROCVAL,data.frame(cf.probs[,positiveLabel]))
      colnames(ROCVAL) = c(colnames(ROCVAL)[-ncol(ROCVAL)],i)
      
      if( i %in% rownames(df.Impt)){
        varImpor = varImp (train.cf, scale = FALSE)
        df.Impt[i,rownames(varImpor$importance)] = varImpor$importance$Overall
      }
      fitpredt <- function(t) as.factor(ifelse(cf.probs[,positiveLabel] > t , positiveLabel,negative_label))
      Youden = as.numeric(c())
      thres = seq(.1, .9, by = 0.01)
      
      for (threshold in thres){
        # 
        cm<-caret::confusionMatrix(as.factor(fitpredt(threshold)),as.factor(dataTest.ML[,"y"]),positive = positiveLabel)
        
        Sp = cm$byClass['Specificity']
        Ss = cm$byClass['Sensitivity']
        y = Sp+Ss-1
        Youden = append(Youden,y)
      }
      a = caret::confusionMatrix(as.factor(fitpredt(thres[which.max(Youden)])), as.factor(dataTest.ML[,"y"]),positive = positiveLabel)
      print(a)
      rocCurve.cf <- roc(dataTest.ML[,"y"],cf.probs[,positiveLabel],levels = c(negative_label,positiveLabel),direction = "<")
      
      
      Concord = append(Concord,round(Concordance(as.numeric(as.character(ifelse(dataTest.ML[,"y"] == positiveLabel,1,0))), cf.probs)$Concordance,2))
      
      AUC = append(AUC,round(ci.auc(rocCurve.cf)[2],2))
      
      Specificity = append(Specificity,round(a$byClass['Specificity'],2))
      Precision = append(Precision,round(a$overall['Accuracy'],2))
      Sensitivity = append(Sensitivity,round(a$byClass['Sensitivity'],2))
      PPV = append(PPV,round(a$byClass['Pos Pred Value'],2))
      NPV = append(NPV,round(a$byClass['Neg Pred Value'],2))
      TP = append(TP,round(a$table[1,1],0))
      TN = append(TN,round(a$table[2,2],0))
      FP = append(FP,round(a$table[1,2],0))
      FN = append(FN,round(a$table[2,1],0))
      KAPPA = append(KAPPA,round(a$overall["Kappa"],2))
      
      min_ci_auc = append(min_ci_auc,round(ci.auc(rocCurve.cf)[1],2))
      max_ci_auc = append(max_ci_auc,round(ci.auc(rocCurve.cf)[3],2))
      
    }
    
    
    else if(i_model == "svmRadial"){
      
      ctrl <- trainControl(method = "cv", number = 10, repeats = 3,
                           classProbs = TRUE, summaryFunction = twoClassSummary,
                           allowParallel = TRUE)
      set.seed(2)
      svmGrid = expand.grid(
        .sigma = as.numeric(sigest(y ~.,data = dataTrain.ML,scaled=FALSE)),
        .C = c(0.1, 0.25,0.5, 1,10)
      )
      
      svmTuned = train(
        y ~ .,
        data = dataTrain.ML,
        method = "svmRadial",
        tuneGrid = svmGrid,
        metric = "ROC",
        trControl = ctrl,
        preProcess = NULL,
        scaled = FALSE,
        fit = FALSE)
      
      
      svm_model = ksvm(y ~ .,
                       data = dataTrain.ML,
                       kernel = "rbfdot",
                       kpar = list(sigma=svmTuned$bestTune$sigma),
                       C = svmTuned$bestTune$C,
                       prob.model = TRUE,
                       scaled = FALSE)
      
      predict_loan_status_svm = predict(svm_model,dataTrain.ML,type="decision")
      
      
      if( i %in% rownames(df.Impt)){
        M <- rminer::fit(y~., data=dataTrain.ML, model="svm", kpar=list(sigma=svmTuned$bestTune$sigma), C=svmTuned$bestTune$C)
        df.Impt[i,colnames(dataTrain.ML[,-1])] = Importance(M, data=dataTrain.ML)$imp
      }
      
      rocCurve_svm = roc(response = as.factor(dataTrain.ML[,"y"]),
                         predictor = predict_loan_status_svm)
      auc_curve = auc(rocCurve_svm)
      
      ROCVAL = cbind(ROCVAL,predict_loan_status_svm)
      colnames(ROCVAL) = c(colnames(ROCVAL)[-ncol(ROCVAL)],i)
      
      cm1 = caret::confusionMatrix(as.factor(ifelse(predict_loan_status_svm < median(predict_loan_status_svm), positiveLabel,negative_label)),dataTrain.ML[,"y"]) 
      cm2 = caret::confusionMatrix(as.factor(ifelse(predict_loan_status_svm < mean(predict_loan_status_svm), positiveLabel,negative_label)),dataTrain.ML[,"y"]) 
      
      if (cm1$overall["Kappa"]>cm2$overall["Kappa"]) a = cm1 else a = cm2
      print(a)
      Concord = append(Concord,round(1-Concordance(as.numeric(as.character(ifelse(dataTrain.ML[,"y"] == positiveLabel,1,0))), predict_loan_status_svm)$Concordance,2))
      
      
      KAPPA = append(KAPPA,round(a$overall['Kappa'],2))
      AUC = append(AUC,round(auc_curve,2))
      Specificity = append(Specificity,round(a$byClass['Specificity'],2))
      Precision = append(Precision,round(a$overall['Accuracy'],2))
      Sensitivity = append(Sensitivity,round(a$byClass['Sensitivity'],2))
      PPV = append(PPV,round(a$byClass['Pos Pred Value'],2))
      NPV = append(NPV,round(a$byClass['Neg Pred Value'],2))
      TP = append(TP,a$table[1,1])
      TN = append(TN,a$table[2,2])
      FP = append(FP,a$table[1,2])
      FN = append(FN,a$table[2,1])
      
      min_ci_auc = append(min_ci_auc,round(ci.auc(rocCurve_svm)[1],2))
      max_ci_auc = append(max_ci_auc,round(ci.auc(rocCurve_svm)[3],2))
    }
    
    
    else{
      set.seed(1)
      fit <- train(y ~.,
                   data      = dataTrain.ML,
                   method    = i_model  ,
                   trControl = trainControl(method = "cv",
                                            number = ,
                                            savePredictions = TRUE,allowParallel = FALSE))
      
      Youden = as.numeric(c())
      fitpred <- as.vector(predict(fit,dataTest.ML,type = "prob"))
      fitpredt <- function(t) as.factor(ifelse(fitpred[,positiveLabel] > t , positiveLabel,negative_label))
      
      
      ROCVAL = cbind(ROCVAL,data.frame(fitpred[,positiveLabel]))
      colnames(ROCVAL) = c(colnames(ROCVAL)[-ncol(ROCVAL)],i)
      
      thres = seq(.1, .9, by = 0.01)
      for (threshold in thres){
        cm<-caret::confusionMatrix(as.factor(fitpredt(threshold)),as.factor(dataTest.ML[,"y"]),positive = positiveLabel)
        
        Sp = cm$byClass['Specificity']
        Ss = cm$byClass['Sensitivity']
        youd = Sp+Ss-1
        Youden = append(Youden,youd)
      }
      
      
      if( i %in% rownames(df.Impt)){
        varImpor = varImp (fit, scale = FALSE)
        df.Impt[i,rownames(varImpor$importance)] = round(varImpor$importance[,positiveLabel],2)  
        
      }
      
      Concord = append(Concord,round(Concordance(as.numeric(as.character(ifelse(dataTrain.ML[,"y"] == positiveLabel,1,0))), fitpred)$Concordance,2))
      
      
      a = caret::confusionMatrix(as.factor(fitpredt(thres[which.max(Youden)])), as.factor(dataTest.ML[,"y"]),positive = positiveLabel)
      
      rpartROC <- roc(y ~ predict(fit, type = "prob")[,positiveLabel], data = dataTest.ML,levels = c(positiveLabel, negative_label),direction = ">")
      
      print(a)
      
      Specificity = append(Specificity,round(a$byClass['Specificity'],2))
      Precision = append(Precision,round(a$overall['Accuracy'],2))
      Sensitivity = append(Sensitivity,round(a$byClass['Sensitivity'],2))
      PPV = append(PPV,round(a$byClass['Pos Pred Value'],2))
      NPV = append(NPV,round(a$byClass['Neg Pred Value'],2))
      TP = append(TP,round(a$table[2,2],0))
      TN = append(TN,round(a$table[1,1],0))
      FP = append(FP,round(a$table[2,1],0))
      FN = append(FN,round(a$table[1,2],0))
      AUC = append(AUC,round(ci.auc(rpartROC)[2],2))
      KAPPA = append(KAPPA,round(a$overall["Kappa"],2))
      min_ci_auc = append(min_ci_auc,round(ci.auc(rpartROC)[1],2))
      max_ci_auc = append(max_ci_auc,round(ci.auc(rpartROC)[3],2))
    }
    
  }
  
  resPerf = data.frame(
    resPerf, 
    AUC,
    Precision,
    Specificity,
    Sensitivity,
    PPV,
    NPV,
    TP,
    FP,
    TN,
    FN,
    KAPPA,
    Concord 
  )
  
  resultsAUC = data.frame(
    resultsAUC,
    min_ci_auc,
    AUC,
    max_ci_auc
    
  )
  
  
  if(length(df.Impt)>0){  newdf = NULL
  for (i in 1:nrow(df.Impt)) {
    dfVarI_ = NULL
    s = t(df.Impt[i,grpeNewCat[[i]]])
    
    rownt = rownames(s)
    dfVarI = data.frame(s,data.frame(rownt))
    
    
    colnames(dfVarI) = c('Contribution','Parameters')
    
    dfVarI_ = cbind(dfVarI,Categories = rep(rownames(df.Impt)[i],nrow(dfVarI)))
    
    if (i == 1)
      newdf = dfVarI_
    else
      newdf <- rbind(newdf, dfVarI_)
    
  }
  
  newdf2 <- newdf %>%
    arrange(Categories, Contribution) %>%
    mutate(Parameters = factor(Parameters, levels = unique(Parameters)))
  
  if(ncol(ROCVAL)>2){
    ROCVAL <- melt_roc(ROCVAL, "D", colnames(ROCVAL)[-1])
  }else{
    colnames(ROCVAL) = c("D","M1")
  }
  }else newdf2 = NULL
  
  
  return(list(res = resPerf,ROCVALs = ROCVAL, resultsAUCPlot = resultsAUC,df.Impts = newdf2))
}