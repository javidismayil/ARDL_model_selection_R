library(readxl)
data<-ts(data.frame(read_xlsx("C:/Users/PC/Desktop/veri.xlsx")), start=c(2008,10), frequency = 12)
dependet<-data[,1:2]
independent<-data[,3:10]
ardl_model_selection<-function(data, dependet, independent, order){
  

  library(stats)
  library(lmtest)
  library(tictoc)
  library(tseries)
  library(ARDL)
  library(purrr)
  tic()

  dependet_names<-colnames(dependet)
  independent_names<-colnames(independent)
  all_combination <- map(1:length(independent_names), 
                          ~ combn(independent_names, .x, simplify = FALSE)) %>%
    unlist(recursive = FALSE)
  
  all_formula<-list()
  all_models <- list()
  for (dep in 1:length(dependet_names)){
    for (indep in 1:length(all_combination)){
      formulaa<-as.formula(paste(dependet_names[dep][[1]], "~", paste(all_combination[indep][[1]], collapse = "+")))
      all_formula<-append(all_formula, formulaa)
      }
  }
  m=1
  summodel<-c()
  correct_model<-c()
  for (i in all_formula){
    model<-auto_ardl(i, data, max_order = order)
    modelA<-ARDL::ardl(i, data, order=c(model$best_order))
    sum<-summary(modelA)
    bound_test<-bounds_f_test(modelA,case=2)
    jb<-jarque.bera.test(modelA$residuals)
    gq<-gqtest(modelA)
    bg<-bgtest(modelA, order=2)
    if (bg$p.value>0.05 & jb$p.value>0.05 & gq$p.value>0.05 & bound_test$p.value<0.05){
      all_models[[m]]<-modelA
      summodel[[m]]<-summary(modelA)
      correct_model[[m]]=i
      m=m+1
      #all_models<-append(all_models, modelA)
      print(i)
    }
  }
  which(all_formula==i)
  rsquared<-c()
  for (i in 1:length(all_models)){
    print(i)
     rsquared[[i]]<-summodel[[i]]$r.squared
    }
  best<-which.max(rsquared)
  output<-list(`best model`=all_models[[best]], `rsquare`=summodel[[best]]$r.squared, 
               `output`=summodel[[best]], `all_model`=all_models, `formulas`=correct_model)
  toc()
  return(output)
}



   