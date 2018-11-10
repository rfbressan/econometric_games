# Funcoes auxiliares para os templates
library(magrittr)

# adf_table ---------------------------------------------------------------

# Funcao para criar uma tabela Latex com os resultados do teste ADF do 
# pacote URCA

table_adf = function(DF, ...){
  # Stores ... arguments as list
  var_list = as.list(unlist(list(...)))
  
  # Subset DF based on var_list
  if(length(var_list) == 0){
    subset = DF
    var_list = as.list(colnames(DF))
  }else{
    subset = select_(DF, .dots = var_list)
  }
  
  # Run model for each variable, and stored as list of lists, each element of the 
  # outer list contains a list of the variable's drift and trend estimates
  tests = lapply(subset, function(x){
    out1 = urca::ur.df(x, type = "drift", selectlags = "BIC")
    out2 = urca::ur.df(x, type = "trend", selectlags = "BIC")
    return(list(out1, out2))
  })
  
  # Store each model estimates in to a dataframe with corresponding variable names
  est_df = lapply(tests, function(x){
    data.frame(drift = x[[1]]@teststat[1], trend = x[[2]]@teststat[1])
  })
  
  # Combine the list into a single dataframe
  est_table = do.call(rbind, est_df)
  
  critical_vals = data.frame(tests[[1]][[1]]@cval[1,], 
                             tests[[1]][[2]]@cval[1,])
  
  # Check for significance level and add *'s accordingly
  est_table = Map(function(x, y){
    ifelse(x < y[1], paste(round(x, 4), "***"), 
           ifelse(x < y[2], paste(round(x, 4), "**"),
                  ifelse(x < y[3], paste(round(x, 4), "*"), 
                         as.character(round(x, 4)))))
  }, est_table, critical_vals) %>% data.frame(row.names = unlist(var_list))
  
  # Set footnotes
  footnote1 = paste("ADF critical values drift:", paste(critical_vals[[1]], collapse = " "))
  footnote2 = paste("ADF critical values trend:", paste(critical_vals[[2]], collapse = " "))
  
  # Create nice looking table with critical values  
  stargazer::stargazer(est_table, type = "latex", summary = FALSE,
                       notes = c(footnote1, footnote2),
                       header = FALSE,
                       title = "Testes de raiz unitária ADF.")
}

# table_joh ---------------------------------------------------------------

# Johansen teste
table_joh = function(DF, ...){
  # Stores ... arguments as list
  var_list = as.list(unlist(list(...)))
  
  # Subset DF based on var_list
  if(length(var_list) == 0){
    subset = DF
    var_list = as.list(colnames(DF))
  }else{
    subset = select_(DF, .dots = var_list)
  }
  
  # Run model for each variable, and stored as list of lists, each element of the 
  # outer list contains a list of the variable's drift and trend estimates
  traco <-  urca::ca.jo(subset, type = "trace", ecdet = "none", spec = "longrun")
  autovalor  <-  urca::ca.jo(subset, type = "eigen", ecdet = "none", spec = "longrun")
  tests <- list(traco = traco, autovalor = autovalor)
  
  # Statistics values
  stat_l <- lapply(tests, function(x){
    x@teststat
  })
#  stat <- do.call(cbind, stat_l)
  
  # Critical values for each rank and each test
  cval_l <- lapply(tests, function(x){
    t(x@cval)
  })
#  cval <- do.call(rbind, cval_l)
  
  # Check for significance level and add *'s accordingly
  asterisk <- function(stat, cval){
    ans <- data.frame(matrix(rep(NA_character_, length(stat)), ncol = 1),
                      stringsAsFactors = FALSE)
    for (i in seq_along(stat)) {
      if (stat[i] > cval[3, i]) {
        ans[i, 1] <- paste(round(stat[i], 4), "***")
      } else if (stat[i] > cval[2, i]) {
        ans[i, 1] <- paste(round(stat[i], 4), "**")
      } else if (stat[i] > cval[1, i]) {
        ans[i, 1] <- paste(round(stat[i], 4), "*")
      } else {
        ans[i, 1] <- as.character(round(stat[i], 4))
      }
    }
    return(ans)
  } 
  
  # Gera lista com as estimativas com *
  est_l <- Map(asterisk, stat_l, cval_l)
  
  # Data Frame com as estatisticas
  est_table <- do.call(cbind, est_l)
  rnames <- gsub(" \\|", "", row.names(tests[[1]]@cval))
  row.names(est_table) <- rnames
  colnames(est_table) <- names(tests)
  # Set footnotes
  footnote1  <- "Significância: *** 1\\%, ** 5\\%, * 10\\%"
  #footnote2 = paste("ADF critical values trend:", paste(critical_vals[[2]], collapse = " "))
  
  # Create nice looking table with critical values  
  stargazer::stargazer(est_table, type = "latex", summary = FALSE,
                       notes = c(footnote1),
                       header = FALSE,
                       title = "Teste de cointegração de Johansen")
}

# table_eg ---------------------------------------------------------------

# Funcao para criar uma tabela Latex com os resultados do teste Engle-Granger
# 
table_eg = function(DF, ...){
  # Stores ... arguments as list
  var_list = as.list(unlist(list(...)))
  
  # Subset DF based on var_list
  if(length(var_list) == 0){
    subset = DF
    var_list = as.list(colnames(DF))
  }else{
    subset = select_(DF, .dots = var_list)
  }
  
  # Run model for each variable, and stored as list of lists, each element of the 
  # outer list contains a list of the variable's drift and trend estimates
  tests = lapply(subset, function(x){
    out1 = urca::ur.df(x, type = "none", selectlags = "BIC")
    out2 = urca::ur.df(x, type = "drift", selectlags = "BIC")
    return(list(out1, out2))
  })
  
  # Store each model estimates in to a dataframe with corresponding variable names
  est_df = lapply(tests, function(x){
    data.frame(constant = x[[1]]@teststat[1], drift = x[[2]]@teststat[1])
  })
  
  # Combine the list into a single dataframe
  est_table = do.call(rbind, est_df)
  
  critical_vals = data.frame(c(-3.39, -2.76, -2.45), 
                             c(-3.96, -3.37, -3.07))
  
  # Check for significance level and add *'s accordingly
  est_table = Map(function(x, y){
    ifelse(x < y[1], paste(round(x, 4), "***"), 
           ifelse(x < y[2], paste(round(x, 4), "**"),
                  ifelse(x < y[3], paste(round(x, 4), "*"), 
                         as.character(round(x, 4)))))
  }, est_table, critical_vals) %>% data.frame(row.names = unlist(var_list))
  
  # Set footnotes
  footnote1 = paste("ADF critical values constant:", paste(critical_vals[[1]], collapse = " "))
  footnote2 = paste("ADF critical values drift:", paste(critical_vals[[2]], collapse = " "))
  
  # Create nice looking table with critical values  
  stargazer::stargazer(est_table, type = "latex", summary = FALSE,
                       notes = c(footnote1, footnote2),
                       header = FALSE,
                       title = "Testes de Engle-Granger.")
}

