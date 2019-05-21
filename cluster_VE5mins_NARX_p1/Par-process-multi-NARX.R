training <- function(
  parameter,
  lag,
  fold,
  input.var.names,
  output.var.names,
  signal.train,
  signal.test,
  inputs
)
{
  fmla.str <- paste(inputs, collapse = " + ")
  fmla.str <- paste(output.var.names, "~", fmla.str)
  fmla <- formula(fmla.str)
  params <- list(formula = fmla, data = signal.train$folded.signal, scale = FALSE)
  params <- c(params, list(type = "nu-regression", kernel = "radial"))
  params <- c(params, parameter)
  
  start.time <- Sys.time()
  model <- do.call(svm, params)
  end.time <-Sys.time()
  
  stats <- eval.model(
    parameter,
    lag,
    fold,
    input.var.names,
    output.var.names,
    signal.train,
    signal.test,
    inputs,
    model
  )
  results <- list(models = list(model), stats = stats) 
}
lag.abp <- function(
  abp.step.signal,
  lags
)
{
  max.lag <- max(unlist(lags)) + 1
  indices <- 1:nrow(abp.step.signal)
  lag.mat <- embed(indices, max.lag)
  
  #col.names <- list("MABP","CrCP","CBFV")
  col.names <- list("MABP","CBFV")
  print("despues de los nombres de las columnas")
  columns <- NULL
  lagged.columns.names <- c()
  for(colname in col.names){
    lag.order <- lags[[colname]]
    if(!is.null(lag.order) && lag.order > 0)
      for(i in lag.order:1){
        new.colname <- paste(colname, paste0("lag", i), sep = ".")
        lagged.columns.names <- c(lagged.columns.names, new.colname)
        columns[[new.colname]] <- abp.step.signal[lag.mat[, i + 1], colname]
      }
    columns[[colname]] <- abp.step.signal[lag.mat[, 1], colname]
  }
  X<-data.frame(columns)
  sorting <- order(lag.mat[, 1])
  X <- X[sorting, ]
}
eval.model <- function(
  parameter,
  lag,
  fold,
  input.var.names,
  output.var.names,
  signal.train,
  signal.test,
  inputs,
  model
){
  fitted.signal <- round(model[["fitted"]], 4)
  train.cor <- cor(fitted.signal, signal.train[['folded.signal']]['CBFV'])
  
  x <- signal.test[['folded.signal']]['MABP']
  y <- signal.test[['folded.signal']]["CBFV"]
  #z <- signal.test[['folded.signal']]["CrCP"]
  mean.cbfv <- mean(y$CBFV)
  
  abp.step.signal <- data.frame(
    MABP = x,
    #CrCP = z,
    CBFV= round(mean.cbfv,4)
  )
  
  
  
  
  lag.abp.step <- lag.abp(abp.step.signal, lag)
  
  lag.abp.step <- lag.abp.step[inputs]
  
  valor <- as.integer(min(unlist(lag)))
  
  columns <- sum(unlist(lag)) + 2
  
  
  response.ARX.model <- NULL
  for (indice in 1:nrow(lag.abp.step)){
    #print(lag.abp.step[indice,1:columns])
    step.response <- predict(model, lag.abp.step[indice,1:columns])
    response.ARX.model[indice] <- round(step.response,4)
    
    if(lag[["CBFV"]]>1){
      for(b in lag[["CBFV"]]:2){
        var.name.old <- paste0("CBFV.lag",b)
        var.name.new <- paste0("CBFV.lag",b-1)
        lag.abp.step[indice+1,var.name.old] <- lag.abp.step[indice,var.name.new]
      }
      lag.abp.step[indice+1,"CBFV.lag1"] <- round(step.response,4)
    }else{
      var.name <- "CBFV.lag1"
      lag.abp.step[indice+1,var.name] <- round(step.response,4)
    }
  }
  
  
  start <- max(unlist(lag)) +1
  CBFV_real <- y$CBFV[start:length(y$CBFV)]
  
  test.cor <- cor(response.ARX.model,CBFV_real)
  
  data.frame(
    MABP = lag["MABP"],
    #CrCP = lag["CrCP"],
    CBFV = lag["CBFV"],
    gamma = parameter['gamma'],
    nu = parameter['nu'],
    cost = parameter['cost'],
    train.cor = train.cor,
    test.cor = test.cor,
    fold = fold
  )
}





# eval.model <- function(
#   parameter,
#   lag,
#   fold,
#   input.var.names,
#   output.var.names,
#   signal.train,
#   signal.test,
#   inputs,
#   model
# ){
#   fitted.signal <- round(model[["fitted"]], 4)
#   train.cor <- cor(fitted.signal, signal.train[['folded.signal']]['CBFV'])
#   train.cor <- round(train.cor, 3)
#   
#   x <- signal.test[['folded.signal']][, inputs]
#   y <- signal.test[['folded.signal']]["CBFV"]
#   pred <- predict(model, x)
#   pred <- round(pred, 3)
#   test.cor <- cor(pred, y)
#   test.cor <- round(test.cor, 3)
#   
#   data.frame(
#     MABP = lag["MABP"],
#     CrCP = lag["CrCP"],
#     CBFV = lag["CBFV"],
#     gamma = parameter['gamma'],
#     nu = parameter['nu'],
#     cost = parameter['cost'],
#     train.cor = train.cor,
#     test.cor = test.cor,
#     fold = fold
#   )
# }
retardos_multi <- function(
  arch_entrada,
  lags
)
{
  #signal <- read.table(arch_entrada, col.names = c("MABP","CrCP","CBFV") )
  signal <- read.table(arch_entrada, col.names = c("MABP","CBFV") )
  
  signal[['MABP']] <- round(signal[['MABP']],4)
  #signal[['CrCP']] <- round(signal[['CrCP']],3)
  signal[['CBFV']] <- round(signal[['CBFV']],4)
  
  max.lag <- max(unlist(lags)) + 1
  indices <- 1:nrow(signal)
  lag.mat <- embed(indices, max.lag)
  
  #col.names <- list("MABP","CrCP","CBFV")
  col.names <- list("MABP","CBFV")
  columns <- NULL
  lagged.columns.names <- c()
  for(colname in col.names){
    lag.order <- lags[[colname]]
    if(!is.null(lag.order) && lag.order > 0)
      for(i in lag.order:1){
        new.colname <- paste(colname, paste0("lag", i), sep = ".")
        lagged.columns.names <- c(lagged.columns.names, new.colname)
        columns[[new.colname]] <- signal[lag.mat[, i + 1], colname]
      }
    columns[[colname]] <- signal[lag.mat[, 1], colname]
  }
  folded.signal <- data.frame(columns)
  sorting <- order(lag.mat[, 1])
  folded.signal <- folded.signal[sorting, ]
  list(folded.signal = folded.signal, lagged.columns.names = lagged.columns.names)
}

get.results <- function(
    parametros,
    lags,
    src.basename,
    src.dir,
    src.ext,
    keep.nstats
    
  )
{
  print("dentro de get results")
  parameter <- expand.grid(parametros)
  #folds <- list(fold1 = 'p1', fold2 = 'p2')
  folds <- list(fold1 = '01der', fold2 = '02der')
  #folds <- list(fold1 = '01izq', fold2 = '02izq')
  
  #input.var.names <- c("MABP","CrCP")
  input.var.names <- c("MABP")
  output.var.names <- c('CBFV')
  print("despues del input y output")
  
  
  archivo1 <- paste(src.basename,folds[['fold1']],sep="_")
  archivo1 <- paste(archivo1,src.ext,sep=".")
  
  archivo2 <- paste(src.basename,folds[['fold2']],sep="_")
  archivo2 <- paste(archivo2,src.ext,sep=".")
  
  file.fold1 <- paste(src.dir,archivo1, sep="/")
  file.fold2 <- paste(src.dir,archivo2, sep="/")
  
  print("antes del lag linea 242")
  #lag <- list(MABP = lags[,1][["MABP"]],CrCP = lags[,1][["CrCP"]],CBFV=lags[,1][["CBFV"]])
  lag <- list(MABP = lags[,1][["MABP"]],CBFV=lags[,1][["CBFV"]])
  print("lag")
  
  fold <-lags[,1][["fold"]]
  if(fold == 1){    
    signal.train <- retardos_multi(file.fold1, lag)
    signal.test <- retardos_multi(file.fold2, lag)
  }else{
    signal.train <- retardos_multi(file.fold2, lag)
    signal.test <- retardos_multi(file.fold1, lag)
  }
  
  
  inputs <- c(signal.train[["lagged.columns.names"]],input.var.names)
  inputs <- sort(inputs, decreasing = TRUE)

  output = apply(parameter, 1,               function(p) training(p,lag,fold,input.var.names,output.var.names,signal.train,signal.test,inputs))
  
  models <- sapply(output, function(l) l[[1]])
  stats.list <- lapply(output, function(l) l[[2]])
  stats <- do.call(rbind, stats.list)
  #colnames(stats) <- c("MABP","CrCP","CBFV","gamma","nu","cost","train.cor","test.cor","fold")
  colnames(stats) <- c("MABP","CBFV","gamma","nu","cost","train.cor","test.cor","fold")
  
  i <- order(
    -stats["test.cor"]
  )
  stats <- stats[i, ]
  if(nrow(stats) > keep.nstats)
  {
    i <- 1:keep.nstats
    stats <- stats[i, ]
  }
  
  rownames(stats) <- NULL
  
  stats
}