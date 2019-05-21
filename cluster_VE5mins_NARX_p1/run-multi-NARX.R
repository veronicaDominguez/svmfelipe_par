#script.dir <- dirname(sys.frame(1)$ofile)
script.dir<-"C:/Users/Usuario/Google Drive/USACH/TESIS/programas/cluster_VE5mins_NARX_p1"
PARALLEL.SCRIPT.BASENAME <- paste("Par", "process-multi-NARX", sep = "-")
PARALLEL.SCRIPT.BASENAME <- paste(PARALLEL.SCRIPT.BASENAME, "R", sep = ".")
PARALLEL.SCRIPT.NAME <- file.path(script.dir, PARALLEL.SCRIPT.BASENAME)
source(PARALLEL.SCRIPT.NAME)

library(doParallel)
library(e1071)


process.parallel <- function(
  src.dir,
  tgt.dir,
  src.ext,
  tgt.ext,
  parametros,
  lags,
  keep.nstats,
  src.basename,
  export.names = export.names,
  export.packages = export.packages
){

  tgt.basename <- paste(src.basename, tgt.ext, sep = ".")
  tgt.file <- file.path(tgt.dir, tgt.basename)
  print("dentro de process parallel")
  print("antes del lags.df")
  lags.df <- expand.grid(lags, KEEP.OUT.ATTRS = FALSE)
  print("antes del lags.results linea 30")
  lags.results <- foreach(
     lag.list = t(lags.df),
    .inorder = TRUE,
    .export = export.names,
    .packages = export.packages,
    .errorhandling = "pass",
    .verbose = FALSE
  ) #%dopar%
    get.results(
      parametros,
      lags = lag.list,
      src.basename = src.basename,
      src.dir = src.dir,
      src.ext = src.ext,
      keep.nstats = keep.nstats
    )
    print("antes del lags.results")
  lags.results <- do.call(rbind, lags.results)
  
  i <- order(
    -lags.results["test.cor"]
  )
  lags.results <- lags.results[i, ]
  if(nrow(lags.results) > keep.nstats)
  {
    i <- 1:keep.nstats
    lags.results <- lags.results[i, ]
  }
  
  rownames(lags.results) <- NULL
  saveRDS(lags.results, file = tgt.file, compress = FALSE)
}

run <- function(
  #nworkers = detectCores(),
  nworkers=2,
  src.basenames = c(
                    'P040-VE_'
                     
                    ),
  src.ext = "txt",
  tgt.ext = "RData",
  src.folder = "Data",
  results.folder = "Results",
  type.folder = "Multivariado",
  model.folder = "NARX"
)
{
  src.dir <- file.path(script.dir, src.folder)
  tgt.dir <- file.path(script.dir, results.folder,type.folder,model.folder)
  dir.create(tgt.dir, showWarnings = FALSE, recursive = TRUE)

  print("antes de los valores de las variables")
  sigma <- 2^seq(-4, 12, 1)
  gamma <- 1 / (2*sigma ^ 2)
  cost <- 2^seq(-2, 14, 1)
  nu <- seq(0.1, 0.9, 0.1)
  #lags <- list(MABP = 1:8,CrCP= 1:8,CBFV = 1:6,fold = 1:2)
  lags <- list(MABP = 1:8,CBFV = 1:6,fold = 1:2)
  print("despues de la lista para los lags")
  
  parametros <- list(
                  gamma = gamma,
                  nu = nu,
                  cost = cost
                )
  print("despues de los parametros")
  export.names <- c(
    'get.results',
    'retardos_multi',
    'training',
    'eval.model',
    'lag.abp'
  )
  
  print("linea 103 run")
  export.packages <- c("e1071")
  
  cluster <- makeCluster(nworkers)
  registerDoParallel(cluster)
  start1.time <- Sys.time()
  print("antes del for linea 109")
  for (src.basename in src.basenames) {
    cat("instance ", src.basename, "\n")
    print("antes del process parallel linea 112")
    process.parallel(
      src.dir = src.dir,
      tgt.dir = tgt.dir,
      src.ext = src.ext,
      tgt.ext = tgt.ext,
      parametros = parametros,
      lags = lags,
      keep.nstats = 10000,
      src.basename = src.basename,
      export.names = export.names,
      export.packages = export.packages
    )
    print("despues del process parallel linea 125")
  }
  print("antes del end1 linea 127")
  end1.time <-Sys.time()
  print(end1.time-start1.time)
  stopCluster(cluster)
}
