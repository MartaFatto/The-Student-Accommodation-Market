#######################################################################
### Function for predicting preference shares from a MNL model with ### 
### bootstrap percentiles prediction intervals                      ### 
#######################################################################

# model: mlogit object returned by mlogit()
# data: a data frame containing the set of designs for which you want to 
#       predict shares.  Same format of the data used to estimate model. 
# nsim: number of bootstrap samples, default is 500
# conflevel: desired confidence level, default is 0.95
# library "parallel" is necessary

BootCI.predict.mnl <- function(model, data, nsim=500, conflevel=0.95) {
  dataModel <- model$model
  dataModel$probabilities <- NULL
  dataModel$linpred <- NULL
  idx <- dataModel$idx 
  dataModel$idx <- NULL
  dataModel <- data.frame(dataModel, idx)
  idVar <- unique(dataModel[,names(idx)[1]])
  
  bootstrapping <- function(x) {
    idbootsamp <- data.frame(sample(idVar, replace=T))
    names(idbootsamp) <- names(idx)[1]
    bootsamp <- merge(idbootsamp, dataModel, by=names(idx)[1], all.x=T)
    bootsamp[,names(idx)[1]] <- rep(1:length(table(idx[,1])), each=length(table(idx[,3])))
    bootsamp.mlogit  <- dfidx(bootsamp, idx = list(c(names(idx)[1:2]), names(idx)[3]),
                              drop.index=F)    
    bootfit <- update(model, data = bootsamp.mlogit)
    data.model <- model.matrix(update(bootfit$formula, 0 ~ .), data = data)[,-1]
    logitUtility <- data.model%*%bootfit$coef
    share <- exp(logitUtility)/sum(exp(logitUtility))
    share
  }
  
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, library(mlogit))
  clusterExport(cl, varlist=c("idVar", "dataModel", "idx", "model", "data"), 
                envir=environment())
  bootdistr <- parLapply(cl, 1:nsim, fun=bootstrapping)
  stopCluster(cl)
  
  bootdistr <- do.call(cbind, bootdistr)
  lowl <- (1-conflevel)/2
  upl <- 1-lowl  
  bootperc <- t(apply(bootdistr, 1, function(x) quantile(x, probs=c(lowl, upl))))
  pointpred <- predict.mnl(model, data)
  predictedShares <- cbind(pointpred[,1], bootperc, pointpred[,2:ncol(pointpred)])
  names(predictedShares)[1] <- "share" 
  predictedShares
}