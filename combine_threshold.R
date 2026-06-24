library(magrittr)
library(purrr)
library(abind)
library(dplyr)
load_as_list <- function(fname) {
  e <- new.env()
  load(fname, e)
  as.list(e)
}
ages <- seq(0,120,by=0.1)
grid_scaled <- list(
  (ages - 60)/6,
  (ages - 72)/6,
  (ages - 82)/4,
  (ages - 60)/3.75
)
# for (prefix in c("flex_CIs", "S_CIs", "para_CIs", "sflex_CIs", "sS_CIs")) {
#   for(setting in 1:4){
#     for(truth in 1:4){
#       filenames <- 1:100 %>% sprintf(paste('archive/', prefix, "_%03d_", setting, truth, ".rda", sep = ""), .)
#       comb_list <- lapply(filenames, load_as_list)
#       varnames <- c('Q50','Q25','Q75','Q05','Q95')
#       comb_env <- new.env()
#       for (var in varnames) {
#         assign(
#           var,
#           Reduce(rbind, 
#             lapply(1:100, function(x) get(var, comb_list[[x]]))
#           ),
#           comb_env
#         )
#       }
#       save(list = ls(comb_env), file = paste(prefix, setting, truth, "_Q50.rdata", sep = ""), envir = comb_env)
#     }}}
# 
# for (prefix in c("S_CIs", "sS_CIs")) {
#   for(setting in 1:4){
#     for(truth in 1:4){
#       filenames <- 1:100 %>% sprintf(paste('archive/', prefix, "_%03d_", setting, truth, ".rda", sep = ""), .)
#       comb_list <- lapply(filenames, load_as_list)
#       varnames <- 'turning'
#       comb_env <- new.env()
#       for (var in varnames) {
#         assign(
#           var,
#           Reduce(rbind, 
#                  lapply(1:100, function(x) get(var, comb_list[[x]]))
#           ),
#           comb_env
#         )
#       }
#       save(list = ls(comb_env), file = paste(prefix, setting, truth, "_turning.rdata", sep = ""), envir = comb_env)
#     }}}

mat_name <- list(paste('setting',1:4),
                 c('Logit','Moderate Asym','Highly Asym','Non S'))

for (prefix in c("para_CIs")) {
  bias <- list(Q50=matrix(NA,4,4,dimnames = mat_name),
               Q25=matrix(NA,4,4,dimnames = mat_name),
               Q75=matrix(NA,4,4,dimnames = mat_name),
               Q05=matrix(NA,4,4,dimnames = mat_name),
               Q95=matrix(NA,4,4,dimnames = mat_name),
               inflect=matrix(NA,4,4,dimnames = mat_name)
  )
  var <- bias
  coverage <- bias
  for(setting in 1:4){
    for(truth in 1:4){
      load(file = paste(prefix, setting, truth, "_Q50.rdata", sep = ""))
      true_Q05 <- c(50.27781,51.45975,52.09351,46.40708)[truth]
      true_Q25 <- c(59.50694,60.30865,60.71275,52.99598)[truth]
      true_Q50 <- c(65,65,65,65)[truth]
      true_Q75 <- c(70.49306,68.65995,68.01088,77.00402)[truth]
      true_Q95 <- c(79.72219,73.07728,70.14810,83.59292)[truth]
      
      true_inflect <- c(65,67.93893,70.1481,NA)[truth]
      bias$Q50[setting,truth] <- mean(Q50[,3] - true_Q50)
      var$Q50[setting,truth] <- mean(Q50[,6])
      coverage$Q50[setting,truth] <- mean((true_Q50-Q50[,1])*(true_Q50-Q50[,2])<=0)
      bias$Q25[setting,truth] <- mean(Q25[,3] - true_Q25)
      var$Q25[setting,truth] <- mean(Q25[,6])
      coverage$Q25[setting,truth] <- mean((true_Q25-Q25[,1])*(true_Q25-Q25[,2])<=0)
      bias$Q75[setting,truth] <- mean(Q75[,3] - true_Q75)
      var$Q75[setting,truth] <- mean(Q75[,6])
      coverage$Q75[setting,truth] <- mean((true_Q75-Q75[,1])*(true_Q75-Q75[,2])<=0)
      bias$Q05[setting,truth] <- mean(Q05[,3] - true_Q05)
      var$Q05[setting,truth] <- mean(Q05[,6])
      coverage$Q05[setting,truth] <- mean((true_Q05-Q05[,1])*(true_Q05-Q05[,2])<=0)
      bias$Q95[setting,truth] <- mean(Q95[,3] - true_Q95)
      var$Q95[setting,truth] <- mean(Q95[,6])
      coverage$Q95[setting,truth] <- mean((true_Q95-Q95[,1])*(true_Q95-Q95[,2])<=0)
      bias$inflect[setting,truth] <- mean(Q50[,3] - true_inflect)
      var$inflect[setting,truth] <- mean(Q50[,6])
      coverage$inflect[setting,truth] <- mean((true_inflect-Q50[,1])*(true_inflect-Q50[,2])<=0)
    }}}
lapply(names(bias), function(x) sqrt(bias[[x]]^2+var[[x]])) -> rmse
names(rmse) <- names(bias)

bias <- bias[c('Q05','Q25','Q50','inflect')]
var <- var[c('Q05','Q25','Q50','inflect')]
coverage <- coverage[c('Q05','Q25','Q50','inflect')]
rmse <- rmse[c('Q05','Q25','Q50','inflect')]

sink('para_thresholds.txt')
print('Bias:')
print(bias)
print('Variance:')
print(var)
print('RMSE:')
print(rmse)
print('Coverage:')
print(coverage)
sink()

for (prefix in c("flex", "S", "sflex", "sS")) {
  bias <- list(Q50=matrix(NA,4,4,dimnames = mat_name),
               Q25=matrix(NA,4,4,dimnames = mat_name),
               Q75=matrix(NA,4,4,dimnames = mat_name),
               Q05=matrix(NA,4,4,dimnames = mat_name),
               Q95=matrix(NA,4,4,dimnames = mat_name),
               inflect=matrix(NA,4,4,dimnames = mat_name)
  )
  var <- bias
  coverage <- bias
  for(setting in 1:4){
    for(truth in 1:4){
      load(file = paste(prefix, '_CIs', setting, truth, "_Q50.rdata", sep = ""))
      true_Q05 <- c(50.27781,51.45975,52.09351,46.40708)[truth]
      true_Q25 <- c(59.50694,60.30865,60.71275,52.99598)[truth]
      true_Q50 <- c(65,65,65,65)[truth]
      true_Q75 <- c(70.49306,68.65995,68.01088,77.00402)[truth]
      true_Q95 <- c(79.72219,73.07728,70.14810,83.59292)[truth]
      
      bias$Q50[setting,truth] <- mean(Q50[,3] - true_Q50)
      var$Q50[setting,truth] <- mean(Q50[,6])
      coverage$Q50[setting,truth] <- mean((true_Q50-Q50[,1])*(true_Q50-Q50[,2])<=0)
      bias$Q25[setting,truth] <- mean(Q25[,3] - true_Q25)
      var$Q25[setting,truth] <- mean(Q25[,6])
      coverage$Q25[setting,truth] <- mean((true_Q25-Q25[,1])*(true_Q25-Q25[,2])<=0)
      bias$Q75[setting,truth] <- mean(Q75[,3] - true_Q75)
      var$Q75[setting,truth] <- mean(Q75[,6])
      coverage$Q75[setting,truth] <- mean((true_Q75-Q75[,1])*(true_Q75-Q75[,2])<=0)
      bias$Q05[setting,truth] <- mean(Q05[,3] - true_Q05)
      var$Q05[setting,truth] <- mean(Q05[,6])
      coverage$Q05[setting,truth] <- mean((true_Q05-Q05[,1])*(true_Q05-Q05[,2])<=0)
      bias$Q95[setting,truth] <- mean(Q95[,3] - true_Q95)
      var$Q95[setting,truth] <- mean(Q95[,6])
      coverage$Q95[setting,truth] <- mean((true_Q95-Q95[,1])*(true_Q95-Q95[,2])<=0)
      if(prefix %in% c('S','sS')){
        load(file = paste(prefix, '_CIs', setting, truth, "_turning.rdata", sep = ""))
      true_inflect <- c(65,67.93893,70.1481,NA)[truth]
      bias$inflect[setting,truth] <- mean(turning[,3] - true_inflect)
      var$inflect[setting,truth] <- mean(turning[,6])
      coverage$inflect[setting,truth] <- mean((true_inflect-turning[,1])*(true_inflect-turning[,2])<=0)
      }
    }}
  lapply(names(bias), function(x) sqrt(bias[[x]]^2+var[[x]])) -> rmse
  names(rmse) <- names(bias)
  sink(paste(prefix,'_thresholds.txt',sep=''))
  
  bias <- bias[c('Q05','Q25','Q50','inflect')]
  var <- var[c('Q05','Q25','Q50','inflect')]
  coverage <- coverage[c('Q05','Q25','Q50','inflect')]
  rmse <- rmse[c('Q05','Q25','Q50','inflect')]
  
  print('Bias:')
  print(bias)
  print('Variance:')
  print(var)
  print('RMSE:')
  print(rmse)
  print('Coverage:')
  print(coverage)
  sink()
}
