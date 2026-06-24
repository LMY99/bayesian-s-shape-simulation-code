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
for (prefix in 'bars'){#c("flex_CIs", "S_CIs", "para_CIs", "sflex_CIs", "sS_CIs", "bars")) {
  for(setting in 1:4){
    for(truth in 1:4){
  filenames <- 1:100 %>% sprintf(paste('archive/', prefix, "_%03d_", setting, truth, ".rda", sep = ""), .)
  comb_list <- lapply(filenames, load_as_list)
  varnames <- ifelse(prefix=='bars', 'results', 'CI_repeat') # names(comb_list[[1]])
  comb_env <- new.env()
  for (var in varnames) {
    assign(
      var,
      reduce(lapply(1:100, function(x) cbind(get(var, comb_list[[x]]),Iter=x)), abind, along = 1),
      comb_env
    )
  }
  # truth0 <- case_match(
  #   rep(truth, 1201),
  #   1 ~ 2 * plogis(grid_scaled[[1]]),
  #   2 ~ 2 * plogis(grid_scaled[[2]] * exp(0.5 * grid_scaled[[2]] / sqrt(grid_scaled[[2]]^2+1))),
  #   3 ~ 2 * plogis(grid_scaled[[3]] * exp(1.0 * grid_scaled[[3]] / sqrt(grid_scaled[[3]]^2+1))),
  #   4 ~ plogis(grid_scaled[[4]], location = -4, scale = 1) +
  #     plogis(grid_scaled[[4]], location = +4, scale = 1)
  # )
  # bias <- colMeans(comb_env$CI_repeat[,,1]) - truth0
  # for(i in 1:100){
  #   comb_env$CI_repeat[i,,4] <- truth0
  #   comb_env$CI_repeat[i,,7] <- (comb_env$CI_repeat[i,,1] - truth0)^2
  #   comb_env$CI_repeat[i,,6] <- comb_env$CI_repeat[i,,7] + comb_env$CI_repeat[i,,8]
  # }
  save(list = ls(comb_env), file = paste(prefix, setting, truth, ".rdata", sep = ""), envir = comb_env)
    }}}

for (prefix in c("flex_CIs", "S_CIs", "para_CIs", "sflex_CIs", "sS_CIs")) {
  for(setting in 1:4){
    for(truth in 1:4){
      filenames <- 1:100 %>% sprintf(paste('archive/', prefix, "_%03d_", setting, truth, ".rda", sep = ""), .)
      comb_list <- lapply(filenames, load_as_list)
      varnames <- names(comb_list[[1]])
      comb_env <- new.env()
      for (var in varnames) {
        assign(
          var,
          reduce(lapply(1:100, function(x) cbind(get(var, comb_list[[x]]),Iter=x)), abind, along = 1),
          comb_env
        )
      }
      # truth0 <- case_match(
      #   rep(truth, 1201),
      #   1 ~ 2 * plogis(grid_scaled[[1]]),
      #   2 ~ 2 * plogis(grid_scaled[[2]] * exp(0.5 * grid_scaled[[2]] / sqrt(grid_scaled[[2]]^2+1))),
      #   3 ~ 2 * plogis(grid_scaled[[3]] * exp(1.0 * grid_scaled[[3]] / sqrt(grid_scaled[[3]]^2+1))),
      #   4 ~ plogis(grid_scaled[[4]], location = -4, scale = 1) +
      #     plogis(grid_scaled[[4]], location = +4, scale = 1)
      # )
      # bias <- colMeans(comb_env$CI_repeat[,,1]) - truth0
      # for(i in 1:100){
      #   comb_env$CI_repeat[i,,4] <- truth0
      #   comb_env$CI_repeat[i,,7] <- (comb_env$CI_repeat[i,,1] - truth0)^2
      #   comb_env$CI_repeat[i,,6] <- comb_env$CI_repeat[i,,7] + comb_env$CI_repeat[i,,8]
      # }
      save(list = ls(comb_env), file = paste(prefix, setting, truth, ".rdata", sep = ""), envir = comb_env)
    }}}

rm(list = ls())
gc()
