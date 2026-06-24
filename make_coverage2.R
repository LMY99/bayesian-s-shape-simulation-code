bars_list <- dir(pattern='bars')
bars_coverage <- matrix(NA, 100, 16)
for(n in 1:16){
  load(bars_list[n])
  tapply(dplyr::between(results[,'truth'],results[,'lower'],results[,'upper']), 
         results[,'Iter'],mean) -> bars_coverage[,n]
}
ci_list <- dir(pattern='_CIs\\d\\d\\.rdata')
ci_coverage <- list()
ci_mean_obs_coverage <- list()
for(n in 1:80){
  load(ci_list[n])
  idx <- which(CI_repeat[,"age"] %in% seq(0,120,by=0.1))
  tapply(dplyr::between(CI_repeat[idx,'truth'],CI_repeat[idx,'lower'],CI_repeat[idx,'upper']),
         CI_repeat[idx,'age'],mean) -> ci_coverage[[ci_list[n]]]
  tapply(dplyr::between(CI_repeat[-idx,'truth'],CI_repeat[-idx,'lower'],CI_repeat[-idx,'upper']),
         CI_repeat[-idx,'Iter'],mean) -> ci_mean_obs_coverage[[ci_list[n]]]
}
save(bars_coverage, ci_coverage, ci_mean_obs_coverage, file='coveragerate.rda')

bars_list <- dir(pattern='bars')
bars_length <- matrix(NA, 100, 16)
for(n in 1:16){
  load(bars_list[n])
  tapply(results[,'upper']-results[,'lower'], 
         results[,'Iter'],mean) -> bars_length[,n]
}
ci_list <- dir(pattern='_CIs\\d\\d\\.rdata')
ci_length <- list()
ci_mean_obs_length <- list()
for(n in 1:80){
  load(ci_list[n])
  idx <- which(CI_repeat[,"age"] %in% seq(0,120,by=0.1))
  tapply(-CI_repeat[idx,'lower']+CI_repeat[idx,'upper'],
         CI_repeat[idx,'age'],mean) -> ci_length[[ci_list[n]]]
  tapply(-CI_repeat[-idx,'lower']+CI_repeat[-idx,'upper'],
         CI_repeat[-idx,'Iter'],mean) -> ci_mean_obs_length[[ci_list[n]]]
}
save(bars_length, ci_length, ci_mean_obs_length, file='length.rda')
