args <- commandArgs(trailingOnly = TRUE)
seed <- as.integer(args[1])

set.seed(seed)
setting <- as.integer(args[2])
# setting = 1: baseline age [40, 80], N = 250, Poisson(10)
# setting = 2: baseline age [30, 90], N = 250, Poisson(10)
# setting = 3: baseline age [40, 80], N = 750, Poisson(5)
# setting = 4: baseline age [30, 90], N = 1000, Poisson(15)
true_curve <- as.integer(args[3])
source("functions_flex.R")
source('BARS.R')

start_time_low <- ifelse(setting %in% c(2,4), 30, 40)
start_time_upper <- ifelse(setting %in% c(2,4), 90, 80) # Normal
interval_time_min <- 1
interval_time_exp_rate <- 20 # 1+Exp(rate)
num_visits_mean <- c(10,10,5,15)[setting] # Poisson
N_cont_covars <- 2 # N(0,1) continous covariates
N_binary_covars <- 2
p_binary_covars <- 0.5
random_effect_var <- 1
residual_var <- 0.5

true_fixed_effect <- matrix(c(
  -0.5, -0.5, -0.5, -0.5,
  +0.1, +0.1, +0.1, +0.1
), nrow = 2, ncol = 4, byrow = TRUE)
nX <- 2
a0 <- 1
b0 <- 70
d0 <- 4
mode1 <- 50
range_L1 <- 30
range_R1 <- 100
mean1 <- 70
sd1 <- 5
mean2 <- 100
sd2 <- 5
p1 <- 0.4
p2 <- 0.6

N <- c(250,250,750,1000)[setting]

dataset_num <- 1

R <- 10000L

CI_repeat <- array(0, dim = c(dataset_num, 1201, 8))
CI_covariate_repeat <- array(0, dim = c(dataset_num, nrow(true_fixed_effect), 7))
turning <- array(0, dim = c(dataset_num, 6))
true_turning <- rep(0, dataset_num)
amp <- array(0, dim = c(dataset_num, 3))
true_amp <- rep(0, dataset_num)
scales <- array(0, dim = c(dataset_num, 3))
true_scales <- rep(0, dataset_num)
Q50 <- array(0, dim = c(dataset_num, 6))
true_Q50 <- rep(0, dataset_num)

RE_repeat <- array(0, dim = c(dataset_num, N, 7))
# RE + fixed intercept
offset_repeat <- array(0, dim = c(dataset_num, N, 7))

sigmay_repeat <- array(0, dim = c(dataset_num, 7))
sigmaw_repeat <- array(0, dim = c(dataset_num, 7))

for (di in 1:dataset_num) {
  cat(sprintf("%d:\n ", di))
  # Generate a set of data
  
  start_times <- runif(N, start_time_low, start_time_upper)
  visits <- pmax(rpois(N, num_visits_mean), 1)
  df <- data.frame()
  for (i in 1:N) {
    interval <- rexp(visits[i] - 1, interval_time_exp_rate) + 1
    df <- rbind(df, data.frame(
      ageori = start_times[i] + c(0, cumsum(interval)),
      id = rep(i, visits[i])
    ))
  }
  df <- df[dplyr::between(df$ageori, 0, 120), ]
  dfi <- 24
  qknot <- (1:(dfi - 3)) / (dfi - 2)
  VIF <- 0.1
  boundary.knot <- c(0, 120)
  t01 <- (df$ageori - boundary.knot[1]) / (boundary.knot[2] - boundary.knot[1])
  t01 <- t01[dplyr::between(t01, 0, 1)]
  knot <- betaKDE(t01, s = VIF, q = qknot)$quantile
  knot <- (boundary.knot[2] - boundary.knot[1]) * knot + boundary.knot[1]
  
  X <- cbind(
    X1 = rbinom(N, 1, p_binary_covars[1]),
    X2 = rnorm(N)
  )
  X_names <- colnames(X)
  df <- cbind(df, X[df$id, ])
  
  Y <- as.matrix(df[, X_names]) %*% true_fixed_effect
  truthRE <- matrix(rnorm(N * 4, sd = sqrt(random_effect_var)), nrow = N, ncol = ncol(Y))
  Y <- Y + matrix(rnorm(length(Y), sd = sqrt(residual_var)), nrow = nrow(Y), ncol = ncol(Y))
  Y <- Y + truthRE[df$id, ]
  
  coef00 <- c(0, 0, c(1, 4, 7, 1) / 100, 0, 0)
  B00 <- splines2::ibs(df$ageori, knots = knot, degree = 2, intercept = TRUE, Boundary.knots = c(0, 120))
  Y[, 1] <- Y[, 1] + dual_sigmoid(df$ageori, inflect = 130-dual_sigmoid_midpoint(65,1.0,1.0,5),
                                  height_left = 1.0, height_right = 1.0, scale_left = 5)
  Y[, 2] <- Y[, 2] + dual_sigmoid(df$ageori, inflect = 130-dual_sigmoid_midpoint(65,1.4,0.6,5),
                                  height_left = 1.4, height_right = 0.6, scale_left = 5)
  Y[, 3] <- Y[, 3] + dual_sigmoid(df$ageori, inflect = 130-dual_sigmoid_midpoint(65,1.9,0.1,5),
                                  height_left = 1.9, height_right = 0.1, scale_left = 5)
  Y[, 4] <- Y[, 4] + plogis((df$ageori-65)/3,-4)+plogis((df$ageori-65)/3,+4)
  colnames(Y) <- c("Y1", "Y2", "Y3", "Y4")
  
  mis <- missing_pattern(nrow(Y), 4, 1 / 5, 1 / 10)
  for (i in 1:nrow(Y)) {
    for (j in 1:ncol(Y)) {
      if (mis[i, j]) Y[i, j] <- NA
    }
  }
  
  Y <- Y[, true_curve]
  mis <- mis[, true_curve]
  truthRE0 <- truthRE[df$id, true_curve]
  df <- cbind(df, Y, truthRE0)
  
  
  library(ggplot2)
  
  K <- 1 # Number of biomarkers
  # All non-age covariates
  X <- as.matrix(df[, c("X1", "X2")])
  Y <- as.matrix(df[, c("Y")], ncol = K) # Biomarkers array
  x0 <- df$ageori[!is.na(Y)]
  y0 <- Y[!is.na(Y)]
  y0 <- y0[order(x0)]; x0 <- x0[order(x0)]
  out <- bars(x0, y0, prior="poisson", priorparam=20, fits=TRUE)

  if(true_curve==1) truth <- dual_sigmoid(x0, inflect = 130-dual_sigmoid_midpoint(65,1.0,1.0,5),
                                          height_left = 1.0, height_right = 1.0, scale_left = 5)
  if(true_curve==2) truth <- dual_sigmoid(x0, inflect = 130-dual_sigmoid_midpoint(65,1.4,0.6,5),
                                          height_left = 1.4, height_right = 0.6, scale_left = 5)
  if(true_curve==3) truth <- dual_sigmoid(x0, inflect = 130-dual_sigmoid_midpoint(65,1.9,0.1,5),
                                          height_left = 1.9, height_right = 0.1, scale_left = 5)
  if(true_curve==4) truth <- plogis((x0-65)/3,-4)+plogis((x0-65)/3,+4)
  
  
  system('rm bars_params bars_points samp_* summ_*')
  results <- cbind(x=x0,
        truth=truth,
        mean=out$postmeans,
        lower=apply(out$sampfits,2,quantile,0.025),
        upper=apply(out$sampfits,2,quantile,0.975),
        var=apply(out$sampfits,2,var)
  )
  save(results, file=sprintf('bars_result/bars_%03d_%d%d.rda',seed,setting,true_curve))
}


# sink("Parametric_specs.txt")
# cat(paste("Amplitude: True value =", mean(true_amp), "Coverage = ", mean((amp[, 2] - true_amp) * (amp[, 3] - true_amp) <= 0)))
# cat(paste("Scale: True value =", mean(true_scales), "Coverage = ", mean((scales[, 2] - true_scales) * (scales[, 3] - true_scales) <= 0)))
# sink()
