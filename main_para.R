args <- commandArgs(trailingOnly = TRUE)
seed <- as.integer(args[1])

set.seed(seed)
setting <- as.integer(args[2])
# setting = 1: baseline age [50, 70], N = 250, Poisson(10)
# setting = 2: baseline age [35, 90], N = 250, Poisson(10)
# setting = 3: baseline age [50, 70], N = 750, Poisson(5)
true_curve <- as.integer(args[3])
source("functions_flex.R")

library(rstan)

start_time_low <- ifelse(setting==2, 30, 40)
start_time_upper <- ifelse(setting==2, 90, 80) # Normal
interval_time_min <- 1
interval_time_exp_rate <- 20 # 1+Exp(rate)
num_visits_mean <- ifelse(setting==3, 5, 10) # Poisson
N_cont_covars <- 2 # N(0,1) continous covariates
N_binary_covars <- 2
p_binary_covars <- 0.5
random_effect_var <- 1
residual_var <- 0.5

true_fixed_effect <- matrix(c(
  -0.5, -0.5, -0.5, -0.5,
  +0.1, +0.1, +0.1, +0.1
), nrow = 2, ncol = 4, byrow = TRUE)
nX <- 3
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

N <- ifelse(setting==3, 750, 250)

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
  age_scaled <- list(
    (df$ageori - 60)/6,
    (df$ageori - 72)/6,
    (df$ageori - 82)/4,
    (df$ageori - 60)/3.75
  )
  Y[, 1] <- Y[, 1] + 2 * plogis(age_scaled[[1]])
  Y[, 2] <- Y[, 2] + 2 * plogis(age_scaled[[2]] * exp(0.5 * age_scaled[[2]] / sqrt(age_scaled[[2]]^2+1)))
  Y[, 3] <- Y[, 3] + 2 * plogis(age_scaled[[3]] * exp(1.0 * age_scaled[[3]] / sqrt(age_scaled[[3]]^2+1)))
  Y[, 4] <- Y[, 4] + plogis(age_scaled[[4]], location = -4, scale = 1) +
    plogis(age_scaled[[4]], location = +4, scale = 1)
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
  y_obs <- matrix(nrow = nrow(Y), ncol = ncol(Y))
  for (i in 1:nrow(Y)) {
    for (j in 1:ncol(Y)) {
      y_obs[i, j] <- as.numeric(!is.na(Y[i, j]))
    }
  }
  Y[is.na(Y)] <- 0
  dat <- list(
    Nobs = nrow(X), Npred = ncol(X),
    Nout = ncol(Y), Nind = max(df$id),
    x = X, t = df$ageori, y = Y, y_obs = y_obs,
    jj = df$id
  )

  options(mc.cores = parallel::detectCores())
  rstan_options(auto_write = FALSE)

  stan.fit <- stan(file = "logistic.stan", data = dat, iter = R, chains = 1, seed = 419)
  stan.array <- rstan::extract(stan.fit)
  rm(stan.fit)
  gc()

  ages <- seq(0, 120, by = 0.1)
  points <- array(0, c(length(ages), R / 2))

  est <- matrix(0, length(ages), 3)

  for (j in 1:1) {
    for (t in seq_along(ages)) {
      points[t, ] <- plogis((ages[t] - stan.array$lpos[, j]) / stan.array$lscale[, j]) * stan.array$lamp[, j]
    }
  }
  ci_level <- 0.95
  est <- apply(points, 1, function(x) c(mean(x), HDInterval::hdi(density(x, from = 0), credMass = ci_level, allowSplit = FALSE)))
  var_est <- apply(points, 1, var)
  est <- data.frame(t(est))
  colnames(est) <- c("avg", "lower", "upper")
  grid_scaled <- list(
    (ages - 60)/6,
    (ages - 72)/6,
    (ages - 82)/4,
    (ages - 60)/3.75
  )
  est$truth <- list(
    2 * plogis(grid_scaled[[1]]),
    2 * plogis(grid_scaled[[2]] * exp(0.5 * grid_scaled[[2]] / sqrt(grid_scaled[[2]]^2+1))),
    2 * plogis(grid_scaled[[3]] * exp(1.0 * grid_scaled[[3]] / sqrt(grid_scaled[[3]]^2+1))),
    plogis(grid_scaled[[4]], location = -4, scale = 1) +
      plogis(grid_scaled[[4]], location = +4, scale = 1)
  )[[true_curve]]
  est$age <- ages
  est$MSE <- (est$avg - est$truth)^2 + var_est
  est$bias2 <- (est$avg - est$truth)^2
  est$var <- var_est

  Q50s <- stan.array$lpos
  Q50[di, 1:2] <- HDInterval::hdi(density(Q50s, from = 0), credMass = ci_level, allowSplit = FALSE)
  Q50[di, 3] <- mean(Q50s)
  true_Q50[di] <- ages[min(which(est$truth >= max(est$truth) / 2))]
  Q50[di, 5] <- (Q50[di, 3] - true_Q50[di])^2
  Q50[di, 6] <- var(Q50s)
  Q50[di, 4] <- sum(Q50[di, 5:6])

  CI_repeat[di, , ] <- as.matrix(est)

  CI_covariate_repeat[di, , 1:3] <- t(apply(
    stan.array$beta[, 1, ], 2,
    function(x) c(mean(x), HDInterval::hdi(density(x), credMass = ci_level, allowSplit = FALSE))
  ))
  CI_covariate_repeat[di, , 4] <- c(-0.5, 0.1)
  CI_covariate_repeat[di, , 7] <- t(apply(stan.array$beta[, 1, ], 2, var))
  CI_covariate_repeat[di, , 6] <- (CI_covariate_repeat[di, , 1] - CI_covariate_repeat[di, , 4])^2
  CI_covariate_repeat[di, , 5] <- CI_covariate_repeat[di, , 6] + CI_covariate_repeat[di, , 7]

  RE_repeat[di, , 1:3] <- t(apply(
    stan.array$randomint[, 1, ], 2,
    function(x) c(mean(x), HDInterval::hdi(density(x), credMass = ci_level, allowSplit = FALSE))
  ))
  RE_repeat[di, , 4] <- truthRE[, 2]
  RE_repeat[di, , 7] <- t(apply(stan.array$randomint[, 1, ], 2, var))
  RE_repeat[di, , 6] <- (RE_repeat[di, , 1] - RE_repeat[di, , 4])^2
  RE_repeat[di, , 5] <- RE_repeat[di, , 6] + RE_repeat[di, , 7]

  offsets <- stan.array$randomint[, 1, ]
  for (j in 1:(R / 2)) {
    offsets[j, ] <- offsets[j, ] + stan.array$beta[j, 1, 1]
  }

  offset_repeat[di, , 1:3] <- t(apply(
    offsets, 2,
    function(x) c(mean(x), HDInterval::hdi(density(x), credMass = ci_level, allowSplit = FALSE))
  ))
  offset_repeat[di, , 4] <- truthRE[, 2] + 0.0
  offset_repeat[di, , 7] <- t(apply(offsets, 2, var))
  offset_repeat[di, , 6] <- (offset_repeat[di, , 1] - offset_repeat[di, , 4])^2
  offset_repeat[di, , 5] <- offset_repeat[di, , 6] + offset_repeat[di, , 7]

  sigmay_repeat[di, 1] <- mean(stan.array$sigmaerror)
  sigmay_repeat[di, 2:3] <- HDInterval::hdi(density(stan.array$sigmaerror, from = 0), credMass = ci_level, allowSplit = FALSE)
  sigmay_repeat[di, 4] <- residual_var
  sigmay_repeat[di, 6:7] <- c(
    (sigmay_repeat[di, 1] - sigmay_repeat[di, 4])^2,
    var(stan.array$sigmaerror)
  )
  sigmay_repeat[di, 5] <- sum(sigmay_repeat[di, 6:7])
  sigmaw_repeat[di, 1] <- mean(stan.array$sigmarandom)
  sigmaw_repeat[di, 2:3] <- HDInterval::hdi(density(stan.array$sigmarandom, from = 0), credMass = ci_level, allowSplit = FALSE)
  sigmaw_repeat[di, 4] <- random_effect_var
  sigmaw_repeat[di, 6:7] <- c(
    (sigmaw_repeat[di, 1] - sigmaw_repeat[di, 4])^2,
    var(stan.array$sigmarandom)
  )
  sigmaw_repeat[di, 5] <- sum(sigmaw_repeat[di, 6:7])

  true_turning[di] <- 70
  turning[di, 1:3] <- c(
    mean(stan.array$lpos),
    HDInterval::hdi(density(stan.array$lpos, from = 0), credMass = ci_level, allowSplit = FALSE)
  )
  turning[di, 5] <- (turning[di, 1] - true_turning[di])^2
  turning[di, 6] <- var(stan.array$lpos)
  turning[di, 4] <- sum(turning[di, 5:6])
  true_amp[di] <- 2
  amp[di, ] <- c(
    mean(stan.array$lamp),
    HDInterval::hdi(density(stan.array$lamp, from = 0), credMass = ci_level, allowSplit = FALSE)
  )
  true_scales[di] <- 5
  scales[di, ] <- c(
    mean(stan.array$lscale),
    HDInterval::hdi(density(stan.array$lscale, from = 0), credMass = ci_level, allowSplit = FALSE)
  )
}

save(CI_repeat, turning, true_turning, CI_covariate_repeat, RE_repeat, offset_repeat,
  Q50, true_Q50,
  sigmay_repeat, sigmaw_repeat,
  file = sprintf("para_CIs_%03d_%d%d.rda", seed, setting, true_curve)
)
covered <- apply(CI_repeat, c(1, 2), function(x) (x[4] - x[2]) * (x[4] - x[3]) <= 0)
cover_rate <- apply(covered, 2, mean)

# sink("Parametric_specs.txt")
# cat(paste("Amplitude: True value =", mean(true_amp), "Coverage = ", mean((amp[, 2] - true_amp) * (amp[, 3] - true_amp) <= 0)))
# cat(paste("Scale: True value =", mean(true_scales), "Coverage = ", mean((scales[, 2] - true_scales) * (scales[, 3] - true_scales) <= 0)))
# sink()
