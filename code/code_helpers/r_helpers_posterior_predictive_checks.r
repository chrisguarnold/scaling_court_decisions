# -----------------------------------------------------------------------------
# Posterior Predictive Checks 
# 
# Replication Files for Arnold, Engst, Gschwend: 
# "Scaling Court Decisions with Citation Networks"
# 
# Chris Arnold, Cardiff University
# February 2021
# -----------------------------------------------------------------------------


# -- Extracting Models and Predict ---------------------------------------------
# function to extract the last X model draws from the posterior
draw_last_X_models <- function(model_posterior, nr_models_to_consider){
  first_model_in <- length(model_posterior$gamma) - nr_models_to_consider + 1
  last_model_in <- length(model_posterior$gamma)
  alpha <- model_posterior$alpha[first_model_in: last_model_in,]
  beta <- model_posterior$beta[first_model_in: last_model_in,]
  mu_beta <- model_posterior$mu_beta[first_model_in: last_model_in]
  theta1 <- model_posterior$theta1[first_model_in: last_model_in,]
  phi1 <- model_posterior$phi1[first_model_in: last_model_in,]
  mu_phi1 <- model_posterior$mu_phi1[first_model_in: last_model_in]
  gamma_all <- model_posterior$gamma[first_model_in: last_model_in]
  # catch phi and nu for the negative binomial in case it is there
  if (!is.null(model_posterior$phi)){
    fi <- model_posterior$phi[first_model_in: last_model_in]
    nu <- model_posterior$nu[first_model_in: last_model_in]
  }
  J <- length(theta1[1,]) # number legal decisions
  K <- length(phi1[1,])   # number legal sources
  N <- K*J
  jj <- NA
  for (i in (1:J)){
    newline <- rep(i,K)
    jj <- append(jj,newline)
  }   # decision position in the decision X sources matrix
  jj <- jj[-1]
  kk <- rep(seq(1,K), J)
  all_models <- list(alpha, beta, mu_beta, theta1, phi1, mu_phi1, gamma_all, J, 
                     K, N, jj, kk, nr_models_to_consider)
  names(all_models) <- c("alpha", "beta", "mu_beta", "theta1", "phi1", "mu_phi1",
                         "gamma_all", "J", "K", "N", "jj", "kk", "N_samples")
  return(all_models)
}


# function to predict Y on the basis of the last X model draws
predict_from_last_X_models <- function(models_posterior, path_to_stan_model, 
                                       nr_models_to_consider){
  allmodels <- draw_last_X_models(models_posterior, nr_models_to_consider)
    data_in <- list(
      J = allmodels$J,
      K = allmodels$K,
      N = allmodels$N,
      alpha = allmodels$alpha,
      phi1 = allmodels$phi1,
      mu_phi1 = allmodels$mu_phi1,
      theta1 = allmodels$theta1,
      beta = allmodels$beta,
      mu_beta = allmodels$mu_beta,
      jj = allmodels$jj,
      kk = allmodels$kk,
      gamma = allmodels$gamma_all,
      N_samples = allmodels$N_samples
    )
  pred <- stan(file = path_to_stan_model,
               data = data_in, seed = project.seed,
               chains = 1, iter = 1,
               algorithm = 'Fixed_param'
  )
  return(pred)
}




# -- Plotting ----------------------------------------------------------------- 
# Overlaying the histograms
# inspired by: 
# https://betanalpha.github.io/assets/case_studies/principled_bayesian_workflow.html


plot_PPC_orig_hist <- function(predictions, y_orig_data, path_to_figure, nr_potential_citations){
  # Define colours
  cols <- c(rgb(211,55,74, 40, maxColorValue = 255),
            rgb(211,55,74, 60, maxColorValue = 255),
            rgb(211,55,74, 80, maxColorValue = 255),
            rgb(211,55,74, 100, maxColorValue = 255),
            rgb(211,55,74, 255, maxColorValue = 255)
  )
  # Set core parameters
  simu_ys <- extract(predictions)$y
  R <- dim(simu_ys)[2] # nr of models 
  # Cut out unrealistically high predictions
  for (r in (1:R)){
    simu_ys[1,r,][simu_ys[1,r,] > 5000] <- 5000
  }
  # Define graphics helper vars
  idx <- rep(0:nr_potential_citations, each=2)
  x <- sapply(1:length(idx), function(b) if(b %% 2 == 0) idx[b] + 0.5 else idx[b] - 0.5)
  counts <- sapply(1:R, function(r) hist(simu_ys[1,r,], breaks=(0:(nr_potential_citations + 1))-0.5, plot=FALSE)$counts)
  pad_counts <- sapply(1:R, function(r) do.call(cbind, lapply(idx, function(n) counts[n + 1, r])))
  probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  # for PPC data
  cred <- sapply(1:(nr_potential_citations + 1), function(b) quantile(counts[b,], probs=probs))
  pad_cred <- do.call(cbind, lapply(idx, function(n) cred[1:9,n + 1]))
  pad_cred_sqrt <- sqrt(pad_cred)
  # For real data 
  obs_counts <- hist(y_orig_data, breaks=(0:(nr_potential_citations + 1))-0.5, plot=FALSE)$counts
  pad_obs <- do.call(cbind, lapply(idx, function(n) obs_counts[n + 1]))
  # Plotting
  # setEPS()
  # postscript(path_to_figure)
  cairo_ps(path_to_figure)
  plot(1, bty="n", 
       las = 1,
       xlim=c(-0.5, 20.5), xlab="Nr. of Citations", ylab="Citation Counts",
       ylim=c(0, max(sqrt(cred[9,]))), yaxt='n')
  # Calculate y-axis
  distance <- floor(max(sqrt(cred[9,]))/4)
  ticks_at <- seq(0,4*distance, distance)
  axis(2, at = ticks_at, las = 1, labels = ticks_at*ticks_at)
  # draw
  polygon(c(x, rev(x)), c(pad_cred_sqrt[1,], rev(pad_cred_sqrt[9,])),
          col = cols[1], border = NA)
  polygon(c(x, rev(x)), c(pad_cred_sqrt[2,], rev(pad_cred_sqrt[8,])),
          col = cols[2], border = NA)
  polygon(c(x, rev(x)), c(pad_cred_sqrt[3,], rev(pad_cred_sqrt[7,])),
          col = cols[3], border = NA)
  polygon(c(x, rev(x)), c(pad_cred_sqrt[4,], rev(pad_cred_sqrt[6,])),
          col = cols[4], border = NA)
  lines(x, pad_cred_sqrt[5,], col=cols[5], lwd=2)
  # real data
  lines(x, sqrt(pad_obs), col=rgb(21,44,81, maxColorValue = 255), lty=1, lw=2)
  dev.off()
}  


# -- Implement -----------------------------------------------------------------
# Wrapper to implement it all.
plot_PPC_from_model <- function(path_to_model_posterior, path_to_orig_data,
                                path_to_stan_model, path_to_figure,
                                nr_potential_citations=40, # helps set graphics
                                nr_models_to_consider=50 # selects the X last models from the chain
                                ){
  # posterior data
  cat("---- reading model... ---- \n")
  load(path_to_model_posterior)
  # model_posterior <- draws.stan
  model_posterior <- extract(fit.stan)
  # original data
  y_orig <- as.numeric(read.csv(path_to_orig_data, sep = ',', header = FALSE))
  # 
  cat('---- starting predictions... ----\n')
  preds <- predict_from_last_X_models(model_posterior, path_to_stan_model, 
                                            nr_models_to_consider)
  cat('---- plotting... ----\n')
  plot_PPC_orig_hist(preds, y_orig, path_to_figure, nr_potential_citations)
  cat('---- done... ----\n')
}

