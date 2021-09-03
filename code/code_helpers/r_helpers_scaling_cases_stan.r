# -----------------------------------------------------------------------------
# Functions for Managing the Scaling
# 
# Replication Files for Arnold, Engst, Gschwend: 
# "Scaling Court Decisions with Citation Networks"
# 
# Chris Arnold, Cardiff University
# February 2021
# -----------------------------------------------------------------------------


# Data -------------------------------------------------------------------------
read.in.connectivity.data <- function(data_location, count, cit_filter){
  if (count==FALSE){
    if (cit_filter==FALSE){
      J <- as.integer(read.csv(paste0(data_location,"J.csv"), header = FALSE))
      K <- as.integer(read.csv(paste0(data_location,"K.csv"), header = FALSE))
      N <- as.integer(read.csv(paste0(data_location,"N.csv"), header = FALSE))
      jj <- unlist(read.csv(paste0(data_location,"jj.csv"), header = FALSE))
      kk <- unlist(read.csv(paste0(data_location,"kk.csv"), header = FALSE))
      y <- unlist(read.csv(paste0(data_location,"y.csv"), header = FALSE))
      # ger_hyp <- unlist(read.csv(paste0(data_location,"gerort_ri_ident.csv"), header = FALSE))
    } else if (cit_filter==TRUE){
      J <- as.integer(read.csv(paste0(data_location,"J_cit_filter.csv"), header = FALSE))
      K <- as.integer(read.csv(paste0(data_location,"K_cit_filter.csv"), header = FALSE))
      N <- as.integer(read.csv(paste0(data_location,"N_cit_filter.csv"), header = FALSE))
      jj <- unlist(read.csv(paste0(data_location,"jj_cit_filter.csv"), header = FALSE))
      kk <- unlist(read.csv(paste0(data_location,"kk_cit_filter.csv"), header = FALSE))
      y <- unlist(read.csv(paste0(data_location,"y_cit_filter.csv"), header = FALSE))  
      # ger_hyp <- unlist(read.csv(paste0(data_location,"gerort_ri_ident.csv"), header = FALSE))
    }
  } else if (count==TRUE){
    if (cit_filter==FALSE){
      J <- as.integer(read.csv(paste0(data_location,"J.csv"), header = FALSE))
      K <- as.integer(read.csv(paste0(data_location,"K.csv"), header = FALSE))
      N <- as.integer(read.csv(paste0(data_location,"N.csv"), header = FALSE))
      jj <- unlist(read.csv(paste0(data_location,"jj.csv"), header = FALSE))
      kk <- unlist(read.csv(paste0(data_location,"kk.csv"), header = FALSE))
      y <- unlist(read.csv(paste0(data_location,"y_count.csv"), header = FALSE))
      # ger_hyp <- unlist(read.csv(paste0(data_location,"gerort_ri_ident.csv"), header = FALSE))
    } else if (cit_filter==TRUE){
      J <- as.integer(read.csv(paste0(data_location,"J_cit_filter.csv"), header = FALSE))
      K <- as.integer(read.csv(paste0(data_location,"K_cit_filter.csv"), header = FALSE))
      N <- as.integer(read.csv(paste0(data_location,"N_cit_filter.csv"), header = FALSE))
      jj <- unlist(read.csv(paste0(data_location,"jj_cit_filter.csv"), header = FALSE))
      kk <- unlist(read.csv(paste0(data_location,"kk_cit_filter.csv"), header = FALSE))
      y <- unlist(read.csv(paste0(data_location,"y_count_cit_filter.csv"), header = FALSE))
      # ger_hyp <- unlist(read.csv(paste0(data_location,"gerort_ri_ident.csv"), header = FALSE))
    }
  }
  data_juris <- list(J = J,
                     K = K,
                     N = N,
                     jj = jj+1,
                     kk =kk+1,
                     y = y)
                     # ger_hyp = ger_hyp)
  return(data_juris)  
}




# Pre Run Helpers --------------------------------------------------------------

# Calculate median over parameter. Distinguish between different result types 
median_over_param_list <- function(param_list) {
  if (length(class(param_list))==2){
    if (class(param_list)[1]=="matrix"){
      out <- apply(param_list, 2, median)  
    }
  } else if (length(class(param_list))==1){
    if (class(param_list)=="array"){
      out <- median(param_list)
    }
  }
  return(out)
}


# Calculate starting values
make_starting_values <- function(vb_result){
   results <- extract(vb_result)
   # assign means and names
   results_means <- list(NA)
   for (i in seq(1,length(results))){
     results_means[[i]] <- median_over_param_list(results[[i]])
   }
   names(results_means) <- names(results)
   results_means_unlist <- unlist(results_means, recursive = FALSE)
   return(results_means_unlist)
}


# Assign the correct names
lister <- function(inputlist, inits.for.run){
  one.list <- as.list(inputlist)
  names(one.list) <- names(inits.for.run)  
  return(one.list)
}

# Function to get the starting values in the correct shape
# Takes as input results from either NUTS pre run or VB pre run
# Returns the list with the correct shape
start.val.maker <- function(fit.vb.or.nuts, nr.cores){
  inits.for.run <-make_starting_values(fit.vb.or.nuts)
  # now bring into shape: need to align with nr of chains
  init.multiplier <- seq(1, 3, (1/(nr.cores-1)*2))
  init.matrix <- matrix(
    rep(inits.for.run, length(init.multiplier)), ncol = length(init.multiplier))
  for (i in seq(1, length(init.multiplier))){
    init.matrix[,i] <- init.matrix[,i]*init.multiplier[i]
  }
  init.list <- split(init.matrix, col(init.matrix))
  init.list.shaped <- lapply(init.list, lister, inits.for.run = inits.for.run)
  return(init.list.shaped)
}


# -- Estimate Starting Values --------------------------------------------------

start.values.and.anchors.from.vb <- function(data_location, count, cit_filter, 
                                     nr.cores, model_file, seed.input){
   pre.fit.vb <- vb(
     stan_model(file = model_file) , 
     data = read.in.connectivity.data(data_location, count, cit_filter), 
     init = "random", 
     seed = project.seed)
   anchors <- find.anchors(pre.fit.vb)
   init.list.shaped <- start.val.maker(pre.fit.vb, nr.cores = nr.cores)
   results <- list(init.list.shaped, anchors)
   return(results)
}


start.values.and.anchors.from.stan <- function(data_location, count, cit_filter, 
                                          nr.cores, model_file, seed.input){
  cat(paste('Using 1 core \n'))
  pre.fit.stan <- stan(
    file =  model_file,
    data = read.in.connectivity.data(data_location, count, cit_filter),  
    chains = 1,    # number of Markov chains
    iter = 2000,    # total number of iterations per chain
    cores = 1,    # number of cores 
    refresh = 50,    # show progress every 'refresh' iterations
    init = 'random',
    seed = seed.input,
    control=list(adapt_delta=0.8)
  )
  anchors <- find.anchors(pre.fit.stan)
  init.list.shaped <- start.val.maker(pre.fit.stan, nr.cores = nr.cores)
  results <- list(init.list.shaped, anchors)
  return(results)
}




# -- Find Anchors ------------------------------------------------------------------
# Takes the fit from the first run on 1 chain and returns positions 
# of the left and right anchor for theta
find.anchors <- function(stan.fit){
  theta.medians <- apply(extract(stan.fit)$theta1, 2, median)
  left.anchor <- which(theta.medians %in% min(theta.medians))
  right.anchor <- which(theta.medians %in% max(theta.medians))
  return(c(left.anchor, right.anchor))
}





# Function for Running Stan ===================================================
# wrapper to run the overall function
run.stan.to.estimate <- function(data_location, count,
                                 cit_filter, nr.iterations, model, seed.input,
                                 anchors.input = FALSE,
                                 init.model = FALSE,
                                 nr.cores.input = FALSE,
                                 random.inits = FALSE){
  # Select correct stan models 
  if (model == 'poisson'){
    if (anchors.input != FALSE){
      model_file <- 'code_helpers/stan_poisson_1D_decenter_anchors.stan'
    } else {
      model_file <- 'code_helpers/stan_poisson_1D_decenter.stan'
    }
  } else if (model == 'hierarchical'){
    model_file <- 'code_helpers/stan_poisson_estimate_ri_ident.stan'
  } else {
    cat('Please specify correct model. Either "poisson" or "hierarchical" \n')
  }
  
  # Select nr of cores to run on
  if (nr.cores.input != FALSE){
    nr.cores = nr.cores.input
  } else {
    nr.cores = parallel::detectCores()  
  }
  
  # calculate the length of each chain
  length.run <- round(nr.iterations/nr.cores)
  
  # Generate Data adding the anchors if you specify them explicitly. 
  # First part of two for setting anchors
  if (class(anchors.input[1]) == 'numeric'){
    data.temp <- read.in.connectivity.data(data_location, count, cit_filter)
    data.final <- c(data.temp, 
                    left_anchor = anchors.input[1], 
                    right_anchor = anchors.input[2])  
  }
  
  # Running the models
  time1 <- proc.time()
  # Generating the inits
  if (init.model=='vb'){
    results <- start.values.and.anchors.from.vb(
      data_location, count=count, 
      nr.cores=nr.cores,
      cit_filter=cit_filter,
      model_file = 'code_helpers/stan_poisson_pre_run.stan'
      )  
    init.list <- results [[1]]
    anchors <- results [[2]]
    cat('Running model with inits from a VB run \n')
  } else if (init.model=='stan'){
    results <- start.values.and.anchors.from.stan(
      data_location, 
      count=count, 
      nr.cores=nr.cores,
      cit_filter=cit_filter, 
      model_file='code_helpers/stan_poisson_pre_run.stan', 
      seed.input=seed.input
      )
    init.list <- results [[1]]
    anchors <- results [[2]]
    cat('Running model with inits from a 1 chain NUTS run \n')
  } else cat("Specify correct init model, either 'vb' or 'stan'")
  
  # option to override the calculcated inits. Makes sense if you want to have 
  # anchors, but random inits.
  if (random.inits==TRUE) init.list<-'random' 
  
  # If you want to have anchors but do not provide them, take them from the 
  # function in start.values.and.anchors.from.vb. Second part of chosing anchors
  if (anchors.input == TRUE){
    # assert that there is a pre-run to estimate the anchors
    if (init.model %in% c('vb', 'stan')){
      data.temp <- read.in.connectivity.data(data_location, count, cit_filter)
      data.final <- c(data.temp, left_anchor = anchors[1], right_anchor = anchors[2])   
      cat('Left anchor is', anchors[1], 'and right anchor is', anchors[2],'\n')
    } else cat("You can only calculate anchors if you chose to estimate a pre-run")
  } else if (anchors.input==FALSE) {
    data.final <- read.in.connectivity.data(data_location, count, cit_filter)
  }
  cat(paste('Now turning to the full estimation. I am using', nr.cores, "cores for it. \n"))
  if (nr.cores == parallel::detectCores()) cat('Turn off the heating: Using all available cores.\n')
  
  # Estimating for real
  fit2 <- stan(
    file = model_file,  # Stan program
    data = data.final,  # Named list of data
    chains = nr.cores,  # Number of Markov chains. 1 per core
    iter = length.run,  # Total number of iterations per chain
    cores = nr.cores,   # Number of cores (using 2 just for the vignette)
    refresh = 50,
    init = init.list,
    open_progress = TRUE,
    seed = seed.input,
    control=list(adapt_delta=0.8) # this can reduce divergence errors
  )
  time2 <- proc.time()
  duration = time2 - time1
  cat(paste('it took me', round(duration[3]/60, digits = 2), 'mins'))
  return(fit2)
}



