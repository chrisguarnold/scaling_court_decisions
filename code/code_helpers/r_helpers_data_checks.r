# ------------------------------------------------------------------------------
# Make overview over data matrix
# 
# Replication Files for Arnold, Engst, Gschwend: 
# "Scaling Court Decisions with Citation Networks"
# 
# Chris Arnold, Cardiff University
# February 2021
# ------------------------------------------------------------------------------



# -- Plotting Adjacency Matrix -------------------------------------------------

# Dichotomous Data
plot.adj.matrix <- function(data_location, cit_filter = FALSE){
  if (cit_filter == FALSE){
    # Read Data 
    J <- as.integer(read.csv(paste0(data_location,"J.csv"), header = FALSE))
    K <- as.integer(read.csv(paste0(data_location,"K.csv"), header = FALSE))
    N <- as.integer(read.csv(paste0(data_location,"N.csv"), header = FALSE))
    jj <- unlist(read.csv(paste0(data_location,"jj.csv"), header = FALSE))
    kk <- unlist(read.csv(paste0(data_location,"kk.csv"), header = FALSE))
    y <- unlist(read.csv(paste0(data_location,"y.csv"), header = FALSE))
  } else if (cit_filter == TRUE){
    # Read Data 
    J <- as.integer(read.csv(paste0(data_location,"J_cit_filter.csv"), header = FALSE))
    K <- as.integer(read.csv(paste0(data_location,"K_cit_filter.csv"), header = FALSE))
    N <- as.integer(read.csv(paste0(data_location,"N_cit_filter.csv"), header = FALSE))
    jj <- unlist(read.csv(paste0(data_location,"jj_cit_filter.csv"), header = FALSE))
    kk <- unlist(read.csv(paste0(data_location,"kk_cit_filter.csv"), header = FALSE))
    y <- unlist(read.csv(paste0(data_location,"y_cit_filter.csv"), header = FALSE))
  }
  # make data 
  # python starts at 0
  jj <- jj+1
  kk <- kk+1
  dat1 <- data.frame(jj,kk,y)
  # visualise
  ggplot(data.frame(dat1), aes(kk,jj,z=y)) + 
    geom_tile(aes(fill = y)) + theme_classic() + 
    scale_fill_gradient(guide=FALSE, low = 'white', high = cardiffblue) +
    xlab("Legal Sources") + ylab("Decisions") +
    scale_x_continuous(expand = expansion(.02)) +
    scale_y_continuous(expand = expansion(.02)) +
    theme(axis.line=element_blank())
}


# Count Data
plot.adj.matrix.counts <- function(data_location){
    # Read Data
    J <- as.integer(read.csv(paste0(data_location,"J_cit_filter.csv"), header = FALSE))
    K <- as.integer(read.csv(paste0(data_location,"K_cit_filter.csv"), header = FALSE))
    N <- as.integer(read.csv(paste0(data_location,"N_cit_filter.csv"), header = FALSE))
    jj <- unlist(read.csv(paste0(data_location,"jj_cit_filter.csv"), header = FALSE))
    kk <- unlist(read.csv(paste0(data_location,"kk_cit_filter.csv"), header = FALSE))
    y <- unlist(read.csv(paste0(data_location,"y_count_cit_filter.csv"), header = FALSE))
  # make data
  # python starts at 0
  jj <- jj+1
  kk <- kk+1
  dat1 <- data.frame(jj,kk,y)
  # visualise
  ggplot(data.frame(dat1), aes(kk,jj,z=y)) +
    geom_tile(aes(fill = y)) + theme_classic() +
    scale_fill_gradient(guide=FALSE, low = 'white', high = cardiffblue) +
    xlab("Legal Sources") + ylab("Decisions") + 
    scale_x_continuous(expand = expansion(.02)) +
    scale_y_continuous(expand = expansion(.02), 
                       breaks= function(x) seq(ceiling(x[1]), floor(x[2]), by = 2)) +
    theme(axis.line=element_blank())
}


plot.adj.matrix.counts.toydata <- function(data_location){
  # Read Data
  J <- as.integer(read.csv(paste0(data_location,"J.csv"), header = FALSE))
  K <- as.integer(read.csv(paste0(data_location,"K.csv"), header = FALSE))
  N <- as.integer(read.csv(paste0(data_location,"N.csv"), header = FALSE))
  jj <- unlist(read.csv(paste0(data_location,"jj.csv"), header = FALSE))
  kk <- unlist(read.csv(paste0(data_location,"kk.csv"), header = FALSE))
  y <- unlist(read.csv(paste0(data_location,"y_count.csv"), header = FALSE))
  # make data
  # python starts at 0
  jj <- jj+1
  kk <- kk+1
  dat1 <- data.frame(jj,kk,y)
  # visualise
  ggplot(data.frame(dat1), aes(kk,jj,z=y)) +
    geom_tile(aes(fill = y)) + theme_classic() +
    scale_fill_gradient(guide=FALSE, low = 'white', high = cardiffblue) +
    xlab("Legal Sources") + ylab("Decisions") + 
    scale_x_continuous(expand = expansion(.02)) +
    scale_y_continuous(expand = expansion(.02), 
                       breaks= function(x) seq(ceiling(x[1]), floor(x[2]), by = 2)) +
    theme(axis.line=element_blank())
}



