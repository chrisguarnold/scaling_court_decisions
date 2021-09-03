# ------------------------------------------------------------------------------
# Visualising Results
# 
# Replication Files for Arnold, Engst, Gschwend: 
# "Scaling Court Decisions with Citation Networks"
# 
# Chris Arnold, Cardiff University
# February 2021
# ------------------------------------------------------------------------------


# -- Utils
read.in.data <- function(data.location){
  data.temp <- list(NA)
  file.names <- list.files(data.location)
  for (i in seq(1,length(file.names))){
    one.file <- paste0(data.location,file.names[i]) # generate the link to the file
    data.temp[[i]] <- read.csv(one.file, header = FALSE) # write data 
  }
  # assign names
  names(data.temp) <- gsub('.csv','',file.names)
  return(data.temp)
}



stickit.together <- function(input){
  label<-paste(input[1], input[2], ' Aktz:', input[3])
  return(label)
}

high.bound <- function(dat){
  out <- quantile(x = dat, .95)
  return(out)
}

low.bound <- function(dat){
  out <- quantile(x = dat, .05)
  return(out)
}







plot.nice.theta.w.courts <- function(draws, more.vars.dat, 
                                      gerort.prior.knowledge,
                                      inverter = 1, darkgrey = 'grey30',
                                      lightgrey = 'grey70'){
  # Make Data for Labels
  gerort <- na.omit(as.character(data.frame(t(more.vars.dat$gerort))[,1]))
  gertyp <- na.omit(as.character(data.frame(t(more.vars.dat$gertyp))[,1]))
  aktenzeichen <- na.omit(as.character(data.frame(t(more.vars.dat$aktenzeichen))[,1]))
  labels.dat.temp <- cbind(gertyp, gerort, aktenzeichen)
  labels <- apply(labels.dat.temp, 1, stickit.together)
  # theta data 
  theta <- apply(draws$theta, 2, median)*inverter
  higher <- theta.hb <- apply(draws$theta, 2, high.bound)*inverter
  lower <- theta.lb <- apply(draws$theta, 2, low.bound)*inverter
  if (inverter == -1){
    lower <- theta.hb
    higher <- theta.lb
  }
  ruler <- seq(1,length(theta))
  plot(theta, ruler, pch = 16, cex = 1.2, yaxt='n', 
       col = gerort.prior.knowledge,
       xlab = "Latent Dimension", ylab = "Decisions", bty = 'n',
       xlim = c(min(lower-.2), max(higher+.2)),
       ylim = c(0.5, max(ruler + .5)))
  abline(v=0, lty = 3, col = 'grey50')
  for (i in seq(1,length(theta))){
    lines(c(theta.hb[i], theta.lb[i]), c(ruler[i], ruler[i]), lwd = 2, 
          col = gerort.prior.knowledge[i])  
  }
  text(x = theta, y = ruler, labels = labels , pos = 1, cex = 0.8, offset = .3)
  return(c(min(lower-.2), max(higher+.2)))
}





# calculate the mean of posteriors from those with prior knowledge about their positions
diff.in.type.of.gers.plotter <- function(draws, gerort.prior.knowledge, 
                                         xlim_for_topplot, inverter){
  theta <- draws$theta *inverter
  draws.theta.prior.knowledge <- theta[,gerort.prior.knowledge =='#D3374A']
  if (table(gerort.prior.knowledge =='#D3374A')[2] > 1){
    thetas.means.prior.knowledge <- apply(draws.theta.prior.knowledge, 1, mean)  
  } else if (table(gerort.prior.knowledge =='#D3374A')[2] == 1){
    thetas.means.prior.knowledge <- draws.theta.prior.knowledge
  }
  # calculate the mean of posteriors from those without prior knowledge about their positions
  draws.theta.no.prior.knowledge <- theta[,gerort.prior.knowledge !='#D3374A']
  if (table(gerort.prior.knowledge !='#D3374A')[2] > 1){
    thetas.means.no.prior.knowledge <- apply(draws.theta.no.prior.knowledge, 1, mean)
  } else if (table(gerort.prior.knowledge !='#D3374A')[2] == 1){
    thetas.means.no.prior.knowledge <- draws.theta.no.prior.knowledge
  }
  # calculate difference 
  thetas.diff.prior.knowledge <- thetas.means.no.prior.knowledge - thetas.means.prior.knowledge
  # plot
  plot(1,1, type="n", xlab="", ylab="", yaxt='n', xaxt='n', bty='n',
       ylim=c(0.5, 1.5), xlim=xlim_for_topplot
  )
  lines(c(high.bound(thetas.diff.prior.knowledge), low.bound(thetas.diff.prior.knowledge)),
        c(1,1), lwd = 2)
  points(mean(thetas.diff.prior.knowledge), 1, cex = 1, pch = 17)
  abline(v=0, lty = 3, col = 'grey50')
  text(mean(thetas.diff.prior.knowledge), 1, cex = 0.8,
       labels = 'Mean Difference Between Groups', pos = 3)
}


smallset <- c('haui', 'superhaui')
largeset <- c('haui', 'superhaui', 'toll', 'lll')

smallset %in% largeset

largeset %in% smallset

# new version with type of court difference 
plot.nice.theta.w.courts.wrapper <- function(draws, more.vars.dat, 
                                             gerort.prior.knowledge.courts,
                                             inverter = 1, darkgrey = 'grey30',
                                             lightgrey = 'grey70'){
  gerort = more.vars.dat$gerort[!is.na(more.vars.dat$gerort)]
  courts.with.prior.knowledge <- gerort %in% gerort.prior.knowledge.courts  
  # create colour pattern 
  gerort.prior.knowledge <- rep(cardiffblue, length(gerort))
  gerort.prior.knowledge[courts.with.prior.knowledge] <- cardiffred
  # plot
  par(fig = c(0,1,0, 0.85))
  # This is the main plot. Also returns important values for later.
  xlim_for_topplot<-plot.nice.theta.w.courts(draws, more.vars.dat, 
                                             gerort.prior.knowledge, 
                                             inverter)
  
  # ruler <- seq(1,length(theta))
  # Boxplot
  par(fig = c(0,1,0.7, 1), new = TRUE)
  diff.in.type.of.gers.plotter(draws, gerort.prior.knowledge, 
                               xlim_for_topplot, inverter)

}








# # old version with boxplot
# plot.nice.theta.w.courts.wrapper <- function(draws, more.vars.dat, 
#                                               gerort.prior.knowledge.courts,
#                                               inverter = 1, darkgrey = 'grey30',
#                                               lightgrey = 'grey70'){
#   gerort = more.vars.dat$gerort[!is.na(more.vars.dat$gerort)]
#   # create colour pattern 
#   colours <- c(cardiffblue, cardiffred, cardiffgold, cardiffgrey, cardiffpurple1)
#   gerort.prior.knowledge <- rep(colours[1], length(gerort))
#   counter <- 2
#   for (i in gerort.prior.knowledge.courts){
#     gerort.prior.knowledge[gerort==i] <- colours[counter]
#   }
#   # plot
#   par(fig = c(0,1,0, 0.85))
#   # This is the main plot. Also returns important values for later.
#   xlim_for_boxplot<-plot.nice.theta.w.courts(draws, more.vars.dat, 
#                                               gerort.prior.knowledge, 
#                                               inverter)
#   theta <- apply(draws$theta, 2, median)*inverter
#   ruler <- seq(1,length(theta))
#   # Boxplot
#   par(fig = c(0,1,0.7, 1), new = TRUE)
#   boxplot(theta ~ gerort.prior.knowledge, horizontal = TRUE, axes = FALSE, 
#           col = colours[1:length(unique(gerort.prior.knowledge))], 
#           ylim = xlim_for_boxplot, xlab = '', ylab = '', 
#           boxwex = c(.3,.3))
#   text(xlim_for_boxplot[1],1, pos = 4, label = "All Others", cex = .8)
#   if (length(gerort.prior.knowledge.courts)==2){
#     lower.text.location <- 1.25
#     for (i in 1:length(gerort.prior.knowledge.courts)){
#       text(xlim_for_boxplot[1],(lower.text.location+i*.5), pos = 4, cex = .8,
#            label = gerort.prior.knowledge.courts[i])
#     }  
#   } else if (length(gerort.prior.knowledge.courts)==3){
#     lower.text.location <- 1.4
#     for (i in 1:length(gerort.prior.knowledge.courts)){
#       text(xlim_for_boxplot[1],(lower.text.location+i*.3), pos = 4, cex = .8,
#            label = gerort.prior.knowledge.courts[i])
#     }
#   }
# }
# 

















# -- Plotting Phi ------------------------------------------------------------------


plot.nice.phi <- function(draws, more.vars.dat, inverter = 1, cartels = FALSE){
  labels <- lapply(more.vars.dat$set_of_all_links_cit_filter, as.character)
  if(cartels == TRUE){
    labels <- unlist(lapply(more.vars.dat$set_of_all_links_cit_filter, as.character))
  }
  phi <- apply(draws$phi, 2, median)*inverter
  higher <- phi.hb <- apply(draws$phi, 2, high.bound)*inverter
  lower <- phi.lb <- apply(draws$phi, 2, low.bound)*inverter
  if (inverter == -1){
    lower <- phi.hb
    higher <- phi.lb
  }
  ruler <- seq(1,length(phi))
  plot(phi, ruler, pch = 16, cex = 1, bty = 'n', col = cardiffgold,
       xlab = "Latent Dimension", ylab = "Legal Source", yaxt = 'n',
       xlim = c(min(lower-.2), max(higher+.2)),
       ylim = c(0.5, max(ruler + .5)))
  points(phi, ruler, pch = 16, cex = 1.5, bty = 'n', col = cardiffgold)
  for (i in seq(1,length(phi))){
    lines(c(phi.hb[i], phi.lb[i]), c(ruler[i], ruler[i]), lwd = 2,col = cardiffgold)  
  }
  text(x = phi, y = ruler, labels = labels, pos = 1, offset = .1,
       cex = .8, col = "black")
}





eiffeltower.plot.nice.phi <- function(draws, more.vars.dat, inverter = 1, cartels = FALSE){
  labels <- lapply(more.vars.dat$set_of_all_links_cit_filter, as.character)
  if(cartels == TRUE){
    labels <- unlist(lapply(more.vars.dat$set_of_all_links_cit_filter, as.character))
  }
  phi <- apply(draws$phi, 2, median)*inverter
  higher <- phi.hb <- apply(draws$phi, 2, high.bound)*inverter
  lower <- phi.lb <- apply(draws$phi, 2, low.bound)*inverter
  alpha <- apply(draws$alpha, 2, median)*inverter
  alpha.higher <- alpha.hb <- apply(draws$alpha, 2, high.bound)*inverter
  alpha.lower <- alpha.lb <- apply(draws$alpha, 2, low.bound)*inverter
  if (inverter == -1){
    lower <- phi.hb
    higher <- phi.lb
  }
  plot(phi, alpha, pch = 16, cex = 1, bty = 'n', col = cardiffblue,
       xlab = "Latent Dimension", ylab = "Idiosyncratic LLH to Be Cited", yaxt = 'n',
       xlim = c(min(lower-.2), max(higher+.2)),
       ylim = c(min(alpha.lower-.2), max(alpha.higher+.2)))
  points(phi, alpha, pch = 16, cex = 1, bty = 'n', col = cardiffblue)
  text(x = phi, y = alpha, labels = labels , pos = 1, offset = .1,
       cex = .6, col = "black")
}



# Good for the Appendix
plot.nice.theta <- function(draws, more.vars.dat, inverter = 1){
  # Make Data for Labels
  gerort <- na.omit(as.character(data.frame(t(more.vars.dat$gerort))[,1]))
  # datum <- na.omit(data.frame(t(more.vars.dat$datum))[,1])
  gertyp <- na.omit(as.character(data.frame(t(more.vars.dat$gertyp))[,1]))
  aktenzeichen <- na.omit(as.character(data.frame(t(more.vars.dat$aktenzeichen))[,1]))
  labels.dat.temp <- cbind(gertyp, gerort, aktenzeichen)
  labels <- apply(labels.dat.temp, 1, stickit.together)
  # theta data
  theta <- apply(draws$theta, 2, median)*inverter
  higher <- theta.hb <- apply(draws$theta, 2, high.bound)*inverter
  lower <- theta.lb <- apply(draws$theta, 2, low.bound)*inverter
  if (inverter == -1){
    lower <- theta.hb
    higher <- theta.lb
  }
  ruler <- seq(1,length(theta))
  plot(theta, ruler, pch = 16, cex = 2, bty = 'n', yaxt='n',
       xlab = "Latent Dimension", ylab = "Decision",
       xlim = c(min(lower-.2), max(higher+.2)),
       ylim = c(0.5, max(ruler + .5)))
  for (i in seq(1,length(theta))){
    lines(c(theta.hb[i], theta.lb[i]), c(ruler[i], ruler[i]), lwd = 2)
  }
  text(x = theta, y = ruler, labels = labels , pos = 1, cex = 0.8)
}

