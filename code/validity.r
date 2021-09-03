# Replication Files for Arnold, Engst, Gschwend: 
# "Scaling Court Decisions with Citation Networks"

# Load the data-frame that includes the manually coded outcomes of decisions #
##############################################################################
outcomes <- read.csv2("../data/1_original_seed_decisions/outcomes/outcomes.csv")

# Load the estimated position data and connect to manually coded outcomes #
# to eventually inspect the relationship between position and outcomes    # 
###########################################################################
# Step 1: Set of decisions from Privacy Infringement—Compensation (Fig.2a)#
##------------------------------------------------------------------------#
data_location <-"../data/3_for_estimation/presslaw_images/mlt/"
# Load the estimated positions
load(file = '../data/4_sampling_outputs/presslaw_images_mlt_poisson.RData')

# Extract the positions
draws.stan <- extract(fit.stan)
more.vars.dat <- read.in.data(data_location)
    # Extract the location of the court
    gerort <- na.omit(as.character
                      (data.frame(t(more.vars.dat$gerort))[,1]))
    # Extract the type of the court
    gertyp <- na.omit(as.character
                      (data.frame(t(more.vars.dat$gertyp))[,1]))
    # Extract the cases file number
    aktenzeichen <- na.omit(as.character
                            (data.frame(t(more.vars.dat$aktenzeichen))[,1]))
    # Extract the median position from the thetas (transpose as described in paper)
    theta <- apply(draws.stan$theta, 2, median)*(-1)
        # Get the lower and upper bound of the credible intervals 
        higher <- theta.hb <- apply(draws.stan$theta, 2, high.bound)*(-1)
        lower <- theta.lb <- apply(draws.stan$theta, 2, low.bound)*(-1)
# Combine all the information that was extracted in a dataframe 
# as this is the information we wish to link to the outcomes
dat <- data.frame(cbind(gertyp, gerort, aktenzeichen, theta, higher, lower))

# Create the outcomes & positions dataset for decisions from Figure 2a
outcomes3a <- subset(outcomes, outcomes$figure == "3a") # Note: Fig. 2a was once Fig. 3a
    # We use the file number as the variable to merge the outcomes to the positions
    # Thus, re-lable that variable to match the name from the positions data.
    colnames(outcomes3a)[colnames(outcomes3a) == "reference"] <- "aktenzeichen"
        outcomes3a$court <- NULL # Don't need this in outcome
    # Just to ensure that in both datasets the file numbers are characters
    outcomes3a$aktenzeichen <- as.character(outcomes3a$aktenzeichen)
    dat$aktenzeichen <- as.character(dat$aktenzeichen)
    
# Assign different colors for later plot depending on actor favored in outcome 
outcomes3a$farbe <- NA
outcomes3a$farbe <- ifelse(outcomes3a$advan == 1
                           , cardiffred, outcomes3a$farbe)
outcomes3a$farbe <- ifelse(outcomes3a$advan == 2
                           , cardiffgold, outcomes3a$farbe)
outcomes3a$farbe <- ifelse(outcomes3a$advan == 3
                           , cardiffblue, outcomes3a$farbe)  

# Create the dataset linking positions and outcomes from Fig. 2a 
outcomes3a <- merge(outcomes3a, dat, by = c("aktenzeichen")
                    , all.x = T, all.y = T)
    # Check: This needs to be true, otherwise there is an
    # error in the assignment of lines during the merge
    nrow(outcomes3a) == nrow(dat)  

# Subset the data to all courts that made at least three decisions
# The courts where identified by visual inspection
outcomes3a <- subset(outcomes3a, outcomes3a$gerort == "Berlin"
                               | outcomes3a$gerort == "Köln"
                               | outcomes3a$gerort == "Hamburg")
outcomes3a <- subset(outcomes3a, outcomes3a$gertyp == "LG")  
# Ensure that thetas are numeric
outcomes3a$theta <- as.numeric(as.character(outcomes3a$theta))  

# To later have a nice plot assigne the values on the y-axis 
# at which a court should later occur. 
# First, group by courts
outcomes3a$yaxe <- 1.25
    outcomes3a$yaxe <- ifelse(outcomes3a$gerort == "Köln"
                              , .75, outcomes3a$yaxe)
    outcomes3a$yaxe <- ifelse(outcomes3a$gerort == "Hamburg"
                              , 1, outcomes3a$yaxe)
# Second, order by positions and then by courts 
outcomes3a <- outcomes3a[order(outcomes3a$theta),]
outcomes3a <- outcomes3a[order(outcomes3a$yaxe),]
# Now re-assigne the values for the later plot on y-axis by the order
outcomes3a$yaxe <- rbind(.2,.3,.4,.5,.6,.9,1,1.1,1.2,1.3,1.4,1.7,1.8,1.9)
### --- Data linking poistions and outcomes for courts in Fig. 2a done --- ###


# Load the estimated position data and connect to manually coded outcomes #
# to eventually inspect the relationship between position and outcomes    # 
###########################################################################
# Step 2: Set of decisions from rivacy Infringement—Injunction (Fig.2b)   #
##------------------------------------------------------------------------#
data_location <-"../data/3_for_estimation/presslaw_online_linking/mlt/"
# Load the estimated positions
load(file = '../data/4_sampling_outputs/presslaw_online_linking_mlt_poisson.RData')

# Extract the positions
draws.stan <- extract(fit.stan)
more.vars.dat <- read.in.data(data_location)
    # Extract the location of the court
    gerort <- na.omit(as.character
                      (data.frame(t(more.vars.dat$gerort))[,1]))
    # Extract the type of the court
    gertyp <- na.omit(as.character
                      (data.frame(t(more.vars.dat$gertyp))[,1]))
    # Extract the cases file number
    aktenzeichen <- na.omit(as.character
                            (data.frame(t(more.vars.dat$aktenzeichen))[,1]))
    # Extract the median position from the thetas (transpose as described in paper)
    theta <- apply(draws.stan$theta, 2, median)*(-1)
        # Get the lower and upper bound of the credible intervals 
        higher <- theta.hb <- apply(draws.stan$theta, 2, high.bound)*(-1)
        lower <- theta.lb <- apply(draws.stan$theta, 2, low.bound)*(-1)
# Combine all the information that was extracted in a dataframe 
# as this is the information we wish to link to the outcomes
dat <- data.frame(cbind(gertyp, gerort, aktenzeichen, theta, higher, lower))

# Create the outcomes & positions dataset for decisions from Figure 2b
outcomes3b <- subset(outcomes, outcomes$figure == "3b") # Note: Fig. 2b was once Fig. 3b
    # We use the file number as the variable to merge the outcomes to the positions
    # Thus, re-lable that variable to match the name from the positions data.
    colnames(outcomes3b)[colnames(outcomes3b) == "reference"] <- "aktenzeichen"
        outcomes3b$court <- NULL # Don't need this in outcome
    # Just to ensure that in both datasets the file numbers are characters
    outcomes3b$aktenzeichen <- as.character(outcomes3b$aktenzeichen)
    dat$aktenzeichen <- as.character(dat$aktenzeichen)
    
# Assign different colors for later plot depending on actor favored in outcome 
outcomes3b$farbe <- NA
    outcomes3b$farbe <- ifelse(outcomes3b$advan == 1
                               , cardiffred, outcomes3b$farbe)
    outcomes3b$farbe <- ifelse(outcomes3b$advan == 2
                               , cardiffgold, outcomes3b$farbe)
    outcomes3b$farbe <- ifelse(outcomes3b$advan == 3
                               , cardiffblue, outcomes3b$farbe)  

# Create the dataset linking positions and outcomes from Fig. 2b 
outcomes3b <- merge(outcomes3b, dat, by = c("aktenzeichen")
                    , all.x = T, all.y = T)
    # Check: This needs to be true, otherwise there is an
    # error in the assignment of lines during the merge
    nrow(outcomes3b) == nrow(dat)  

# Subset the data to all courts that made at least three decisions
# The courts where identified by visual inspection
outcomes3b <- subset(outcomes3b, outcomes3b$gerort == "Berlin"
                                | outcomes3b$gerort == "Hamburg")
outcomes3b <- subset(outcomes3b, !(outcomes3b$gerort == "Hamburg" 
                                   & outcomes3b$gertyp == "LG"))  
# Ensure that thetas are numeric
outcomes3b$theta <- as.numeric(as.character(outcomes3b$theta))  

# To later have a nice plot assigne the values on the y-axis 
# at which a court should later occur. 
# First, group by courts
outcomes3b$yaxe <- 2
outcomes3b$yaxe <- ifelse(outcomes3b$gerort == "Hamburg"
                          , 1.75, outcomes3b$yaxe)
outcomes3b$yaxe <- ifelse(outcomes3b$gerort == "Berlin" 
                          & outcomes3b$gertyp == "KG"
                          , 2.25, outcomes3b$yaxe)
# Second, order by positions and then by courts 
outcomes3b <- outcomes3b[order(outcomes3b$theta),]
outcomes3b <- outcomes3b[order(outcomes3b$yaxe),]
# Now re-assigne the values for the later plot on y-axis by the order
outcomes3b$yaxe <- rbind(2.5,2.6,2.7,2.8,3.1,3.2,3.3,3.4,3.5,3.6,3.9,4,4.1,4.2)
### --- Data linking poistions and outcomes for courts in Fig. 2b done --- ###


# Load the estimated position data and connect to manually coded outcomes #
# to eventually inspect the relationship between position and outcomes    # 
###########################################################################
# Step 3: Set of decisions on Antitrust (Fig.6)                           #  
##------------------------------------------------------------------------#
data_location <-"../data/3_for_estimation/antitrust/"
# Load the estimated positions
load(file = '../data/4_sampling_outputs/cartels_count_poisson.RData')

# Extract the positions
draws.stan <- extract(fit.stan)
more.vars.dat <- read.in.data(data_location)
    # Extract the location of the court
    gerort <- na.omit(as.character
                      (data.frame(t(more.vars.dat$gerort))[,1]))
    # Extract the type of the court
    gertyp <- na.omit(as.character
                      (data.frame(t(more.vars.dat$gertyp))[,1]))
    # Extract the cases file number
    aktenzeichen <- na.omit(as.character
                            (data.frame(t(more.vars.dat$aktenzeichen))[,1]))
    # Extract the median position from the thetas (transpose as described in paper)
    theta <- apply(draws.stan$theta, 2, median)*(-1)
        # Get the lower and upper bound of the credible intervals 
        higher <- theta.hb <- apply(draws.stan$theta, 2, high.bound)*(-1)
        lower <- theta.lb <- apply(draws.stan$theta, 2, low.bound)*(-1)
# Combine all the information that was extracted in a dataframe 
# as this is the information we wish to link to the outcomes        
dat <- data.frame(cbind(gertyp, gerort, aktenzeichen, theta, higher, lower))

# Create the outcomes & positions dataset for decisions from Figure 6
outcomes7 <- subset(outcomes, outcomes$figure == "7") # Note: Fig. 2b was once Fig. 3b
    # We use the file number as the variable to merge the outcomes to the positions
    # Thus, re-lable that variable to match the name from the positions data.
    colnames(outcomes7)[colnames(outcomes7) == "reference"] <- "aktenzeichen"
    outcomes7$court <- NULL # Don't need this in outcome
    # Just to ensure that in both datasets the file numbers are characters
    outcomes7$aktenzeichen <- as.character(outcomes7$aktenzeichen)
    dat$aktenzeichen <- as.character(dat$aktenzeichen)

# Assign different colors for later plot depending on actor favored in outcome 
outcomes7$farbe <- NA
  outcomes7$farbe <- ifelse(outcomes7$advan == 1
                            , cardiffred, outcomes7$farbe)
  outcomes7$farbe <- ifelse(outcomes7$advan == 2
                            , cardiffgold, outcomes7$farbe)
  outcomes7$farbe <- ifelse(outcomes7$advan == 3
                            , cardiffblue, outcomes7$farbe)  

# Create the dataset linking positions and outcomes from Fig. 6 
outcomes7 <- merge(outcomes7, dat, by = c("aktenzeichen")
                     , all.x = T, all.y = T)
    # Check: This needs to be true, otherwise there is an
    # error in the assignment of lines during the merge
    nrow(outcomes7) == nrow(dat)  
  
# Subset the data to all courts that made at least three decisions
# The courts where identified by visual inspection    
outcomes7 <- subset(outcomes7, outcomes7$gerort == "Dortmund" 
                             | outcomes7$gerort == "Köln")
outcomes7 <- subset(outcomes7, outcomes7$gertyp == "LG")  
# Ensure that thetas are numeric
outcomes7$theta <- as.numeric(as.character(outcomes7$theta))  

# To later have a nice plot assigne the values on the y-axis 
# at which a court should later occur. 
# First, group by courts
outcomes7$yaxe <- ifelse(outcomes7$gerort == "Dortmund"
                         , 3, 2.75)
# Second, order by positions and then by courts 
outcomes7 <- outcomes7[order(outcomes7$theta),]
outcomes7 <- outcomes7[order(outcomes7$yaxe),]
# Now re-assigne the values for the later plot on y-axis by the order
outcomes7$yaxe <- rbind(4.8,4.9,5,5.1,5.4,5.5,5.6,5.7,5.8)
### --- Data linking poistions and outcomes for courts in Fig. 6 done --- ###

# Create the plot for Appendix G - Figure G1 #
##############################################
# Combine the datasets composed in the previous steps
outcomes <- rbind(outcomes3a, outcomes3b, outcomes7)
    # Ensure that all values in the plot are numeric
    outcomes$theta <- as.numeric(outcomes$theta)
    outcomes$lower <- as.numeric(outcomes$lower)
    outcomes$higher <- as.numeric(outcomes$higher)
  
setEPS()
postscript("../figures_and_tables/appendix/figure_G1.eps", width=9.5, height=6)
par(mar=c(4.5, 9.5, 0, 0)) # Define margins of plot
plot(outcomes$theta, outcomes$yaxe # Plot positions over y-values
    , pch = 20
    , xlim = c(-3.25, 2.5)
    , ylim = c(0,7)
    , xlab = "Latent Dimension"
    , yaxt="n" # Don't show y-axis as it is defined manually
    , ylab=""
    , frame.plot=F
    , col = "white") # Make points white so they don't appear yet; reason:
# First, plot credible intervalls 
segments(outcomes$lower, outcomes$yaxe, outcomes$higher, outcomes$yaxe
         , col="gray", lty=1, lwd=1.5, lend=0)
# Second, overlay point estimates over credible intervals
points(outcomes$theta, outcomes$yaxe, pch = 1, cex = 2, col = outcomes$farbe)

# The lables at the y-axis done manually according to the courts 
axis(2, las = 1 
     , at = c(4.95, 5.6)
     , labels = c("",""), tck = 0.02)
  mtext("Decisions on\nAntitrust", 2, 1, at = 5.3, las = 1)
  
  text(-3.45, 5.6, "LG\nDortmund", pos = 4,  srt=0, xpd = TRUE
       , cex = 1,  col = "black")
  text(-3.45, 4.95, "LG\nKöln", pos = 4,  srt=0, xpd = TRUE
       , cex = 1,  col = "black")

axis(2, las = 1 
      , at = c(2.65,3.35, 4.05)
      , labels = c("", "Decisions on\nPrivacy Infringement\n-Injunction", ""), tck = 0.02)

  text(-3.45, 2.65, "OLG\nHamburg", pos = 4,  srt=0
       , xpd = TRUE, cex = 1,  col = "black")
  text(-3.45, 3.35, "LG\nBerlin", pos = 4,  srt=0
       , xpd = TRUE, cex = 1,  col = "black")
  text(-3.45, 4.05, "KG\nBerlin", pos = 4,  srt=0
       , xpd = TRUE, cex = 1,  col = "black")

axis(2, las = 1 
     , at = c(.4, 1.15, 1.8)
     , labels = c("", "Decisions on\nPrivacy Infringement\n-Compensation", ""), tck = 0.02)

  text(-3.45, .4, "LG\nKöln", pos = 4,  srt=0, xpd = TRUE
       , cex = 1,  col = "black")
  text(-3.45, 1.15, "LG\nHamburg", pos = 4,  srt=0, xpd = TRUE
       , cex = 1,  col = "black")
  text(-3.45, 1.8, "LG\nBerlin", pos = 4,  srt=0, xpd = TRUE
       , cex = 1,  col = "black")

# Insert legent to the plot  
legend(-.4, 7.25
        , legend = c("Favors private person / small company"
                     ,"Favors neither, nor"
                     ,"Favors press / cartel")
        , pch = 1
        , pt.cex = 2
        , col = c(cardiffred,cardiffgold,cardiffblue)
        , border = FALSE
        , bg = NULL 
        , bty = "n"
        , x.intersp=1
        , horiz = FALSE)
dev.off()


# First differences to compare differences in clusters (as mentioned in App.G) #
################################################################################
# Note: FD are not shown in appendix but mentioned in text (see Fn. 5 in App. G) 

# Privacy Infringement—Compensation - LG Köln cases #
###################################-----------------#
# Load the data with the estimated positions 
data_location <-"../data/3_for_estimation/presslaw_images/mlt/"
load(file = '../data/4_sampling_outputs/presslaw_images_mlt_poisson.RData')

# Extract the positions
draws.stan <- extract(fit.stan)
more.vars.dat <- read.in.data(data_location)
# Extract the court place
gerort <- na.omit(as.character
                  (data.frame(t(more.vars.dat$gerort))[,1]))
# Extract the court type
gertyp <- na.omit(as.character
                  (data.frame(t(more.vars.dat$gertyp))[,1]))
# Extract the file number 
aktenzeichen <- na.omit(as.character
                        (data.frame(t(more.vars.dat$aktenzeichen))[,1]))
# This shows that the LG Köln cases are the theats No.: 2, 14, 16, 18, 20  
cbind(gertyp, gerort, aktenzeichen)
# Frome these lines extract the thetas (and transpose) 
draws_28O3312 <- draws.stan$theta[,2]*-1
draws_28O14808 <- draws.stan$theta[,14]*-1
draws_28O26309 <- draws.stan$theta[,16]*-1
draws_28O46614 <- draws.stan$theta[,18]*-1
draws_28O19512 <- draws.stan$theta[,20]*-1
# Comparison of all cases to 28 O 195/12, the red outlying one 
comp1 <- draws_28O3312 - draws_28O19512
comp2 <- draws_28O14808 - draws_28O19512
comp3 <- draws_28O26309 - draws_28O19512
comp4 <- draws_28O46614 - draws_28O19512
# Check that the differences are not significant
quantile(comp1, c(.025, .975))
quantile(comp2 , c(.025, .975))
quantile(comp3, c(.025, .975))
quantile(comp4, c(.025, .975))

# Privacy Infringement—Compensation - LG Hamburg #
#------------------------------------------------#
# Compare in LG Hamburg cluster the middle red one to the three blue one
# The next line shows the respective LG Hamburg cases theats No.:  
cbind(gertyp, gerort, aktenzeichen)
# Frome these lines extract the thetas (and transpose) 
draws_324O117207 <- draws.stan$theta[,3]*-1
draws_324O85208 <- draws.stan$theta[,10]*-1
draws_324O67405 <- draws.stan$theta[,11]*-1
draws_324O12407 <- draws.stan$theta[,19]*-1
# Check that the differences are not significant
quantile((draws_324O85208 - draws_324O117207), c(.025, .975)) # Indistinguishable from Zero
quantile((draws_324O67405 - draws_324O117207), c(.025, .975)) # Indistinguishable from Zero
quantile((draws_324O12407 - draws_324O117207), c(.025, .975)) # Indistinguishable from Zero

# Privacy Infringement—Compensation - LG Berlin #
#-----------------------------------------------#
# Compare in LG Berlin cluster the red one to all the other once (all different color)
# The next line shows the respective LG Berlin cases theats No.:  
cbind(gertyp, gerort, aktenzeichen)
# Frome these lines extract the thetas (and transpose) 
draws_27O12015 <- draws.stan$theta[,24]*-1
draws_27O34803 <- draws.stan$theta[,13]*-1
draws_27O106306 <- draws.stan$theta[,15]*-1
# Check that the differences are not significant
quantile((draws_27O34803 - draws_27O12015), c(.025, .975)) # Indistinguishable from Zero
quantile((draws_27O106306 - draws_27O12015), c(.025, .975)) # Indistinguishable from Zero


# Privacy Infringement—Injunction - KG Berlin cases #
#################################-------------------#
# Load the data with the estimated positions
data_location <-"../data/3_for_estimation/presslaw_online_linking/mlt/"
load(file = '../data/4_sampling_outputs/presslaw_online_linking_mlt_poisson.RData')

# Extract the positions
draws.stan <- extract(fit.stan)
more.vars.dat <- read.in.data(data_location)
# Extract the court place
gerort <- na.omit(as.character
                  (data.frame(t(more.vars.dat$gerort))[,1]))
# Extract the court type
gertyp <- na.omit(as.character
                  (data.frame(t(more.vars.dat$gertyp))[,1]))
# Extract the file number 
aktenzeichen <- na.omit(as.character
                        (data.frame(t(more.vars.dat$aktenzeichen))[,1]))
# The next line shows that the KG Berlin cases are theats No.: 1, 6, 16, 18  
cbind(gertyp, gerort, aktenzeichen)
# Frome these lines extract the thetas (and transpose) 
draws_9U10505 <- draws.stan$theta[,1]*-1
draws_9U5505 <- draws.stan$theta[,6]*-1
draws_9W11908 <- draws.stan$theta[,16]*-1
draws_9U2709 <- draws.stan$theta[,18]*-1
# Comparison of all cases to KG 9 W 119/08, the red outlying one 
comp1 <- draws_9U10505 - draws_9W11908
comp2 <- draws_9U5505 - draws_9W11908
comp3 <- draws_9U2709 - draws_9W11908
# Check that the differences are not significant
quantile(comp1, c(.025, .975))  # Difference significant >> breaks pattern
quantile(comp2 , c(.025, .975)) # Difference significant >> breaks pattern
quantile(comp3, c(.025, .975))

# Privacy Infringement—Injunction - LG Berlin cases #
#---------------------------------------------------#
# Compare in the cluster of five LG Berlin case the outlining blue one to the 
# extended red one within the cluster.
# The next line shows the respective LG Berlin cases theats No.: 
cbind(gertyp, gerort, aktenzeichen)
# From these lines extract the thetas (and transpose) 
draws_27S207 <- draws.stan$theta[,20]*-1
draws_27O83604 <- draws.stan$theta[,13]*-1
# Check that the differences are not significant
quantile((draws_27O83604 - draws_27S207), c(.025, .975)) # Indistinguishable from Zero

# Privacy Infringement—Injunction - OLG Hamburg cases #
#-----------------------------------------------------#
# Compare in the cluster of four OLG Hamburg case the outlining yellow one to the 
# extended blue one within the cluster.
# The next line shows the respective OLG Hamburg cases theats No.:   
cbind(gertyp, gerort, aktenzeichen)
# From these lines extract the thetas (and transpose) 
draws_7U5110 <- draws.stan$theta[,22]*-1
draws_7U12606 <- draws.stan$theta[,11]*-1
# Check that the differences are not significant
quantile((draws_7U12606 - draws_7U5110), c(.025, .975)) # Indistinguishable from Zero


# Antitrust - LG Köln cases #
###########-----------------#
# Load the data with the estimated positions
data_location <-"../data/3_for_estimation/antitrust/"
load(file = '../data/4_sampling_outputs/cartels_count_poisson.RData')

# Extract the positions
draws.stan <- extract(fit.stan)
more.vars.dat <- read.in.data(data_location)
# Extract the court place
gerort <- na.omit(as.character
                  (data.frame(t(more.vars.dat$gerort))[,1]))
# Extract the court type
gertyp <- na.omit(as.character
                  (data.frame(t(more.vars.dat$gertyp))[,1]))
# Extract the file number 
aktenzeichen <- na.omit(as.character
                        (data.frame(t(more.vars.dat$aktenzeichen))[,1]))
# The next line shows that the LG Köln cases are theats No.: 13, 14, 15, 16   
cbind(gertyp, gerort, aktenzeichen)
# Frome these lines extract the thetas (and transpose) 
draws_88O111 <- draws.stan$theta[,13]*-1
draws_88O511 <- draws.stan$theta[,14]*-1
draws_90O111 <- draws.stan$theta[,15]*-1
draws_90O5712 <- draws.stan$theta[,16]*-1
# Comparison of all cases to 88 O 5/11, the red outlying one 
comp1 <- draws_88O111 - draws_88O511
comp2 <- draws_90O111 - draws_88O511
comp3 <- draws_90O5712 - draws_88O511
# Check that the differences are not significant
quantile(comp1, c(.025, .975)) # Indistinguishable from Zero
quantile(comp2 , c(.025, .975)) # Indistinguishable from Zero
quantile(comp3, c(.025, .975)) # Indistinguishable from Zero