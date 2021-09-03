# Replication Files for Arnold, Engst, Gschwend: 
# "Scaling Court Decisions with Citation Networks"
# Ensure that UTF-8 is read correctly #
#-------------------------------------#
Sys.setlocale("LC_ALL", 'en_US.UTF-8')

# sessionInfo()
##########################################################
# System preconditions                                   #
#--------------------------------------------------------#
# R Version:  3.5.1 (2018-07-02) -- "Feather Spray"      #
# Platform:   x86_64-apple-darwin15.6.0 (64-bit)         #
# System:     Apple Macbook Pro - MacOS Mojava 10.14.6   #
#--------------------------------------------------------#
# Further session information                            #
#--------------------------------------------------------#
# Matrix products: default                               #
# locale: de_DE.UTF-8                                    #
# Base packages: stats,graphics,grDevices,utils,datasets #
#                methods,base                            #
# Attached packages:                                     #
# plyr_1.8.4, stringr_1.3.1, XML_3.98-1.20               #
# Loaded via a namespace (and not attached):             #
# compiler_3.5.1, magrittr_1.5, tools_3.5.1,             #  
# Rcpp_1.0.1, stringi_1.2.4                              #
##########################################################

# Prepare working-directories
masterwd <- gsub("/code", "", getwd())
    local1 <- paste0(masterwd, "/data/1_original_seed_decisions/antitrust_manuel/") 
    local2 <- paste0(masterwd, "/data/1_original_seed_decisions/antitrust_final/") 

# Load the decision #
#####################
document <- list.files(path=local1
                       , pattern=".*csv")
k <- 1

for (k in 1:length(document)) {
setwd(local1)
  mydata <- read.csv2(document[k], encoding = "UTF-8-Mac")
  mydata <- subset(mydata, select = c("manuel", "freq"))

# Umlaute  
mydata$manuel <- gsub("\\\xdc", "ue", mydata$manuel)  
# Edit special characters  
mydata$manuel <- gsub("\\\xa0", " ", mydata$manuel)
mydata$manuel <- gsub("Urteil", "", mydata$manuel)
mydata$manuel <- gsub("vom", "", mydata$manuel)
mydata$manuel <- gsub("Anmerkung", "", mydata$manuel)
mydata$manuel <- gsub("Beschl v", "", mydata$manuel)
mydata$manuel <- gsub("Urt v", "", mydata$manuel)
mydata$manuel <- gsub("Vergleiche", "", mydata$manuel)
mydata$manuel <- gsub("Anschluss", "", mydata$manuel)
mydata$manuel <- gsub("vorgehend", "", mydata$manuel)
mydata$manuel <- gsub("Entgegen", "", mydata$manuel)
mydata$manuel <- gsub("Bestaetigung", "", mydata$manuel)
mydata$manuel <- gsub("Entscheidungsbesprechung", "", mydata$manuel)
mydata$manuel <- gsub("in der Entscheidung", "", mydata$manuel)
mydata$manuel <- gsub("entschied bereits im", "", mydata$manuel)
mydata$manuel <- gsub("in der Fassung", "", mydata$manuel)
mydata$manuel <- gsub(" v ", "", mydata$manuel) # vom
mydata$manuel <- gsub("Beschluss", "", mydata$manuel) # vom
mydata$manuel <- gsub("nachgehend ", "", mydata$manuel) # vom
mydata$manuel <- gsub("So auch ", "", mydata$manuel) # vom
mydata$manuel <- gsub("Festhaltung ", "", mydata$manuel) # vom
mydata$manuel <- gsub("im Text ", "", mydata$manuel) # vom

# Make all to lower
mydata$manuel <- tolower(mydata$manuel)
# Delete all spaces 
mydata$manuel <- gsub("[[:space:]]", "", mydata$manuel)

# Extract a list of all unique citations
cites <- unique(mydata$manuel) 

# Get the sum of all correct ciations in one dataframe
correct <- data.frame(matrix(NA, nrow = 0, ncol = 2)) 
  colnames(correct)[1] <- "manuel"  # Archib No. of the DIP file 
  colnames(correct)[2] <- "freq"  # Abrevation of leading committee in Bundestag

for (i in 1:length(cites)) {
  group <- subset(mydata, mydata$manuel == cites[i])
  group$freq <- sum(group$freq)
  group <- unique(group)
  correct <- rbind(correct, group)
}

# Define the name of the output file and safe  
name <- str_sub(document[k], 1, nchar(document[k])-4)
setwd(local2)
write.csv2(correct, paste0(name,".csv"))
}

# Set workingdirectory back to the main files
setwd(paste0(masterwd,"/code/"))

