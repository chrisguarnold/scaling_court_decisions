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
    local1 <- paste0(masterwd, "/data/1_original_seed_decisions/antitrust_final/") 
    local2 <- paste0(masterwd, "/data/2_pickles/") 

# Load the manually corrected and cleaned decision #
####################################################
document <- list.files(path=local1
                       , pattern=".*csv")

setwd(local1)
mydata <- read.csv2(document[1], encoding = "UTF-8-Mac")
  mydata$X <- NULL
  colnames(mydata)[1] <- "cite"
  colnames(mydata)[2] <- str_sub(document[1], 1, nchar(document[1])-4)
 
for (k in 2:length(document)) {
setwd(local1)
  addon <- read.csv2(document[k], encoding = "UTF-8-Mac")
    addon$X <- NULL
    colnames(addon)[1] <- "cite"
    colnames(addon)[2] <- str_sub(document[k], 1, nchar(document[k])-4)

mydata <- merge(mydata, addon, by = c("cite"), all.x = T, all.y = T)
}    
    
length(unique(mydata$cite)) == nrow(mydata) # If true then each cite is unique
# Make all NAs to Zero
mydata[is.na(mydata)] <- 0
  
# Make the final dataframe
final <- as.data.frame(t(mydata))

header <- unclass(final[1,])
words <- data.frame(matrix(NA, nrow = 0, ncol = 1)) 
  colnames(words)[1] <- "NAME"

for (i in 1:length(header)) {
  word <- data.frame(as.character(header[[i]][1]))
    colnames(word)[1] <- "NAME"
  words <- rbind(words,word)
}

colnames(final) <- as.character(words$NAME)
final <- final[2:nrow(final),]

setwd(local2) 
write.csv2(final, "citation_matrix_antitrust_check.csv")

# Set workingdirectory back to the main files
setwd(paste0(masterwd,"/code/"))