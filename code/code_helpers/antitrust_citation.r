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
# System:     Apple Macbook Pro - MacOS Mojave 10.14.6   #
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
    local1 <- paste0(masterwd, "/data/1_original_seed_decisions/antitrust_decisions/") 
    local2 <- paste0(masterwd, "/data/1_original_seed_decisions/") 
    local3 <- paste0(masterwd, "/data/1_original_seed_decisions/antitrust_citations")

setwd(local1)
    
# Set working directory #
#-----------------------#
setwd(local1)

# Load the decision #
#####################
document <- list.files(path=local1
                       , pattern=".*htm")

for (i in 1:length(document)) {
  # Entail the html structure
  setwd(local1)
  decision <- htmlParse(document[i], encoding = "UTF-8-Mac")
  # Save the entailed structure (trick to read each line)  
  setwd(local2)
  capture.output(decision, file="decision.txt")
  # Reload the decision form the .txt file line-by-line
  decision <- readLines("decision.txt", encoding = "UTF-8-Mac")

# Edit the decision #
#####################

# 1. Identify only main body of the decision in the .html #
#---------------------------------------------------------#
decision <-
decision[
  which(grepl("Dokumentkopf", decision) == TRUE):
  which(grepl("DocInhaltEnde", decision) == TRUE)
  ]

# Further reduction of the text by strings in the .html
decision <- decision[which(grepl("<strong>Gericht:</strong>"
                                 , decision) == TRUE):
                       (which(grepl("docLayoutCopyright", decision) == TRUE)-5)]

# 2. Identify lines with links and split strings # 
# at the links to ensure that the part written   #
# between the .html link command will be at the  #
# beginning of a line in our .html text          #
#------------------------------------------------#
decision <- str_split(decision, "suppressed_link")
  # This leads to a list which we unlist
  decision <- unlist(decision)

# Next keep lines only that begin with \"> as these are
# the symboles that follow on a split of the link
decision <- decision[(which(grepl("\">", substr(decision,1,3)) == TRUE))]
# Each text that is surrounded by a link commad is 
# followed by the symbols </. which is why we exclude
# everything behind those symbols 
decision <- gsub("</.*", "", decision)
# And finally we clean out the beginning characters 
# and only the legal source sourounded by a link-
# command will remain
decision <- gsub(".*\">", "", decision)

# 3. We clean the sources in a couple of steps #
#----------------------------------------------#
decision <- as.character(decision)
# Keep only one blank between words
decision <- gsub("^ *|(?<= ) | *$", "", decision, perl=T)
# Trim white spaces at end and beginning of a full string
decision <- str_trim(decision, side = c("both"))
# Exclude § as special caracter 
decision <- gsub("Â§", "", decision)
# Trim white spaces at end and beginning of a full string
decision <- str_trim(decision, side = c("both"))

# Keep only cites with no more than 75 characters
# decision <- decision[nchar(decision) <= 70] # This line is not used in manual coding

# When the first three charackters are only (Rn., then the line is not needed
decision <- decision[(which(grepl("\\(Rn.", substr(decision,1,4)) == FALSE))]
decision <- decision[(which(grepl("Rn.", substr(decision,1,3)) == FALSE))]
decision <- decision[(which(grepl("RN", substr(decision,1,2)) == FALSE))]
decision <- decision[(which(grepl("Rdnr.", substr(decision,1,5)) == FALSE))]
# Keep lines only if they do not only contain numbers 
decision <- decision[(grepl("[A-Za-z]", decision, perl = T) == TRUE)]
# When the first three charackters are only Rz., then the line is not needed
decision <- decision[(which(grepl("Rz.", substr(decision,1,3)) == FALSE))]
# To short of a string to indentify where it belongs too
decision <- decision[(which(grepl("Tz.", substr(decision,1,3)) == FALSE))]
# Vizual inspection showed the folling string is not needed
# and everything before this string. Hence, we only need 
# everything that follows after this string
if(!is.na(table(grepl("...mehr", decision, perl = T))[2])){
    decision <- decision[(which(grepl("...mehr", decision, perl = T) == TRUE) + 1):
                length(decision)]
}else{
  print("NO ...mehr")
}

# 4. To make sources comparabale we simplyfy further #
#----------------------------------------------------#
# 1) Translate special characters 
decision <- gsub("Ã¤", "ae", decision)
decision <- gsub("Ã\u009c", "ue", decision)
decision <- gsub("Ã¼", "ue", decision)
decision <- gsub("Ã¶", "oe", decision)
decision <- gsub("Ã©", "e", decision) # normaly é
decision <- gsub("â\u0080\u0093", ",", decision) # normaly é

# 2) exclude all special charackters
decision <- gsub("\\.", "", decision)
decision <- gsub("\\/", "", decision)
decision <- gsub("\\(", "", decision)
decision <- gsub("\\)", "", decision)
decision <- gsub("\\,", "", decision)
decision <- gsub("\\;", "", decision)
decision <- gsub("\\-", "", decision)
decision <- gsub("\\[", "", decision)
decision <- gsub("\\]", "", decision)

# 3) exclude a few words to make it more comparable
decision <- gsub("Urteil", "", decision)
decision <- gsub("vom", "", decision)
decision <- gsub("Anmerkung", "", decision)
decision <- gsub("Beschl v", "", decision)
decision <- gsub("Urt v", "", decision)
decision <- gsub("Vergleiche", "", decision)
decision <- gsub("Anschluss", "", decision)
decision <- gsub("vorgehend", "", decision)
decision <- gsub("Entgegen", "", decision)
decision <- gsub("Bestaetigung", "", decision)
decision <- gsub("Entscheidungsbesprechung", "", decision)
# decision <- gsub("Aufsatz", "", decision)
decision <- gsub("in der Entscheidung", "", decision)
decision <- gsub("entschied bereits im", "", decision)
decision <- gsub("in der Fassung", "", decision)
decision <- gsub(" v ", "", decision) # vom
decision <- gsub("Beschluss", "", decision) # vom

# 4) Make date unique
decision <- gsub("Januar", "01", decision)
decision <- gsub("Februar", "02", decision)
decision <- gsub("Maerz", "03", decision)
decision <- gsub("April", "04", decision)
decision <- gsub("Mai", "05", decision)
decision <- gsub("Juni", "06", decision)
decision <- gsub("Juli", "07", decision)
decision <- gsub("August", "08", decision)
decision <- gsub("September", "09", decision)
decision <- gsub("Oktober", "10", decision)
decision <- gsub("November", "11", decision)
decision <- gsub("Dezember", "12", decision)

# 5) Exclude most spaces 
# Keep only one blank between words
decision <- gsub("^ *|(?<= ) | *$", "", decision, perl=T)
# Trim white spaces at end and beginning of a full string
decision <- str_trim(decision, side = c("both"))

# 5. Create the data-matrix to save for manual coding #
#######################################################
# Convert the legal sources to a data-frame
decision <- data.frame(decision)
# Use plyr to count how often one source occurs
decision <- count(decision$decision)
# Rename the column with the sources 
  document[i] <- gsub(".htm.*", "", document[i])
  colnames(decision)[1] <- paste0(document[i])

# Simplified citations with tolower and all spaces excluded 
decision$simple <- tolower(decision[,1])
# Delete all spaces 
decision$simple <- gsub("[[:space:]]", "", decision$simple)
# Copy the first column for the manual coding 
decision$manuel <- decision[,1]
# Add a column for the coder's comments
decision$comment <- ""

setwd(local3)
# Save the dataset with the sources for one decision
write.csv2(decision, paste0(colnames(decision)[1]
                            ,".csv")
           , row.names=FALSE)  
}

# Delete the decision.txt which is not necessary anymore
setwd(local2)
unlink("decision.txt")

# Set workingdirectory back to the main files
setwd(paste0(masterwd,"/code/"))

