# ------------------------------------------------------------------------------
# Converting the Hand Consolidated Data Matrix into STAN Readable Format
# 
# Replication Files for Arnold, Engst, Gschwend: 
# "Scaling Court Decisions with Citation Networks"
# 
# Chris Arnold, Cardiff University
# February 2021
# ------------------------------------------------------------------------------


# --- Read Data ----------------------------------------------------------------
data_location_read <- '../data/2_pickles/citation_matrix_antitrust.csv'
dat <- read.csv(data_location_read, sep = ';', row.names = 1)


# --- Reshape ------------------------------------------------------------------
# Goal: Return Three Data Matrices:
#   1 real, 1 with count, 1 with citation matrix

# J number of legal documents
J <- length(rownames(dat))
# K number of different sources overall
K <- length(colnames(dat))
#  N
N <- J*K

# y[N] dummy if user i follows elite j
set_of_all_links <- colnames(dat)

# y connection matrix as a vector 
# y is binary vector 
# y_count is count 
y_count <- vector()
for(i in seq(1,J)){
  y_count <- append(y_count, dat[i,])
}
y_count <- unlist(y_count)
y <- y_count
y[y_count > 1] <- 1

table(y_count)
table(y)

#  jj[N] verdict/decision for observation y_n
jj <- vector()
for(i in seq(0,J-1)){ # accounting for python starting at 0
  jj <- append(jj, rep(i,K))
}

#  kk[N]
# legal document/source for observation y_n
kk <- rep(seq(0,K-1), J) # accounting for python starting at 0


# This filters those legal sources out, that appear only once --------------
# Step 1: Get a mask
# y into matrix: Rows nr. of cases, cols nr of sources
# y_matrix = np.asarray(y).reshape(J,K)
y_matrix <- dat
# colsums
# citations = y_matrix.sum(axis=0)
citations <- colSums(y_matrix) 
# colsums > 1 == True
mask_sources_to_keep_one_decision <- citations > 1
# TF vector * nr of decisions (gets mask to right shape)
mask_sources_to_keep <- rep(mask_sources_to_keep_one_decision,J)
# Step 2: Select with the mask
y_cit_filter <- y[mask_sources_to_keep]
y_count_cit_filter <- y_count[mask_sources_to_keep]
set_of_all_links_cit_filter <- set_of_all_links[mask_sources_to_keep_one_decision]
K_cit_filter <- sum(mask_sources_to_keep_one_decision)
N_cit_filter <- length(y_cit_filter)
J_cit_filter <- N_cit_filter/K_cit_filter

jj_cit_filter <- vector()
# re-create counters
for(i in seq(0,J_cit_filter-1)){
  jj_cit_filter <- append(jj_cit_filter, rep(i,K_cit_filter))
}

#  kk[N]
# legal document/source for observation y_n
kk_cit_filter <- rep(seq(0,K_cit_filter-1), J_cit_filter)


# --- Write Data ---------------------------------------------------------------
data_location <- "../data/3_for_estimation/antitrust/"

myFile = paste0(data_location,'J.csv')
write.table(J, file = myFile, sep = ',', row.names = FALSE, col.names = FALSE)

myFile = paste0(data_location,'K.csv')
write.table(K, file = myFile, sep = ',', row.names = FALSE, col.names = FALSE)

myFile = paste0(data_location,'N.csv')
write.table(N, myFile, sep = ',', row.names = FALSE, col.names = FALSE)

myFile = paste0(data_location, 'jj.csv')
cat(paste(paste(jj, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'kk.csv')
cat(paste(paste(kk, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'y.csv')
cat(paste(paste(y, collapse=','), '\n', sep=''), file = myFile, sep='')

# Writing out citation filtered data
myFile = paste0(data_location,'y_cit_filter.csv')
cat(paste(paste(y_cit_filter, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'y_count_cit_filter.csv')
cat(paste(paste(y_count_cit_filter, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'jj_cit_filter.csv')
cat(paste(paste(jj_cit_filter, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'kk_cit_filter.csv')
cat(paste(paste(kk_cit_filter, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'set_of_all_links_cit_filter.csv')
cat(paste(paste(set_of_all_links_cit_filter, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'K_cit_filter.csv')
cat(paste(paste(K_cit_filter, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'N_cit_filter.csv')
cat(paste(paste(N_cit_filter, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'J_cit_filter.csv')
cat(paste(paste(J_cit_filter, collapse=','), '\n', sep=''), file = myFile, sep='')


# -- Metadata ------------------------------------------------------------------
data_location_read <- '../data/2_pickles/metadata_antitrust.csv'
meta.data <- read.csv(data_location_read, sep = ',')

myFile = paste0(data_location,'aktenzeichen.csv')
cat(paste(paste(meta.data$aktenzeichen, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'gertyp.csv')
cat(paste(paste(meta.data$gerichtstyp, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'gerort.csv')
cat(paste(paste(meta.data$gerichtsort, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'spruchkoerper.csv')
cat(paste(paste(meta.data$spruchkoerper, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'datum.csv')
cat(paste(paste(meta.data$datum, collapse=','), '\n', sep=''), file = myFile, sep='')












# Reorder the data along time
# --- Read Data ----------------------------------------------------------------
data_location_read <- '../data/2_pickles/citation_matrix_antitrust.csv'
dat <- read.csv(data_location_read, sep = ';', row.names = 1)

data_location_read <- '../data/2_pickles/metadata_antitrust.csv'
meta.data <- read.csv(data_location_read, sep = ',')

dat$date <- meta.data$datum
dat$date <- as.Date(dat$date, "%d.%m.%Y")
dat <- dat[order(dat$date),]

meta.data$date <- as.Date(meta.data$datum, "%d.%m.%Y")
meta.data <- meta.data[order(meta.data$date),]


# --- Reshape ------------------------------------------------------------------
# Goal: Return Three Data Matrices:
#   1 real, 1 with count, 1 with citation matrix

# first, get rid of the added date column
dat <- dat[, !names(dat) %in% 'date']


# J number of legal documents
J <- length(rownames(dat))
# K number of different sources overall
K <- length(colnames(dat))
#  N
N <- J*K

# y[N] dummy if user i follows elite j
set_of_all_links <- colnames(dat)

# y connection matrix as a vector 
# y is binary vector 
# y_count is count 
y_count <- vector()
for(i in seq(1,J)){
    y_count <- append(y_count, dat[i,])
}
y_count <- unlist(y_count)
y <- y_count
y[y_count > 1] <- 1

table(y_count)
table(y)

#  jj[N] verdict/decision for observation y_n
jj <- vector()
for(i in seq(0,J-1)){ # accounting for python starting at 0
    jj <- append(jj, rep(i,K))
}

#  kk[N]
# legal document/source for observation y_n
kk <- rep(seq(0,K-1), J) # accounting for python starting at 0



# This filters those legal sources out, that appear only once --------------
# Step 1: Get a mask
# y into matrix: Rows nr. of cases, cols nr of sources
# y_matrix = np.asarray(y).reshape(J,K)
y_matrix <- as.matrix(dat)
# colsums
# citations = y_matrix.sum(axis=0)
citations <- colSums(y_matrix) 
# colsums > 1 == True
mask_sources_to_keep_one_decision <- citations > 1
# TF vector * nr of decisions (gets mask to right shape)
mask_sources_to_keep <- rep(mask_sources_to_keep_one_decision,J)
# Step 2: Select with the mask
y_cit_filter <- y[mask_sources_to_keep]
y_count_cit_filter <- y_count[mask_sources_to_keep]
set_of_all_links_cit_filter <- set_of_all_links[mask_sources_to_keep_one_decision]
K_cit_filter <- sum(mask_sources_to_keep_one_decision)
N_cit_filter <- length(y_cit_filter)
J_cit_filter <- N_cit_filter/K_cit_filter

jj_cit_filter <- vector()
# re-create counters
for(i in seq(0,J_cit_filter-1)){
    jj_cit_filter <- append(jj_cit_filter, rep(i,K_cit_filter))
}

#  kk[N]
# legal document/source for observation y_n
kk_cit_filter <- rep(seq(0,K_cit_filter-1), J_cit_filter)

# --- Write Data ---------------------------------------------------------------
data_location <- "../data/3_for_estimation/antitrust_ordered_by_time/"

myFile = paste0(data_location,'J.csv')
write.table(J, file = myFile, sep = ',', row.names = FALSE, col.names = FALSE)

myFile = paste0(data_location,'K.csv')
write.table(K, file = myFile, sep = ',', row.names = FALSE, col.names = FALSE)

myFile = paste0(data_location,'N.csv')
write.table(N, myFile, sep = ',', row.names = FALSE, col.names = FALSE)

myFile = paste0(data_location, 'jj.csv')
cat(paste(paste(jj, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'kk.csv')
cat(paste(paste(kk, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'y.csv')
cat(paste(paste(y, collapse=','), '\n', sep=''), file = myFile, sep='')

# Writing out citation filtered data
myFile = paste0(data_location,'y_cit_filter.csv')
cat(paste(paste(y_cit_filter, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'y_count_cit_filter.csv')
cat(paste(paste(y_count_cit_filter, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'jj_cit_filter.csv')
cat(paste(paste(jj_cit_filter, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'kk_cit_filter.csv')
cat(paste(paste(kk_cit_filter, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'set_of_all_links_cit_filter.csv')
cat(paste(paste(set_of_all_links_cit_filter, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'K_cit_filter.csv')
cat(paste(paste(K_cit_filter, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'N_cit_filter.csv')
cat(paste(paste(N_cit_filter, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'J_cit_filter.csv')
cat(paste(paste(J_cit_filter, collapse=','), '\n', sep=''), file = myFile, sep='')


# -- Metadata ------------------------------------------------------------------

myFile = paste0(data_location,'aktenzeichen.csv')
cat(paste(paste(meta.data$aktenzeichen, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'gertyp.csv')
cat(paste(paste(meta.data$gerichtstyp, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'gerort.csv')
cat(paste(paste(meta.data$gerichtsort, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'spruchkoerper.csv')
cat(paste(paste(meta.data$spruchkoerper, collapse=','), '\n', sep=''), file = myFile, sep='')

myFile = paste0(data_location,'datum.csv')
cat(paste(paste(meta.data$datum, collapse=','), '\n', sep=''), file = myFile, sep='')

















