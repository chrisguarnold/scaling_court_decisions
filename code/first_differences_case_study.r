# ------------------------------------------------------------------------------
# Calculate First Differences for idea points in figure 
# 
# Replication Files for Arnold, Engst, Gschwend: 
# "Scaling Court Decisions with Citation Networks"
# 
# Chris Arnold, Cardiff University
# February 2021
# ------------------------------------------------------------------------------


load("../data/4_sampling_outputs/presslaw_images_mlt_poisson.RData")

# Vars of interest
draws_M_9 <- draws.stan$theta[,5]
draws_HH_324 <- draws.stan$theta[,12]
draws_K_28 <- draws.stan$theta[,18]

# FD
M_minus_HH <- draws_M_9 - draws_HH_324
M_minus_K <- draws_M_9 - draws_K_28
HH_minus_K <- draws_HH_324 - draws_K_28

# Real Plot
medians <- c(median(M_minus_HH), median(M_minus_K), median(HH_minus_K))
id <- c(1,2,3)

setEPS()
postscript("../figures_and_tables/main/figure_3.eps", width = 7, height = 5)
plot(medians, id, bty = 'n', yaxt = 'n', ylab = '', 
     xlab = "Difference on Latent Dimension", las = 1, 
     xlim = c(-4.5, 2), ylim = c(0.5,3.5), 
     pch = 16, col = cardiffred, cex = 1.5)

abline(v=0, lwd = 2, lty = 2, col = cardiffblue)

lines(quantile(M_minus_HH, c(.025, .975)), c(1,1), col = cardiffred, lwd = 2)
lines(quantile(M_minus_K , c(.025, .975)), c(2,2), col = cardiffred, lwd = 2)
lines(quantile(HH_minus_K, c(.025, .975)), c(3,3), col = cardiffred, lwd = 2)

text(median(M_minus_HH), 1, labels = "München Decision 9 O 23075/07 - \n Hamburg Decision 324 O 161/15", pos = 1)
text(median(M_minus_K), 2, labels = "München Decision 9 O 23075/07 - \n Köln Decision 28 O 466/14", pos = 1)
text(median(HH_minus_K), 3, labels = "Hamburg Decision 324 O 161/15 - \n Köln Decision 28 O 466/14", pos = 1)
dev.off()
