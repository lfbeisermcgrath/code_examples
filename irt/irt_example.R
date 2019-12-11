# Quick and dirty example of estimating and storing of latent traits
# from a 2-dimensional Bayesian IRT model using MCMCpack
# Liam F. Beiser-McGrath, 11/12/19

setwd("~/Dropbox/GitHub/code_examples/irt/")

# load packages
library(matrixStats)
library(MCMCpack)
library(tidyverse)

set.seed(12345)

# Simulate response data for 40 subjects (J) for 20 items (I)
# (indexing matches MCMCpack notation)
subjects <- 40
items <- 20

df <- data.frame(matrix(rbinom(subjects * items, 1, 0.4), subjects, items))

# number of dimensions of latent trait
kdims <- 2

# estimate with MCMCirtKd using default priors and settings
out <- MCMCirtKd(df, dimensions = kdims, store.item = TRUE)

# store posterior 2.5, 50, and 97.5th percentiles
out_ptiles <- colQuantiles(out, probs = c(0.025, 0.5, 0.975))
# tidy up colnames
colnames(out_ptiles) <- paste0("ptile", gsub("%", "", colnames(out_ptiles)))
# put in data frame format
out_ptiles <- as.data.frame(out_ptiles)
# store rownames as explicit variable
out_ptiles$param <- rownames(out_ptiles)

# plot latent positions
out_ptiles %>%
  # keep only thetas
  filter(str_detect(param, "theta")) %>% 
  # separate out respondent id
  separate(param, c("parname", "respid", "dimnum")) %>% 
  # make wide so each row is respondent, each column is a posterior summary
  pivot_wider(names_from = c(parname, dimnum),
              values_from = c(ptile2.5, ptile50, ptile97.5)) %>%
  # plot
  ggplot(aes(x = ptile50_theta_1, y = ptile50_theta_2)) +
  # use respondent id as the points
  geom_text(aes(label = respid)) +
  # plot 95% credible intervals for both dimensions
  geom_errorbar(aes(ymin = ptile2.5_theta_2, ymax = ptile97.5_theta_2),
                alpha = 0.2) +
  geom_errorbarh(aes(xmin = ptile2.5_theta_1, xmax = ptile97.5_theta_1),
                 alpha = 0.2) +
  xlab(expression(paste(theta[1]))) +
  ylab(expression(paste(theta[2]))) +
  theme_minimal() +
  ggtitle("Respondent's Latent Ability from a Bayesian 2-Dimensional IRT Model")
# save plot for sharing
ggsave("2d_irt.pdf", height = 8, width = 8)