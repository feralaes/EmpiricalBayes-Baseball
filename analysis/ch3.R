# 3.1 Setup: the Lahman baseball dataset ----
library(dplyr) 
library(tidyr) 
library(Lahman) # to retrieve Batting dataset 

## Filter out pitchers. 
# Pitchers tend to be the unusually weak batters and should be analyzed 
# separately, so we remove them using dplyr’s anti_join. We'll revisit pitchers 
# in depth in Chapter 11.5.
career <- Batting %>% 
  filter(AB > 0) %>% 
  anti_join(Pitching, by = "playerID") %>% 
  group_by(playerID) %>% 
  summarize(H = sum(H), AB = sum(AB)) %>% 
  mutate(average = H / AB)

## Include names along with the player IDs 
career <- Master %>% 
  tibble::as_tibble() %>% 
  dplyr::select(playerID, nameFirst, nameLast) %>% 
  unite(name, nameFirst, nameLast, sep = " ") %>% 
  inner_join(career, by = "playerID") %>% 
  dplyr::select(-playerID)

# 3.2 Step 1: Estimate a prior from all your data Consider ----

library(stats4)
library(VGAM)
# Filtered for players with more than 500 at-bats, to reduce the amount of noise.
career_filtered <- career %>% 
  filter(AB > 500)

# log-likelihood function 
ll <- function(alpha, beta){ 
  x     <- career_filtered$H 
  total <- career_filtered$AB 
  
  log_lik <- - sum(VGAM::dbetabinom.ab(x, total, alpha, beta, log = TRUE))
  
  return(log_lik)
}

# maximum likelihood estimation 
m <- mle(ll, start = list(alpha = 1, beta = 10), 
         method = "L-BFGS-B", 
         lower = c(0.0001, .1))
ab <- coef(m)

alpha0 <- ab[1] 
beta0 <- ab[2]

# 3.3 Step 2: Use that distribution as a prior for each individual estimate ----
## “Empirical Bayes estimate” or 
## "Empirical Bayesian shrinkage towards a Beta prior"
career_eb <- career %>% 
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0))

# 3.4 Results ----
library(ggplot2)
## Best batters
career_eb %>% 
  arrange(-eb_estimate) %>%
  head()
## Worst batters
career_eb %>% 
  arrange(eb_estimate) %>%
  head()

## Prior mean
prior_mean <- alpha0 / (alpha0 + beta0)
career_eb %>%
  ggplot(aes(x = average, y = eb_estimate, color = AB)) +
  geom_point() +
  geom_hline(yintercept = prior_mean, color = "red", linetype = 2) + 
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = 1) + 
  scale_color_continuous("Number of at-bats", 
                         trans = "log", 
                         breaks = c(0, 10, 100, 1000, 10000)) +
  xlab("Batting average") +
  ylab("Empirical Bayes Batting average") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.75))

# 3.5 So easy it feels like cheating There ----
## There are two steps in empirical Bayes estimation:
## 1. Estimate the overall distribution of your data. 
## 2. Use that distribution as your prior for estimating each average.
