library(tidyverse)

people <- read_csv("Term Project/hormone-diversity-individual.csv")
team <- read_csv("Term Project/hormone-diversity-teams.csv")

full <- people %>%
  full_join(team, by = "team.id")

mean_by_team <- people %>%
  group_by(team.id) %>%
  summarize(mean_test = mean(Testosterone, na.rm = TRUE), .groups = "drop")


sorted <- team %>%
  left_join(mean_by_team, by = "team.id")

men_split <- 200
female_split <- 60

people_cleaned <- people %>%
  left_join(team, by = "team.id") %>% 
  mutate(
    Testosterone_level = case_when(
      Gender == "Male"   & Testosterone > men_split    ~ "High",
      Gender == "Female" & Testosterone > female_split ~ "High",
      TRUE                                                  ~ "Low"
    )
  )


people_cleaned %>%
  ggplot(aes(x = final.performance, color = Testosterone_level)) +
  geom_density()

people_cleaned %>%
  ggplot(aes(x = Testosterone_level)) +
  geom_bar()

people_cleaned %>%
  filter(Gender == 'Male') %>%
  summary(mean = mean(Testosterone),
          sd = sd(Testosterone))


people_cleaned %>%
  summarize(mean = mean(final.performance, na.rm = T),
            sd = sd(final.performance, na.rm = T))



people_cleaned %>%
  ggplot(aes(x = final.performance)) +
  geom_histogram()

x <- seq(-3, 1.5, by = 0.01)

ggplot() +
  geom_line(mapping = aes(x = x, y = dnorm(x, -1, .5), color = "high")) +
  geom_line(mapping = aes(x = x, y = dnorm(x, 0, .5), color = "low"))



library(invgamma)

update.mu <- function(sigma2,ys,m,v){
  n <- length(ys)
  vstar <- 1/(n/sigma2 + 1/v)
  mstar <- vstar * (n*mean(ys)/sigma2 + m/v)
  rnorm(1,mstar,sqrt(vstar))
}

update.var <- function(mu,ys,a,b){
  n <- length(ys)
  astar <- a + n/2
  bstar <- b + 0.5*sum((ys-mu)^2)
  rinvgamma(1,astar,bstar)
}

ig_from_sigma_mom <- function(mean_sigma, sd_sigma) {
  stopifnot(mean_sigma > 0, sd_sigma > 0)
  mean_var <- mean_sigma^2
  sd_var <- 2 * mean_sigma * sd_sigma
  a <- 2 + (mean_var^2)/(sd_var^2)
  b <- mean_var * (a - 1)
  list(a = a, b = b)
}

set.seed(1)
y_low <- people_cleaned %>%
  filter(Testosterone_level == "Low") %>%
  pull(final.performance)
       
m_low <- 0.5; v_low <- 0.5 # mu ~ N(m,v)
a_low <- 0.01; b_low <- 0.01

J <- 1000

mu_low <- rep(NA, J + 1)
sigma2_low <- rep(NA, J + 1)

mu_low[1] <- mean(y_low)
sigma2_low[1] <- var(y_low)

for (j in 2:(J+1)){
  # Step 1a: get the next mu value in the sequence
  mu_low[j] <- update.mu(sigma2_low[j - 1], y_low, m_low, v_low)
  
  # Step 1b: get the next sigma2 value in the sequence
  sigma2_low[j] <- update.var(mu_low[j], y_low, a_low, b_low)
}


ggplot() +
  geom_line(mapping = aes(x = c(1:length(mu_low)), y = mu_low))

ggplot() +
  geom_line(mapping = aes(x = c(1:length(sigma2_low)), y = sigma2_low))

pull_low <- rnorm(1001, mean = mu_low, sd = sqrt(sigma2_low))

ggplot() +
  geom_density(mapping = aes(x = pull_low))

#High

set.seed(1)
y_high <- people_cleaned %>%
  filter(Testosterone_level == "High") %>%
  pull(final.performance)

m_high <- -1; v_high <- 0.5 # mu ~ N(m,v)
a_high <- 0.01; b_high <- 0.01

J <- 1000

mu_high <- rep(NA, J + 1)
sigma2_high <- rep(NA, J + 1)

mu_high[1] <- mean(y_high)
sigma2_high[1] <- var(y_high)

for (j in 2:(J+1)){
  # Step 1a: get the next mu value in the sequence
  mu_high[j] <- update.mu(sigma2_high[j - 1], y_high, m_high, v_high)
  
  # Step 1b: get the next sigma2 value in the sequence
  sigma2_high[j] <- update.var(mu_high[j], y_high, a_high, b_high)
}


ggplot() +
  geom_line(mapping = aes(x = c(1:length(mu_high)), y = mu_high))

ggplot() +
  geom_line(mapping = aes(x = c(1:length(sigma2_high)), y = sigma2_high))

pull_high <- rnorm(1001, mean = mu_high, sd = sqrt(sigma2_high))

ggplot() +
  geom_density(mapping = aes(x = pull_high))



#Diff

diff <- mu_low - mu_high

quantile(diff, probs = c(0.1))

