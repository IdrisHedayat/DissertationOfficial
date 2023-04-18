# Load necessary packages
library(dplyr)
library(tidyr)
library(rstan)

premstan = readRDS("IdrisPrem2223.rds")

# Extract unique teams and assign indices
teams <- unique(premstan$Team)
team_indices <- 1:length(teams)
team_lookup <- data.frame(Team = teams, Index = team_indices)

# Add team indices to the premstan
premstan <- premstan %>%
  left_join(team_lookup, by = c("Team")) %>%
  left_join(team_lookup, by = c("Opponent" = "Team"), suffix = c("", "_opponent"))

# Extract goals and team indices
y <- premstan %>% select(Goal, Home = Index, Away = Index_opponent)

# Prepare the covariate matrix
X <- premstan %>%
  select(form, GDdiff, total_G, diff_rank) %>%
  as.matrix()

# Number of matches and teams , and covariates
N <- nrow(premstan)
T <- length(teams)
K <- ncol(X)

# Define the Stan model code as a string
stan_code <- "
data {
  int<lower=0> N; // number of matches
  int<lower=0> T; // number of teams
  int y[N, 2]; // goals scored by home and away teams
  int home_team[N]; // home team index
  int away_team[N]; // away team index
  int<lower=1> K; // number of covariates
  matrix[N, K] X; // covariates
}

parameters {
  vector[T] theta; // attack strength
  vector[T] alpha; // defense weakness
  real home_advantage; // home advantage
  vector[K] beta; // coefficients for covariates
  
  real mu_theta;
  real mu_alpha;
  real<lower=0> sigma_theta;
  real<lower=0> sigma_alpha;
}

model {
  vector[N] lambda_home;
  vector[N] lambda_away;

  for (n in 1:N) {
    lambda_home[n] = exp(theta[home_team[n]] + alpha[away_team[n]] + home_advantage + X[n] * beta);
    lambda_away[n] = exp(theta[away_team[n]] + alpha[home_team[n]] + X[n] * beta);
  }

  y[:, 1] ~ poisson(lambda_home);
  y[:, 2] ~ poisson(lambda_away);

  theta ~ normal(mu_theta, sigma_theta);
  alpha ~ normal(mu_alpha, sigma_alpha);
  
  mu_theta ~ normal(0, 10);
  mu_alpha ~ normal(0, 10);
  sigma_theta ~ inv_gamma(2, 0.1);
  sigma_alpha ~ inv_gamma(2, 0.1);
}
"
# Compile the Stan model from the string
stan_model <- stan_model(model_code = stan_code)

# Prepare data for Stan
stan_data <- list(N = N, T = T, y = y, home_team = premstan$Home, away_team = premstan$Away, K = K, X = X)

# Fit the model
fit <- sampling(stan_model, data = stan_data, iter = 2000, chains = 4)

# Extract posterior samples
posterior_samples <- extract(fit)

# Make predictions for future matches
# You will need to prepare the covariates for future matches in a similar way as before.
