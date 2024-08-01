library(rstan)
library(readxl)
library(ggplot2)
library(dplyr)  # For data manipulation

# Read the Excel file
data <- read_excel("Dataset.xlsx")

# Create the dataframe with the specified columns
df <- data.frame(
  data_time = data[[1]],
  DC_power = data[[2]],
  AC_power = data[[3]],
  ambient_temperature = data[[4]],
  module_temperature = data[[5]],
  irradiation = data[[6]]
)

# Create a new variable for power increase based on module temperature
df$DC_power_increase <- df$AC_power * df$module_temperature * 3 / 10  # Simplified calculation

# Define the Stan model
stan_code <- "
data {
  int<lower=0> N;
  vector[N] DC_power_increase;
  vector[N] module_temperature;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  DC_power_increase ~ normal(alpha + beta * module_temperature, sigma);
}
"

# Prepare the data for Stan
stan_data <- list(
  N = nrow(df),
  DC_power_increase = df$DC_power_increase,
  module_temperature = df$module_temperature
)

# Compile the Stan model
stan_model <- stan_model(model_code = stan_code)

# Fit the model using MCMC with 4 chains
mcmc_samples <- sampling(stan_model, data = stan_data, chains = 4)

# Extract the summary for all parameters
summary_results <- summary(mcmc_samples)$summary

# Convert the summary results to a data frame
summary_df <- as.data.frame(summary_results)

# Create a parameter column from row names
summary_df$parameter <- rownames(summary_df)

# Select relevant columns and rename them
parameter_summary <- summary_df %>%
  filter(parameter %in% c("alpha", "beta", "sigma", "lp__")) %>%
  select(parameter, mean, sd, `2.5%`, `97.5%`, Rhat) %>%
  rename(
    Estimate = mean,
    Est.Error = sd,
    `l-95% CI` = `2.5%`,
    `u-95% CI` = `97.5%`
  )

# Extract samples for parameters
samples <- extract(mcmc_samples)

# Function to calculate effective sample size
effective_sample_size <- function(x) {
  n <- length(x)
  acf_values <- acf(x, plot = FALSE)$acf[-1]  # Get autocorrelation values
  ess <- n / (1 + 2 * sum(acf_values))  # Effective sample size formula
  return(ess)
}

# Calculate Bulk ESS and Tail ESS for each parameter
ess_df <- data.frame(
  parameter = c("alpha", "beta", "sigma"),
  Bulk_ESS = c(effective_sample_size(samples$alpha), 
               effective_sample_size(samples$beta), 
               effective_sample_size(samples$sigma)),
  Tail_ESS = c(effective_sample_size(samples$alpha), 
               effective_sample_size(samples$beta), 
               effective_sample_size(samples$sigma))  # Using the same for demonstration
)

# Combine summaries with effective sample sizes
final_summary <- parameter_summary %>%
  left_join(ess_df, by = "parameter")

# Print the summary with chain information
print(final_summary)

# Plot the relationship between module temperature and power increase
stan_model_plot <- ggplot(df, aes(x = module_temperature, y = DC_power_increase)) +
  geom_point(color = 'blue') +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = 'black', size = 1.5) +
  labs(x = "Module Temperature", y = "DC Power Increase") +
  theme_minimal()
print(stan_model_plot)