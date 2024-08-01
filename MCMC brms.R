library(readxl)
library(ggplot2)
library(brms)
library(dplyr)  # For data manipulation
library(posterior)  # For MCMC sampling functions

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

# Fit the Bayesian model
brms_model <- brm(DC_power_increase ~ module_temperature + I(module_temperature^2), data = df)

# Obtain the summary of the linear model
summary_brms_model <- summary(brms_model)
print(summary_brms_model)

# Plot the relationship between module temperature and power increase
brms_model_plot <- ggplot(df, aes(x = module_temperature, y = DC_power_increase)) +
  geom_point(color = 'blue') +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = 'black', size = 1.5) +
  labs(x = "Module Temperature", y = "DC Power Increase") +
  theme_minimal()
print(brms_model_plot)

# Obtain Markov Chain Monte Carlo samples
mcmc_chain_samples <- posterior::as_draws_df(brms_model)

# Extract statistics specifically for alpha, beta, sigma, and lp__
parameter_summary <- mcmc_chain_samples %>%
  select(.chain, b_module_temperature, b_Intercept, sigma, lp__) %>%
  group_by(.chain) %>%
  summarise(
    alpha_mean = mean(b_Intercept, na.rm = TRUE),
    alpha_sd = sd(b_Intercept, na.rm = TRUE),
    beta_mean = mean(b_module_temperature, na.rm = TRUE),
    beta_sd = sd(b_module_temperature, na.rm = TRUE),
    sigma_mean = mean(sigma, na.rm = TRUE),
    sigma_sd = sd(sigma, na.rm = TRUE),
    lp_mean = mean(lp__, na.rm = TRUE),
    lp_sd = sd(lp__, na.rm = TRUE)
  )

# Print all rows of the parameter summary
print(parameter_summary, n = Inf)

# Plot MCMC samples from each chain
mcmc_plot <- mcmc_trace(brms_model)
print(mcmc_plot)
