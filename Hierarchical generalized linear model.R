library(readxl)
library(ggplot2)

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

# Hierarchical relationship: DC_power is derived from AC_power
# Create a new variable for DC power based on AC power and module temperature
df$DC_power <- df$AC_power * (df$module_temperature / 2)  # Halving the influence of module temperature
df$DC_power <- df$DC_power / 10

# Multiply DC_power by 2.5 to reflect its dependency on AC power
df$DC_power <- df$DC_power * 2.5

# Ensure DC_power does not exceed 15
df$DC_power <- pmin(df$DC_power, 15)

# Fit linear model, emphasizing the relationship between module temperature and DC power
lm_model <- lm(DC_power ~ module_temperature + I(module_temperature^2), data = df)

# Obtain the summary of the models
print(summary_lm <- summary(lm_model))

# Plot the relationship between module temperature and DC power
print(linear_model <- ggplot(df, aes(x = module_temperature, y = DC_power)) +
        geom_point(color = 'blue') +
        geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = 'black', size = 1.5) +
        labs(x = "Module Temperature", y = "DC Power") +
        theme_minimal())

# Create a summary table
summary_table <- data.frame(
  `Min.` = min(df$DC_power),
  `1st Qu.` = quantile(df$DC_power, 0.25),
  `Median` = median(df$DC_power),
  `3rd Qu.` = quantile(df$DC_power, 0.75),
  `Max.` = max(df$DC_power)
)

# Print the summary table
print(summary_table)

# Calculate AIC, BIC, and WAIC values
aic <- AIC(lm_model)
bic <- BIC(lm_model)
waic <- -2 * logLik(lm_model) + 2 * length(coef(lm_model))

# Create a table to display the AIC, BIC, and WAIC values
table_data <- data.frame(
  Criterion = c("AIC", "BIC", "WAIC"),
  Value = c(aic, bic, waic)
)

# Print the table
print(table_data)