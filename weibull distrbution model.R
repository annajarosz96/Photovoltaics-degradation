library(readxl)
library(ggplot2)
library(fitdistrplus)

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

# Filter the dataframe to include only rows where module temperature is greater than 20
filtered_data <- df[df$module_temperature > 20, ]

# Remove zero values from the data
filtered_data <- filtered_data[filtered_data$DC_power > 0, ]

# Fit a Weibull distribution to the filtered data
weibull_fit <- fitdist(filtered_data$DC_power, "weibull")

# Create a sequence of module temperatures for prediction
new_module_temp <- seq(min(filtered_data$module_temperature), max(filtered_data$module_temperature), length.out = 100)

# Predict the DC power using the Weibull distribution
weibull_samples <- rweibull(length(new_module_temp), shape = weibull_fit$estimate[1], scale = weibull_fit$estimate[2])

# Create a new dataframe for plotting
plot_data <- data.frame(
  module_temperature = new_module_temp,
  DC_power = weibull_samples
)

# Create a scatter plot with blue dots and a red line
ggplot(data = filtered_data, aes(x = module_temperature, y = DC_power)) +
  geom_point(color = "blue") +
  labs(x = "Module Temperature", y = "DC Power") +
  theme_minimal()

# Calculate the linear regression model
lm_model <- lm(DC_power ~ module_temperature, data = filtered_data)

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