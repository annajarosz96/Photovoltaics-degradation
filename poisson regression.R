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

# Filter the dataframe to include only rows where module temperature is greater than 20
quad_reg <- df[df$module_temperature > 20, ]

# Perform the Poisson regression analysis
model <- glm(DC_power ~ module_temperature, data = quad_reg, family = poisson)

# Create a sequence of module temperatures for prediction
new_module_temp <- seq(min(quad_reg$module_temperature), max(quad_reg$module_temperature), length.out = 100)

# Predict the DC power using the Poisson regression model
predicted_DC_power <- predict(model, newdata = data.frame(module_temperature = new_module_temp), type = "response")

# Create a new dataframe for plotting
plot_data <- data.frame(
  module_temperature = new_module_temp,
  DC_power = predicted_DC_power
)

# Create a scatter plot with blue dots and a red line
ggplot(data = df, aes(x = module_temperature, y = DC_power)) +
  geom_point(color = "blue") +
  geom_line(data = plot_data, aes(x = module_temperature, y = DC_power), color = "black", size = 1.5) +
  labs(x = "Module Temperature", y = "DC Power") +
  theme_minimal()

# Print the summary with the results
summary(model)

# Calculate AIC, BIC, and WAIC values
aic <- AIC(model)
bic <- BIC(model)
waic <- -2 * logLik(model) + 2 * length(coef(model))

# Create a table to display the AIC, BIC, and WAIC values
table_data <- data.frame(
  Criterion = c("AIC", "BIC", "WAIC"),
  Value = c(aic, bic, waic)
)

# Print the table
print(table_data)