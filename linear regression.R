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
lin_reg <- df[df$module_temperature > 20, ]

# Perform the lin_reg analysis
lin_reg <- lm(DC_power ~ module_temperature, data = lin_reg)

# Create a new dataframe for plotting
plot_data <- data.frame(
  module_temperature = seq(min(lin_reg$model$module_temperature), 55, length.out = 100),
  DC_power = predict(lin_reg, newdata = data.frame(module_temperature = seq(min(lin_reg$model$module_temperature), 55, length.out = 100)))
)

# Append the decreasing part of the plot
plot_data <- rbind(plot_data, data.frame(
  module_temperature = seq(55, max(lin_reg$model$module_temperature), length.out = 100),
  DC_power = predict(lin_reg, newdata = data.frame(module_temperature = seq(55, max(lin_reg$model$module_temperature), length.out = 100)))
))

# Create a scatter plot with blue dots and a black line
ggplot(data = lin_reg$model, aes(x = module_temperature, y = DC_power)) +
  geom_point(color = "blue") +
  geom_line(data = plot_data, aes(x = module_temperature, y = DC_power), color = "black", size = 1.5) +
  labs(x = "Module Temperature", y = "DC Power") +
  theme_minimal()

summary(lin_reg)

# Calculate AIC, BIC, and WAIC values
aic <- AIC(lin_reg)
bic <- BIC(lin_reg)
waic <- -2 * logLik(lin_reg) + 2 * length(coef(lin_reg$model))

# Create a table to display the AIC, BIC, and WAIC values
table_data <- data.frame(
  Criterion = c("AIC", "BIC", "WAIC"),
  Value = c(aic, bic, waic)
)

# Print the table
print(table_data)