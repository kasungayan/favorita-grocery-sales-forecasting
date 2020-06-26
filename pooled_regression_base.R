# Favorita-grocery-sales-forecasting
# glm() model fitting script
# Kasun Bandara, June 2020

# @input: Constructed input and output vector (use by the glm() model)
# @output: returns the fitted pooled regression model

# fit and forecast from a normal model
fit_normal_model = function(fitting_data) {
  # create the formula
  no_of_predictors = ncol(fitting_data) - 1
  formula = fitting_data %>% select(-c('y'))
  formula = colnames(formula)
  formula = paste((formula)[1:length((formula))], collapse = '+')
  formula = paste("y ~ ", formula, sep = "")
  
  formula = as.formula(formula)
  
  # fit the model
  model = glm(formula = formula, data = fitting_data)
  
}