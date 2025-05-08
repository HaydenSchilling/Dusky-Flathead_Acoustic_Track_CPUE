# Get randomized quantile residuals
simulationOutput <- simulateResiduals(m)
residuals <- simulationOutput$scaledResiduals

# Generate theoretical quantiles of uniform
theoretical <- qunif(p = (1:length(residuals) - 0.5) / length(residuals))
# p is a vector of probabilities to calculate the quantiles

# Now plot the residual qq-plot
qq <- ggplot() + 
  geom_point(aes(x = theoretical, y = sort(residuals))) + 
  geom_abline(slope = 1, linetype = "dashed", colour = "red") +
  labs(x = "Theoretical quantiles",
       y = "Randomized quantile residuals")

# Extract the linear predictors from the models (c.)
# Heteroscedacticity
# Extract the necessary onjects to make ggplots
quantiles <- testQuantiles(simulationOutput)
pred <- quantiles$predictions

# Get the rank transformed model predictors
predictor <- simulationOutput$fittedPredictedResponse # extract from DHARMa
predictor <- rank(predictor, ties.method = "average") # rank-transform
predictor <- predictor / max(predictor) # scale on 0-1

# Vector of quantiles for plotting
quants <- c(0.25, 0.5, 0.75)

# now plot them
full_var <- ggplot() + 
  geom_hline(yintercept = quants, linetype = "dashed", colour = "black") +
  geom_point(aes(x = predictor, y = residuals), alpha = 0.01) +
  geom_line(aes(x = pred$pred, y = pred$`1`), colour = "black") +
  geom_line(aes(x = pred$pred, y = pred$`3`), colour = "black") +
  geom_line(aes(x = pred$pred, y = pred$`5`), colour = "black") +
  labs(x = "Model predictions (rank transformed)",
       y = "Randomized quantile residuals")
