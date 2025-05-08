#### Function to make ggplot versions of DHARMa plots

### Example to use
# source("Dusky_Env_Analysis/Scripts/dharma-to-gg_fn.R") # load function
# 
# plots <- DHARMa_plot(g3) # specify model
# plot_a <- plots[[1]] # QQ pplot
# plot_a
# 
# plot_b <- plots[[2]] # residual plot
# plot_b
# 
# library(patchwork) # put plots together
# plot_a + plot_b
# ggsave("Dusky_Env_Analysis/Plots/model checks/Binary Movement model residuals.png", dpi=300, width = 21, height=14.8, units="cm") # save plot


DHARMa_plot <- function(model){
  
# Get randomized quantile residuals
simulationOutput <- simulateResiduals(model)
residuals <- simulationOutput$scaledResiduals

# Generate theoretical quantiles of uniform
theoretical <- qunif(p = (1:length(residuals) - 0.5) / length(residuals))
# p is a vector of probabilities to calculate the quantiles

# Now plot the residual qq-plot
qq <- ggplot() + 
  geom_point(aes(x = theoretical, y = sort(residuals))) + 
  geom_abline(slope = 1, linetype = "dashed", colour = "red") +
  labs(x = "Theoretical quantiles",
       y = "Randomized quantile residuals")+ 
  theme_bw() + theme(axis.text = element_text(colour="black", size=10))

#qq

# Extract the linear predictors from the models (c.)
# Heteroscedacticity
# Extract the necessary onjects to make ggplots
quantiles <- testQuantiles(simulationOutput, plot=T)
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
  geom_point(aes(x = predictor, y = residuals), alpha = 0.05) +
  geom_line(aes(x = pred$pred, y = pred$`1`), colour = "black") +
  geom_line(aes(x = pred$pred, y = pred$`3`), colour = "black") +
  geom_line(aes(x = pred$pred, y = pred$`5`), colour = "black") +
  labs(x = "Model predictions (rank transformed)",
       y = "Randomized quantile residuals") +
  theme_bw() + theme(axis.text = element_text(colour="black", size=10))

result_list <- list(qq,full_var)
return(result_list)
}