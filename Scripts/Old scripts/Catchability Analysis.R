library(tidyverse)
library(mgcv)
library(gratia)

# load data
mod_dat <- read_csv("Data/Flathead Env data to model.csv")


# Factors to include: Authorised FisherID, Estuary, Lunar, Month, Flow, Wind speed

# data investigation and cleaning
hist(mod_dat$Response)
hist(mod_dat$EffortQty)
range(mod_dat$EffortQty)

mod_dat$FiscalYear <- as.factor(as.character(mod_dat$FiscalYear))
mod_dat$Estuary <- as.factor(as.character(mod_dat$Estuary))
mod_dat$AuthorisedFisherID <- as.factor(as.character(mod_dat$AuthorisedFisherID))

# The first day was = 0, not sure if it'd have an effect but just in case
mod_dat$Days_since_start <- mod_dat$Days_since_start + 1

mod_dat <- mod_dat %>% filter(EffortQty<3000) # remove really unreliable effort


# full model
m_full <- gam(list((Response) ~ Estuary + s(lunar_phase, bs="cc") + s(Month, bs = "cc", k=6, by = Estuary)+
                s(Flow_mean_scaled, by = Estuary)+ # mean flow previous 14 days
                s(Wind_speed_lag1, by = Estuary)+ # mean wind previous day (when net was set)
                s(AuthorisedFisherID ,bs ='re')+
                s(FiscalYear ,bs ='re')+
                              #    s(Estuary,Days_since_start, bs="re")+ # from Dan re autocorrelation
                  offset(log(EffortQty)),~1,~1), #+ s(Month, bs= "cc", k=6),
              data = mod_dat, family = twlss()) # could also do tw() - requires no list format

acf(residuals.gam(m_full)) # Shows autocorrelation

appraise(m_full)

# try to fix autocorrelation
m_full2 <- gam(list((Response) ~ Estuary + s(lunar_phase, bs="cc") + s(Month, bs = "cc", k=6, by = Estuary)+
                s(Flow_mean_scaled, by = Estuary)+
                s(Wind_speed_lag1, by = Estuary)+
                s(AuthorisedFisherID ,bs ='re')+
                s(FiscalYear ,bs ='re')+
                    s(Estuary,Days_since_start, bs="re")+ # from Dan re autocorrelation
                offset(log(EffortQty)),~1,~1), #+ s(Month, bs= "cc", k=6),
              data = mod_dat, family = twlss())

acf(residuals.gam(m_full2)) # Shows autocorrelation - some improvement.
  
  
appraise(m_full2)

draw(m_full2)

summary(m_full2)

# Dan ------------------------------------------------------------------------ #
# Fit model GS (like a random slopes model)
# Similar shape of response among estuaries, but some variation allowed

formula <- formula(
  # Harvested biomass (kg)
  Response ~ 
    # Fixed effects
    Estuary + # not sure about having something as a fixed effect and random effect...
    # Estuary intercept
    # Lunar cycle main effect (0-6.2 radians)
    s(lunar_phase, bs = "cc") + 
    # Estuary-specific effect
    s(lunar_phase, Estuary, bs = "fs") +
    # Seasonality
    s(Month, bs = "cc", k = 6) +
    # Estuary-specific (necessary?)
    s(Month, Estuary, bs = "fs") +
    # Mean flowed (scaled) from the previous fortnight
    s(Flow_mean_scaled, bs = "tp") + 
    # Estuary-specific effect
    s(Flow_mean_scaled, Estuary, bs = "fs") + 
    # Wind speed (km/h) on the day the net was set
    s(Wind_speed_lag1, bs = "tp") + 
    # Estuary-specific effect
    s(Wind_speed_lag1,  Estuary, bs = "fs") +
    # Random effects
    # Fisher intercept (skipper skill, searching, etc.)
    s(AuthorisedFisherID, bs ='re') +
    # Interannual variations (fecundity, larval supply)
    s(FiscalYear, bs = "re") +
    # Estuary intercept and temporal random effect
    s(Days_since_start, Estuary, bs = "re") +
    # Adjust for effort (units?)
    offset(log(EffortQty))
)

# Set boundary knots for the cyclic variables
# Ensures that Jan and Dec 'join up'
knots <- list(
  lunar_phase = c(0.5, 6.5),
  Month = c(0.5, 12.5)
)

m <- bam(
  formula = formula,
  data = mod_dat, 
  knots = knots,
  family = tw(link = "log"),
  method = "REML",
  select = TRUE
)

# Check residuals
appraise(m) # not great - heavy upper tail
acf(residuals(m)) # autocorrelation up to about lag = 7

# Fit an AR1() model
# Extract the correlation for lag = 1 from the naive model (`rho`)
rho <- acf(residuals(m), plot = FALSE)$acf[2]

# Use `Tweedie()` instead of `tw()` but must supply a fixed `p`
# Get it from the previous model
p <- gratia::theta(m)

# Also need to designate the first obs. in each timeseries (one for each estuary)
mod_dat <- mod_dat %>%
  group_by(Estuary) %>% 
  mutate(AR.start = if_else(Days_since_start == 1, TRUE, FALSE)) %>%
  ungroup()

# Fit the AR1() model
m1 <- bam(
  formula = formula,
  data = mod_dat, 
  knots = knots,
  family = Tweedie(p = p, link = power(lambda = 0)),
  method = "fREML",
  discrete = TRUE,
  rho = rho, 
  AR.start = AR.start,
  select = TRUE
)

# Check residuals
# Can't use appraise() anymore because it doesn't get residuals with 
# autocorrelation accounted for
# Extract residuals
residuals <- itsadug::resid_gam(m1, AR_start = AR.start)

# Generate theoretical residuals
theoretical <- qnorm(p = (1:length(residuals) - 0.5) / length(residuals))

# QQ-plot
ggplot() + 
  geom_point(aes(x = theoretical, 
                 y = sort(residuals))) + 
  geom_abline(slope = 1, linetype = "dashed", colour = "red")
# Still some heavy tails but looking better...

# Check ACF
acf(residuals) # better, but still correlation at lags > 1, ARMA model? Possible with gamm() which uses lme4 in the background

# Look at the estimates
draw(m1, fun = inv_link(m1)) & theme(legend.position = "bottom")

# Note that the black line is the overall estimate and the coloured lines are
# the estuary specific deviations from this. So, for example, in Wallis
# Lake (blue line) when Flow_mean_scaled == 10 then CPUE == 1.25 (black line) + 1.6 (blue line) == 2.85 kg
# I'm sure I have some code somewhere to extract/plot these estimates (if they're of interest)
# ---------------------------------------------------------------------------- #

### In progress - test of recruitment period (not looking good.)

mod_dat$Onshore_winds_scaled


 mod_dat$LogSheetEventID <- as.factor(as.character(mod_dat$LogSheetEventID))
# test flow and winds in recruitment period too ### not sure if this should be separate model
m_full3 <- gam(list((Response) ~ Estuary + #s(lunar_phase, bs="cc") + 
                      s(Month, bs = "cc", k=6, by = Estuary)+
                      #s(Flow_mean_scaled, by = Estuary)+
                      s(recruit_flow_scaled, by = Estuary)+
                      s(Onshore_winds_scaled)+
                      s(Offshore_winds_scaled)+
                      #s(Wind_speed_lag1, by = Estuary)+
                      s(AuthorisedFisherID ,bs ='re')+
                      s(LogSheetEventID, bs='re')+
                      s(FiscalYear ,bs ='re')+
                      s(Estuary,Days_since_start, bs="re")+ # from Dan re autocorrelation
                      offset(log(EffortQty)),~1,~1), #+ s(Month, bs= "cc", k=6),
               data = mod_dat, family = twlss())

summary(m_full3)
draw(m_full3)
appraise(m_full3)
cor.test(mod_dat$Onshore_winds_scaled, mod_dat$Offshore_winds_scaled)
