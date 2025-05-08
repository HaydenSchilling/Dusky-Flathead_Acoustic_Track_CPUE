## Test of different scales of analysis
## Test monthly and weekly data


library(tidyverse)
library(lubridate)
library(mgcv)

library(glmmTMB)
library(tweedie)
library(DHARMa)

library(sdmTMB)


####  LOAD DATA  ####

monthly <- read_csv("Data/Flathead Env data to model Monthly.csv") %>%
  mutate(Month = as.character(Month),
         AuthorisedFisherID = as.character(AuthorisedFisherID))
monthly <- as.data.frame(monthly)

weekly <- read_csv("Data/Flathead Env data to model Weekly.csv") %>%
  mutate(Week = as.character(Week),
         AuthorisedFisherID = as.character(AuthorisedFisherID))
weekly <- as.data.frame(weekly)


####  PREP DATA  ####

## Drop Zeros - the rationale is that fishers with zeros are probably targeting something else
monthly <- monthly %>% filter(Response > 0)  #removes 184 observations
weekly <- weekly %>% filter(Response > 0)
# * an alternative could be to do a zero-inflated GLMM with different variables for =0 and >0 components

monthly$Estuary <- as.factor(monthly$Estuary)
monthly$AuthorisedFisherID <- as.factor(monthly$AuthorisedFisherID)
monthly$FiscalYear <- as.factor(monthly$FiscalYear)
monthly$Month <- as.numeric(monthly$Month)

weekly$Estuary <- as.factor(weekly$Estuary)
weekly$AuthorisedFisherID <- as.factor(weekly$AuthorisedFisherID)
weekly$FiscalYear <- as.factor(weekly$FiscalYear)
weekly$Week <- as.numeric(weekly$Week)

ff <- table(monthly$AuthorisedFisherID)
length(ff[ff<=5])  #43 fishers with very little effort
ff_ids <- names(which(ff>2))  #exclude fishers with 1-2 records
monthly <- monthly[monthly$AuthorisedFisherID %in% ff_ids,]
weekly <- weekly[weekly$AuthorisedFisherID %in% ff_ids,]

monthly$Mean_daily_Flow_log <- log(monthly$Mean_daily_Flow + 0.2)
weekly$Mean_daily_Flow_log <- log(weekly$Mean_daily_Flow + 0.2)

#should order by time if autocorrelation is to be assessed
monthly <- monthly[order(monthly$Days_since_start),]
yearsx <- ymd(paste0(weekly$CalYear,"-01-01"))
weekly$Date <- yearsx + weeks(weekly$Week)
weekly <- weekly[order(weekly$Date),]


####  EXPLORE DATA  ####

summ <- monthly %>% filter(Estuary == "Wallis Lake") %>%  #"Lake Illawarra" "Tuggerah Lakes" "Wallis Lake" 
  group_by(CalYear, Month) %>% summarise(total_flathead = sum(Response)) %>%
  mutate(Date = as.Date(paste0(CalYear,"-",Month,"-15")))
# Wallis Lake 1434 observations, TL 947, LI 256
ggplot(summ, aes(Date, total_flathead)) + geom_point() + geom_line()

hist(monthly$Response, breaks=30)
table(monthly$Month[monthly$Estuary=="Wallis Lake"])
# WL has data coverage in months 3-11, TL months 3-11, LI mostly months 6-8
# So month can be continuous (possibly cyclical) in WL and TL, but should be a factor for IL with only months 6-8 modelled
# So either the estuaries are modelled separately, or we force each estuary to share the same Month cycle

namesx <- names(monthly)
par(mfrow=c(3,4))
for (nn in c(10,12:15,20:26)) {
  plot(monthly[,nn], monthly$Response,
       main=namesx[nn])
  hist(monthly[,nn], breaks=30, main=namesx[nn])
}
# most of these look ok; use logged flow

# ## Could also create a t-1 lagged flow variable; first we need to predict a continuous series
# # This is a bit wrong bc it combines estuaries
# plot(monthly$Days_since_start, monthly$Mean_daily_Flow_log)
# f <- approxfun(monthly$Days_since_start, monthly$Mean_daily_Flow_log)
# mind <- min(monthly$Days_since_start)
# maxd <- max(monthly$Days_since_start)
# cont_days <- seq(mind, maxd)
# interp_flow <- f(cont_days)
# xx <- interp_flow[monthly$Days_since_start]  #for some reason won't predict all values


####  ANALYSE DATA  ####
## Analysing each estuary separately is challenging due to few samples from Lake Illawarra

## MONTHLY GAMM
## Specify knots to ensure Month 'cc' interpolates over missing months correctly

M1 <- gam(Response ~ s(Month, k=5, bs="cc") + s(Mean_daily_Flow_log) + s(Wind_speed) +
            offset(log(Effort_days)),
          data=monthly, family=gaussian(link="log"), 
          knots=list(Month=c(0.1,12.1)), method="REML")
summary(M1)
plot(M1, scale=0, pages=1, scheme=1)

M1.1 <- gam(Response ~ s(Month, k=5, bs="cc") + s(Mean_daily_Flow_log) + s(Wind_speed) +
              s(CalYear) + s(AuthorisedFisherID, bs="re") +
              offset(log(Effort_days)),
            data=monthly, family=gaussian(link="log"), 
            knots=list(Month=c(0.1,12.1)), method="REML")
summary(M1.1)
plot(M1.1, scale=0, pages=1, scheme=1)
gam.check(M1.1)

M1.2 <- gam(Response ~ s(Month, k=5, bs="cc") + s(Mean_daily_Flow_log) + s(Wind_speed) +
              s(CalYear) + s(AuthorisedFisherID, bs="re") +
              offset(log(Effort_days)),
            data=monthly, family=tw(),
            knots=list(Month=c(0.1,12.1)), method="REML")
summary(M1.2)
plot(M1.2, scale=0, pages=1, scheme=1)
gam.check(M1.2)

M1.3 <- gam(Response ~ Estuary +  #currently the 'final' model
              s(Month, k=5, bs="cc") + 
              s(Mean_daily_Flow_log) + 
              s(Mean_daily_Flow_log, by=Estuary, m=1) +
              s(Wind_speed) +
              s(CalYear) + 
              s(AuthorisedFisherID, bs="re") +
              offset(log(Effort_days)),
            data=monthly, family=tw(), 
            knots=list(Month=c(0.1,12.1)), method="REML")
summary(M1.3)
plot(M1.3, scale=0, pages=1, scheme=1)
gam.check(M1.3)


M1.4 <- gam(Response ~ Estuary +  #currently the 'final' model
              s(Month, k=5, bs="cc") + 
              s(Mean_daily_Flow_log) + 
              s(Mean_daily_Flow_log, by=Estuary, m=1) +
              s(Wind_speed) +
              s(log(recruit_flow))+
              s(log(recruit_flow), by = Estuary, m=1)+
              s(CalYear) + 
              s(AuthorisedFisherID, bs="re") +
              offset(log(Effort_days)),
            data=monthly, family=tw(), 
            knots=list(Month=c(0.1,12.1)), method="REML")
summary(M1.4)
plot(M1.4, scale=0, pages=1, scheme=1)
gam.check(M1.4)

concurvity(M1.4)

## Test Bream

M1.3b <- gam(total_bream ~ Estuary +  #currently the 'final' model
              s(Month, k=5, bs="cc") + 
              s(Mean_daily_Flow_log) + 
              s(Mean_daily_Flow_log, by=Estuary, m=1) +
              s(Wind_speed) +
              s(CalYear) + 
              s(AuthorisedFisherID, bs="re") +
              offset(log(Effort_days)),
            data=monthly, family=tw(), 
            knots=list(Month=c(0.1,12.1)), method="REML")
summary(M1.3b)
plot(M1.3b, scale=0, pages=1, scheme=1)
gam.check(M1.3b)

resid_date <- data.frame(res = residuals(M1.3, type="response"),
                         date = monthly$Days_since_start)
resid_date <- resid_date[order(resid_date$date),]
resid_date <- aggregate(res ~ date, data=resid_date, FUN=mean)
plot(resid_date$date, resid_date$re)
lines(resid_date$date, resid_date$re); abline(h=0)
# ^ could be some remaining temporal autocorrelation there
#'acf' not particularly useful bc of large time gaps but iof used must be assessed relative to estuaries

# I'm unsure whether the 's(Days_since_start, Estuary, bs = "re")' is useful for autocorrelation
# In theory it is a random slopes model, but whether that slope means anything is unclear; incl. bc the
# ... coverage of sampling varies so much among estuaries.
# Better off having a 'fixed' time terms, like s(CalYear) term, and seeing whether a better model comes from 
# ... s(Days, by="estuary).
# Or perhaps s(Days, Estuary, bs="fs", m=1)
# Or perhaps s(Days_since_start) + te(Days_since_start, Estuary, bs=c("tp","re"))
# A temporal autocorrelation structure (AR1) is also unwise due to missing dates in all estuaries
# gamm could also use 'correlation = corCAR1()'

# Syntax explanation:
#s(Fisher, Estuary, bs="re"): Different estuaries have different Fisher effects; estimates a single Fisher:Estuary variance
#s(Days, bs="re", Estuary): similar, but estimates a Fisher variance for each estuary


## MONTHLY GLMM

monthly$date_numFactor <- numFactor(monthly$Days_since_start)

M2 <- glmmTMB(Response ~ Estuary + 
                Month + 
                Mean_daily_Flow_log + 
                Wind_speed +
                CalYear + 
                (1|AuthorisedFisherID) +
                ou(ou(date_numFactor + 0 | Estuary)),  #**** untested
              offset = log(Effort_days),
              data = monthly, family = tweedie())
# ^ not sure this temp. corr. structure is viable; may benefit from pooling fishermen

# M2 <- glmmTMB(Response ~ Estuary + 
#                 Month + 
#                 Mean_daily_Flow_log + 
#                 Wind_speed +
#                 CalYear + 
#                 (1|AuthorisedFisherID),
#               offset = log(Effort_days),
#               data = monthly, family = tweedie())

resids <- simulateResiduals(M2)
plot(resids)

car::Anova(M2)
#summary(M2)
performance::r2(M2)
plot(effects::allEffects(M2))

# Mx <- glmmTMB(Response ~ Estuary*Month + Estuary*Flow_mean_scaled + Estuary*Wind_speed +
#                 (1|CalYear) + (1|AuthorisedFisherID),
#               offset = log(Effort_days),
#               data = monthly2, family = tweedie(link="log"))


## MONTHLY GLMM with splines and temporal random effect

xxx


### WEEKLY GAMM

M3 <- gam(Response ~ Estuary +
              s(Week, k=5) + 
              s(Mean_daily_Flow_log) + 
              s(Mean_daily_Flow_log, by=Estuary, m=1) +
              s(Wind_speed) +
              s(CalYear) + 
              s(AuthorisedFisherID, bs="re") +
              offset(log(Effort_days)),
            data=weekly, family=tw(), method="REML")
summary(M3)
plot(M3, scale=0, pages=1, scheme=1)
gam.check(M3)
## more variation at weekly scale which is harder to explain


### PREDICT BEST MODELS (M1.3, M3)

P1.3 <- predict(M1.3, type="response")
monthly$fitted <- P1.3
P1.3 <- predict(M1.3, type="response", newdata=monthly, exclude="s(AuthorisedFisherID)")
monthly$fittedNoRE <- P1.3

summ <- monthly %>% filter(Estuary == "Wallis Lake") %>%  #"Lake Illawarra" "Tuggerah Lakes" "Wallis Lake" 
  group_by(CalYear, Month) %>% summarise(total_flathead = sum(Response)) %>%
  mutate(Date = as.Date(paste0(CalYear,"-",Month,"-15")))

summF <- monthly %>% filter(Estuary == "Wallis Lake") %>%  #"Lake Illawarra" "Tuggerah Lakes" "Wallis Lake" 
  group_by(CalYear, Month) %>% summarise(fitted_flathead = sum(fitted)) %>%
  mutate(Date = as.Date(paste0(CalYear,"-",Month,"-15")))

summFnoRE <- monthly %>% filter(Estuary == "Wallis Lake") %>%  #"Lake Illawarra" "Tuggerah Lakes" "Wallis Lake" 
  group_by(CalYear, Month) %>% summarise(fitted_flathead = sum(fittedNoRE)) %>%
  mutate(Date = as.Date(paste0(CalYear,"-",Month,"-15")))


plot(summ$Date, summ$total_flathead, type="l", ylim=c(0,8000), lwd=2); points(summ$Date, summ$total_flathead)
lines(summF$Date, summF$fitted_flathead, col="blue"); points(summF$Date, summF$fitted_flathead, col="blue")
lines(summFnoRE$Date, summFnoRE$fitted_flathead, col="purple"); points(summFnoRE$Date, summFnoRE$fitted_flathead, col="purple")

### CROSS VALIDATION FOR PREDICTIVE PERFORMANCE
## Use LOYO
## And focus on final year forecasting

xxx

#### Predict out effects of flow from Model 1.3

# M1.3 <- gam(Response ~ Estuary +  #currently the 'final' model
#               s(Month, k=5, bs="cc") + 
#               s(Mean_daily_Flow_log) + 
#               s(Mean_daily_Flow_log, by=Estuary, m=1) +
#               s(Wind_speed) +
#               s(CalYear) + 
#               s(AuthorisedFisherID, bs="re") +
#               offset(log(Effort_days)),
#             data=monthly, family=tw(), 
#             knots=list(Month=c(0.1,12.1)), method="REML")


new_dat = expand.grid("Month" = median(monthly$Month),
                 "Mean_daily_Flow_log" = seq(min(monthly$Mean_daily_Flow_log), max(monthly$Mean_daily_Flow_log), by=0.05),
                 "Wind_speed" = median(monthly$Wind_speed),
                 "recruit_flow" = median(monthly$recruit_flow),
                 "CalYear" = median(monthly$CalYear),
                 "Estuary" = unique(monthly$Estuary),
                 "Effort_days" = median(monthly$Effort_days),
                 "AuthorisedFisherID" = 771845)


pred_values <- predict(M1.4, newdata = new_dat, se.fit = T, type="response", exclude = "AuthorisedFisherID")

new_dat$fit <- pred_values$fit
new_dat$se_fit <- pred_values$se.fit

min_max <- monthly %>% group_by(Estuary) %>%
  summarise(min_flow = min(Mean_daily_Flow_log),
            max_flow = max(Mean_daily_Flow_log))

new_dat <- new_dat %>% left_join(min_max)
new_dat <- new_dat %>% filter(Mean_daily_Flow_log >= min_flow) %>%
  filter(Mean_daily_Flow_log <= max_flow)

ggplot(new_dat, aes(Mean_daily_Flow_log, fit)) + geom_line() +
  facet_wrap(~Estuary, scales = "free_x") +
  geom_ribbon(aes(ymin=fit-se_fit, ymax= fit+se_fit), alpha=0.5)+
  geom_rug(data=monthly, aes(x=Mean_daily_Flow_log), inherit.aes = F)

ggsave("Flow predictions by estuary.png", dpi=300, width=21, height=14.8, units="cm")

# unlogged
ggplot(new_dat, aes(exp(Mean_daily_Flow_log), fit)) + geom_line() +
  facet_wrap(~Estuary) + 
  geom_ribbon(aes(ymin=fit-se_fit, ymax= fit+se_fit), alpha=0.5)
