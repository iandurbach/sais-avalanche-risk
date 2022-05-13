library(mgcv)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

data <- readRDS("output/cleaned_fulldataset.Rds")

# renaming variables to get rid of white space
names(data) <- str_replace_all(names(data), " ", "_")
names(data)

# NA if 9999
data <- data %>% na_if(-9999)

# check distribution of Air Temp
hist(data$Air_Temp)

# remove any extreme Air Temps
data <- data %>% filter(Air_Temp >= -20, Air_Temp <= 20)

# # OR replace extreme Air Temps with some value
# data <- data %>% mutate(Air_Temp = pmin(Air_Temp, 20),
#                         Air_Temp = pmax(Air_Temp, -20))

# check distribution of Air Temp
hist(data$Air_Temp)
summary(data$Air_Temp)
sort(data$Air_Temp)[1:10]

# convert avalanche risks to values in range [0,1]
data$OAH_num01 <- (data$OAH_num-1)/4
summary(data$OAH_num) # categorical
summary(data$OAH_num01) # proportion
hist(data$OAH_num)
hist(data$OAH_num01)

### filter by area here if you want

data <- data %>% filter(Area == "Lochaber")

### model building

# basic GAM with beta response
gmod1 <- gam(OAH_num01 ~ s(Air_Temp), data=data,family=betar(link="logit"))
summary(gmod1)
plot(gmod1)
gam.check(gmod1)

# always look at the help when in doubt
?betar

# says eps can give numerical problems, so try increasing eps
gmod2 <- gam(OAH_num01 ~ s(Air_Temp), data=data,family=betar(link="logit", eps = 0.01))
summary(gmod2)
plot(gmod2)
gam.check(gmod2) # QQ plots look a bit better

# ordered categorical response is probably better for this data
gmod3 <- gam(OAH_num ~ s(Air_Temp), data=data,family=ocat(R=5))
summary(gmod3)
plot(gmod3)
gam.check(gmod3)

# help with ocat
?ocat

### interpreting the ordered categorical model

# predictions for ocat responses are probabilities of falling into each category
xx <- predict.gam(gmod3, type = "response")
View(xx)

# this shows how probability of falling into each risk category changes as a function 
# of air temp
dat <- data %>% filter(!is.na(OAH_num), !is.na(Air_Temp))
mycols <- levels(dat$OAH)
ocat_model <- gmod3
ocat_predict = predict(ocat_model,type = "response")
colnames(ocat_predict) = mycols
ocat_predict = as.data.frame(ocat_predict)%>%
  mutate(x= dat$Air_Temp,y=factor(dat$OAH_num, levels = 1:5, labels = mycols)) %>%
  #mutate(x= fitted(ocat_model),y=as.numeric(dat$OAH_num))%>%
  gather(pred_level,prediction,mycols) %>%
  mutate(obs_val = as.numeric(y==pred_level)) %>%
  mutate(pred_level = factor(pred_level, levels = mycols))

ggplot(aes(x= x, y= obs_val),data=ocat_predict)+
  facet_grid(.~pred_level)+
  geom_point()+
  geom_line(aes(y= prediction))+
  xlab("Air Temperature") + ylab("Probability") +
  theme_bw() 

## added 13/5


### Model comparison example

dat <- data %>% 
  filter(!is.na(OAH_num), !is.na(Air_Temp), !is.na(Wind_Speed), Wind_Speed < 100)

gmod4 <- gam(OAH_num ~ s(Air_Temp), data=dat,family=ocat(R=5))
gmod5 <- gam(OAH_num ~ s(Wind_Speed), data=dat,family=ocat(R=5))
gmod6 <- gam(OAH_num ~ s(Air_Temp) + s(Wind_Speed), data=dat,family=ocat(R=5))
gmod7 <- gam(OAH_num ~ s(Air_Temp,Wind_Speed), data=dat,family=ocat(R=5))
AIC(gmod4,gmod5,gmod6,gmod7)

# best model has lowest AIC
gam.check(gmod6)

# predictions for ocat responses are probabilities of falling into each category
xx <- predict.gam(gmod6, type = "response")
View(xx)

### Prediction plots for models with more than one variable

# try the same code as before to plot effect of air temp on risk

mycols <- levels(dat$OAH)
ocat_model <- gmod6
ocat_predict = predict(ocat_model,type = "response")
colnames(ocat_predict) = mycols
ocat_predict = as.data.frame(ocat_predict)%>%
  mutate(x= dat$Air_Temp,y=factor(dat$OAH_num, levels = 1:5, labels = mycols)) %>%
  #mutate(x= fitted(ocat_model),y=as.numeric(dat$OAH_num))%>%
  gather(pred_level,prediction,mycols) %>%
  mutate(obs_val = as.numeric(y==pred_level)) %>%
  mutate(pred_level = factor(pred_level, levels = mycols))

ggplot(aes(x= x, y= obs_val),data=ocat_predict)+
  facet_grid(.~pred_level)+
  geom_point()+
  geom_line(aes(y= prediction))+
  xlab("Air Temperature") + ylab("Probability") +
  theme_bw() 

# note that this doesn't work, because we are not holding Wind Speed constant when we 
# make predictions on our observed data (two observations might have the same Air Temp
# but two different Wind Speeds -- they will end up with different predictions, which is
# fine, but then we don't want to plot those values when assessing the MARGINAL effect
# of Air Temp... that's why we get the jagged/sawtooth up-down pattern in the plot above)

# we need to create a new dataset that varies Air Temp and holds Wind Speed constant

mycols <- levels(dat$OAH)
newdat <- data.frame(Air_Temp = seq(from = min(dat$Air_Temp), to = max(dat$Air_Temp), length.out = 500),
                     Wind_Speed = mean(dat$Wind_Speed))

# now we can use a similar plotting code to before, except applied to this newdata we created
ocat_model <- gmod6
# note use of "newdata" argument, so predictions are for this new data
ocat_predict = predict(ocat_model, newdata = newdat, type = "response") 

ocat_predict = as.data.frame(ocat_predict)
colnames(ocat_predict) <- mycols
ocat_predict <- ocat_predict %>%
  mutate(x= newdat$Air_Temp) %>%
  pivot_longer(1:5, names_to = "pred_level", values_to = "obs_val") 
ocat_predict$pred_level <- factor(ocat_predict$pred_level, levels = mycols)

ggplot(aes(x= x, y= obs_val),data=ocat_predict)+
  facet_grid(.~pred_level)+
  geom_line()+
  xlab("Air Temperature") + ylab("Probability") +
  theme_bw() 

# do something similar for a plot of effect of wind speed

newdat <- data.frame(Wind_Speed = seq(from = min(dat$Wind_Speed), to = max(dat$Wind_Speed), length.out = 500),
                     Air_Temp = mean(dat$Air_Temp))

ocat_model <- gmod6
ocat_predict = predict(ocat_model, newdata = newdat, type = "response")
ocat_predict = as.data.frame(ocat_predict)
colnames(ocat_predict) <- mycols
ocat_predict <- ocat_predict %>%
  mutate(x= newdat$Wind_Speed) %>%
  pivot_longer(1:5, names_to = "pred_level", values_to = "obs_val") 
ocat_predict$pred_level <- factor(ocat_predict$pred_level, levels = mycols)

ggplot(aes(x= x, y= obs_val),data=ocat_predict)+
  facet_grid(.~pred_level)+
  geom_line()+
  xlab("Wind Speed") + ylab("Probability") +
  theme_bw() 




