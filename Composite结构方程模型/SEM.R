#piecewiseSEM
library(nlme)
library(lme4)
library(piecewiseSEM)
library(QuantPsyc)
#读取数据
Rt<-read.csv(file.choose(),header = T,row.names = 1)
#气候变量的组合
model1 <- lm(N_effect ~  PDQ + PCQ + PWaQ, Rt)
coefs(model1, standardize = 'scale')
beta_PDQ <-  summary(model1)$coefficients[2, 1]
beta_PCQ <- summary(model1)$coefficients[3, 1]
beta_PWaQ <-  summary(model1)$coefficients[4, 1]
climate <- beta_PDQ * Rt$PDQ + beta_PCQ * Rt$PCQ + beta_PWaQ * Rt$PWaQ
Rt$climate <- climate
summary(lm(N_effect ~ climate, Rt))
coefs(lm(N_effect ~ climate, Rt))
#生态系统类型变量的组合
model2 <- lm(N_effect ~ Cropland + Forest + wetland + Grassland, Rt)
summary(model2)$coefficients
beta_Cropland <- summary(model2)$coefficients[2, 1]
beta_Forest <-  summary(model2)$coefficients[3, 1]
beta_wetland <- summary(model2)$coefficients[4, 1]
ecosystem <- beta_Cropland * Rt$Cropland + beta_Forest * Rt$Forest +  beta_wetland * Rt$wetland
Rt$ecosystem <- ecosystem
summary(lm(N_effect ~ ecosystem, Rt))
coefs(lm(N_effect ~ ecosystem, Rt))
#土壤变量的组合
model3 <- lm(N_effect ~ SOC + TN + C_N, Rt)
coefs(model3, standardize = 'scale')
beta_SOC <- summary(model3)$coefficients[2, 1]
beta_TN <-  summary(model3)$coefficients[3, 1]
beta_C_N <- summary(model3)$coefficients[4, 1]
soil <- beta_SOC * Rt$SOC + beta_TN * Rt$TN + beta_C_N * Rt$C_N
Rt$soil <- soil
summary(lm(N_effect ~ soil, Rt))
coefs(lm(N_effect ~ soil, Rt)) 
#地理变量的组合
model0 <- lm(N_effect ~ Lon + Lat, Rt)
coefs(model0, standardize = 'scale')
beta_Lon <-  summary(model0)$coefficients[2, 1]
beta_Lat <- summary(model0)$coefficients[3, 1]
geography <- beta_Lon * Rt$Lon + beta_Lat * Rt$Lat
Rt$geography <- geography
summary(lm(N_effect ~ geography, Rt))
coefs(lm(N_effect ~ geography, Rt))
#基于混合效应模型的多元回归
microbe.list <- list(
  lme(climate ~ geography , random = ~ 1 | site , na.action = na.omit,
      data = Rt),
  lme(soil ~ climate + Rate+ ecosystem, random = ~ 1 | site , na.action = na.omit,
      data = Rt),
  lme(N_effect ~   climate + ecosystem + soil, random = ~ 1 | site , na.action = na.omit,
      data = Rt)
)
#SEM
microbe.psem <- as.psem(microbe.list)
(new.summary <- summary(microbe.psem, .progressBar = F))
plot(microbe.psem)


install.packages("piecewiseSEM",version="4.2")
