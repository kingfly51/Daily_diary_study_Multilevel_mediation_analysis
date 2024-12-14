
#####1.导入数据并进行预处理#####
library(readxl)
use_data<-read_excel("use_data.xlsx")
use_data2<-use_data
udf<-use_data2[,c(3,7,21,20,11,35,37:39)]
udf<-na.omit(udf)#剔除缺失值
udf$number2<-as.character(udf$number2)
udf$SMA<-as.numeric(udf$SMA)
udf$gender<-as.numeric(udf$gender)
udf$nation<-as.numeric(udf$nation)
#剔除密集追踪期间值完全一样的被试。
library(tidyverse)
udf <- udf %>%
  group_by(number2) %>%                          # 按照Number分组
  filter(!(n_distinct(SMA) ==1 |n_distinct(ris) ==1 | n_distinct(anxiety) ==1 )) %>%  # 保留至少有一个不同的SMA或sd
  ungroup()   

#####2.Preliminary analyses#####
#检验性别、年龄和民族的影响
library(lme4)
agn_fit <- lmer(SMA ~ age + gender + nation + (1+age + gender + nation | number2), data = udf)
summary(agn_fit)
agn_fit2 <- lmer(anxiety ~ age + gender + nation + (1+age + gender + nation | number2), data = udf)
summary(agn_fit2)
agn_fit3 <- lmer(ris ~ age + gender + nation + (1+age + gender + nation | number2), data = udf)
summary(agn_fit3)


#####3.within-person level#####
data_centered <- udf %>%
  group_by(number2) %>%
  mutate(SMA = SMA - mean(SMA)) %>%
  mutate(anxiety = anxiety - mean(anxiety)) %>%
  mutate(ris = ris-mean(ris)) %>%
  ungroup()

library(multilevelmediation)
library(boot)
# 设置随机种子
set.seed(1234)
fit <- modmed.mlm(data_centered,
                  L2ID = "number2", X = "ris", Y = "SMA", M = "anxiety",covars.m = c("sec_day_vacation","Day"),covars.y = c("sec_day_vacation","Day"),
                  random.a = TRUE, random.b = TRUE, random.cprime = TRUE,
                  control=list(opt="nlm")
)
summary(fit$model)

fit2 <- modmed.mlm(data_centered,
                  L2ID = "number2", X = "SMA", Y = "dep", M = "sleep_duration",
                  random.a = FALSE, random.b = TRUE, random.cprime = TRUE,random.int.m=TRUE,random.int.y=TRUE,
                  control=list(opt="nlm")
)
summary(fit2$model)

anova(fit$model,fit2$model)

extract.modmed.mlm(fit) #returns all the estimates (fixed and random effects) from the model
extract.modmed.mlm(fit, type = "indirect") #returns the indirect effect estimate
extract.modmed.mlm(fit, type = "a") #returns the 'a' path estimate
extract.modmed.mlm(fit, type = "b") #returns the 'b' path estimate
extract.modmed.mlm(fit, type = "cprime") #returns the 'cprime' path estimate
extract.modmed.mlm(fit, type = "covab") #returns the covariance between the random effects for
## the 'a' and 'b' paths (returns nothing if either random.a or random.b are FALSE)

library(parallel)
ncpu <- 8 # number of processing cores to use
RNGkind("L'Ecuyer-CMRG") # set type of random number generation that works in parallel
cl <- makeCluster(ncpu) # make a cluster
clusterSetRNGStream(cl, 9912) # set random number seeds for cluster
### Level 2 (L2) bootstrapping

data_centered<-as.data.frame(data_centered)
boot.result_L2 <- boot(data_centered,
                       statistic = boot.modmed.mlm, R = 1000,
                       L2ID = "number2", X = "ris", Y = "SMA", M = "anxiety",covars.m = c("sec_day_vacation","Day"),covars.y = c("sec_day_vacation","Day"),
                       random.a = TRUE, random.b = TRUE, random.cprime = TRUE,
                       type = "all", boot.lvl = "2",
                       control=list(opt="nlm"),
                       parallel="snow", ncpus=ncpu, cl=cl # omit this line if no parallel processing
)

#检查结果，默认情况下，95%的置信区间采用百分位数方法返回。
extract.boot.modmed.mlm(boot.result_L2, type="indirect", ci.conf=.95)#med=0.037,95%CI=[0.003,0.077]


######4. between-person level ######
library(bmlm)
library(mediation)
udf_bl <- isolate(udf, 
               by = "number2", 
               value = c("SMA", "ris", "anxiety"),
               which="between")#"within","both"
data_bl<-udf_bl[,c("number2","SMA_cb","ris_cb","anxiety_cb")]
data_bl<-unique(data_bl)
# 中介变量模型
model.M <- lm(anxiety_cb ~ ris_cb, data = data_bl)
summary(model.M)
# 因变量模型
model.Y <- lm(SMA_cb ~ anxiety_cb + ris_cb, data = data_bl)
summary(model.Y)
# 进行中介效应分析
mediation_result <- mediate(model.M, model.Y, treat = "ris_cb", mediator = "anxiety_cb", sims = 1000)
# 查看中介效应结果
summary(mediation_result)#med=0.09,95%CI=[0.02,0.17]


