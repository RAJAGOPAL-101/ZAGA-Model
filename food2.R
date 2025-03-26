
Rooster_data=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/bihar consumption/Roster_updated_use.csv",header=T)
Rooster_data=subset(Rooster_data,Rooster_data$Sl_no=="1")
write.csv(Rooster_data,"C://Users//jayk6//OneDrive//Desktop//Rajagoapl//bihar consumption//Rooster Data.csv",row.names = F)

Fruit_data= read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/Bihar Quality food data/Fruit con and exp.csv",header=T)
percentile_95 <- quantile(Fruit_data$Household.Consumption, 0.95, na.rm = TRUE)
Fruit_data <- Fruit_data[Fruit_data$Household.Consumption <= percentile_95, ]
Fruit_data$Education[is.na(Fruit_data$Education)]=1
Fruit_data$Education <- ifelse(Fruit_data$Education %in% c(2, 3,10), 2,Fruit_data$Education)
Fruit_data$Education <- ifelse(Fruit_data$Education %in% c(4, 5), 3,Fruit_data$Education)
Fruit_data$Education <- ifelse(Fruit_data$Education %in% c(6, 7), 4,Fruit_data$Education)
Fruit_data$Education <- ifelse(Fruit_data$Education %in% c(8, 9), 5,Fruit_data$Education)
Fruit_data$Education <- ifelse(Fruit_data$Education %in% c(1, 11), 1,Fruit_data$Education)


Fruit_data$Size.of.the.land[is.na(Fruit_data$Size.of.the.land)]=0
sapply(Fruit_data,class)
Fruit_data$Gender=as.numeric(Fruit_data$Gender)
Fruit_data$Education=as.factor(Fruit_data$Education)
Fruit_data$Own.any.Agriculture.Land=as.factor(Fruit_data$Own.any.Agriculture.Land)
Summary_data_fruit=setdiff(names(Fruit_data),c("UniqueId","Name"               
                                                ))

summary_fruit_data <- Fruit_data %>%
  select(all_of(Summary_data_fruit))            
Summary
tbl_summary(summary_fruit_data)
Fruit_data$Gender= as.numeric(Fruit_data$Gender)

#Checking For Normality for House hold Consumption
shapiro.test(Fruit_data$Household.Consumption)#not normal

#Performing a non parametric test HH cons and Gender
#H0=there is no significant difference b/w median
wilcox.test(Fruit_data$Household.Consumption~Fruit_data$Gender)
#Since P-value= 0.014 is less than 0.05 we reject H0
#There is significant difference
median(Fruit_data[Fruit_data$Gender==1,]$Household.Consumption)


#Performing Non para test for HH cons and education
#H0=there is no correlation
kruskal.test(Fruit_data$Household.Consumption~Fruit_data$Education)
#since p-value=.2e-16 is less than 0.05 we reject H0 and rho =0.1853118
#there is significant correlation


#performing a non para test for Hh con and Owns any land
#H0=there is no significant difference
wilcox.test(Fruit_data$Household.Consumption~Fruit_data$Own.any.Agriculture.Land)
#Since  p-value = 2.765e-10 we reject H0
#There is significant difference 

#Checking for Normality of MPCE
shapiro.test(Fruit_data$MPCE) #not normal
#performing a non para test for HH Cons And MPCE
#H0=there is no significant difference
z=cor.test(Fruit_data$Household.Consumption,Fruit_data$MPCE,method="spearman",exact = F)
#Since p-value =2.2e-16 less than 0.05 we reject H0 and rho= 0.3164
#There is significant difference


quantile(Fruit_data[Fruit_data$Own.any.Agriculture.Land==2,]$Household.Consumption)

ggplot(Fruit_data, aes(Household.Consumption = Household.Consumption, MPCE = MPCE)) +
  geom_point() +
  geom_smooth(method = "lm", formula = Household.Consumption ~ MPCE, se = FALSE, color = "red") +
  labs(title = paste("Spearman correlation =", round(z$parameter, 2)))
#representingRegression Model of zero adjusted Model Gamma model
library(gamlss)
Model=gamlss(Fruit_data$Household.Consumption ~ Fruit_data$Gender+Fruit_data$MPCE+Fruit_data$Education+Fruit_data$Own.any.Agriculture.Land,sigma.formula   = ~Fruit_data$Gender+Fruit_data$MPCE+Fruit_data$Education+Fruit_data$Own.any.Agriculture.Land ,nu.formula = ~Fruit_data$Gender+Fruit_data$MPCE+Fruit_data$Education+Fruit_data$Own.any.Agriculture.Land ,  family =ZAGA)
write.csv(Fruit_data,"C://Users//jayk6//OneDrive//Desktop//Rajagoapl//bihar consumption//Model.csv",row.names = F)
summary(Model)
Fruit_data$Gender <- as.factor(Fruit_data$Gender)
Fruit_data$Education <- as.factor(Fruit_data$Education)
Fruit_data$Own.any.Agriculture.Land <- as.factor(Fruit_data$Own.any.Agriculture.Land)
Model=gamlss(Fruit_data$Household.Consumption ~ Fruit_data$MPCE, family = ZAGA)
summary(Model)
Model=gamlssMX(  Fruit_data$Household.Consumption ~ Fruit_data$Gender+Fruit_data$MPCE+Fruit_data$Education+Fruit_data$Own.any.Agriculture.Land,family = MIX)
Model=gamlss.mx

quantile(Fruit_data$Household.Consumption,probs = seq(0,1,by=0.025))

Model=gamlss(Fruit_data$Household.Consumption ~ Fruit_data$Gender+Fruit_data$MPCE+Fruit_data$Education+Fruit_data$Own.any.Agriculture.Land,
             sigma.formula   = ~Fruit_data$Gender+Fruit_data$MPCE+Fruit_data$Education+Fruit_data$Own.any.Agriculture.Land ,
             nu.formula = ~Fruit_data$Gender+Fruit_data$MPCE+Fruit_data$Education+Fruit_data$Own.any.Agriculture.Land ,  family =ZAGA)
percentile_95 <- quantile(Fruit_data$Household.Consumption, 0.95, na.rm = TRUE)
Fruit_data <- Fruit_data[Fruit_data$Household.Consumption <= percentile_95, ]
hist(Fruit_data$Household.Consumption,freq = F,xlab ="Per person Household consumption",ylab = "No.of Households",main = "")
density <- density(Fruit_data$Household.Consumption)
lines(density, col="blue", lwd=2)
mu <- mean(Fruit_data$Household.Consumption)
sigma <- sd(Fruit_data$Household.Consumption)
curve(dZAGA(x, mean =Model$mu.coefficients, sd = sigma), col = "red", lwd = 2, add = TRUE)

library(rcompanion) 

# create data vector 
x= c(rnorm(10000, 2000, 45)) 

# draw plot using plotNormalHistogram() function 
# use colour and size arguments for formatting plot 
plotNormalHistogram( x, prob = FALSE, col="black", border="green", 
                     main = "Normal Distribution overlay on Histogram", 
                     length = 10000, linecol="red", lwd=3 ) 
