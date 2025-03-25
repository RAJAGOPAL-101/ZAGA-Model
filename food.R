
library(dplyr)
#loading Data of Meat
Consumed_data=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/Bihar Quality food data/consumed items -meat,fish,egg.csv")
sapply(Consumed_data,class)
#Converting kg to g
Consumed_data=mutate_at(Consumed_data, vars(c("q2.2.5.2.5","q2.2.5.2.6","q2.2.5.2.7","q2.2.5.2.8","q2.2.5.2.9","q2.2.5.2.10")), funs(. * 1000))
#Total consuption of meat per family
Consumed_data$Total_cons_meat=rowSums(Consumed_data[,c("q2.2.5.2.5","q2.2.5.2.6","q2.2.5.2.7","q2.2.5.2.8","q2.2.5.2.9","q2.2.5.2.10")],na.rm = T)
#Average House hold Consuption of meat
Consumed_data$HH_consumption=Consumed_data$Total_cons_meat/Consumed_data$q1.2.2
mean(Consumed_data$HH_consumption)
mean(Consumed_data$HH_consumption)/30
median(Consumed_data$HH_consumption)
median(Consumed_data$HH_consumption)/30
hist(Consumed_data$HH_consumption)

#Total consuption of fish per family
Consumed_data$Total_cons_fish=rowSums(Consumed_data[,c("q2.2.5.2.3","q2.2.5.2.4","q2.2.5.2.5")],na.rm = T)
#converting kg to g
Consumed_data$Total_cons_fish=Consumed_data$Total_cons_fish*1000
#Average House hold Consuption of fish
Consumed_data$HH_consumption_fish=Consumed_data$Total_cons_fish/Consumed_data$q1.2.2
mean(Consumed_data$HH_consumption_fish)
mean(Consumed_data$HH_consumption_fish)/30
hist(Consumed_data$HH_consumption_fish)
median(Consumed_data$HH_consumption_fish)
median(Consumed_data$HH_consumption_fish)/30

#Loading data of milk and milk product
Consumed_milk_data=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/Bihar Quality food data/milk and milk product.csv",header = T)
#Total consuption of milk per family
Consumed_milk_data$Total_cons_milk=rowSums(Consumed_milk_data[,c( "q2.2.4.2.1","q2.2.4.2.2","q2.2.4.2.3","q2.2.4.2.4")],na.rm = T)
#Converting liter to ml
Consumed_milk_data$Total_cons_milk=Consumed_milk_data$Total_cons_milk*1000
#Average House hold Consuption of milk
Consumed_milk_data$HH_consumption_milk=Consumed_milk_data$Total_cons_milk/Consumed_milk_data$q1.2.2
mean(Consumed_milk_data$HH_consumption_milk)
mean(Consumed_milk_data$HH_consumption_milk)/7
hist(Consumed_milk_data$HH_consumption_milk)
median(Consumed_milk_data$HH_consumption_milk)
median(Consumed_milk_data$HH_consumption_milk)/7

#total consuption of milk product per family
Consumed_milk_data$Total_cons_milk_prdct=rowSums(Consumed_milk_data[,c("q2.2.4.2.5","q2.2.4.2.6","q2.2.4.2.7","q2.2.4.2.8","q2.2.4.2.9","q2.2.4.2.10","q2.2.4.2.11")],na.rm = T)
#converting kg to g
Consumed_milk_data$Total_cons_milk_prdct=Consumed_milk_data$Total_cons_milk_prdct*1000
#Average House hold Consuption of milk product
Consumed_milk_data$HH_consumption_milk_prdct=Consumed_milk_data$Total_cons_milk_prdct/Consumed_milk_data$q1.2.2
mean(Consumed_milk_data$HH_consumption_milk_prdct)
mean(Consumed_milk_data$HH_consumption_milk_prdct)/7
hist(Consumed_milk_data$HH_consumption_milk_prdct)
median(Consumed_milk_data$HH_consumption_milk_prdct)
median(Consumed_milk_data$HH_consumption_milk_prdct)/7

#Total consumption of egg per family
Consumed_data$Total_cons_egg=rowSums(Consumed_data[,c("q2.2.5.2.1","q2.2.5.2.2" )],na.rm = T)
#Average House hold consuption of egg
Consumed_data$HH_consumption_egg=Consumed_data$Total_cons_egg/Consumed_data$q1.2.2
mean(Consumed_data$HH_consumption_egg)
mean(Consumed_data$HH_consumption_egg)/30

#Loading data of Fruits
Consumed_fruit_data=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/Bihar Quality food data/Fruits.csv",header = T)
#Total consumption of fruits(excluding banana,pineapple and orange,musambi) per family in last weeks
Consumed_fruit_data$Total_cons_fruit=rowSums(Consumed_fruit_data[,c("q2.2.8.2.2",  "q2.2.8.2.3","q2.2.8.2.5"  , "q2.2.8.2.7",  "q2.2.8.2.8" , "q2.2.8.2.9", "q2.2.8.2.10", "q2.2.8.2.11", "q2.2.8.2.12", "q2.2.8.2.13" ,"q2.2.8.2.14", "q2.2.8.2.15","q2.2.8.2.16" ,"q2.2.8.2.17" ,"q2.2.8.2.18" ,"q2.2.8.2.19" ,"q2.2.8.2.20", "q2.2.8.2.21")],na.rm=T)
#converting kg to g
Consumed_fruit_data$Total_cons_fruit=Consumed_fruit_data$Total_cons_fruit*1000
#average of HH consumption of Fruit
Consumed_fruit_data$HH_consumption_fruit=Consumed_fruit_data$Total_cons_fruit/Consumed_fruit_data$q1.2.2
mean(Consumed_fruit_data$HH_consumption_fruit)
mean(Consumed_fruit_data$HH_consumption_fruit)/7
hist(Consumed_fruit_data$HH_consumption_fruit)
median(Consumed_fruit_data$HH_consumption_fruit)
median(Consumed_fruit_data$HH_consumption_fruit)/7

#Total consumption of fruits-banana,pineapple and orange,musambi only
Consumed_fruit_data$Total_cons_fruit_x=rowSums(Consumed_fruit_data[,c("q2.2.8.2.1","q2.2.8.2.4","q2.2.8.2.6")],na.rm = T)
#average of HH consumption of fruits-banana,pineapple and orange,musambi only
Consumed_fruit_data$HH_consumption_fruit_x=Consumed_fruit_data$Total_cons_fruit_x/Consumed_fruit_data$q1.2.2
mean(Consumed_fruit_data$HH_consumption_fruit_x)
mean(Consumed_fruit_data$HH_consumption_fruit_x)/7
hist(Consumed_fruit_data$HH_consumption_fruit_x)
median(Consumed_fruit_data$HH_consumption_fruit_x)
median(Consumed_fruit_data$HH_consumption_fruit_x)/7

#Loading data of Nuts and seeds
Consumed_Nuts_data=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/Bihar Quality food data/nuts and seeds.csv",header = T)
#Total consumption of Nuts and seeds
Consumed_Nuts_data$Total_cons_nuts=rowSums(Consumed_Nuts_data[,c("q2.2.10.3.1" , "q2.2.10.3.2"  ,"q2.2.10.3.3", "q2.2.10.3.4",  "q2.2.10.3.5" , "q2.2.10.3.6" )],na.rm = T)
#converting kg to g
Consumed_Nuts_data$Total_cons_nuts=Consumed_Nuts_data$Total_cons_nuts*1000
#Average of HH consumption of Nuts
Consumed_Nuts_data$HH_consumption_nuts=Consumed_Nuts_data$Total_cons_nuts/Consumed_Nuts_data$q1.2.2
mean(Consumed_Nuts_data$HH_consumption_nuts)
mean(Consumed_Nuts_data$HH_consumption_nuts)/30
hist(Consumed_Nuts_data$HH_consumption_nuts)
median(Consumed_Nuts_data$HH_consumption_nuts)
median(Consumed_Nuts_data$HH_consumption_nuts)/30

#loading Data vegetables
c=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/Bihar Quality food data/greeny veg.csv",header=T)
d=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/Bihar Quality food data/o veg.csv",header=T)
#Merging
Consumed_veg_data=merge(c,d,by="UniqueId")
#Total consumption of vegetables
Consumed_veg_data$Total_cons_veg=rowSums(Consumed_veg_data[,c("q2.2.6.2.1","q2.2.6.2.2", "q2.2.6.2.3"  ,"q2.2.6.2.4"  ,"q2.2.6.2.5"  ,"q2.2.6.2.6" , "q2.2.6.2.7" ,"q2.2.6.2.8"  ,"q2.2.6.2.9"  ,"q2.2.6.2.10" ,"q2.2.6.2.11", "q2.2.6.2.12","q2.2.6.2.13" ,"q2.2.6.2.14", "q2.2.6.2.15", "q2.2.6.2.16" ,"q2.2.7.2.1"  ,"q2.2.7.2.2"  ,"q2.2.7.2.3"  ,"q2.2.7.2.4" ,"q2.2.7.2.5" , "q2.2.7.2.6"  ,"q2.2.7.2.7" , "q2.2.7.2.8"  ,"q2.2.7.2.9" ,"q2.2.7.2.10" ,"q2.2.7.2.11" ,"q2.2.7.2.12" ,"q2.2.7.2.13", "q2.2.7.2.14","q2.2.7.2.15",  "q2.2.7.2.17", "q2.2.7.2.18", "q2.2.7.2.19" )],na.rm = T)
#converting kg to g
Consumed_veg_data$Total_cons_veg=Consumed_veg_data$Total_cons_veg*1000
#average consumption of vegetables
Consumed_veg_data$HH_consumption_veg=Consumed_veg_data$Total_cons_veg/Consumed_veg_data$q1.2.2.x
mean(Consumed_veg_data$HH_consumption_veg)
mean(Consumed_veg_data$HH_consumption_veg)/7
hist(Consumed_veg_data$HH_consumption_veg)
median(Consumed_veg_data$HH_consumption_veg)
median(Consumed_veg_data$HH_consumption_veg)/7

#average consumption of vegetables (lemon)
Consumed_veg_data$HH_consumption_lemon=(Consumed_veg_data$q2.2.7.2.16/Consumed_veg_data$q1.2.2.x)
mean(Consumed_veg_data$HH_consumption_lemon,na.rm = T)
median(Consumed_veg_data$HH_consumption_lemon,na.rm = T)
mean(Consumed_veg_data$HH_consumption_lemon,na.rm = T)/7
median(Consumed_veg_data$HH_consumption_lemon,na.rm = T)/7



