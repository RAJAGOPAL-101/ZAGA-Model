#Loading data of milk and milk product
Consumed_milk_data=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/Bihar Quality food data/milk and milk product.csv",header = T)
#Total consuption of milk per family
Consumed_milk_data$Total_cons_milk=rowSums(Consumed_milk_data[,c("q2.2.4.2.1","q2.2.4.2.2","q2.2.4.2.3","q2.2.4.2.4")],na.rm = T)
#Converting liter to ml
Consumed_milk_data$Total_cons_milk=Consumed_milk_data$Total_cons_milk*1000
#Average House hold Consuption of milk
Consumed_milk_data$HH_consumption_milk=Consumed_milk_data$Total_cons_milk/Consumed_milk_data$q1.2.2
mean(Consumed_milk_data$HH_consumption_milk)
mean(Consumed_milk_data$HH_consumption_milk)/7
hist(Consumed_milk_data$HH_consumption_milk)
median(Consumed_milk_data$HH_consumption_milk)
median(Consumed_milk_data$HH_consumption_milk)/7

#Loading data
Consumed_data_milk_milk_prdct=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/bihar consumption/milk and milk products.csv",header = T)
#Average consumption of milk produce from home
Consumed_data_milk_milk_prdct$milk_home_cons=rowSums(Consumed_data_milk_milk_prdct[,c("q2.2.4.3.2","q2.2.4.3.3","q2.2.4.3.4")],na.rm = T)
Consumed_data_milk_milk_prdct$HH_cons_home=Consumed_data_milk_milk_prdct$milk_home_cons/Consumed_data_milk_milk_prdct$q1.2.2
#Converting liter to ml
Consumed_data_milk_milk_prdct$HH_cons_home=Consumed_data_milk_milk_prdct$HH_cons_home*1000
mean(Consumed_data_milk_milk_prdct$HH_cons_home)
mean(Consumed_data_milk_milk_prdct$HH_cons_home)/7
median(Consumed_data_milk_milk_prdct$HH_cons_home)

#Average consumption of milk purchased from market
Consumed_data_milk_milk_prdct$milk_mrkt_cons=rowSums(Consumed_data_milk_milk_prdct[,c("q2.2.4.4.1","q2.2.4.4.2","q2.2.4.4.3","q2.2.4.4.4")],na.rm = T)
Consumed_data_milk_milk_prdct$HH_cons_mrkt=Consumed_data_milk_milk_prdct$milk_mrkt_cons/Consumed_data_milk_milk_prdct$q1.2.2
#converting litre to ml
Consumed_data_milk_milk_prdct$HH_cons_mrkt=Consumed_data_milk_milk_prdct$HH_cons_mrkt*1000
mean(Consumed_data_milk_milk_prdct$HH_cons_mrkt)
mean(Consumed_data_milk_milk_prdct$HH_cons_mrkt)/7

#Average consumption of milk received from ICDS
Consumed_data_milk_milk_prdct$milk_ICDS_cons=rowSums(Consumed_data_milk_milk_prdct[,c("q2.2.4.5.1","q2.2.4.5.2","q2.2.4.5.3","q2.2.4.5.4")],na.rm = T)
Consumed_data_milk_milk_prdct$HH_cons_ICDS=Consumed_data_milk_milk_prdct$milk_ICDS_cons/Consumed_data_milk_milk_prdct$q1.2.2
#Converting litre to ml
Consumed_data_milk_milk_prdct$HH_cons_ICDS=Consumed_data_milk_milk_prdct$HH_cons_ICDS*1000
mean(Consumed_data_milk_milk_prdct$HH_cons_ICDS)
mean(Consumed_data_milk_milk_prdct$HH_cons_ICDS)/7

#Average consumption of milk received as gift/loans
Consumed_data_milk_milk_prdct$milk_gift_cons=rowSums(Consumed_data_milk_milk_prdct[,c("q2.2.4.6.1","q2.2.4.6.2","q2.2.4.6.3","q2.2.4.6.4")],na.rm = T)
Consumed_data_milk_milk_prdct$HH_cons_gift=Consumed_data_milk_milk_prdct$milk_gift_cons/Consumed_data_milk_milk_prdct$q1.2.2
#Converting litre to  ml
Consumed_data_milk_milk_prdct$HH_cons_gift=Consumed_data_milk_milk_prdct$HH_cons_gift*1000
mean(Consumed_data_milk_milk_prdct$HH_cons_gift)
mean(Consumed_data_milk_milk_prdct$HH_cons_gift)/7


#Expenditure of milk
#Calculating per person consumption for each product
columns_to_divide <- c("q2.2.4.2.1","q2.2.4.2.2","q2.2.4.2.3","q2.2.4.2.4")  # Columns to divide
divider_column <- "q1.2.2"  # Column to divide by

# Perform the division
for (col in columns_to_divide) {
  Consumed_milk_data[[col]] <- Consumed_milk_data[[col]] / Consumed_milk_data[[divider_column]]
}

#Selecting price of products
selected_cols <-Consumed_data_milk_milk_prdct[, c("q2.2.4.7.1","q2.2.4.7.2","q2.2.4.7.3","q2.2.4.7.4")]
#selecting consumption rate of products
selected_cols2=Consumed_milk_data[,c("q2.2.4.2.1","q2.2.4.2.2","q2.2.4.2.3","q2.2.4.2.4")]
selected_cols[is.na(selected_cols)]=0
selected_cols2[is.na(selected_cols2)]=0

matrix_data1 <- as.matrix(selected_cols)
matrix_data2<- as.matrix(selected_cols2)
expenditure=data.frame(matrix_data1*matrix_data2)
#total house hold expenditure
Consumed_data_milk_milk_prdct$HH_expe_milk=rowSums(expenditure[,c("q2.2.4.7.1","q2.2.4.7.2","q2.2.4.7.3","q2.2.4.7.4")],na.rm=T)
#Average household hold expenditure of milk
mean(Consumed_data_milk_milk_prdct$HH_expe_milk)
median(Consumed_data_milk_milk_prdct$HH_expe_milk)
quantile(Consumed_data_milk_milk_prdct$HH_expe_milk,probs = seq(0,1,by=0.025))
###################################################################################################################################################################################################

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

#average consumption of milk product produced from home
Consumed_data_milk_milk_prdct$milk_prdct_home=rowSums(Consumed_data_milk_milk_prdct[,c("q2.2.4.3.7","q2.2.4.3.8","q2.2.4.3.9","q2.2.4.3.10","q2.2.4.3.11")],na.rm = T)
Consumed_data_milk_milk_prdct$HH_cons_prdct_home=Consumed_data_milk_milk_prdct$milk_prdct_home/Consumed_data_milk_milk_prdct$q1.2.2
#converting kg to g
Consumed_data_milk_milk_prdct$HH_cons_prdct_home=Consumed_data_milk_milk_prdct$HH_cons_prdct_home*1000
mean(Consumed_data_milk_milk_prdct$HH_cons_prdct_home)
mean(Consumed_data_milk_milk_prdct$HH_cons_prdct_home)/7

#Average consumption of milk product purchased frm market
Consumed_data_milk_milk_prdct$milk_prdct_mrkt=rowSums(Consumed_data_milk_milk_prdct[,c("q2.2.4.4.6","q2.2.4.4.7","q2.2.4.4.8","q2.2.4.4.9","q2.2.4.4.10","q2.2.4.4.11")],na.rm = T)
Consumed_data_milk_milk_prdct$HH_cons_prdct_mrkt=Consumed_data_milk_milk_prdct$milk_prdct_mrkt/Consumed_data_milk_milk_prdct$q1.2.2
#converting kg to g
Consumed_data_milk_milk_prdct$HH_cons_prdct_mrkt=Consumed_data_milk_milk_prdct$HH_cons_prdct_mrkt*1000
mean(Consumed_data_milk_milk_prdct$HH_cons_prdct_mrkt)
mean(Consumed_data_milk_milk_prdct$HH_cons_prdct_mrkt)/7

#Averafe consumption of milk product received from ICDS
Consumed_data_milk_milk_prdct$milk_prdct_ICDS=rowSums(Consumed_data_milk_milk_prdct[,c("q2.2.4.5.5","q2.2.4.5.6","q2.2.4.5.7","q2.2.4.5.8","q2.2.4.5.9","q2.2.4.5.10","q2.2.4.5.11")],na.rm = T)
Consumed_data_milk_milk_prdct$HH_cons_prdct_ICDS=Consumed_data_milk_milk_prdct$milk_prdct_ICDS/Consumed_data_milk_milk_prdct$q1.2.2
#converting kg to g
Consumed_data_milk_milk_prdct$HH_cons_prdct_ICDS=Consumed_data_milk_milk_prdct$HH_cons_prdct_ICDS*1000
mean(Consumed_data_milk_milk_prdct$HH_cons_prdct_ICDS)
mean(Consumed_data_milk_milk_prdct$HH_cons_prdct_ICDS)/7

#Average consumption of milk product received as gift/loan
Consumed_data_milk_milk_prdct$milk_prdct_gift=rowSums(Consumed_data_milk_milk_prdct[,c("q2.2.4.6.5","q2.2.4.6.6","q2.2.4.6.7","q2.2.4.6.8","q2.2.4.6.9","q2.2.4.6.10","q2.2.4.6.11")],na.rm = T)
Consumed_data_milk_milk_prdct$HH_cons_prdct_gift=Consumed_data_milk_milk_prdct$milk_prdct_gift/Consumed_data_milk_milk_prdct$q1.2.2
#converting kg to g
Consumed_data_milk_milk_prdct$HH_cons_prdct_gift=Consumed_data_milk_milk_prdct$HH_cons_prdct_gift*1000
mean(Consumed_data_milk_milk_prdct$HH_cons_prdct_gift)
mean(Consumed_data_milk_milk_prdct$HH_cons_prdct_gift)/7



#Expenditure of milk product
#Calculating per person consumption for each product
columns_to_divide <- c("q2.2.4.2.5","q2.2.4.2.6","q2.2.4.2.7","q2.2.4.2.8","q2.2.4.2.9","q2.2.4.2.10","q2.2.4.2.11")  # Columns to divide
divider_column <- "q1.2.2"  # Column to divide by

# Perform the division
for (col in columns_to_divide) {
  Consumed_milk_data[[col]] <- Consumed_milk_data[[col]] / Consumed_milk_data[[divider_column]]
}

#Selecting price of products"
selected_cols <-Consumed_data_milk_milk_prdct[, c("q2.2.4.7.5","q2.2.4.7.6","q2.2.4.7.7","q2.2.4.7.8","q2.2.4.7.9","q2.2.4.7.10","q2.2.4.7.11")]
#selecting consumption rate of products
selected_cols2=Consumed_milk_data[,c("q2.2.4.2.5","q2.2.4.2.6","q2.2.4.2.7","q2.2.4.2.8","q2.2.4.2.9","q2.2.4.2.10","q2.2.4.2.11")]
matrix_data1 <- as.matrix(selected_cols)
matrix_data2<- as.matrix(selected_cols2)
expenditure=data.frame(matrix_data1*matrix_data2)
#total house hold expenditure
Consumed_data_milk_milk_prdct$HH_expe_milk_prdct=rowSums(expenditure[,c("q2.2.4.7.5","q2.2.4.7.6","q2.2.4.7.7","q2.2.4.7.8","q2.2.4.7.9","q2.2.4.7.10","q2.2.4.7.11")],na.rm=T)
#Average household hold expenditure of milk
mean(Consumed_data_milk_milk_prdct$HH_expe_milk_prdct)
median(Consumed_data_milk_milk_prdct$HH_expe_milk_prdct)
quantile(Consumed_data_milk_milk_prdct$HH_expe_milk_prdct,probs = seq(0,1,by=0.025))
############################################################################################################






###################################################################################################################################################################





#loading Data of Meat
Consumed_data=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/Bihar Quality food data/consumed items -meat,fish,egg.csv")
#Total consuption of meat per family
Consumed_data$Total_cons_meat=rowSums(Consumed_data[,c("q2.2.5.2.5","q2.2.5.2.6","q2.2.5.2.7","q2.2.5.2.8","q2.2.5.2.9","q2.2.5.2.10")],na.rm = T)
#Average House hold Consuption of meat
Consumed_data$HH_consumption=Consumed_data$Total_cons_meat/Consumed_data$q1.2.2
#Converting kg to g
Consumed_data$HH_consumption=Consumed_data$HH_consumption*1000
mean(Consumed_data$HH_consumption)
mean(Consumed_data$HH_consumption)/30
median(Consumed_data$HH_consumption)
median(Consumed_data$HH_consumption)/30
hist(Consumed_data$HH_consumption)
#Loading data of egg,meat,fish
Consumed_data_emf=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/bihar consumption/egg,fish,meat.csv",header = T)
#Total consumption of meat produced on house
Consumed_data_emf$meat_home_cons=rowSums(Consumed_data_emf[,c("q2.2.5.3.6","q2.2.5.3.7","q2.2.5.3.8" ,"q2.2.5.3.9","q2.2.5.3.10")],na.rm = T)
#Average consumption of meat produced on home
Consumed_data_emf$HH_consumption=Consumed_data_emf$meat_home_cons/Consumed_data_emf$q1.2.2
#converting Kg to Gg
Consumed_data_emf$HH_consumption=Consumed_data_emf$HH_consumption*1000
mean(Consumed_data_emf$HH_consumption)
mean(Consumed_data_emf$HH_consumption)/30
median(Consumed_data_emf$HH_consumption)

#Average consumption of meat that was purchased frm market
Consumed_data_emf$meat_mrkt_cons=rowSums(Consumed_data_emf[,c("q2.2.5.4.6","q2.2.5.4.7","q2.2.5.4.8" ,"q2.2.5.4.9","q2.2.5.4.10")],na.rm=T)
Consumed_data_emf$HH_consumption_mrkt=Consumed_data_emf$meat_mrkt_cons/Consumed_data_emf$q1.2.2
#convert Kg to g
Consumed_data_emf$HH_consumption_mrkt=Consumed_data_emf$HH_consumption_mrkt*1000
mean(Consumed_data_emf$HH_consumption_mrkt)
mean(Consumed_data_emf$HH_consumption_mrkt)/30
median(Consumed_data_emf$HH_consumption_mrkt)
#Average consumption of meat received from ICDS
Consumed_data_emf$meat_ICDS_cons=rowSums(Consumed_data_emf[,c("q2.2.5.5.6","q2.2.5.5.7","q2.2.5.5.8" ,"q2.2.5.5.9","q2.2.5.5.10")])
Consumed_data_emf$HH_consumption_ICDS=Consumed_data_emf$meat_ICDS_cons/Consumed_data_emf$q1.2.2
#Converting kg to g 
Consumed_data_emf$HH_consumption_ICDS=Consumed_data_emf$HH_consumption_ICDS*1000
mean(Consumed_data_emf$HH_consumption_ICDS,na.rm = T)
mean(Consumed_data_emf$HH_consumption_ICDS,na.rm = T)/30

#Average consumption of meat received as gift/loan
Consumed_data_emf$meat_gift_cons=rowSums(Consumed_data_emf[,c("q2.2.5.6.6","q2.2.5.6.7","q2.2.5.6.8" ,"q2.2.5.6.9","q2.2.5.6.10")],na.rm = T)
Consumed_data_emf$HH_consumption_gift=Consumed_data_emf$meat_gift_cons/Consumed_data_emf$q1.2.2
#converting kg to g
Consumed_data_emf$HH_consumption_gift=Consumed_data_emf$HH_consumption_gift*1000
mean(Consumed_data_emf$HH_consumption_gift)
mean(Consumed_data_emf$HH_consumption_gift)/30
median(Consumed_data_emf$HH_consumption_gift)


#Expenditure of meat
#Calculating per person consumption for each product
columns_to_divide <- c("q2.2.5.2.5","q2.2.5.2.6","q2.2.5.2.7","q2.2.5.2.8","q2.2.5.2.9","q2.2.5.2.10")  # Columns to divide
divider_column <- "q1.2.2"  # Column to divide by

# Perform the division
for (col in columns_to_divide) {
  Consumed_data[[col]] <- Consumed_data[[col]] / Consumed_data[[divider_column]]
}

#Selecting price of products
selected_cols <-Consumed_data_emf[, c("q2.2.5.7.5","q2.2.5.7.6","q2.2.5.7.7","q2.2.5.7.8","q2.2.5.7.9","q2.2.5.7.10")]
#selecting consumption rate of products
selected_cols2=Consumed_data[,c("q2.2.5.2.5","q2.2.5.2.6","q2.2.5.2.7","q2.2.5.2.8","q2.2.5.2.9","q2.2.5.2.10")]
matrix_data1 <- as.matrix(selected_cols)
matrix_data2<- as.matrix(selected_cols2)
expenditure=data.frame(matrix_data1*matrix_data2)
#total house hold expenditure
Consumed_data_emf$HH_expe_meat=rowSums(expenditure[,c("q2.2.5.7.5","q2.2.5.7.6","q2.2.5.7.7","q2.2.5.7.8","q2.2.5.7.9","q2.2.5.7.10")],na.rm=T)
#Average household hold expenditure of meat
mean(Consumed_data_emf$HH_expe_meat)
median(Consumed_data_emf$HH_expe_meat)
quantile(Consumed_data_emf$HH_expe_meat,probs = seq(0,1,by=0.025))
###########################################################################################################




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
#Average consumption of fish produce on home
Consumed_data_emf$fish_home_cons=rowSums(Consumed_data_emf[,c("q2.2.5.3.3","q2.2.5.3.4","q2.2.5.3.5")],na.rm = T)
Consumed_data_emf$HH_Cons_fish_home=Consumed_data_emf$fish_home_cons/Consumed_data_emf$q1.2.2
#converting kg to g
Consumed_data_emf$HH_Cons_fish_home=Consumed_data_emf$HH_Cons_fish_home*1000
mean(Consumed_data_emf$HH_Cons_fish_home)
mean(Consumed_data_emf$HH_Cons_fish_home)/30
median(Consumed_data_emf$HH_Cons_fish_home)
#Average consumption of fish purchacrd frm market
Consumed_data_emf$fish_mrkt_cons=rowSums(Consumed_data_emf[,c("q2.2.5.4.3","q2.2.5.4.4","q2.2.5.4.5")],na.rm = T)
Consumed_data_emf$HH_Cons_fish_mrkt=Consumed_data_emf$fish_mrkt_cons/Consumed_data_emf$q1.2.2
#converting kg to g
Consumed_data_emf$HH_Cons_fish_mrkt=Consumed_data_emf$HH_Cons_fish_mrkt*1000
mean(Consumed_data_emf$HH_Cons_fish_mrkt)
mean(Consumed_data_emf$HH_Cons_fish_mrkt)/30
median(Consumed_data_emf$HH_Cons_fish_mrkt)
#average consumption of fish received from ICDS
Consumed_data_emf$fish_ICDS_cons=rowSums(Consumed_data_emf[,c("q2.2.5.5.3","q2.2.5.5.4","q2.2.5.5.5")],na.rm=T)
Consumed_data_emf$HH_Cons_fish_ICDS=Consumed_data_emf$fish_ICDS_cons/Consumed_data_emf$q1.2.2
mean(Consumed_data_emf$HH_Cons_fish_ICDS)

#average consumption of fish received as gift /loan
Consumed_data_emf$fish_gift_cons=rowSums(Consumed_data_emf[,c("q2.2.5.6.3","q2.2.5.6.4","q2.2.5.6.5")],na.rm = T)
Consumed_data_emf$HH_Cons_fish_gift=Consumed_data_emf$fish_gift_cons/Consumed_data_emf$q1.2.2
#converting kg to g
Consumed_data_emf$HH_Cons_fish_gift=Consumed_data_emf$HH_Cons_fish_gift*1000
mean(Consumed_data_emf$HH_Cons_fish_gift)


#Expenditure of fish
#Calculating per person consumption for each product
columns_to_divide <- c("q2.2.5.2.3","q2.2.5.2.4","q2.2.5.2.5")  # Columns to divide
divider_column <- "q1.2.2"  # Column to divide by

# Perform the division
for (col in columns_to_divide) {
  Consumed_data[[col]] <- Consumed_data[[col]] / Consumed_data[[divider_column]]
}

#Selecting price of products
selected_cols <-Consumed_data_emf[, c("q2.2.5.7.3","q2.2.5.7.4","q2.2.5.7.5")]
selected_cols[is.na(selected_cols)]=0
#selecting consumption rate of products
selected_cols2=Consumed_data[,c("q2.2.5.2.3","q2.2.5.2.4","q2.2.5.2.5")]
selected_cols2[is.na(selected_cols2)]=0

matrix_data1 <- as.matrix(selected_cols)
matrix_data2<- as.matrix(selected_cols2)
expenditure=data.frame(matrix_data1*matrix_data2)
#total house hold expenditure
Consumed_data_emf$HH_expe_fish=rowSums(expenditure[,c("q2.2.5.7.3","q2.2.5.7.4","q2.2.5.7.5")],na.rm=T)
#Average household hold expenditure of fish per person
mean(Consumed_data_emf$HH_expe_fish)
median(Consumed_data_emf$HH_expe_fish)
quantile(Consumed_data_emf$HH_expe_fish,probs = seq(0,1,by=0.025))



###########################################################################################################


#Total consumption of egg per family
Consumed_data$Total_cons_egg=rowSums(Consumed_data[,c("q2.2.5.2.1","q2.2.5.2.2" )],na.rm = T)
#Average House hold consuption of egg
Consumed_data$HH_consumption_egg=Consumed_data$Total_cons_egg/Consumed_data$q1.2.2
mean(Consumed_data$HH_consumption_egg)
mean(Consumed_data$HH_consumption_egg)/30
hist(Consumed_data$HH_consumption_egg)
#Average consumption of eggs produced on home
Consumed_data_emf$HH_Cons_egg_home=Consumed_data_emf$q2.2.5.3.1/Consumed_data_emf$q1.2.2
mean(Consumed_data_emf$HH_Cons_egg_home,na.rm = T)
mean(Consumed_data_emf$HH_Cons_egg_home,na.rm = T)/30

#Average consumption of eggs purchased frm market
Consumed_data_emf$egg_mrkt_cons=rowSums(Consumed_data_emf[,c("q2.2.5.4.1","q2.2.5.4.2" )],na.rm = T)
Consumed_data_emf$HH_Cons_egg_mrkt=Consumed_data_emf$egg_mrkt_cons/Consumed_data_emf$q1.2.2
mean(Consumed_data_emf$HH_Cons_egg_mrkt)
mean(Consumed_data_emf$HH_Cons_egg_mrkt)/30

#Average consumption of eggs received frm ICDS
Consumed_data_emf$egg_ICDS_cons=rowSums(Consumed_data_emf[,c("q2.2.5.5.1","q2.2.5.5.2" )],na.rm = T)
Consumed_data_emf$HH_Cons_egg_ICDS=Consumed_data_emf$egg_ICDS_cons/Consumed_data_emf$q1.2.2
mean(Consumed_data_emf$HH_Cons_egg_ICDS)
mean(Consumed_data_emf$HH_Cons_egg_ICDS)/30

#Average consumption of eggs received as Gifts/loan
Consumed_data_emf$egg_gift_cons=rowSums(Consumed_data_emf[,c("q2.2.5.6.1","q2.2.5.6.2" )],na.rm = T)
Consumed_data_emf$HH_Cons_egg_gift=Consumed_data_emf$egg_gift_cons/Consumed_data_emf$q1.2.2
mean(Consumed_data_emf$HH_Cons_egg_gift)
mean(Consumed_data_emf$HH_Cons_egg_gift)/30

#Expenditure of egg
#Calculating per person consumption for each product
columns_to_divide <- c("q2.2.5.2.1","q2.2.5.2.2")  # Columns to divide
divider_column <- "q1.2.2"  # Column to divide by

# Perform the division
for (col in columns_to_divide) {
  Consumed_data[[col]] <- Consumed_data[[col]] / Consumed_data[[divider_column]]
}

#Selecting price of products
selected_cols <-Consumed_data_emf[, c("q2.2.5.7.1","q2.2.5.7.2")]
#selecting consumption rate of products
selected_cols2=Consumed_data[,c("q2.2.5.2.1","q2.2.5.2.2")]
matrix_data1 <- as.matrix(selected_cols)
matrix_data2<- as.matrix(selected_cols2)
expenditure=data.frame(matrix_data1*matrix_data2)
#total house hold expenditure
Consumed_data_emf$HH_expe_egg=rowSums(expenditure[,c("q2.2.5.7.1","q2.2.5.7.2")],na.rm=T)
#Average household hold expenditure of egg
mean(Consumed_data_emf$HH_expe_egg)
median(Consumed_data_emf$HH_expe_egg)
quantile(Consumed_data_emf$HH_expe_egg,probs = seq(0,1,by=0.025))


#####################################################################################################################################



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

#Loading data vegetables
glv=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/bihar consumption/glv.csv",header = T)
other_veg=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/bihar consumption/other veg,sugar,fruits.csv",header = T)
Consumed_data_veg=merge(glv,other_veg,by="UniqueId")

#Average Consumption of vegetables frm home produced
Consumed_data_veg$veg_cons_home=rowSums(Consumed_data_veg[,c("q2.2.6.3.1","q2.2.6.3.2", "q2.2.6.3.3"  ,"q2.2.6.3.4"  ,"q2.2.6.3.5"  ,"q2.2.6.3.6" , "q2.2.6.3.7" ,"q2.2.6.3.8"  ,"q2.2.6.3.9"  ,"q2.2.6.3.10" ,"q2.2.6.3.11", "q2.2.6.3.12","q2.2.6.3.13" ,"q2.2.6.3.14", "q2.2.6.3.15", "q2.2.6.3.16" ,"q2.2.7.3.1"  ,"q2.2.7.3.2"  ,"q2.2.7.3.3"  ,"q2.2.7.3.4" ,"q2.2.7.3.5" , "q2.2.7.3.6"  ,"q2.2.7.3.7" , "q2.2.7.3.8"  ,"q2.2.7.3.9" ,"q2.2.7.3.10" ,"q2.2.7.3.11" ,"q2.2.7.3.12" ,"q2.2.7.3.13", "q2.2.7.3.14","q2.2.7.3.15",  "q2.2.7.3.17", "q2.2.7.3.18", "q2.2.7.3.19" )],na.rm = T)
Consumed_data_veg$HH_home_cons=Consumed_data_veg$veg_cons_home/Consumed_data_veg$q1.2.2.x
#Converting kg to g
Consumed_data_veg$HH_home_cons=Consumed_data_veg$HH_home_cons*1000
mean(Consumed_data_veg$HH_home_cons)
mean(Consumed_data_veg$HH_home_cons)/7

#Average Consumption of veg purchased frm market
Consumed_data_veg$veg_cons_mrkt=rowSums(Consumed_data_veg[,c("q2.2.6.4.1","q2.2.6.4.2", "q2.2.6.4.3"  ,"q2.2.6.4.4"  ,"q2.2.6.4.5"  ,"q2.2.6.4.6" , "q2.2.6.4.7" ,"q2.2.6.4.8"  ,"q2.2.6.4.9"  ,"q2.2.6.4.10" ,"q2.2.6.4.11", "q2.2.6.4.12","q2.2.6.4.13" ,"q2.2.6.4.14", "q2.2.6.4.15", "q2.2.6.4.16" ,"q2.2.7.4.1"  ,"q2.2.7.4.2"  ,"q2.2.7.4.3"  ,"q2.2.7.4.4" ,"q2.2.7.4.5" , "q2.2.7.4.6"  ,"q2.2.7.4.7" , "q2.2.7.4.8"  ,"q2.2.7.4.9" ,"q2.2.7.4.10" ,"q2.2.7.4.11" ,"q2.2.7.4.12" ,"q2.2.7.4.13", "q2.2.7.4.14","q2.2.7.4.15",  "q2.2.7.4.17", "q2.2.7.4.18", "q2.2.7.4.19" )],na.rm = T)
Consumed_data_veg$HH_mrkt_cons=Consumed_data_veg$veg_cons_mrkt/Consumed_data_veg$q1.2.2.x
#Converting kg to g
Consumed_data_veg$HH_mrkt_cons=Consumed_data_veg$HH_mrkt_cons*1000
mean(Consumed_data_veg$HH_mrkt_cons)
mean(Consumed_data_veg$HH_mrkt_cons)/7

#Average consumption of vegetables received from ICDS
Consumed_data_veg$veg_cons_ICDS=rowSums(Consumed_data_veg[,c("q2.2.6.5.1","q2.2.6.5.2", "q2.2.6.5.3"  ,"q2.2.6.5.4"  ,"q2.2.6.5.5"  ,"q2.2.6.5.6" , "q2.2.6.5.7" ,"q2.2.6.5.8"  ,"q2.2.6.5.9"  ,"q2.2.6.5.10" ,"q2.2.6.5.11", "q2.2.6.5.12","q2.2.6.5.13" ,"q2.2.6.5.14", "q2.2.6.5.15", "q2.2.6.5.16" ,"q2.2.7.5.1"  ,"q2.2.7.5.2"  ,"q2.2.7.5.3"  ,"q2.2.7.5.4" ,"q2.2.7.5.5" , "q2.2.7.5.6"  ,"q2.2.7.5.7" , "q2.2.7.5.8"  ,"q2.2.7.5.9" ,"q2.2.7.5.10" ,"q2.2.7.5.11" ,"q2.2.7.5.12" ,"q2.2.7.5.13", "q2.2.7.5.14","q2.2.7.5.15",  "q2.2.7.5.17", "q2.2.7.5.18", "q2.2.7.5.19" )],na.rm = T)
Consumed_data_veg$HH_ICDS_cons=Consumed_data_veg$veg_cons_ICDS/Consumed_data_veg$q1.2.2.x
#Converting kg to g
Consumed_data_veg$HH_ICDS_cons=Consumed_data_veg$HH_ICDS_cons*1000
mean(Consumed_data_veg$HH_ICDS_cons)
mean(Consumed_data_veg$HH_ICDS_cons)/7

#Average consumption of vegetables received as gift/loan
Consumed_data_veg$veg_cons_gift=rowSums(Consumed_data_veg[,c("q2.2.6.6.1","q2.2.6.6.2", "q2.2.6.6.3"  ,"q2.2.6.6.4"  ,"q2.2.6.6.5"  ,"q2.2.6.6.6" , "q2.2.6.6.7" ,"q2.2.6.6.8"  ,"q2.2.6.6.9"  ,"q2.2.6.6.10" ,"q2.2.6.6.11", "q2.2.6.6.12","q2.2.6.6.13" ,"q2.2.6.6.14", "q2.2.6.6.15", "q2.2.6.6.16" ,"q2.2.7.6.1"  ,"q2.2.7.6.2"  ,"q2.2.7.6.3"  ,"q2.2.7.6.4" ,"q2.2.7.6.5" , "q2.2.7.6.6"  ,"q2.2.7.6.7" , "q2.2.7.6.8"  ,"q2.2.7.6.9" ,"q2.2.7.6.10" ,"q2.2.7.6.11" ,"q2.2.7.6.12" ,"q2.2.7.6.13", "q2.2.7.5.14","q2.2.7.6.15",  "q2.2.7.6.17", "q2.2.7.6.18", "q2.2.7.6.19" )],na.rm = T)
Consumed_data_veg$HH_gift_cons=Consumed_data_veg$veg_cons_gift/Consumed_data_veg$q1.2.2.x
#Converting kg to g
Consumed_data_veg$HH_gift_cons=Consumed_data_veg$HH_gift_cons*1000
mean(Consumed_data_veg$HH_gift_cons)
mean(Consumed_data_veg$HH_gift_cons)/7


#Expenditure

#Calculating per person consumption for each product
columns_to_divide <- c("q2.2.6.2.1","q2.2.6.2.2", "q2.2.6.2.3"  ,"q2.2.6.2.4"  
                       ,"q2.2.6.2.5"  ,"q2.2.6.2.6" , "q2.2.6.2.7" ,"q2.2.6.2.8"  
                       ,"q2.2.6.2.9"  ,"q2.2.6.2.10" ,"q2.2.6.2.11", "q2.2.6.2.12"
                       ,"q2.2.6.2.13" ,"q2.2.6.2.14", "q2.2.6.2.15", "q2.2.6.2.16" 
                       ,"q2.2.7.2.1"  ,"q2.2.7.2.2"  ,"q2.2.7.2.3"  ,"q2.2.7.2.4" 
                       ,"q2.2.7.2.5" , "q2.2.7.2.6"  ,"q2.2.7.2.7" , "q2.2.7.2.8"  
                       ,"q2.2.7.2.9" ,"q2.2.7.2.10" ,"q2.2.7.2.11" ,"q2.2.7.2.12" 
                       ,"q2.2.7.2.13", "q2.2.7.2.14","q2.2.7.2.15",  "q2.2.7.2.17"
                       , "q2.2.7.2.18", "q2.2.7.2.19")  # Columns to divide
divider_column <- "q1.2.2.x"  # Column to divide by

# Perform the division
for (col in columns_to_divide) {
  Consumed_veg_data[[col]] <- Consumed_veg_data[[col]]/Consumed_veg_data[[divider_column]]
}

#Selecting price of products
selected_cols <-Consumed_data_veg[, c("q2.2.6.7.1","q2.2.6.7.2", "q2.2.6.7.3"  ,"q2.2.6.7.4"  ,"q2.2.6.7.5" 
                                      ,"q2.2.6.7.6" , "q2.2.6.7.7" ,"q2.2.6.7.8"  ,"q2.2.6.7.9"  ,"q2.2.6.7.10"
                                      ,"q2.2.6.7.11", "q2.2.6.7.12","q2.2.6.7.13" ,"q2.2.6.7.14", "q2.2.6.7.15",
                                      "q2.2.6.7.16" ,"q2.2.7.7.1"  ,"q2.2.7.7.2"  ,"q2.2.7.7.3"  ,"q2.2.7.7.4" ,
                                      "q2.2.7.7.5" , "q2.2.7.7.6"  ,"q2.2.7.7.7" , "q2.2.7.7.8"  ,"q2.2.7.7.9" ,
                                      "q2.2.7.7.10" ,"q2.2.7.7.11" ,"q2.2.7.7.12" ,"q2.2.7.7.13", "q2.2.7.7.14",
                                      "q2.2.7.7.15",  "q2.2.7.7.17", "q2.2.7.7.18", "q2.2.7.7.19")]
#selecting consumption rate of products
selected_cols2=Consumed_veg_data[,c("q2.2.6.2.1","q2.2.6.2.2", "q2.2.6.2.3"  ,"q2.2.6.2.4"  ,"q2.2.6.2.5"  ,"q2.2.6.2.6" , "q2.2.6.2.7" ,"q2.2.6.2.8"  ,"q2.2.6.2.9"  ,"q2.2.6.2.10" ,"q2.2.6.2.11", "q2.2.6.2.12","q2.2.6.2.13" ,"q2.2.6.2.14", "q2.2.6.2.15", "q2.2.6.2.16" ,"q2.2.7.2.1"  ,"q2.2.7.2.2"  ,"q2.2.7.2.3"  ,"q2.2.7.2.4" ,"q2.2.7.2.5" , "q2.2.7.2.6"  ,"q2.2.7.2.7" , "q2.2.7.2.8"  ,"q2.2.7.2.9" ,"q2.2.7.2.10" ,"q2.2.7.2.11" ,"q2.2.7.2.12" ,"q2.2.7.2.13", "q2.2.7.2.14","q2.2.7.2.15",  "q2.2.7.2.17", "q2.2.7.2.18", "q2.2.7.2.19")]
matrix_data1 <- as.matrix(selected_cols)
matrix_data2<- as.matrix(selected_cols2)
expenditure=data.frame(matrix_data1*matrix_data2)
#total house hold expenditure
Consumed_data_veg$HH_expe_veg=rowSums(expenditure[,c("q2.2.6.7.1","q2.2.6.7.2", "q2.2.6.7.3"  ,"q2.2.6.7.4"  ,"q2.2.6.7.5" 
                                                      ,"q2.2.6.7.6" , "q2.2.6.7.7" ,"q2.2.6.7.8"  ,"q2.2.6.7.9"  ,"q2.2.6.7.10"
                                                      ,"q2.2.6.7.11", "q2.2.6.7.12","q2.2.6.7.13" ,"q2.2.6.7.14", "q2.2.6.7.15",
                                                      "q2.2.6.7.16" ,"q2.2.7.7.1"  ,"q2.2.7.7.2"  ,"q2.2.7.7.3"  ,"q2.2.7.7.4" ,
                                                      "q2.2.7.7.5" , "q2.2.7.7.6"  ,"q2.2.7.7.7" , "q2.2.7.7.8"  ,"q2.2.7.7.9" ,
                                                      "q2.2.7.7.10" ,"q2.2.7.7.11" ,"q2.2.7.7.12" ,"q2.2.7.7.13", "q2.2.7.7.14",
                                                      "q2.2.7.7.15",  "q2.2.7.7.17", "q2.2.7.7.18", "q2.2.7.7.19")],na.rm=T)
#Average household hold expenditure of vegetables
mean(Consumed_data_veg$HH_expe_veg)
median(Consumed_data_veg$HH_expe_veg)
quantile(Consumed_data_veg$HH_expe_veg,probs = seq(0,1,by=0.025))



#average household expenditure of vegetables-lemon
Consumed_veg_data$q2.2.7.2.16=Consumed_veg_data$q2.2.7.2.16/Consumed_veg_data$q1.2.2.x
mean(Consumed_veg_data$q2.2.7.2.16*Consumed_data_veg$q2.2.7.7.16,na.rm=T)
median(Consumed_veg_data$q2.2.7.2.16*Consumed_data_veg$q2.2.7.7.16,na.rm=T)
quantile(Consumed_veg_data$q2.2.7.2.16*Consumed_data_veg$q2.2.7.7.16,probs = seq(0,1,by=0.025),na.rm = T)


###########################################################################################################

#Loading data of cereals
Consumed_cereals_data=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/Bihar Quality food data/cereals.csv",header = T)
Consumed_cereals_data$total_cons_cereals=rowSums(Consumed_cereals_data[,c("q2.2.1.2.1","q2.2.1.2.2","q2.2.1.2.3","q2.2.1.2.4","q2.2.1.2.5","q2.2.1.2.6", 
                                                                          "q2.2.1.2.7","q2.2.1.2.8","q2.2.1.2.9","q2.2.1.2.10",
                                                                          "q2.2.1.2.11","q2.2.1.2.12","q2.2.1.2.13","q2.2.1.2.14","q2.2.1.2.15",
                                                                          "q2.2.1.2.16","q2.2.1.2.17")],na.rm = T)
#Average house hold consumption of cereals
Consumed_cereals_data$HH_cons_cereals=Consumed_cereals_data$total_cons_cereals/Consumed_cereals_data$q1.2.2
#Converting Kg to g
Consumed_cereals_data$HH_cons_cereals=Consumed_cereals_data$HH_cons_cereals*1000
mean(Consumed_cereals_data$HH_cons_cereals)
mean(Consumed_cereals_data$HH_cons_cereals)/30
median(Consumed_cereals_data$HH_cons_cereals)
median(Consumed_cereals_data$HH_cons_cereals)/30
hist(Consumed_cereals_data$HH_cons_cereals)

#Loading data
consumed_data_cereals=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/bihar consumption/cerealss.csv",header = T)
#Average  HH consumption of cereals produced at home
consumed_data_cereals$cereals_cons_home=rowSums(consumed_data_cereals[,c("q2.2.1.3.2","q2.2.1.3.3","q2.2.1.3.5","q2.2.1.3.6", 
                                                                         "q2.2.1.3.7","q2.2.1.3.8","q2.2.1.3.9","q2.2.1.3.10",
                                                                         "q2.2.1.3.11","q2.2.1.3.12","q2.2.1.3.13","q2.2.1.3.14","q2.2.1.3.15",
                                                                         "q2.2.1.3.16","q2.2.1.3.17")],na.rm = T)
consumed_data_cereals$HH_cons_home=consumed_data_cereals$cereals_cons_home/consumed_data_cereals$q1.2.2
#Converting Kg to g
consumed_data_cereals$HH_cons_home=consumed_data_cereals$HH_cons_home*1000
mean(consumed_data_cereals$HH_cons_home)
median(consumed_data_cereals$HH_cons_home)
mean(consumed_data_cereals$HH_cons_home)/30
median(consumed_data_cereals$HH_cons_home)/30

#Average consumption of cereals purchased from market
consumed_data_cereals$cereals_mrkt_cons=rowSums(consumed_data_cereals[,c("q2.2.1.4.1","q2.2.1.4.2","q2.2.1.4.3","q2.2.1.4.4","q2.2.1.4.5","q2.2.1.4.6", 
                                                                         "q2.2.1.4.7","q2.2.1.4.8","q2.2.1.4.9","q2.2.1.4.10",
                                                                         "q2.2.1.4.11","q2.2.1.4.12","q2.2.1.4.13","q2.2.1.4.14","q2.2.1.4.15",
                                                                         "q2.2.1.4.16","q2.2.1.4.17")],na.rm = T)
consumed_data_cereals$HH_cons_mrkt=consumed_data_cereals$cereals_mrkt_cons/consumed_data_cereals$q1.2.2
#Converting kg to g
consumed_data_cereals$HH_cons_mrkt=consumed_data_cereals$HH_cons_mrkt*1000
mean(consumed_data_cereals$HH_cons_mrkt)
median(consumed_data_cereals$HH_cons_mrkt)
mean(consumed_data_cereals$HH_cons_mrkt)/30
median(consumed_data_cereals$HH_cons_mrkt)/30

#Average consumption of cereals received from ICDS
consumed_data_cereals$cereals_ICDS_cons=rowSums(consumed_data_cereals[,c("q2.2.1.5.2","q2.2.1.5.3","q2.2.1.5.5","q2.2.1.5.6", 
                                                                         "q2.2.1.5.7","q2.2.1.5.8","q2.2.1.5.9","q2.2.1.5.10",
                                                                         "q2.2.1.5.11","q2.2.1.5.12","q2.2.1.5.13","q2.2.1.5.14","q2.2.1.5.15",
                                                                         "q2.2.1.5.16","q2.2.1.5.17")],na.rm = T)
consumed_data_cereals$HH_cons_ICDS=consumed_data_cereals$cereals_ICDS_cons/consumed_data_cereals$q1.2.2
#Converting kg to g
consumed_data_cereals$HH_cons_ICDS=consumed_data_cereals$HH_cons_ICDS*1000
mean(consumed_data_cereals$HH_cons_ICDS)
median(consumed_data_cereals$HH_cons_ICDS)
mean(consumed_data_cereals$HH_cons_ICDS)/30
median(consumed_data_cereals$HH_cons_ICDS)/30

#Average consumption of cereals received as gift/loan
consumed_data_cereals$cereals_cons_gift=rowSums(consumed_data_cereals[,c("q2.2.1.4.2","q2.2.1.4.3","q2.2.1.4.5","q2.2.1.4.6", 
                                                                         "q2.2.1.4.7","q2.2.1.4.8","q2.2.1.4.9","q2.2.1.4.10",
                                                                         "q2.2.1.4.11","q2.2.1.4.12","q2.2.1.4.13","q2.2.1.4.14","q2.2.1.4.15",
                                                                         "q2.2.1.4.16","q2.2.1.4.17")],na.rm = T)
consumed_data_cereals$HH_cons_gifts=consumed_data_cereals$cereals_cons_gift/consumed_data_cereals$q1.2.2
mean(consumed_data_cereals$HH_cons_gifts)
mean(consumed_data_cereals$HH_cons_gifts)/30
####################################################################################################################################################################################


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

#Expenditure of fruits
#Loading data of fruits
consumed_data_fruit=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/bihar consumption/oil,veg.fruits.csv",header=T)

#Calculating per person consumption for each product
columns_to_divide <- c("q2.2.8.2.2",  "q2.2.8.2.3","q2.2.8.2.5"  , "q2.2.8.2.7"
                       ,  "q2.2.8.2.8" , "q2.2.8.2.9"
                       , "q2.2.8.2.10", "q2.2.8.2.11", "q2.2.8.2.12", "q2.2.8.2.13" 
                       ,"q2.2.8.2.14", "q2.2.8.2.15","q2.2.8.2.16" ,"q2.2.8.2.17" 
                       ,"q2.2.8.2.18" ,"q2.2.8.2.19" ,"q2.2.8.2.20", "q2.2.8.2.21")  # Columns to divide
divider_column <- "q1.2.2"  # Column to divide by

# Perform the division
for (col in columns_to_divide) {
  Consumed_fruit_data[[col]] <- Consumed_fruit_data[[col]] / Consumed_fruit_data[[divider_column]]
}

#Selecting price of products
selected_cols <-consumed_data_fruit[, c("q2.2.8.3.2",  "q2.2.8.3.3","q2.2.8.3.5" 
                                      , "q2.2.8.3.7",  "q2.2.8.3.8" , "q2.2.8.3.9", "q2.2.8.3.10", 
                                      "q2.2.8.3.11", "q2.2.8.3.12", "q2.2.8.3.13" ,"q2.2.8.3.14", "q2.2.8.3.15"
                                      ,"q2.2.8.3.16" ,"q2.2.8.3.17" ,"q2.2.8.3.18" ,"q2.2.8.3.19"
                                      ,"q2.2.8.3.20")]
#selecting consumption rate of products
selected_cols2=Consumed_fruit_data[,c("q2.2.8.2.2",  "q2.2.8.2.3","q2.2.8.2.5"  , "q2.2.8.2.7"
                                ,  "q2.2.8.2.8" , "q2.2.8.2.9"
                                , "q2.2.8.2.10", "q2.2.8.2.11", "q2.2.8.2.12", "q2.2.8.2.13" 
                                ,"q2.2.8.2.14", "q2.2.8.2.15","q2.2.8.2.16" ,"q2.2.8.2.17" 
                                ,"q2.2.8.2.18" ,"q2.2.8.2.19" ,"q2.2.8.2.20")]
matrix_data1 <- as.matrix(selected_cols)
matrix_data2<- as.matrix(selected_cols2)
expenditure=data.frame(matrix_data1*matrix_data2)
#total house hold expenditure
consumed_data_fruit$HH_expe_fruit=rowSums(expenditure[,c("q2.2.8.3.2",  "q2.2.8.3.3","q2.2.8.3.5" 
                                                     , "q2.2.8.3.7",  "q2.2.8.3.8" , "q2.2.8.3.9", "q2.2.8.3.10", 
                                                     "q2.2.8.3.11", "q2.2.8.3.12", "q2.2.8.3.13" ,"q2.2.8.3.14", "q2.2.8.3.15"
                                                     ,"q2.2.8.3.16" ,"q2.2.8.3.17" ,"q2.2.8.3.18" ,"q2.2.8.3.19"
                                                     ,"q2.2.8.3.20")],na.rm=T)
#Average household hold expenditure per person
mean(consumed_data_fruit$HH_expe_fruit)
median(consumed_data_fruit$HH_expe_fruit)
quantile(consumed_data_fruit$HH_expe_fruit,probs = seq(0,1,by=0.025))

write.csv(Consumed_fruit_data , "C://Users//jayk6//OneDrive//Desktop//Rajagoapl//bihar consumption//HH consumption.csv",row.names = F)

write.csv(consumed_data_fruit , "C://Users//jayk6//OneDrive//Desktop//Rajagoapl//bihar consumption//HH expenditure.csv",row.names = F)












#Total consumption of fruits-banana,pineapple and orange,musambi only
Consumed_fruit_data$Total_cons_fruit_x=rowSums(Consumed_fruit_data[,c("q2.2.8.2.1","q2.2.8.2.4","q2.2.8.2.6")],na.rm = T)
#average of HH consumption of fruits-banana,pineapple and orange,musambi only
Consumed_fruit_data$HH_consumption_fruit_x=Consumed_fruit_data$Total_cons_fruit_x/Consumed_fruit_data$q1.2.2
mean(Consumed_fruit_data$HH_consumption_fruit_x)
mean(Consumed_fruit_data$HH_consumption_fruit_x)/7
hist(Consumed_fruit_data$HH_consumption_fruit_x)
median(Consumed_fruit_data$HH_consumption_fruit_x)
median(Consumed_fruit_data$HH_consumption_fruit_x)/7



#total HH expenditure of banana,pineapple and orange (musambi)
consumed_data_fruit$HH_expe_fruits_x=rowSums(consumed_data_fruit[,c("q2.2.8.3.1","q2.2.8.3.4","q2.2.8.3.6")],na.rm = T)
#Averageuits household hold expenditure of banana,pineapple and orange (musambi)
mean(consumed_data_fruit$HH_expe_fruits_x)
median(consumed_data_fruit$HH_expe_fruits_x)
quantile(consumed_data_fruit$HH_expe_fruits_x,probs = seq(0,1,by=0.025))
#Household expenditure of banana,pineapple and orange
0.789*2.93 #Mean


#Expenditure of banana,pineapple and orange (musambi)
#Loading data of fruits
consumed_data_fruit=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/bihar consumption/oil,veg.fruits.csv",header=T)

#Calculating per person consumption for each product
columns_to_divide <- c("q2.2.8.2.1","q2.2.8.2.4","q2.2.8.2.6")  # Columns to divide
divider_column <- "q1.2.2"  # Column to divide by

# Perform the division
for (col in columns_to_divide) {
  Consumed_fruit_data[[col]] <- Consumed_fruit_data[[col]] / Consumed_fruit_data[[divider_column]]
}

#Selecting price of products
selected_cols <-consumed_data_fruit[, c("q2.2.8.3.1","q2.2.8.3.4","q2.2.8.3.6")]
#selecting consumption rate of products
selected_cols2=Consumed_fruit_data[,c("q2.2.8.2.1","q2.2.8.2.4","q2.2.8.2.6")]
matrix_data1 <- as.matrix(selected_cols)
matrix_data2<- as.matrix(selected_cols2)
expenditure=data.frame(matrix_data1*matrix_data2)
#total house hold expenditure
consumed_data_fruit$HH_expe_fruit_x=rowSums(expenditure[,c("q2.2.8.3.1","q2.2.8.3.4","q2.2.8.3.6")],na.rm=T)
#Average household hold expenditure per person
mean(consumed_data_fruit$HH_expe_fruit_x)
median(consumed_data_fruit$HH_expe_fruit_x)
quantile(consumed_data_fruit$HH_expe_fruit_x,probs = seq(0,1,by=0.025))

###############################################################################################################################
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

#loading nuts and seed data
consumed_data_nuts=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/bihar consumption/oil,veg.fruits.csv",header=T)

#Calculating per person consumption for each product
columns_to_divide <- c("q2.2.10.3.1" , "q2.2.10.3.2"  ,"q2.2.10.3.3", "q2.2.10.3.4",  "q2.2.10.3.5" , "q2.2.10.3.6" )  # Columns to divide
divider_column <- "q1.2.2"  # Column to divide by

# Perform the division
for (col in columns_to_divide) {
  Consumed_Nuts_data[[col]] <- Consumed_Nuts_data[[col]] / Consumed_Nuts_data[[divider_column]]
}

#Selecting price of products
selected_cols <-consumed_data_nuts[, c("q2.2.10.4.1" , "q2.2.10.4.2"  ,"q2.2.10.4.3", "q2.2.10.4.4",  "q2.2.10.4.5" , "q2.2.10.4.6" )]
#selecting consumption rate of products
selected_cols2=Consumed_Nuts_data[,c("q2.2.10.3.1" , "q2.2.10.3.2"  ,"q2.2.10.3.3", "q2.2.10.3.4",  "q2.2.10.3.5" , "q2.2.10.3.6" )]
matrix_data1 <- as.matrix(selected_cols)
matrix_data2<- as.matrix(selected_cols2)
expenditure=data.frame(matrix_data1*matrix_data2)
#total house hold expenditure
consumed_data_nuts$HH_expe_nuts=rowSums(expenditure[,c("q2.2.10.4.1" , "q2.2.10.4.2"  ,"q2.2.10.4.3", "q2.2.10.4.4",  "q2.2.10.4.5" , "q2.2.10.4.6")],na.rm=T)
#Average household hold expenditure per person
mean(consumed_data_nuts$HH_expe_nuts)
median(consumed_data_nuts$HH_expe_nuts)
quantile(consumed_data_nuts$HH_expe_nuts,probs = seq(0,1,by=0.025))
#############################################################################################################################################################################\
#Loading data of pulses
Consumed_pulses_data=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/Bihar Quality food data/pulse.csv",header=T)
#Average consumption of HH cons of pulses
Consumed_pulses_data$total_cons_pulses=rowSums(Consumed_pulses_data[,c("q2.2.2.2.1","q2.2.2.2.2","q2.2.2.2.3" 
                                                                       ,"q2.2.2.2.4","q2.2.2.2.5","q2.2.2.2.6"  
                                                                       ,"q2.2.2.2.7","q2.2.2.2.8","q2.2.2.2.9" 
                                                                       ,"q2.2.2.2.10","q2.2.2.2.11" ,"q2.2.2.2.12"
                                                                       ,"q2.2.2.2.13","q2.2.2.2.14")],na.rm = T)
Consumed_pulses_data$HH_cons_pulses=Consumed_pulses_data$total_cons_pulses/Consumed_pulses_data$q1.2.2
#Converting kg to g
Consumed_pulses_data$HH_cons_pulses=Consumed_pulses_data$HH_cons_pulses*1000
mean(Consumed_pulses_data$HH_cons_pulses)
mean(Consumed_pulses_data$HH_cons_pulses)/30
median(Consumed_pulses_data$HH_cons_pulses)
median(Consumed_pulses_data$HH_cons_pulses)/30
hist(Consumed_pulses_data$HH_cons_pulses)

#Loading data
Consumed_data_pulses=read.csv("C:/Users/jayk6/OneDrive/Desktop/Rajagoapl/bihar consumption/pulses.csv",header=T)
#Average HH consumption pulses produced on home
Consumed_data_pulses$pulses_cons_home=rowSums(Consumed_data_pulses[,c("q2.2.2.3.1","q2.2.2.3.2","q2.2.2.3.3" 
                                                                      ,"q2.2.2.3.4","q2.2.2.3.5","q2.2.2.3.6"  
                                                                      ,"q2.2.2.3.7","q2.2.2.3.8","q2.2.2.3.9" 
                                                                      ,"q2.2.2.3.10","q2.2.2.3.11" ,"q2.2.2.3.12"
                                                                      ,"q2.2.2.3.13","q2.2.2.3.14")],na.rm = T)
Consumed_data_pulses$HH_cons_home=Consumed_data_pulses$pulses_cons_home/Consumed_data_pulses$q1.2.2
#converting kg to g
Consumed_data_pulses$HH_cons_home=Consumed_data_pulses$HH_cons_home*1000
mean(Consumed_data_pulses$HH_cons_home)
median(Consumed_data_pulses$HH_cons_home)
mean(Consumed_data_pulses$HH_cons_home)/30

#Average HH consumption pulses purchased from market
Consumed_data_pulses$pulses_cons_mrkt=rowSums(Consumed_data_pulses[,c("q2.2.2.4.1","q2.2.2.4.2","q2.2.2.4.3" 
                                                                      ,"q2.2.2.4.4","q2.2.2.4.5","q2.2.2.4.6"  
                                                                      ,"q2.2.2.4.7","q2.2.2.4.8","q2.2.2.4.9" 
                                                                      ,"q2.2.2.4.10","q2.2.2.4.11" ,"q2.2.2.4.12"
                                                                      ,"q2.2.2.4.13","q2.2.2.4.14")],na.rm = T)
Consumed_data_pulses$HH_cons_mrkt=Consumed_data_pulses$pulses_cons_mrkt/Consumed_data_pulses$q1.2.2
#converting kg to g
Consumed_data_pulses$HH_cons_mrkt=Consumed_data_pulses$HH_cons_mrkt*1000
mean(Consumed_data_pulses$HH_cons_mrkt)
median(Consumed_data_pulses$HH_cons_mrkt)
mean(Consumed_data_pulses$HH_cons_mrkt)/30

#Average HH consumption pulses received from ICDS
Consumed_data_pulses$pulses_cons_ICDS=rowSums(Consumed_data_pulses[,c("q2.2.2.5.1","q2.2.2.5.2","q2.2.2.5.3" 
                                                                      ,"q2.2.2.5.4","q2.2.2.5.5","q2.2.2.5.6"  
                                                                      ,"q2.2.2.5.7","q2.2.2.5.8","q2.2.2.5.9" 
                                                                      ,"q2.2.2.5.10","q2.2.2.5.11" ,"q2.2.2.5.12"
                                                                      ,"q2.2.2.5.13","q2.2.2.5.14")],na.rm = T)
Consumed_data_pulses$HH_cons_ICDS=Consumed_data_pulses$pulses_cons_ICDS/Consumed_data_pulses$q1.2.2
#converting kg to g
Consumed_data_pulses$HH_cons_ICDS=Consumed_data_pulses$HH_cons_ICDS*1000
mean(Consumed_data_pulses$HH_cons_ICDS)
median(Consumed_data_pulses$HH_cons_ICDS)
mean(Consumed_data_pulses$HH_cons_ICDS)/30

#Average HH consumption pulses received as gift/loan
Consumed_data_pulses$pulses_cons_gift=rowSums(Consumed_data_pulses[,c("q2.2.2.6.1","q2.2.2.6.2","q2.2.2.6.3" 
                                                                      ,"q2.2.2.6.4","q2.2.2.6.5","q2.2.2.6.6"  
                                                                      ,"q2.2.2.6.7","q2.2.2.6.8","q2.2.2.6.9" 
                                                                      ,"q2.2.2.6.10","q2.2.2.6.11" ,"q2.2.2.6.12"
                                                                      ,"q2.2.2.6.13","q2.2.2.6.14")],na.rm = T)
Consumed_data_pulses$HH_cons_gift=Consumed_data_pulses$pulses_cons_gift/Consumed_data_pulses$q1.2.2
#converting kg to g
Consumed_data_pulses$HH_cons_gift=Consumed_data_pulses$HH_cons_gift*1000
mean(Consumed_data_pulses$HH_cons_gift)
median(Consumed_data_pulses$HH_cons_gift)
mean(Consumed_data_pulses$HH_cons_gift)/30


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