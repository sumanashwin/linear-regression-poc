#-----------------------------------------------------------------------------------------------------------------------
#                       Analysis starts here 
#----------------------------------------------------------------------------------------------------------------------

#----------Importing the required packages for the analyis--------------------------------------------------------------

library(ggplot2)
library(corrplot)
library(MASS)
library(car)
library(tidyr)
library(stringr)
library(corrplot)
library(dplyr)

#------------------------------------------------------------------------------------------------------------

#--------Importing the dataset and storing it in variable names carprice-------------------------------------------------

carprice <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE) #The dataset has 205 obs and 26 variables

str(carprice)   # Currently the variables are of mixed datatypes, the integer,numeric and character.

#--------------------------------------------------------------------------------------------------------------

#-----------------------------DATA PREPARATION PART 1 (DATA CLEANING) -------------------------------------------

#1. The rows and columns are consistent in terms of numbers. There are no missing rows and columns, 
#   summary rows,misaligned rows,extra rows,missing column name

#2. Dealing with missing values.
sum(is.na(carprice))     #There are no NA's

#3. Dealing with blanks
car_blanks <- sapply(carprice, function(x) length(which(x == ""))) # checking for blank "" values
car_blanks                #There are no blank values


#4. Checking for duplicated rows.
sum(duplicated(carprice$car_ID))  # 0. There are no duplicates
carprice <- carprice[!duplicated(carprice),]  #Double checking for the duplicates.The number of observations remains unchanged


#5. Standardising Numbers.

#a. All the observations have the same consistent units

#b Ensuring digits as factors are not numeric
str(carprice)

# Variable symboling  should be categorical, where as its shown as int,converting to factor
carprice$symboling <- as.factor(carprice$symboling)

#Further binning the symboling to 3 levels namely "high risk","med risk" and "low risk"
levels(carprice$symboling)[1:2] <- "low risk"  #for rating of -2 and -1

levels(carprice$symboling)[2:3]  <- "med risk" #for rating of 0,1


levels(carprice$symboling)[3:4] <- "med risk"   #for rating of 2 and 3



#c. Rounding off the precision for the numeric columns
col_num <- sapply(carprice,is.numeric)  #getting the columns which are of type numeric
carprice_numeric <- carprice[,col_num]
carprice[,col_num] <- lapply(carprice_numeric,function(x) round(x,2))  #rounding of to 2 decimal places


#d. Checking for outliers and dealing with the same

# To check for outliers, we find out the quantile values at each 1% interval and wherever there is a high jump
# from one quantile to another, we cap/floor those values.

# Checking outliers for the wheelbase
quantile(carprice$wheelbase,seq(0,1,0.01))
# There is a jump of 5 units from 99% to 100%, but the jump is not significant. 
boxplot(carprice$wheelbase) #No outliers

#Checking outliers for the carlength
quantile(carprice$carlength,seq(0,1,0.01))
boxplot(carprice$carlength) # There is one outlier at the lower tail.
# There is a fluctuation between 0% and 2%, but the jump is not significant
boxplot(carprice$carlength)  


#Checking outliers for carwidth
boxplot(carprice$carwidth)  
quantile(carprice$carwidth,seq(0,1,0.01)) #there are no outliers thats significant


#Checking outliers for carheight
boxplot(carprice$carheight)  #There are no outliers in the boxplot
quantile(carprice$carheight,seq(0,1,0.01)) # There are no sporadic jumps between the quantiles


#Checking outliers for curbweight
boxplot(carprice$curbweight)  #Box plot does not show outliers but is skewed towards the left
quantile(carprice$curbweight,seq(0,1,0.01)) #There is a huge variation from 0% to 1% 
#Lets floor the values below 1%(1819.72) to 1819.72 
carprice$curbweight[which(carprice$curbweight < 1819.72)] <- 1819.72
boxplot(carprice$curbweight) 

#Checking outliers for enginesize
boxplot(carprice$enginesize) #There are outliers,but not significant
quantile(carprice$enginesize,seq(0,1,0.01)) #There is not a significance jump between quantiles

#Checking outliers for boreratio
boxplot(carprice$boreratio) #There are no outliers,but not significant
quantile(carprice$boreratio,seq(0,1,0.01)) #There are no high jumps between the quantile intervals


#Checking outliers for stroke
boxplot(carprice$stroke) #There are outliers more towards the lower quantiles,not significant
quantile(carprice$stroke,seq(0,1,0.01)) #There is no significant jumps in the quantiles

#Checking outliers for compression ratio
boxplot(carprice$compressionratio) #There are outliers
quantile(carprice$compressionratio,seq(0,1,0.01)) #There is a large jump from 90% to 91%, but after that the
#variation is constant

#Checking outliers for horsepower
boxplot(carprice$horsepower) #There are outliers
quantile(carprice$horsepower,seq(0,1,0.01)) #There is a large jump from 99% to 100%, hence capping all the values above 99%(207.00) to 288.00
carprice$horsepower[which(carprice$horsepower > 207.00)] <- 207.00
boxplot(carprice$horsepower) # We have dealt with the outliers.

#Checking outliers for peakrpm
boxplot(carprice$peakrpm) #There are outliers
quantile(carprice$peakrpm,seq(0,1,0.01)) #There is a large jump from 99% to 100%, hence capping all the values above 99%(6000) to 6000
carprice$peakrpm[which(carprice$peakrpm > 6000)] <- 6000
boxplot(carprice$peakrpm) # We have dealt with the outliers.


#Checking outliers for citympg
boxplot(carprice$citympg) #There are outliers
quantile(carprice$citympg,seq(0,1,0.01)) #There are no outliers

#Checking outliers for highwaympg
boxplot(carprice$highwaympg) #There are outliers
quantile(carprice$highwaympg,seq(0,1,0.01)) #There are no outliers

#Checking outliers for price
boxplot(carprice$price) 
quantile(carprice$price,seq(0,1,0.01)) #There are no outliers

#e. Removing the car_ID column as its just the row numbers 
carprice <- carprice[,-1]

#6. Standardising the text

#a. Seperating the car model and car brand

carprice <- separate(carprice,col=CarName,into=c("car_brand"),sep=" ") #extracting the brand of the car

#b. Handling the cases for the car_brand column

carprice$car_brand <- tolower(carprice$car_brand)

#c.Converting the car_brand column to factor type

carprice$car_brand <- as.factor(carprice$car_brand)


#d. Checking the missed spellings
summary(carprice$car_brand)
#maxda to be replaced with mazda ,porcshce to be replaced with  porsche,toyouta to be replaced with toyota
#vokswagen and vw to be replaced with volkswagen

carprice$car_brand <- str_replace_all(as.character(carprice$car_brand),"maxda","mazda")
carprice$car_brand <- str_replace_all(as.character(carprice$car_brand),"porcshce","porsche")
carprice$car_brand <- str_replace_all(as.character(carprice$car_brand),"toyouta","toyota")
carprice$car_brand <- str_replace_all(as.character(carprice$car_brand),"vokswagen","volkswagen")
carprice$car_brand <- str_replace_all(as.character(carprice$car_brand),"vw","volkswagen")

carprice$car_brand <- as.factor(carprice$car_brand) # converting back to factor type
summary(carprice$car_brand) #successfuly treated, all the names are unique.


#e. Converting the character type variables to factors
summary(carprice)
str(carprice)

col_char <- sapply(carprice,is.character) #checking which columns are of type char
carprice_char <- carprice[,col_char] #subsetting only character columns
carprice[,col_char] <- lapply(carprice_char,factor)

#f. Checking the summary of all the categorical variables to eliminate redundancy
summary(carprice$fueltype)
summary(carprice$aspiration)
summary(carprice$doornumber)
summary(carprice$carbody)
summary(carprice$drivewheel)  
summary(carprice$enginelocation)  
summary(carprice$enginetype) 
summary(carprice$cylindernumber) 
summary(carprice$fuelsystem)
#All the factor variables are unique

############## DATA CLEANING ENDS HERE ##################################################################################


#-----------------------------DATA PREPARATION PART 2(EDA) -------------------------------------------

#1. Coming up with derived metrics
#a. Horsepower to curbweight is an important ratio to be computed.Its also meaningful from price standpoint

#carprice$power_weight <- round(carprice$horsepower/carprice$curbweight,3)
#Few other derived metrics i thought was car_volume by multiplying length*breadth*height,fuel efficiency, average mpg combining both city and highway mpg
#but none of them added value to my analysis


#2. Univariate Analysis

ggplot(carprice,aes(x=symboling))+geom_bar(fill="purple")+labs(title="Distributions of insurance risk ratings",x="Insurance Risk rating",y="number of vehicles")

# There number of vehicles with '0' as risk rating is higher


ggplot(carprice,aes(x=reorder(car_brand,car_brand,function(x)-length(x))))+geom_bar(col="blue")+labs(title="Distributions of car brands")
#The brand toyota has the highest number of cars in this dataset

ggplot(carprice, aes(x = factor(1), fill = factor(carprice$fueltype))) + geom_bar(width = 1)+ coord_polar(theta = "y",start=0) + theme_void()+labs(title="Distribution by fuel type",y = "Number of Requests", x = "", fill = "fueltype")
#The gas fuel type are more in number compared to diesel

#Histogram for horsepower
ggplot(carprice,aes(x=horsepower))+geom_histogram()+scale_x_continuous(name = "Horse power", breaks = seq(48,162,10), limits = c(48,162))+labs(title="Horse power histogram")

#Histogram for the curbweight
ggplot(carprice,aes(x=curbweight))+geom_histogram(breaks=seq(1819,3503, by =100),col="red", fill="green")+labs(title="Curb weight histogram",y="Number of vehicles")

#Pie chart for drive wheels
ggplot(carprice, aes(x = factor(1), fill = factor(carprice$drivewheel))) + geom_bar(width = 1)+ coord_polar(theta = "y",start=0) + theme_void()+labs(title="Distribution by Drive wheel",y = "Number of Requests", x = "", fill = "drivewheel")
#The front wheel drive (fwd) are larger in number

#Column chart for number of doors
ggplot(carprice,aes(x=doornumber))+geom_bar(fill="purple",width=0.5)+labs(title="Distributions of Number of doors",x="Number of doors",y="number of vehicles")

#Findings from the Univariate analysis on some of the key metrics:
#1. Toyota brand has the most number of vehicles followed by Nissan
#2. Symboling or the Insurance risk rating is between -2 and 3, and most cars have a rating between 0 and 1
#3. Most preferred fuel type is gad
#4. For the distribution of the drive wheels , front wheel drive has most number of cars followed by rear wheel and four wheel. There are very less number of cars for four wheel drive.

#3. Correlation analysis between numeric variables

col_num <- sapply(carprice,is.numeric)  #getting the columns which are of type numeric
carprice_numeric <- carprice[,col_num]

eda_carprice <- cor(carprice_numeric) #storing correlation matrix in eda_carprice variable

#Applying a proper theme for the corrplot to get a an easily readable correlation matrix
corrplot(eda_carprice, order="AOE", method= "square", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         p.mat = 1-abs(eda_carprice), sig.level=0.50, insig = "blank")  

#Inferences from the correlation matrix

#1. Price is highly correlated with enginesize(86) and curbweight(82)
#2. Engine size is highly corrleated with curbweight car length and car width
#3. Curb weight is highly correlated with car length and width and the wheel base
#4. Horse power is positively correlated with enginesize(82) and price(78)


#4. Bivariate Analysis

car_price_brand <- group_by(carprice,car_brand) %>% summarise(price =mean(price)) %>% arrange(desc(price))
ggplot(car_price_brand,aes(x=car_brand,y=price,group=car_brand))+geom_boxplot()

#The average price of the Top 3 brands above $30000 are Jaguar,buick,porche 
#while Less expensive cars costing less than 10000 are Chevrolet, Dodge, Honda, Mitsubishi, Plymoth and Subaru

#Scatter plot of price and curb weight
ggplot(carprice,aes(x=price,y=curbweight))+geom_point()+geom_smooth(aes(x=price,y=curbweight))
#There is a clear trend that as the curbweight increases the price also increases

#Visualising the citympg and highwaympg verus  curbweight

ggplot(carprice,aes(x=citympg,y=curbweight))+geom_point()+geom_smooth(aes(x=citympg,y=curbweight))
ggplot(carprice,aes(x=highwaympg,y=curbweight))+geom_point()+geom_smooth(aes(x=highwaympg,y=curbweight))

#Both the highwaympg and citympg shows a downward trend as the curbweight increases, showing the mpg and curbweight 
# are inversely proportional

#Boxplot for price and drive wheels
ggplot(carprice,aes(x=drivewheel,y=price,group=drivewheel))+geom_boxplot()
#Real wheel drive cars are most expensive

#Conclusion: The EDA on the dataset provides
#1. The distribution of the dataset on various attributes
#2. Correlation among the numeric fields and thier relationship
#3. Visualisations of the factors impacting the price variable
#4. The curbweight and its impact on the mileage(citympg and highwaympg)
#5. The price points of various car brands
#6. Importance of drive wheels and curb weight

############################ The EDA ends here ####################################################################################


#-----------------------------DATA PREPARATION PART 3(Dummy Variables Creation) -------------------------------------------

# 1. First converting the categorical variables with 2 levels to dummy variables

#a. Subsetting the columns which have only 2 levels
carprice_factors_var <- carprice[, sapply(carprice, function(col) is.factor(col))]

#b. Function to convert factors to dummy variables

dummy_variables_fun <- function(x){
  
  dummy_var <- data.frame(model.matrix( ~x, data =carprice_factors_var ))
  dummy_var <- dummy_var[,-1]
}

dummy_var_df    <- data.frame(sapply(carprice_factors_var,function(x) dummy_variables_fun(x)))
carprice_1 <- carprice[-which(colnames(carprice) %in% colnames(carprice_factors_var))]
carprice_1 <- cbind(carprice_1,dummy_var_df)

#Please note that the above function also successfully converts the categorical variables with 2 levels
#I also tested with below commands,for example, it gives the same results
#levels(carprice$fueltype)<-c(1,0)
# carprice$fueltype<- as.numeric(levels(carprice$fueltype))[carprice$fueltype]

#-------------------------DATA PREPARATION ENDS HERE ------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------------------------
#                          MODEL BUILDING STARTS HERE
#------------------------------------------------------------------------------------------------------------------------

# Dataframe carprice_1 contains the data in the desired format for model building

#1. Separating the training and test data
set.seed(100)
trainindices= sample(1:nrow(carprice_1), 0.7*nrow(carprice_1))  #Taking 70%of the dataframe rows for training set,will have 143 obs
train = carprice_1[trainindices,]
test = carprice_1[-trainindices,]   #Test data will contain 30% of rows from carprice_1 dataframe, will have 62 obs


#2. Starting model building process

#a. Build model 1 containing all variables
model_1 <- lm(price~.,train)
summary(model_1)
alias(model_1)
#Observations on model_1
#1. Residuals:
#   Min      1Q  Median      3Q     Max
# -2784.0  -707.6     0.0   629.5  3600.4 

#2, There are NA's as coefficients:Coefficients: (9 not defined because of singularities)
#3.Multiple R-squared: 0.981,	Adjusted R-squared:   0.969 

#b. # Now, lets see how to use stepAIC

# In stepAIC function, we pass our first model i.e model_1 and
# direction is ser as both, because in stepwise,  both the forward selection
# of variables and backward elimination of variables happen simultaneously

step <- stepAIC(model_1,direction = "both")
# so many iterations have been done through the stepwise command,now we need to know our model equation so lets write the Step command here.

step

# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model.
# some insignifican variables have been removed.
# Now store the last model equation of stepwise method into an object called model_2
# You can notice that stepAIC removed 29 variables

#c. Executing the model from the output of the step function
model_2 <-lm(formula = price ~ carwidth + curbweight + enginesize + boreratio + 
               stroke + horsepower + peakrpm + car_brand.xbmw + car_brand.xbuick + 
               car_brand.xdodge + car_brand.xhonda + car_brand.xjaguar + 
               car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
               car_brand.xpeugeot + car_brand.xplymouth + car_brand.xporsche + 
               car_brand.xrenault + car_brand.xsaab + car_brand.xsubaru + 
               car_brand.xtoyota + car_brand.xvolkswagen + aspiration + 
               carbody.xhardtop + carbody.xhatchback + carbody.xsedan + 
               carbody.xwagon + drivewheel.xrwd + enginelocation + enginetype.xohc + 
               enginetype.xrotor + cylindernumber.xfive + fuelsystem.x2bbl + 
               fuelsystem.xmpfi + cylindernumber.xsix, data = train)



#summary of model_2
summary(model_2)
#Multiple R-squared:  0.9799,	Adjusted R-squared:  0.9731 

# Let us check for multicollinearity and remove the variables if they are statistically insignificant
sort(vif(model_2))

#horsepower    pvalue:0.201049   VIF:30.832649 ,since horsepower has both high pvalue and VIF, lets remove it


# model_3:
#lets execute model_3, removing the horsepower which has a high P value showing less significance
model_3 <-lm(formula = price ~ carwidth + curbweight + enginesize + boreratio + 
               stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
               car_brand.xdodge + car_brand.xhonda + car_brand.xjaguar + 
               car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
               car_brand.xpeugeot + car_brand.xplymouth + car_brand.xporsche + 
               car_brand.xrenault + car_brand.xsaab + car_brand.xsubaru + 
               car_brand.xtoyota + car_brand.xvolkswagen + aspiration + 
               carbody.xhardtop + carbody.xhatchback + carbody.xsedan + 
               carbody.xwagon + drivewheel.xrwd + enginelocation + enginetype.xohc + 
               enginetype.xrotor + cylindernumber.xfive + fuelsystem.x2bbl + 
               fuelsystem.xmpfi + cylindernumber.xsix, data = train)


summary(model_3)
# Multiple R-squared:  0.9796,	Adjusted R-squared:  0.973 (adj R2 has not  varied much)

sort(vif(model_3))
#cylindernumber.xsix  has a VIF of  7.480169  and pvalue as 0.148578  (>0.05) , lets remove this in next model


#model_4:
#removing cylindernumber.xsix
model_4 <- lm(formula = price ~ carwidth + curbweight + enginesize + boreratio + 
                stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                car_brand.xdodge + car_brand.xhonda + car_brand.xjaguar + 
                car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
                car_brand.xpeugeot + car_brand.xplymouth + car_brand.xporsche + 
                car_brand.xrenault + car_brand.xsaab + car_brand.xsubaru + 
                car_brand.xtoyota + car_brand.xvolkswagen + aspiration + 
                carbody.xhardtop + carbody.xhatchback + carbody.xsedan + 
                carbody.xwagon + drivewheel.xrwd + enginelocation + enginetype.xohc + 
                enginetype.xrotor + cylindernumber.xfive + fuelsystem.x2bbl + 
                fuelsystem.xmpfi , data = train)

summary(model_4)
#Multiple R-squared:  0.9792,	Adjusted R-squared:  0.9727 (adj R2 has not  varied significantly since last model)

sort(vif(model_4))
# fuelsystem.xmpfi has high VIF of  5.663904 and p value of 0.331468 ,hence removing it in next model

#model_5
#removing fuelsystem.xmpfi

model_5<- lm(formula = price ~ carwidth + curbweight + enginesize + boreratio + 
               stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
               car_brand.xdodge + car_brand.xhonda + car_brand.xjaguar + 
               car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
               car_brand.xpeugeot + car_brand.xplymouth + car_brand.xporsche + 
               car_brand.xrenault + car_brand.xsaab + car_brand.xsubaru + 
               car_brand.xtoyota + car_brand.xvolkswagen + aspiration + 
               carbody.xhardtop + carbody.xhatchback + carbody.xsedan + 
               carbody.xwagon + drivewheel.xrwd + enginelocation + enginetype.xohc + 
               enginetype.xrotor + cylindernumber.xfive + fuelsystem.x2bbl  , data = train)

summary(model_5)
#Multiple R-squared:  0.979,	Adjusted R-squared:  0.9727  (adj R2 has not changed from previous model)

sort(vif(model_5))
# boreratio  has VIF of 5.054282  and high p value of 0.187047 ,hence removing it in next model

#model_6
#removing boreratio

model_6<- lm(formula = price ~ carwidth + curbweight + enginesize  + 
               stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
               car_brand.xdodge + car_brand.xhonda + car_brand.xjaguar + 
               car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
               car_brand.xpeugeot + car_brand.xplymouth + car_brand.xporsche + 
               car_brand.xrenault + car_brand.xsaab + car_brand.xsubaru + 
               car_brand.xtoyota + car_brand.xvolkswagen + aspiration + 
               carbody.xhardtop + carbody.xhatchback + carbody.xsedan + 
               carbody.xwagon + drivewheel.xrwd + enginelocation + enginetype.xohc + 
               enginetype.xrotor + cylindernumber.xfive + fuelsystem.x2bbl  , data = train)

summary(model_6)
#Multiple R-squared:  0.9787,	Adjusted R-squared:  0.9725  (adjusted R2 has not changed significantly from prev model)

sort(vif(model_6))
#fuelsystem.x2bbl  has a VIF of    3.185979   , but a high P value (0.293254 ),hence removing this in next model

#model_7
#removing fuelsystem.x2bbl
model_7<- lm(formula = price ~ carwidth + curbweight + enginesize  + 
               stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
               car_brand.xdodge + car_brand.xhonda + car_brand.xjaguar + 
               car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
               car_brand.xpeugeot + car_brand.xplymouth + car_brand.xporsche + 
               car_brand.xrenault + car_brand.xsaab + car_brand.xsubaru + 
               car_brand.xtoyota + car_brand.xvolkswagen + aspiration + 
               carbody.xhardtop + carbody.xhatchback + carbody.xsedan + 
               carbody.xwagon + drivewheel.xrwd + enginelocation + enginetype.xohc + 
               enginetype.xrotor + cylindernumber.xfive   , data = train)

summary(model_7)
#Multiple R-squared:  0.9785,	Adjusted R-squared:  0.9725  (adj R2 has not changed since last model)

sort(vif(model_7))
#cylindernumber.xfive has a VIF of 2.403857  , but high p value (0.103250 ),hence removing it in next model

#model_8
#removing cylindernumber.xfive
model_8<- lm(formula = price ~ carwidth + curbweight + enginesize  + 
               stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
               car_brand.xdodge + car_brand.xhonda + car_brand.xjaguar + 
               car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
               car_brand.xpeugeot + car_brand.xplymouth + car_brand.xporsche + 
               car_brand.xrenault + car_brand.xsaab + car_brand.xsubaru + 
               car_brand.xtoyota + car_brand.xvolkswagen + aspiration + 
               carbody.xhardtop + carbody.xhatchback + carbody.xsedan + 
               carbody.xwagon + drivewheel.xrwd + enginelocation + enginetype.xohc + 
               enginetype.xrotor , data = train)

summary(model_8)
#Multiple R-squared:  0.978,	Adjusted R-squared:  0.9721  ,(adj R2 has not change significantly since last model)


sort(vif(model_8))
# curbweight  has a VIF of  19.151905  and  P value(0.034492),hence removing it in next model

#model_9
#removing curbweight 
model_9<- lm(formula = price ~ carwidth  + enginesize  + 
               stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
               car_brand.xdodge + car_brand.xhonda + car_brand.xjaguar + 
               car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
               car_brand.xpeugeot + car_brand.xplymouth + car_brand.xporsche + 
               car_brand.xrenault + car_brand.xsaab + car_brand.xsubaru + 
               car_brand.xtoyota + car_brand.xvolkswagen + aspiration + 
               carbody.xhardtop + carbody.xhatchback + carbody.xsedan + 
               carbody.xwagon + drivewheel.xrwd + enginelocation + enginetype.xohc + 
               enginetype.xrotor , data = train)


summary(model_9)
#Multiple R-squared:  0.9771,	Adjusted R-squared:  0.9712  (adjusted R2 has not changed significantly)

sort(vif(model_9))
# car_brand.xsaab has VIF of  1.684121  and pvalue of 0.088937 .,lets remove this in next model

#model_10
#removing car_brand.xsaab
model_10<- lm(formula = price ~ carwidth  + enginesize  + 
                stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                car_brand.xdodge + car_brand.xhonda + car_brand.xjaguar + 
                car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
                car_brand.xpeugeot + car_brand.xplymouth + car_brand.xporsche + 
                car_brand.xrenault  + car_brand.xsubaru + 
                car_brand.xtoyota + car_brand.xvolkswagen + aspiration + 
                carbody.xhardtop + carbody.xhatchback + carbody.xsedan + 
                carbody.xwagon + drivewheel.xrwd + enginelocation + enginetype.xohc + 
                enginetype.xrotor , data = train)


summary(model_10)
#Multiple R-squared:  0.9765,	Adjusted R-squared:  0.9707  (adj R2 has not change significantly since last model)

sort(vif(model_10))
# carbody.xwagon has VIF of  6.718803  and pvalue of0.018019 *   ,lets remove it in next model

#model_11
#removing carbody.xwagon

model_11<- lm(formula = price ~ carwidth  + enginesize  + 
                stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                car_brand.xdodge + car_brand.xhonda + car_brand.xjaguar + 
                car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
                car_brand.xpeugeot + car_brand.xplymouth + car_brand.xporsche + 
                car_brand.xrenault  + car_brand.xsubaru + 
                car_brand.xtoyota + car_brand.xvolkswagen + aspiration + 
                carbody.xhardtop + carbody.xhatchback + carbody.xsedan + 
                drivewheel.xrwd + enginelocation + enginetype.xohc + 
                enginetype.xrotor , data = train)

summary(model_11)
#Multiple R-squared:  0.9753,	Adjusted R-squared:  0.9695  (adj R2 has not change significantly since last model)

sort(vif(model_11))
# carbody.xsedan has a high VIF :   2.517324 and a hugh pvalue of  0.202088   ,lets remove this in next iteration


#model_12
#removing carbody.xsedan
model_12<- lm(formula = price ~ carwidth  + enginesize  + 
                stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                car_brand.xdodge + car_brand.xhonda + car_brand.xjaguar + 
                car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
                car_brand.xpeugeot + car_brand.xplymouth + car_brand.xporsche + 
                car_brand.xrenault  + car_brand.xsubaru + 
                car_brand.xtoyota + car_brand.xvolkswagen + aspiration + 
                carbody.xhardtop + carbody.xhatchback  + 
                drivewheel.xrwd + enginelocation + enginetype.xohc + 
                enginetype.xrotor , data = train)

summary(model_12)
#Multiple R-squared:  0.9749,	Adjusted R-squared:  0.9693  (adj R2 has not change significantly since last model)

sort(vif(model_12))
#carbody.xhardtop  has a VIF of  1.482174 and high p value of 0.367617   . ,lets remove this in next iteration

#model_13
#removing carbody.xhardtop
model_13<- lm(formula = price ~ carwidth  + enginesize  + 
                stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                car_brand.xdodge + car_brand.xhonda + car_brand.xjaguar + 
                car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
                car_brand.xpeugeot + car_brand.xplymouth + car_brand.xporsche + 
                car_brand.xrenault  + car_brand.xsubaru + 
                car_brand.xtoyota + car_brand.xvolkswagen + aspiration + 
                carbody.xhatchback  + 
                drivewheel.xrwd + enginelocation + enginetype.xohc + 
                enginetype.xrotor , data = train)

summary(model_13)
#Multiple R-squared:  0.9747,	Adjusted R-squared:  0.9693   (adj R2 has not changed  since last model)


sort(vif(model_13))
#carbody.xhatchback  has a VIF of  1.435631 and P value of0.062712 .,hence removing this in next iteration

#model_14
#removing carbody.xhatchback
model_14 <- lm(formula = price ~ carwidth  + enginesize  + 
                 stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                 car_brand.xdodge + car_brand.xhonda + car_brand.xjaguar + 
                 car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
                 car_brand.xpeugeot + car_brand.xplymouth + car_brand.xporsche + 
                 car_brand.xrenault  + car_brand.xsubaru + 
                 car_brand.xtoyota + car_brand.xvolkswagen + aspiration + 
                 drivewheel.xrwd + enginelocation + enginetype.xohc + 
                 enginetype.xrotor , data = train)

summary(model_14)
#Multiple R-squared:  0.974,	Adjusted R-squared:  0.9687  (adj R2 has not change significantly since last model)

sort(vif(model_14))
#enginetype.xohc  has a high VIF of   4.076282 andp value of 0.014253 *  ,hence removing it in next model

#model_15
#removing enginetype.xohc

model_15 <- lm(formula = price ~ carwidth  + enginesize  + 
                 stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                 car_brand.xdodge + car_brand.xhonda + car_brand.xjaguar + 
                 car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
                 car_brand.xpeugeot + car_brand.xplymouth + car_brand.xporsche + 
                 car_brand.xrenault  + car_brand.xsubaru + 
                 car_brand.xtoyota + car_brand.xvolkswagen + aspiration + 
                 drivewheel.xrwd + enginelocation  + 
                 enginetype.xrotor , data = train)


summary(model_15)
#Multiple R-squared:  0.9726,	Adjusted R-squared:  0.9673  (adj R2 has not change significantly since last model)


sort(vif(model_15))
#car_brand.xpeugeot has a VIF of  1.832675 and high P value of  0.11469 ,lets remove this in next model due to high VIF

#model_16
#removing car_brand.xpeugeot

model_16 <- lm(formula = price ~ carwidth  + enginesize  + 
                 stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                 car_brand.xdodge + car_brand.xhonda + car_brand.xjaguar + 
                 car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
                 car_brand.xplymouth + car_brand.xporsche + 
                 car_brand.xrenault  + car_brand.xsubaru + 
                 car_brand.xtoyota + car_brand.xvolkswagen + aspiration + 
                 drivewheel.xrwd + enginelocation  + 
                 enginetype.xrotor , data = train)

summary(model_16)
#Multiple R-squared:  0.972,	Adjusted R-squared:  0.9669  (adj R2 has not change significantly since last model)

sort(vif(model_16))
# car_brand.xporsche  has a VIF of   4.606163 and P value of 0.007150 **,lets remove it in next iteration


#model_17
#removing car_brand.xporsche

model_17 <- lm(formula = price ~ carwidth  + enginesize  + 
                 stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                 car_brand.xdodge + car_brand.xhonda + car_brand.xjaguar + 
                 car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
                 car_brand.xplymouth + 
                 car_brand.xrenault  + car_brand.xsubaru + 
                 car_brand.xtoyota + car_brand.xvolkswagen + aspiration + 
                 drivewheel.xrwd + enginelocation  + 
                 enginetype.xrotor , data = train)

summary(model_17)
#Multiple R-squared:  0.9703,	Adjusted R-squared:  0.9651 (adj R2 has not change significantly since last model)

sort(vif(model_17))
#car_brand.xvolkswagen has a VIF of   1.646138    and P value of 0.005009 ** ,lets remove it in next model


#model_18
#removing car_brand.xvolkswagen

model_18 <- lm(formula = price ~ carwidth  + enginesize  + 
                 stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                 car_brand.xdodge + car_brand.xhonda + car_brand.xjaguar + 
                 car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
                 car_brand.xplymouth + 
                 car_brand.xrenault  + car_brand.xsubaru + 
                 car_brand.xtoyota +  aspiration + 
                 drivewheel.xrwd + enginelocation  + 
                 enginetype.xrotor , data = train)

summary(model_18)
#Multiple R-squared:  0.9683,	Adjusted R-squared:  0.9631  (adj R2 has not change significantly since last model)

sort(vif(model_18))
#car_brand.xhonda has a VIF of  2.156753     and P value of 0.021402 *   ,lets remove it in next model

#model_19
#removing car_brand.xhonda

model_19<- lm(formula = price ~ carwidth  + enginesize  + 
                 stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                 car_brand.xdodge  + car_brand.xjaguar + 
                 car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
                 car_brand.xplymouth + 
                 car_brand.xrenault  + car_brand.xsubaru + 
                 car_brand.xtoyota +  aspiration + 
                 drivewheel.xrwd + enginelocation  + 
                 enginetype.xrotor , data = train)

summary(model_19)
#Multiple R-squared:  0.9669,	Adjusted R-squared:  0.9617   (adj R2 has not change significantly since last model)

sort(vif(model_19))
#car_brand.xrenault  has a VIF of  1.169811     and P value of 0.094251 .   ,lets remove it in next model

#model_20
#removing car_brand.xrenault

model_20<- lm(formula = price ~ carwidth  + enginesize  + 
                stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                car_brand.xdodge  + car_brand.xjaguar + 
                car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
                car_brand.xplymouth + 
                car_brand.xsubaru + 
                car_brand.xtoyota +  aspiration + 
                drivewheel.xrwd + enginelocation  + 
                enginetype.xrotor , data = train)

summary(model_20)
#Multiple R-squared:  0.9661,	Adjusted R-squared:  0.9612   (adj R2 has not change significantly since last model)

sort(vif(model_20))
#car_brand.xtoyota    has a VIF of   1.774753     and P value of0.028637 *  ,lets remove it in next model


#model_21
#removing car_brand.xtoyota 

model_21<- lm(formula = price ~ carwidth  + enginesize  + 
                stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                car_brand.xdodge  + car_brand.xjaguar + 
                car_brand.xmazda + car_brand.xmitsubishi + car_brand.xnissan + 
                car_brand.xplymouth + 
                car_brand.xsubaru + 
                aspiration + 
                drivewheel.xrwd + enginelocation  + 
                enginetype.xrotor , data = train)

summary(model_21)
#Multiple R-squared:  0.9648,	Adjusted R-squared:   0.96   (adj R2 has not change significantly since last model)

sort(vif(model_21))
# car_brand.xnissan     has a VIF of   1.130059    and P value of 0.06264 . ,lets remove it in next model

#model_22
#removing car_brand.xnissan  

model_22<- lm(formula = price ~ carwidth  + enginesize  + 
                stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                car_brand.xdodge  + car_brand.xjaguar + 
                car_brand.xmazda + car_brand.xmitsubishi  + 
                car_brand.xplymouth + 
                car_brand.xsubaru + 
                aspiration + 
                drivewheel.xrwd + enginelocation  + 
                enginetype.xrotor , data = train)

summary(model_22)
#Multiple R-squared:  0.9638,	Adjusted R-squared:  0.9592   (adj R2 has not significantly changed  since last model)

sort(vif(model_22))
# car_brand.xmazda   has a VIF of   1.484094    and P value of0.05707 . ,lets remove it in next model

#model_23
#removing car_brand.xmazda

model_23<- lm(formula = price ~ carwidth  + enginesize  + 
                stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                car_brand.xdodge  + car_brand.xjaguar + 
                 car_brand.xmitsubishi  + 
                car_brand.xplymouth + 
                car_brand.xsubaru + 
                aspiration + 
                drivewheel.xrwd + enginelocation  + 
                enginetype.xrotor , data = train)

summary(model_23)
#Multiple R-squared:  0.9627,	Adjusted R-squared:  0.9583    (adj R2 has not significantly changed  since last model)

sort(vif(model_23))
# car_brand.xdodge  has a VIF of     1.132658    and P value of 0.01676 * ,lets remove it in next model

#model_24
#removing car_brand.xdodge 

model_24<- lm(formula = price ~ carwidth  + enginesize  + 
                stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                car_brand.xjaguar + 
                car_brand.xmitsubishi  + 
                car_brand.xplymouth + 
                car_brand.xsubaru + 
                aspiration + 
                drivewheel.xrwd + enginelocation  + 
                enginetype.xrotor , data = train)

summary(model_24)
# Multiple R-squared:  0.961,	Adjusted R-squared:  0.9567   (adj R2 has not significantly changed  since last model)

sort(vif(model_24))
# car_brand.xplymouth  has a VIF of      1.085063      and P value of  0.058404 . ,lets remove it in next model

#model_25
#removing car_brand.xplymouth 

model_25<- lm(formula = price ~ carwidth  + enginesize  + 
                stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                car_brand.xjaguar + 
                car_brand.xmitsubishi  +
                car_brand.xsubaru + 
                aspiration + 
                drivewheel.xrwd + enginelocation  + 
                enginetype.xrotor , data = train)

summary(model_25)
# Multiple R-squared:  0.9599,	Adjusted R-squared:  0.9558   (adj R2 has not significantly changed  since last model)

sort(vif(model_25))
# drivewheel.xrwd   has a VIF of     2.352730      and P value of   0.01998 *   ,lets remove it in next model

#model_26
#removing drivewheel.xrwd 

model_26<- lm(formula = price ~ carwidth  + enginesize  + 
                stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                car_brand.xjaguar + 
                car_brand.xmitsubishi  +
                car_brand.xsubaru + 
                aspiration + enginelocation  + 
                enginetype.xrotor , data = train)

summary(model_26)
# Multiple R-squared:  0.9581,	Adjusted R-squared:  0.9543    (adj R2 has not significantly changed  since last model)

sort(vif(model_26))
# car_brand.xmitsubishi   has a VIF of     1.064910       and P value of   0.004596 **    ,lets remove it in next model

#model_27
#car_brand.xmitsubishi

model_27<- lm(formula = price ~ carwidth  + enginesize  + 
                stroke  + peakrpm + car_brand.xbmw + car_brand.xbuick + 
                car_brand.xjaguar +
                car_brand.xsubaru + 
                aspiration + enginelocation  + 
                enginetype.xrotor , data = train)

summary(model_27)
# Multiple R-squared:  0.9555,	Adjusted R-squared:  0.9517    (adj R2 has not significantly changed  since last model)
#we are now with all significant variable


#model_27 is our final model

#----------------------------------------------------------------------------------------------------------------------------------------------
#                                       MODEL ASSESSMENT
#----------------------------------------------------------------------------------------------------------------------------------------------
# predicting the results in test dataset

Predict_1 <- predict(model_27,test[,-14])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price) #0.912
rsquared <- cor(test$price,test$test_price)^2
rsquared #0.832988

#Inference , we can explain 83% variability in our model

#Plot a.
#Plotting residual error vs predicted value
ggplot(fortify(model_27),aes(x = .fitted, y = .resid))+geom_point()+geom_smooth(method = "lm" )
#This plot shows that the residual error is random in nature and does not show any pattern

#Plot b.
#Plotting actual price versus predicted price

car_observations <- seq(1:62)  #This is to plot on x axis
test$error <- test$test_price-test$price

ggplot(test,aes(x=car_id,y=error,col=price))+geom_point() #error is randomly distributed

ggplot(test, aes(car_observations)) + geom_line(aes(y = test_price, col = "Purple")) + geom_line(aes(y = price, col = "Red"))+ labs(title="Actual versus predicted")


#----------------------------------------------------------------------------------------------------------------------------------------------
#                                ANALYSIS ENDS HERE
#------------------------------------------------------------------------------------------------------------------------------
