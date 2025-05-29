library(readr)
library(dplyr)
library(ggplot2)

df <- read_csv('D:/Datasets/Open_Dataset/crop_yield.csv')
df <- rename(df, CropYield = "Yield_tons_per_hectare")
View(df)
#test numeric variables correlation
estimates <- cor.test(df$Rainfall_mm, df$CropYield, method='pearson')

#test categorical variables correlation
estimates_RainFall <- t.test(CropYield~Fertilizer_Used, data=df)

#test categorical variables correlation for more than 2 group 
estimates_reg <- kruskal.test(CropYield ~ Region, data=df)

estimates_Soil_Type <- kruskal.test(CropYield ~ Soil_Type, data=df)

estimates_Crop <- aov(CropYield ~ Crop, data=df)

estimates_Temperature <- cor.test(df$Temperature_Celsius, df$CropYield, method='pearson')

estimates_Crop <- t.test(CropYield~Crop, data=df)

hist(df$CropYield, main="Histogram", xlab="Crop Yield")
  
uni <- sapply(df, unique) 

#checking residuals
#residuals is diffrence between each field value and group mean
residuals(estimates_Crop)








estimates_rain <- cor.test(df$Rainfall_mm, df$CropYield, method = 'pearson')

corr_rain <- cor(df$Rainfall_mm, df$CropYield)


x <- slice_sample(df, n=20)

ggplot(df, aes(y=CropYield, x=Rainfall_mm))+
  geom_point()+
  geom_smooth(method='lm')+
  labs(title="Relationship Between RainFall and CropYield",
       y="Crop Yield",
       x="Days_to_Harvest")+
  theme_classic()


# p.value < 0.05 => there is significance positive relationship.
# p.value > 0.05 => No Relationship At All.

estimates_day <- cor.test(df$Days_to_Harvest, df$CropYield, method = 'pearson')



# ANOVA
# T.TEST
# KRUSKAL.TEST

x = filter(df, Crop %in% c("Rice", "Soybean"))


estimates_Crop <- t.test(CropYield~Crop, data=x)

estimates_Crop = aov(CropYield~Crop, data=df)

estimates_Crop <- kruskal.test(CropYield~Crop, data=df)

View(summarise(groupby(df, Crop), GroupMean=mean(CropYield)))



