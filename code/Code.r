#Loading Data
df = read_csv('D:/Learn R/R/df_uncleaned.csv',col_names = TRUE)

#Data Inspection
dim(df)
head(df)
summary(df)
str(df)
colSums(is.na(df))
sum(duplicated(df))

#Data Cleaning
#Remove Entire Column
missing_perce=colMeans(is.na(df))*100
percentage=87
colum_keep <- missing_perce <= percentage
data_keep = df[, colum_keep]
View(data_keep)
#Impute Missing Values
df <- mutate(data_keep, YIELD=replace_na(data_keep$YIELD, mean(data_keep$YIELD, na.rm = TRUE)))
df <- rename(df, AreaPlanted="AREA PLANTED", Production="PRODUCTION")
df$AreaPlanted <- replace_na(df$AreaPlanted, mean(df$AreaPlanted, na.rm = TRUE))
df$Production <- replace_na(df$Production, mean(df$Production, na.rm = TRUE))

#DATA VISUALISATION
library(ggplot2)
#Histogram
ggplot(df) + aes(x=YIELD) + geom_histogram(bins = 30, fill='steelblue', color='green', alpha=0.8)+
  labs(
    title='Distribution Of Yield',
    x='Yield',
    y='Frequency'
  )+
  theme_classic()

#Boxplot
ggplot(df) + aes(x=state_alpha,y=YIELD) + geom_boxplot(aes(fill=state_alpha),color='darkblue')+
  labs(
    title="BoxPlot Of Yield Per Sate", x="State",y="Yiled"
  )+
  theme_minimal(base_size=8)

#BAR PLOT
ggplot(df) + aes(y=commodity_desc) + geom_bar(aes(fill=commodity_desc))+
  labs(title = "Commodity BarPlot", x="Frequency",y="Commodity Description")+
  theme_grey()

ggplot(df) + aes(y=state_alpha) + geom_bar(aes(fill=state_alpha))+
  labs(title = "State Rare APPEARANCE", x="Counts", y="State")+
  theme_bw()

#Normalise data  before Visual
df$AreaPlanted <- (df$AreaPlanted - min(df$AreaPlanted)/(max(df$AreaPlanted) - min(df$AreaPlanted)))
df$Production <- (df$Production - min(df$Production)/(max(df$Production) - min(df$Production)))
library(scales)
#ScatterPlot
ggplot(df, aes(x = AreaPlanted, y = Production)) +
  geom_point(color = "darkgreen", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", size=1) +
  scale_x_continuous(labels=comma)+
  scale_y_continuous(labels = comma)+
  labs(
    title = "Relationship Between Area Planted and Production",
    x = "Area Planted (ha)",
    y = "Production (tons)"
  ) +
  theme_minimal()
#checking corralation
cor(df$AreaPlanted, df$Production, use = 'complete.obs')

ggplot(df, aes(x=year, y=AreaPlanted, color=state_alpha))+
  geom_line()+
  scale_y_continuous(labels = scales::comma)
  labs(title="Line PLot Of AreaPlanted At Time",
       x="Year",
       y="Area Planted")+
  theme_minimal()


