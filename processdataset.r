#install.packages("jsonlite")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("scales")
#install.packages("dplyr")
#install.packages("gridExtra")
#install.packages("grid")

#load libraries
library(jsonlite)
library(ggplot2)
require(scales)
library(GGally)
library(gridExtra)
library(grid)
library(dplyr)

#Reading the json file.
js.nf <- fromJSON(txt="sample.txt", simplifyDataFrame=TRUE)

#deleting the dataframe from the memory.
if (exists("df.prod") == "TRUE") {
  rm(df.prod)
}
if (exists("df.temp") == "TRUE") {
  rm(df.temp)
}
if (exists("df.nf") == "TRUE") {
  rm(df.nf)
}

# Create Data set ---------------------------
# df.nf is the data fraem with the nota fiscal content.
df.nf <- js.nf$complemento
df.nf$dataemissao <- js.nf$ide$dhEmi
df.nf$week <- as.numeric(strftime(js.nf$ide$dhEmi, format="%W"))
df.nf$mesa <- as.factor(js.nf$infAdic$infCpl)

# Creating the second dataframe
# df.prod has the content about what the customer has consumed;
for (i in seq(along = js.nf$det)) {     
  #create df.temp and get the product's content
  df.temp <- js.nf$det[[i]][[2]]
  #getting the data emissao.
  df.temp$datahoraemissao <- js.nf$ide[[1]][[i]]  
  df.temp$week <- as.numeric(strftime(js.nf$ide[[1]][[i]], format="%W"))  
  #getting table (mesa)
  df.temp$mesa <- as.factor(js.nf$infAdic[[1]][[i]])
  #combine df.prod with df.temp
  if (exists("df.prod") == "TRUE") {
    df.prod <- rbind(df.prod, df.temp)
  } else {
    df.prod <- df.temp
  }  
}
#removing unecessary dataset
rm(js.nf)
rm(df.temp)
# showing the dataset attributes
names(df.nf)

#filtering the product by buffet.
df.prodbuffet <- df.prod %>% 
  filter(xProd == 'BUFFET')

#The max product price without outliers.
df.prod %>% 
  filter(vProd <200) %>% 
  summarise(
    maximo = max(vProd)
  )

#Calculation the product mean
mean(df.prod$vProd)

#showing the distribution the customer spend at the restaurant
# is similar the distribution the buffet price.
plot1 <- ggplot(aes(x = valorTotal), data = df.nf)+
  geom_histogram(binwidth = 10)+  
  ggtitle("Total spend distribution")+
  xlab("Total")  +
  ylab("Frequency")  

plot2 <- ggplot(aes(vProd), data = df.prodbuffet ) +
  geom_histogram(binwidth = 10)+
  ggtitle("Total spend distribution with buffet")+
  xlab("Total with buffet")  +
  ylab("Frequency")    

grid.arrange(plot1, plot2, ncol = 2)  

#The distribution from the product dataset is very similar from the nota fiscal dataset.
# From now on, I'm going just use the nf datset to make the predictions
# I'm going to remove them
rm(df.prodbuffet)
rm(df.prod)

# new dataset without outlier
df.nf.without.out <- df.nf %>%
  filter(valorTotal < 226)

##How much 50% of the customer spend (first to third quartile )
summary(df.nf$valorTotal)

#charts showing the total spend distribution.
plot1 <- ggplot(aes(x = valorTotal), data = df.nf)+
  geom_histogram(binwidth = 10)+  
  ggtitle("Total spend distribution")+
  xlab("Total")  +
  ylab("Frequency")  

plot2 <- ggplot(aes(x = valorTotal), data = df.nf)+
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(20, 226)) +
  ggtitle("Total spend distribution without outliers")+
  xlab("Total")  +
  ylab("Frequency")  
#showing the plots in a grid
grid.arrange(plot1, plot2, ncol = 2)

#first quartile
quantile(df.nf$valorTotal, 0.25)
#mean
mean(df.nf$valorTotal) 
#third quartile
quantile(df.nf$valorTotal, 0.75)
#
#Calculating the interval confidence to predict how much a customer will spend.
#
#apply log10 to normalize the distribution.
ggplot(aes(x = log10(valorTotal)), data = df.nf.without.out)+
  geom_histogram(binwidth = 0.01) +  
  ggtitle("Total spend distribution log10") +
  xlab("Total spend log 10") +
  ylab("Frequency")  

#Interval about how much 95% of the customer spend without outlier
summary(log10(df.nf.without.out$valorTotal))
lowerinterval = mean(log10(df.nf.without.out$valorTotal)) - (sd(log10(df.nf.without.out$valorTotal)) * 2)
higherinterval = mean(log10(df.nf.without.out$valorTotal)) + (sd(log10(df.nf.without.out$valorTotal)) * 2)
#print the variables.
lowerinterval
higherinterval
10^lowerinterval
10^higherinterval

#grouping the data by week and sum the total spend.
df.nfbyweek <-df.nf.without.out %>%  
  group_by(week) %>%
  summarise(
    sum.valortotal = sum(valorTotal)
)
#
#Calculating the  sales forecast for the next week
#
#plotting a scatter diagram from valorTotal by week
plot1 <- ggplot(aes(x = week, y = valorTotal), data = df.nf.without.out) +
  geom_point(alpha = 0.4, size = 1,color = 'blue', position = 'jitter')+
  geom_smooth(method = "lm", color = "red"  ) +
  ggtitle("Total spend distribution by week") +
  xlab("Week") +
  ylab("Total spend")  
#plotting a scatter diagram from the SUM valorTotal mean by week
plot2 <- ggplot(aes(x = week, y = sum.valortotal), data = df.nfbyweek) +
  geom_point() +
  ggtitle("Sum total spendby week") +
  xlab("Week") +
  ylab("Sum total spend")  
  
#plotting a scatter diagram from the valorTotal mean by week
#with a linear regression line
plot3 <- ggplot(aes(x = week, y = sum.valortotal), data = df.nfbyweek) +
  geom_point()  +
  geom_smooth(method = "lm") +
  ggtitle("Sum total spendby week with a linear regression line") +
  xlab("Week") +
  ylab("Sum total spend")    
  
#showing both diagrams together.
grid.arrange(plot1, plot2, plot3, ncol = 3)

#Calculate the linear regression model
m = lm(sum.valortotal ~ week, data = df.nfbyweek)
summary(m)
modelEstimate <- predict(m, newdata = data.frame(week = 4), interval = "prediction" ,level = .95)
#printing the fit value and the confidence interval.
modelEstimate
#removing the dataframe.
rm(df.nfbyweek)
rm(df.nf.without.out)

# Find the best places at the resturant:
#   (mesas) whith greater frequency
View(df.nf %>% count(mesa, sort = TRUE))





