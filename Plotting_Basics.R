library('ggplot2')

data = read.csv('/Users/bhaskarn/Downloads/CustomerData-1.csv')
data

data$City <- as.factor(data$City)

summary(data)

# age of child cannot be 113, it is an outlier

data <- data[data$MaxAgeOfChild < 15 & data$MinAgeOfChild < 15,]

summary(data)

View(data)

ggplot(data = data) + ggtitle('first plot')
ggplot(data = data, mapping = aes(x = data$City, y =data$TotalRevenueGenerated))

ggplot(data, mapping = aes(x = City, fill=City)) + geom_bar() + ggtitle('Distribution of City variable') + xlab('city') + ylab('total num of customers') + geom_text(stat = 'count', aes(label=..count..), vjust=-0.5)

ggplot(data, aes(x=City, fill=FavoriteGame)) + geom_bar(position = 'dodge') + xlab('city') + ylab('total num of customers') + ggtitle('#Distribution of City variable')

ggplot(data, aes(x=City, fill=FavoriteGame)) + geom_bar() + xlab('city') + ylab('total num of customers') + ggtitle('#Distribution of City variable')

ggplot(data,aes(x=City, fill=FavoriteGame)) + geom_bar(position = "dodge",width = 0.5) + xlab("City") + ylab("# Customers")  + ggtitle("#Customers with Game preference")+ geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.5), vjust=-0.25)

ggplot(data,aes(x=City, fill=FavoriteGame)) + geom_bar(position = "dodge",width = 0.5) + scale_y_continuous(labels=scales::percent)+ xlab("City") + ylab("# Customers")  + ggtitle("#Customers with Game preference")+ geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.5), vjust=-0.25)



library(dplyr)
percentData <- data  %>% group_by(City) %>% count(FavoriteGame) %>% mutate(ratio=scales::percent(n/sum(n)))
ggplot(data, aes(x=City, fill = FavoriteGame))+ geom_bar(position="fill")+ geom_text(data=percentData, aes(y=n,label=ratio),position=position_fill(vjust=0.5))+ylab("%Customers")  +ggtitle("% Customers with Game preference") 

a = ggplot(data, aes(x=City, fill = FavoriteGame)) + 
  geom_bar(position="fill") + 
  geom_text(data=percentData, aes(y=n,label=ratio), position=position_fill(vjust=0.5)) + 
  ylab("%Customers") + 
  ggtitle("% Customers with Game preference") 

plot(a)
a+coord_flip()

ggplot(data, aes(x = TotalRevenueGenerated)) + geom_histogram(bins=10)

ggplot(data, aes(x = TotalRevenueGenerated)) + geom_freqpoly(bins=20)

ggplot(data, aes(x = TotalRevenueGenerated)) + 
  geom_histogram(bins=10) + 
  geom_freqpoly(bins=10)

ggplot(data, aes(x = TotalRevenueGenerated)) + 
  geom_histogram(bins=10, fill='blue')

ggplot(data, aes(x = TotalRevenueGenerated)) +
  geom_freqpoly(aes(color=City),binwidth=50)

ggplot(data, aes(x = City, 
                 y = TotalRevenueGenerated,
                 color=City)) +
  geom_boxplot()

ggplot(data, aes(x = City, TotalRevenueGenerated, fill=FavoriteGame)) +
  geom_boxplot()


ggplot(data=data, 
       mapping = aes(x = log(data$FrequencyOFPlay),
                     y = data$TotalRevenueGenerated)) +
  geom_point()

ggplot(data=data, 
       mapping = aes(x = log(data$FrequencyOFPlay),
                     y = log(data$TotalRevenueGenerated), color=City)) +
  geom_point()
  
ggplot(data=data, 
       mapping = aes(x = log(data$FrequencyOFPlay),
                     y = data$TotalRevenueGenerated, 
                     color=data$City, 
                     size=data$NoOfUnitsPurchased,
                     shape=data$FavoriteGame)) +
  geom_point()

ggplot(data, aes(FrequencyOFPlay, 
               TotalRevenueGenerated,
               colour = City)) + 
  geom_point() +
  coord_cartesian(xlim=seq(0,10000,500))

ggplot(data, aes(FrequencyOFPlay, 
                 TotalRevenueGenerated,
                 colour = City)) + 
  geom_point() +
  coord_cartesian(xlim=seq(0,2000,100), ylim =seq(0,250,20))

summary(data$TotalRevenueGenerated)

g <- ggplot(midwest,
            aes(x=area,
                y=poptotal)) +
  geom_point() +
  geom_smooth(method="lm")  
# set se=FALSE to turnoff confidence bands

g1 <- g + 
  coord_cartesian(xlim = c(0,0.1),
                          ylim=c(0, 1000000))  
# zooms in

# Add Title and Labels
g1 + labs(title="Area Vs Population",
          subtitle="From midwest dataset",
          y="Population",
          x="Area",
          caption="Midwest Demographics")

ggplot(data,aes(x=FavoriteGame,
                fill=NoOfChildren)) +
  geom_bar(width = 0.5) +
  xlab("Favorite Game") + 
  ylab("# Customers")  +
  ggtitle("#Customers favorite Games")+
  theme_classic()



age_binFn <- function(x) {
  if(x<5) bin = '<5'
  else if(x>5&x<7)
    bin = '5-7'
  else bin = '7+'
  return(bin) }



