library(ggplot2)
library(scales)

setwd("/home/yo/Downloads/R")
custdata <- read.table(file = "custdata.tsv", header = T, sep = "\t")


# Histogram plot
plot.hist.age <- ggplot(custdata)
plot.hist.age <- plot.hist.age + geom_histogram(aes(x = age), 
                                                binwidth = 5,
                                                fill="gray")
plot.hist.age


# Density plot
plot.dens.age <- ggplot(custdata)
plot.dens.age <- plot.dens.age + geom_density(aes(x = income))
plot.dens.age <- plot.dens.age + scale_x_continuous(labels = dollar)
plot.dens.age


# Density plot with log scale
ggplot(custdata) + geom_density(aes(x=income)) +
  scale_x_log10(breaks=c(100,1000,10000,100000), labels=dollar) +
  annotation_logticks(sides="bt")


# Bar plot
ggplot(custdata) + geom_bar(aes(x=marital.stat), fill="gray")


# Bar plot with horizontal bars
ggplot(custdata) +
  geom_bar(aes(x=state.of.res), fill="gray") +
  coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8)))


# Bar plot with horizontal bars in increasing order
statesums <- table(custdata$state.of.res)
statef <- as.data.frame(statesums)
colnames(statef)<-c("state.of.res", "count")
summary(statef)

statef <- transform(statef, state.of.res=reorder(state.of.res, count))
summary(statef)
ggplot(statef)+ geom_bar(aes(x=state.of.res,y=count),
                         stat="identity",
                         fill="gray") +
  coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8)))


# Line plot
x <- runif(100)
y <- x^2 + 0.2*x
ggplot(data.frame(x=x,y=y), aes(x=x,y=y)) + geom_line()


# Scatter plot
custdata2 <- subset(custdata,
                    (custdata$age > 0) & 
                      (custdata$age < 100) & 
                      (custdata$income > 0))
cor(custdata2$age, custdata2$income)
ggplot(custdata2, aes(x=age, y=income)) + geom_point() + ylim(0, 200000)


# Scatter plot fit linear fit
ggplot(custdata2, aes(x=age, y=income)) + geom_point() +
  stat_smooth(method="lm") + ylim(0, 200000)


# Scatter plot with smoothing curve (smoothed local linear fits of the data)
ggplot(custdata2, aes(x=age, y=income)) +
  geom_point() + geom_smooth() + ylim(0, 200000)


# 
ggplot(custdata2, aes(x=age, y=as.numeric(health.ins))) +
  geom_point(position=position_jitter(w=0.05, h=0.05)) +
  geom_smooth()


# Hexbin plot
library(hexbin)
ggplot(custdata2, aes(x=age, y=income)) +
  geom_hex(binwidth=c(5, 10000)) +
  geom_smooth(color="white", se=F) +
  ylim(0,200000)


