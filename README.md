## Question 1
On which date was the first COVID case reported in Texas?
### Solution
Load COVID data
```{r}
load("covid.RData")
```

Extract the states,counties,populations and COVID case data from the COVID data frame. Then use subset function to extract data of state Texas
```{r}
newdata<-covid[c(2:3,6:ncol(covid))]
Texas_data<-subset(newdata,state=="Texas")
```

Extract number of COVID cases starting from Jan.22nd,2020
```{r}
caseDataOfTexas<-Texas_data[c(4:ncol(Texas_data))]
```

Get the date with first COVID case, we can know that column 44 of caseDataOfTexas data frame displays the first COVID case reported in Texas 
```{r}
for(n in 1:ncol(caseDataOfTexas)){
  if(colSums(caseDataOfTexas[n])!=0){
    print(paste(n))
    break
    }
}
```

Then find the column name which is the date of first COVID case reported in Texas. According to the output below, we can confirm that the first COVID case in Texas is reported on March.5th,2020
```{r}
head(caseDataOfTexas[44])
```

## Question 2
On which date was the first COVID case reported in the Dallas county, Texas?
### Solution
Extract data of Dallas county
```{r}
Dallas_county_data<-subset(Texas_data,county=="Dallas")
```

Extract COVID case data starting from Jan.22nd,2020 of Dallas
```{r}
caseDataOfDallas<-Dallas_county_data[c(4:ncol(Dallas_county_data))]
```

Get the date with first COVID case in Dallas County. Based on the output, column 50 is the first column with the number of COVID cases is one. Then we can know that column 50 of caseDataOfDallas data frame displays the first COVID case reported in Dallas county.
```{r}
for(n in 1:ncol(caseDataOfDallas)){
  if(caseDataOfDallas[n]!=0){
    print(paste(n))
    break
    }
}
```

Then find the column name which is the date of first COVID case reported in Dallas County. According to the output below, we can confirm that the first COVID case in Dallas County is reported on March.11th,2020
```{r}
head(caseDataOfDallas[50])
```

## Question 3
Which Texas counties have the least and most (total) COVID cases between 1/22/2020 to 3/31/2021, respectively?
### Solution
Uploading "tidyverse" library
```{r}
library(tidyverse)
```
Extract Texas COVID cases data from Jan.22nd,2020 to March.31st,2021
```{r}
DataWithinTimeSlot <- select(Texas_data,county, "2020-01-22":"2021-03-31")
```

Calculate the total number of COVID cases in each county by using rowSums() function,output the states of the rows with the largest and the smallest row sum, which is Harris and Loving.
```{r}
rowsum<-NULL
for(n in 1:nrow(DataWithinTimeSlot)){
  rowsum[n]<-rowSums(
  DataWithinTimeSlot [n,2:ncol(DataWithinTimeSlot)])
}
maximum<-max(rowsum)
minimum<-min(rowsum)
for(n in 1:nrow(DataWithinTimeSlot)){
  if((rowsum[n])==maximum){
    print(paste(DataWithinTimeSlot[n,1]))}
}

for(n in 1:nrow(DataWithinTimeSlot)){
  if((rowsum[n])==minimum){
    print(paste(DataWithinTimeSlot[n,1]))}
}
```
It can show that Harris County has the most COVID cases while Loving County has the lease cases.



## Question 4
Which Texas counties have the lowest and highest COVID rates between 1/22/2020 to 3/31/2021, respectively? We will define rate as cases over population.
### Solution
Extract the population and COVID cases data of each county, then create a data set with COVID rates called rate.
```{r}
Population<-select(Texas_data,
                   county,
                   population,
                   "2020-01-22":"2021-03-31")
rate<-NULL
for(n in 1:nrow(Population)){
  rate[n]<-rowsum[n]/Population[n,2]
}
```

Find the name of the counties with the highest and the lowest rate then we can find that the county with the highest COVID rate is Scurry County, while Loving County has the lowest COVID rate. 
```{r}
maxrate<-max(rate)
minrate<-min(rate)
for(n in 1:nrow(Population)){
  if((rate[n])==maxrate){
    print(paste(Population[n,1]))}
}

for(n in 1:nrow(Population)){
  if((rate[n])==minrate){
    print(paste(Population[n,1]))}
}
```


## Question 5
Which states have the least and most (total) COVID cases between 1/22/2020 to 3/31/2021, respectively?
### Solution
Use aggregate() function to find the total number of COVID cases by each state
```{r}
NumberOfCases<-aggregate(
  rowSums(newdata[,4:ncol(newdata)]),
  by=list(state=newdata$state),
  FUN=sum)
```

Based on the total numbers of cases in each state, which is located at the second row of NumberOfCases dataset, find the states with the least and most COVID cases.It reports that California has the most COVID cases and Vermont has the least COVID cases reported.
```{r}
for(n in 1:nrow(NumberOfCases)){
  if((NumberOfCases[n,2])==max(NumberOfCases[2])){
    print(paste(NumberOfCases[n,1]))}
}
for(n in 1:nrow(NumberOfCases)){
  if((NumberOfCases[n,2])==min(NumberOfCases[2])){
    print(paste(NumberOfCases[n,1]))}
}
```



## Question 6
Which states have the lowest and highest COVID rates between 1/22/2020 to 3/31/2021, respectively?
### Solution
Extract the populations of each state
```{r}
PopulationOfStates<-aggregate(newdata$population,by=list(state=newdata$state),FUN=sum)
```

Compute the COVID rate of each state by dividing the number of cases by population. Then we get the states where the COVID rate is the highest and lowest respective based on the indexes in NumberOfCases dataset. Because in terms of the value of column "state", the indexes of state names in column "state"are identical in dataset NumberOfCases and PopulationOfStates. Finally,we can find that North Dakota has the highest COVID rate while Vermont has the lowest
```{r}
Staterate<-NULL
for(n in 1:nrow(NumberOfCases)){
  Staterate[n]<-NumberOfCases[n,2]/PopulationOfStates[n,2]
}
for(n in 1:nrow(NumberOfCases)){
  if((Staterate[n])==max(Staterate)){
    print(paste(NumberOfCases[n,1]))}
}
for(n in 1:nrow(NumberOfCases)){
  if((Staterate[n])==min(Staterate)){
    print(paste(NumberOfCases[n,1]))}
}
```


## Question 7
Rank these counties by their total COVID cases up to the five given dates 
### Solution
Extract the data of six counties with population larger than 1 million
```{r}
Sixcounties<- subset(Texas_data,population>=1e6)
```

Rank the six counties based on their cumulative cases by five dates in ascending order   
```{r}
#Rank of cumulative cases from Jan.22nd,2020 to March.31st,2020
Cases1 <- select(Sixcounties,county, "2020-01-22":"2020-03-31")
cum1<-data.frame(Sixcounties[1],sums=rowSums(Cases1[2:ncol(Cases1)]))
rank1<-cum1[order(cum1$sums),][1]


#Rank of cumulative cases from Jan.22nd to Jun.30th,2020
Cases2 <- select(Sixcounties,county, "2020-01-22":"2020-06-30")
cum2<-data.frame(Sixcounties[1],sums=rowSums(Cases2[2:ncol(Cases2)]))
rank2<-cum2[order(cum2$sums),][1]

#Rank of cumulative cases from Jan.22nd to Sept.30th,2020
Cases3 <- select(Sixcounties,county, "2020-01-22":"2020-09-30")
cum3<-data.frame(Sixcounties[1],sums=rowSums(Cases3[2:ncol(Cases3)]))
rank3<-cum3[order(cum3$sums),][1]

#Rank of cumulative cases from Jan.22nd to Dec.31th,2020
Cases4 <- select(Sixcounties,county, "2020-01-22":"2020-12-31")
cum4<-data.frame(Sixcounties[1],sums=rowSums(Cases4[2:ncol(Cases4)]))
rank4<-cum4[order(cum4$sums),][1]

#Rank of cumulative cases from Jan.22nd to Mar.31th,2021
Cases5 <- select(Sixcounties,county, "2020-01-22":"2021-03-31")
cum5<-data.frame(Sixcounties[1],sums=rowSums(Cases5[2:ncol(Cases5)]))
rank5<-cum5[order(cum5$sums),][1]
```


To plot kable use "knitr"and "kableExtra" library
```{r}
library(knitr)
library(kableExtra)
```
Output the table for the rank of the cumulative number of COVID cases from last page by using kable function. The results can be displayed in the table above.
```{r}
rankdataframe<-data.frame(rank1,rank2,rank3,rank4,rank5)
rank<-matrix(unlist(t(rankdataframe)), byrow=F, 5,6)
tab <- rbind(rank)
row.names(tab)<-c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31", "2021-03-31")
kable1<-kable(tab, row.names = TRUE,format = "latex", booktabs = TRUE) %>% 
kable_styling(position = "center") %>%
add_header_above(c("Date", "Low $\\\\rightarrow$ high" = 6), escape = FALSE)
kable1
```



## Question 8
Rank the cumulative COVID cases rate of each county by five dates
### Solution
```{r}
# Rank the COVID cases rate by Mar.31st,2020
ratecounty1<-data.frame(Sixcounties[1],rateOfEachCounty=(rowSums(Cases1[2:ncol(Cases1)])
     /(Sixcounties$population)))                                      

rankRate1<-ratecounty1[order(ratecounty1$rateOfEachCounty),][1]

# Rank the COVID cases rate by by Jun.30th,2020
ratecounty2<-data.frame(Sixcounties[1],rateOfEachCounty=(rowSums(Cases2[2:ncol(Cases2)])
                                                         /(Sixcounties$population)))
rankRate2<-ratecounty2[order(ratecounty2$rateOfEachCounty),][1]

# Rank the COVID case rate by Sept.30th,2020
ratecounty3<-data.frame(Sixcounties[1],rateOfEachCounty=(rowSums(Cases3[2:ncol(Cases3)])
                                                         /(Sixcounties$population)))
rankRate3<-ratecounty3[order(ratecounty3$rateOfEachCounty),][1]

# Rank the COVID case rate by Dec.31st,2020
ratecounty4<-data.frame(Sixcounties[1],rateOfEachCounty=(rowSums(Cases4[2:ncol(Cases4)])
                                                         /(Sixcounties$population)))
rankRate4<-ratecounty4[order(ratecounty4$rateOfEachCounty),][1]

# Rank the COVID case rate by Mar.31st,2021
ratecounty5<-data.frame(Sixcounties[1],rateOfEachCounty=(rowSums(Cases5[2:ncol(Cases5)])
                                                         /(Sixcounties$population)))
rankRate5<-ratecounty5[order(ratecounty5$rateOfEachCounty),][1]
```

Output the table for the rank of cumulative COVID rate by using kable function. The results can be displayed in the table above.
```{r}
rankRatedataframe<-data.frame(rankRate1,rankRate2,rankRate3,rankRate4,rankRate5)
rankRate<-matrix(unlist(t(rankRatedataframe)), byrow=F, 5,6)
tab <- rbind(rankRate)
row.names(tab)<-c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31", "2021-03-31")
kable1<-kable(tab, row.names = TRUE,format = "latex", booktabs = TRUE) %>% 
kable_styling(position = "center") %>%
add_header_above(c("Date", "Low $\\\\rightarrow$ high" = 6), escape = FALSE)
kable1
```

## Question 9
Based on the results in #7 and #8, Harris county reported the most COVID cases on 3/31/2020, but Dallas county
reported the highest COVID rate on 3/31/2020. If five days are randomly selected without replacement between
1/22/2020 and 3/31/2021, what are the probabilities that, out of the six counties,

a.Harris county will report the most COVID cases on all five days.

b.Dallas county will report the highest COVID rates on all five days.
### Solution
#### a
It is known that the data of Harris County is at the fourth row of Sixcounties database.
Compute the number of days when Harris County has the most COVID cases, which is 244
```{r}
DayWithMostCases=0
for(n in 4:ncol(Sixcounties)){
    if(Sixcounties[4,n]==max(Sixcounties[n]))
      DayWithMostCases=DayWithMostCases+1
 }
DayWithMostCases
```

Calculate the probability that Harris County has the most daily COVID cases, then use binomial distribution formula to compute the probability that Harris County has most COVID cases in all five randomly selected days is 0.05552683
```{r}
P_Harris<-DayWithMostCases/435
P_FiveDays_Harris<-choose(5,5)*(P_Harris)^5*(1-P_Harris)^(5-5)
P_FiveDays_Harris
```

#### b
It is known that the data of Dallas County is at the third row of Sixcounties database.
Compute the number of days when Dallas County has the highest COVID rate, which is 71
```{r}
DayWithHighestRate=0
for(n in 4:ncol(Sixcounties)){
    if(Sixcounties[3,n]==max((Sixcounties[3,n])/(Sixcounties$population[3])))
      DayWithHighestRate=DayWithHighestRate+1
 }
DayWithHighestRate
```
Calculate the probability that Dallas County has the highest rate of COVID cases, then use binomial distribution formula to compute the probability that Dallas County has the highest rate of COVID cases in all five randomly selected days is 0.0001158365
```{r}
P_Dallas<-DayWithHighestRate/435
P_FiveDays_Dallas<-choose(5,5)*(P_Dallas)^5*(1-P_Dallas)^(5-5)
P_FiveDays_Dallas
```


## Question 10
California, Texas, Florida, and New York are the four largest states by population, yet they are very different
geographically. Repeat #7 and #8 for these four states.

### Solution
#### 1.Plotting the order of four states based on cumulative number of COVID cases by five dates
Extract the COVID data of California,Texas,Florida and New York
```{r}
FourStatesRawData<-subset(newdata,state==c("California","Texas","Florida","New York"))
FourStatesData<-aggregate((FourStatesRawData[,4:ncol(FourStatesRawData)]),by=list
                          (state=FourStatesRawData$state),FUN=sum)
```

Compute and rank the cumulative number of COVID cases by five dates
```{r}
#Rank of cumulative cases from Jan.22nd,2020 to March.31st,2020
StateCases1 <- select(FourStatesData,state, "2020-01-22":"2020-03-31")
StateCum1<-data.frame(FourStatesData[1],sums=rowSums(StateCases1[2:ncol(StateCases1)]))
StateRank1<-StateCum1[order(StateCum1$sums),][1]


#Rank of cumulative cases from Jan.22nd to Jun.30th,2020
StateCases2 <- select(FourStatesData,state, "2020-01-22":"2020-06-30")
StateCum2<-data.frame(FourStatesData[1],sums=rowSums(StateCases2[2:ncol(StateCases2)]))
StateRank2<-StateCum2[order(StateCum2$sums),][1]

#Rank of cumulative cases from Jan.22nd to Sept.30th,2020
StateCases3 <- select(FourStatesData,state, "2020-01-22":"2020-09-30")
StateCum3<-data.frame(FourStatesData[1],sums=rowSums(StateCases3[2:ncol(StateCases3)]))
StateRank3<-StateCum3[order(StateCum3$sums),][1]

#Rank of cumulative cases from Jan.22nd to Dec.31th,2020
StateCases4 <- select(FourStatesData,state, "2020-01-22":"2020-12-31")
StateCum4<-data.frame(FourStatesData[1],sums=rowSums(StateCases4[2:ncol(StateCases4)]))
StateRank4<-StateCum4[order(StateCum4$sums),][1]

#Rank of cumulative cases from Jan.22nd to Mar.31th,2021
StateCases5 <- select(FourStatesData,state, "2020-01-22":"2021-03-31")
StateCum5<-data.frame(FourStatesData[1],sums=rowSums(StateCases5[2:ncol(StateCases5)]))
StateRank5<-StateCum5[order(StateCum5$sums),][1]
```


Plot the table for the rank of cumulative number of COVID cases by using kable() function. The results can be displayed in the table above.
```{r}
staterankdataframe<-data.frame(StateRank1,StateRank2,StateRank3,StateRank4,StateRank5)
staterank<-matrix(unlist(t(staterankdataframe)), byrow=F, 5,4)
tab <- rbind(staterank)
row.names(tab)<-c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31", "2021-03-31")
kable1<-kable(tab, row.names = TRUE,format = "latex", booktabs = TRUE) %>% 
kable_styling(position = "center") %>%
add_header_above(c("Date", "Low $\\\\rightarrow$ high" = 4), escape = FALSE)
kable1
```



#### 2.Plotting the order of four states based on cumulative number of COVID rate by five dates
Extract the population of California,Texas,Florida and New York. First find the indexes of those states at PopulationOfStates data frame, then print the population of each state.
```{r}
for(n in 1:nrow(PopulationOfStates)){
  if(PopulationOfStates[n,1]=="California"|PopulationOfStates[n,1]== 
     "Florida"|PopulationOfStates[n,1]=="New York"|PopulationOfStates[n,1]=="Texas"){
    print(paste(PopulationOfStates[n,1:2]))
    print(paste(n))
  }
}
```

Create a vector with populations of the four states which is needed for the calculation of the cumulative COVID rates
```{r}
FourStatePopulation<-c(PopulationOfStates[4,2],PopulationOfStates[9,2],
                       PopulationOfStates[31,2],PopulationOfStates[42,2])
```


Compute and rank the cumulative rates of COVID cases by five dates
```{r}
# Rank the COVID cases rate by Mar.31st,2020
ratestate1<-data.frame(FourStatesData[1],
                       rateOfEachState=(rowSums(StateCases1[2:ncol(StateCases1)])
                                                         /(FourStatePopulation)))
rankStateRate1<-ratestate1[order(ratestate1$rateOfEachState),][1]

# Rank the COVID cases rate by by Jun.30th,2020
ratestate2<-data.frame(FourStatesData[1],
                       rateOfEachState=(rowSums(StateCases2[2:ncol(StateCases2)])
                                                         /(FourStatePopulation)))
rankStateRate2<-ratestate2[order(ratestate2$rateOfEachState),][1]

# Rank the COVID case rate by Sept.30th,2020
ratestate3<-data.frame(FourStatesData[1],
                       rateOfEachState=(rowSums(StateCases3[2:ncol(StateCases3)])
                                                         /(FourStatePopulation)))
rankStateRate3<-ratestate3[order(ratestate3$rateOfEachState),][1]

# Rank the COVID case rate by Dec.31st,2020
ratestate4<-data.frame(FourStatesData[1],
                       rateOfEachState=(rowSums(StateCases4[2:ncol(StateCases4)])
                                                         /(FourStatePopulation)))
rankStateRate4<-ratestate4[order(ratestate4$rateOfEachState),][1]

# Rank the COVID case rate by Mar.31st,2021
ratestate5<-data.frame(FourStatesData[1],
                       rateOfEachState=(rowSums(StateCases5[2:ncol(StateCases5)])
                                                         /(FourStatePopulation)))
rankStateRate5<-ratestate5[order(ratestate5$rateOfEachState),][1]
```



Plot the table for the rank of cumulative number of COVID cases by using kable() function. The results can be displayed in the table above.
```{r}
rankStateRatedataframe<-data.frame(rankStateRate1,rankStateRate2,rankStateRate3,
                                   rankStateRate4,rankStateRate5)
rankStateRate<-matrix(unlist(t(rankStateRatedataframe)), byrow=F, 5,4)
tab <- rbind(rankStateRate)
row.names(tab)<-c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31", "2021-03-31")
kable1<-kable(tab, row.names = TRUE,format = "latex", booktabs = TRUE) %>% 
kable_styling(position = "center") %>%
add_header_above(c("Date", "Low $\\\\rightarrow$ high" = 4), escape = FALSE)
kable1
```



## Question 11
Based on the results in #10, If five days are randomly selected without replacement between 1/22/2020 and 3/31/2021,
what are the probabilities that, out of the four states,

a. New York will report the most COVID cases on more than three of the five days.

b. New York will report the highest COVID rates on more than three of the five days.
### Solution
#### a
Based on FourStateData dataset, the third row is the case data of New York state.Calculate the number of days when New York State has the most COVID cases
```{r}
NYDayWithMostCases=0
for(n in 2:ncol(FourStatesData)){
    if(FourStatesData[3,n]==max(FourStatesData[n]))
      NYDayWithMostCases=NYDayWithMostCases+1
 }
NYDayWithMostCases
```

Calculate the probability that New York State has the most daily COVID cases in four states, then use binomial distribution formula to compute the probability that New York State has most COVID cases for more than three of five randomly selected days is 0.009766394
```{r}
P_NY<-NYDayWithMostCases/435
P_ThreeDays_NY<-choose(5,5)*(P_NY)^5*(1-P_NY)^0+
                choose(5,4)*(P_NY)^4*(1-P_NY)^1
P_ThreeDays_NY
```

#### b
Calculate the number of days when New York State has the highest rate of COVID cases
```{r}
NYDayWithHighestRate=0
for(n in 2:ncol(FourStatesData)){
    if(FourStatesData[3,n]==max((FourStatesData[3,n])/(PopulationOfStates[31,2])))
      NYDayWithHighestRate=NYDayWithHighestRate+1
 }
NYDayWithHighestRate
```

Calculate the probability that New York State has the highest rate of COVID cases, then use binomial distribution formula to compute the probability that New York has the highest rate of COVID cases for more than three of five randomly selected days is 0.0005252265
```{r}
P_NYRate<-NYDayWithHighestRate/435

P_ThreeDays_NYRate<-choose(5,5)*(P_NYRate)^5*(1-P_NYRate)^(5-5)+
                    choose(5,4)*(P_NYRate)^4*(1-P_NYRate)^(5-4)

P_ThreeDays_NYRate
```

## Question 12
Let X be the random variable that represents the daily COVID cases reported in Texas. Since X is a count data, it is
reasonable to model X with a Poisson random variable, a binomial random variable, or a negative binomial random
variable. However, the observed X goes from 0 to 37035 with a median of 4805. To prevent working with potentially
over-dispersed count data, we consider the transformation Y = log(1 + X), where log(·) is the natural log. Use hanging
rootograms from the vcd package to determine the distribution (out of the Poisson, the binomial, and the negative
binomial) that fits Y the best. To receive full credit, display the rootograms, state the hypotheses, p-value, and explain
your conclusion.
### Solution
Apply transformation to $Y$=$\log(1+X)$
```{r}
X<-colSums(Texas_data[,4:ncol(Texas_data)])
Y<-log(1+X)
```

Find distribution that fits random variable X the best

Use "vcd" library which has goodfit() function to find the best fit model
```{r}
library(vcd)
```
Try Poisson distribution on Y with the hypothesis that Poisson distribution fits X the best, then we know that p-value=0.03<0.05. Therefore this hypothesis is rejected.
```{r}
fit1<-goodfit(Y,"poisson")
plot(fit1,shade=TRUE)
```

Try binomial distribution on Y with the hypothesis that binomial distribution fits X the best, then we know that p-value=2.22e-16<0.05. Therefore this hypothesis is rejected.
```{r}
fit2<-goodfit(Y,"binomial")
plot(fit2,shade=TRUE)
```

## Question 13
Use the best-fit distribution from #12 and the corresponding parameter(s) to estimate E(X), the expected number of
reported COVID cases in one day. This could be achieved by first estimate E(Y), then plug the estimation into the
equation Y = log(1 + X).
### Solution
Find the estimate E(Y) based on the fit data of negative binomial distribution.It shows that E(Y)=5
```{r}
EY<-(fit3$par$size/(fit3$par$prob))-fit3$par$size
EY
```
## Question 14
Plot the 7-day moving average for the COVID cases reported in Dallas county, TX, over 1/22/2020 and 3/31/2021. We
will assume there are no reported cases before 1/22/2020. The plot should have the following components:
• Have a proper main title and axis labels

• The x-axis should contain the dates (or the number of days since 1/22/2020)

• The y-axis should contain the moving averages
### Solution
```{r}
for(n in 1:ncol(caseDataOfDallas)){
  if(caseDataOfDallas[n]!=0){
    print(paste(n))
    break
  }
}
```

Find the 7-day moving average of COVID cases in Dallas county, before Day 50, the 7-day moving average is 0
```{r}
Seven_Day_Moving_Average<-NULL
for(n in 1:ncol(caseDataOfDallas)){
  if(n>=50){
  Seven_Day_Moving_Average[n]<-rowMeans(caseDataOfDallas[(n-6):n])
  }
  else{
    Seven_Day_Moving_Average[n]<-0
  }
}
```

To plot the data use "ggplot2" library
```{r}
library(ggplot2)
```
To plot the 7-day moving average of COVID cases in Dallas county, create a vector displayed on x-axis that contains number 1 to 435, which represent the number of days from Jan.22nd,2020 to March.31,2021. Create a vector on y-axis that contains 7-day moving average of COVID cases each day.
```{r}
Day<-c(1:435)
Seven_Day_Moving_AverageDf<-data.frame(Day,Seven_Day_Moving_Average)
```
Use ggplot library to plot the line-figure of 7-day moving average daily
```{r}
ggplot(data=Seven_Day_Moving_AverageDf, 
       aes(x=Day, y=Seven_Day_Moving_Average))+
       geom_line(color="red")+
       ggtitle("7-Day Moving Average of Dallas County,
              TX from 01/22/2020 to 03/31/2021")
```
## Question 15
Plot the 7-day moving average of COVID cases for the counties in #7 in one plot. The plot should have the following
component:
• Have a proper main title and axis labels

• The x-axis should contain the dates (or the number of days since 1/22/2020)

• The y-axis should contain the moving averages

• Use different colors for the different counties

• Use legend to reflects the different colors
### Solution
Find the number of the day when those counties reported the first COVID case, which is day 47.
```{r}
for(n in 4:ncol(Sixcounties)){
if(colSums(Sixcounties[n])!=0){
print(paste(n))
break
  }
}
```
Create a list contains 7-day moving average for each county, in which the first 47 elements are 0.
```{r}
Seven_Day_Moving_Average_Bexar<-NULL
for(n in 1:435){
  if(n>=47){
  Seven_Day_Moving_Average_Bexar[n]<-rowMeans(Sixcounties[1,(n-6):n])}
  else{
    Seven_Day_Moving_Average_Bexar[n]<-0}
}
Seven_Day_Moving_Average_Collin<-NULL
for(n in 1:435){
  if(n>=47){
  Seven_Day_Moving_Average_Collin[n]<-rowMeans(Sixcounties[2,(n-6):n])}
  else{
    Seven_Day_Moving_Average_Collin[n]<-0}
}
Seven_Day_Moving_Average_Dallas<-NULL
for(n in 1:435){
  if(n>=47){
  Seven_Day_Moving_Average_Dallas[n]<-rowMeans(Sixcounties[3,(n-6):n])}
  else{
    Seven_Day_Moving_Average_Dallas[n]<-0}
}
Seven_Day_Moving_Average_Harris<-NULL
for(n in 1:435){
  if(n>=47){
  Seven_Day_Moving_Average_Harris[n]<-rowMeans(Sixcounties[4,(n-6):n])}
  else{
    Seven_Day_Moving_Average_Harris[n]<-0}
}
Seven_Day_Moving_Average_Tarrant<-NULL
for(n in 1:435){
  if(n>=47){
  Seven_Day_Moving_Average_Tarrant[n]<-rowMeans(Sixcounties[5,(n-6):n])}
  else{
    Seven_Day_Moving_Average_Tarrant[n]<-0}
}
Seven_Day_Moving_Average_Travis<-NULL
for(n in 1:435){
  if(n>=47){
  Seven_Day_Moving_Average_Travis[n]<-rowMeans(Sixcounties[6,(n-6):n])}
  else{
    Seven_Day_Moving_Average_Travis[n]<-0}
}
```

Attach each list created above with the county name. Then create a data frame that contains 7-day moving average of all six counties. This data frame is prepared for plotting the data grouped by the counties.Then plot the lines of the 7-day moving average,each of which are colored differently.
```{r}
a<-data.frame(Bexar=Seven_Day_Moving_Average_Bexar,
           Dallas=Seven_Day_Moving_Average_Dallas,
           Collin=Seven_Day_Moving_Average_Collin,
           Harris=Seven_Day_Moving_Average_Harris,
           Tarrant=Seven_Day_Moving_Average_Tarrant,
           Travis=Seven_Day_Moving_Average_Travis)
row.names(a) <- 1 : 435
Day<-1:435

ggplot(a, aes(x=Day,y=7-DayMovingAveragePerCounty)) + 
  geom_line(aes(y = Bexar, colour = "Bexar")) + 
  geom_line(aes(y = Dallas, colour = "Dallas"))+
  geom_line(aes(y = Collin, colour = "Collin"))+
  geom_line(aes(y = Harris, colour = "Harris"))+
  geom_line(aes(y = Tarrant, colour = "Tarrant"))+
  geom_line(aes(y = Travis, colour = "Travis"))+
  ggtitle("7-Day Moving Average of COVID cases in Six Counties with Largest 
  Population in Texas from 01/22/2020 to 03/31/2021")
```

## Question 16
The 7-day moving average can be used to provide a guild line for the expected number of COVID cases in the next day.
Let Xi be the random variable that represents the number of COVID cases reported in the state of Texas on day i+7, the
could be expressed as
Xi+7 = beta_0Xi +  beta_1Xi+1 +  beta_2Xi+2 +  beta_3Xi+3 +  beta_4Xi+4 +  beta_5Xi+5 +  beta_6Xi+6,
where the parameters can beta_0 = beta_1 = beta_2 = beta_3 = beta_4 = beta_5 = beta_6

Using the 7-day moving average, the expected count for 4/1/2021, would be 3731. Regardless of the results from #12,
suppose the daily COVID counts follows a Poisson distribution with lambda = 7-day moving average, then we can say the
number of the reported COVID case on 4/1/2021, follows a Poisson distribution with lambda = 3731. Under this assumption,
what is the probability that the number of reported case is less than 3500 on 4/1/2021?
### Solution
The probability that the number of reported case is less than 3500 on 4/1/2021 is P(X<3500)=6.927151e-05

```{r}
ppois(3500,3731)
```

## Question 17
With the Poisson assumption in #16, the expected number of reported COVID cases in Texas on 4/2/2021 would be
3801.29. Continuous to apply the Poisson assumption, what is the probability that the number of reported COVID
cases in Texas for 4/2/2021 is less than 3500?
### Solution
The probability that the number of reported COVID
cases in Texas for 4/2/2021 is less than 3500 is P(X<3500)=3.857371e-07

```{r}
ppois(3500,3801.29)
```

## Question 18
Use the approaches in #16 and #17 to answer the following questions.

a. What is the expected number of reported COVID cases in Texas on 5/1/2021?

b. What is the probability that the number of reported COVID cases in Texas for 5/1/2021 is less than 3500?
### Solution
#### a
Create an array with 7-day moving average from April.1st to May.1st,2021, where the 7-day moving average on May.1st,2021 is the expected number of COVID cases. Based on the computation,the expected number of reported COVID cases on May.1st,2021 is approximately 3416
```{r}
sevenmoving<-c(3239,6960,3149,3283,3485,3519,2482)
sevenAssumption<-NULL
for(n in 1:31){
  sevenAssumption[1]=sum(sevenmoving[1:7])/7
  if(n>=2 && n<=7){
    sevenAssumption[n]=(sum(sevenmoving[n:7])+sum(sevenAssumption[1:(n-1)]))/7}
  else{
    sevenAssumption[n]=sum(sevenAssumption[(n-7):(n-1)])/7
  }
}
sevenAssumption[31]
```

#### b
The probability that the number of reported COVID cases in Texas on 5/1/2021 is less than 3500 is P(X<3500)= 0.9253429
```{r}
ppois(3500,sevenAssumption[31])
```


## Question 19
There is a total of 2,816,663 reported COVID cases in Texas between 1/22/2020 and 3/31/2021. The population of Texas in
2019 is 28,995,881. This implies the infection rate in Texas (up to 3/31/2021) is approximately 2816663/28995881 = 0.0971.
Regardless of the results from #12 and #16, suppose the number of infected individuals out of a random sample of
size n in Texas follows a binomial distribution with parameters n and p = 0.0971. What is the probability that the a
random sample of n = 100 will consist of at least 15 infected individuals?
### Solution
The probability that the random sample of size=$n$ will consist of at least 15 infected individuals is $P(X\ge 15)$=1-$P(X< 15)$=0.03165399
```{r}
1-pbinom(15,100,0.0971)
```

## Question 20
What are the normal approximations to the probability in #19? Report the approximations with and without continuity
correction.
### Solution
#### Without continuity correction
Use normal distribution to compute the probability that the a random sample of n = 100 will consist of at least 15 infected individuals, which is $P(X\ge 15)$=1-$P(X< 15)$=0.03700145
```{r}
Mean<-100*0.0971
1-pnorm(15,Mean,sqrt(100*0.0971*(1-0.0971)))
```

#### With continuity correction
Use normal distribution to compute the probability that the a random sample of n = 100 will consist of at least 15 infected individuals with continuity correction, which is $P(X\ge 15)$=$P(X>(15-0.5))$=1-$P(X\le(15-0.5))$=0.05286039
```{r}
1-pnorm((15-0.5),Mean,sqrt(100*0.0971*(1-0.0971)))
```


