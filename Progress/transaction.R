library(tidyverse)
library(skimr)
library(anytime)
library(data.table)
library(ggplot2)
library(stats)

trans <- read.csv('interview/synthetical_payments.csv')
trans$transactionDay <- anydate(trans$transactionTime)
setDT(trans)

##exploration
skim(trans)
dim(trans)
unique_user <- length(unique(trans$userId))


### change to accural basis
a <- trans[ ,seq(from=transactionDay, length.out = period, by='month'), by=list(userId,transactionTime)]
accural <- merge(a, trans, by = c("userId", "transactionTime"))
names(accural)[3] = 'recurring_month'


##MRR
accural[, early_date := min(transactionDay),by = userId]
accural[,style :=if_else(recurring_month == early_date, 'new','recurring')]

mrr <- accural %>% group_by(format(recurring_month, '%Y-%m'), style) %>% 
  summarise(sum = sum(price)) %>% spread(key = style, value = sum, fill = 0) %>% ungroup() %>% 
  mutate(total = new + recurring)

mrr <- mrr %>% mutate(churned = lag(total) - recurring)
mrr <- mrr %>% mutate(pct_new = (new/lag(new) - 1) * 100,
               pct_recurring = (recurring/lag(recurring)-1) *100,
               pct_total = (total/lag(total)-1)*100)

amount_by_type <-mrr %>% mutate (pct_new2total =(new/total)*100,
                                 pct_recurring2total = (recurring/total)*100) 


## users count
user <- accural %>% group_by(format(recurring_month, '%Y-%m'),style) %>% 
  count()

names(user)[1] <- 'month_range'

ggplot(user,aes(month_range))+
  geom_line(aes(y = n,color = style,group = style))

## paid_by user

user1 <- spread(user,key = style,value = n)
new_user <- mrr$new/user1$new
recurring_user <- mrr$recurring/user1$recurring

paid_by_user <- as.data.frame(cbind(user1$month_range,new_user,recurring_user))

ggplot(paid_by_user,aes(V1))+
  geom_line(aes(y = new_user,group = 1),color = 'darkred')+
  geom_line(aes(y = recurring_user,group = 1),color = 'steelblue')


accural[,by = list[format(recurring_month, '%Y-%m'),style]]
        
names(mrr)[1] <- 'month_range')
ggplot(mrr,aes(month_range))+
  geom_line(aes(y = pct_new,group = 1),color = 'darkred')+
  geom_line(aes(y = pct_recurring,group = 1),color = 'steelblue')+
  geom_line(aes(y = pct_total,group = 1))

mrr
###need visualization

accural %>% group_by(format(recurring_month, '%Y-%m')) %>% 
  count(product) %>% spread(key = product, value = n )

pro <- accural %>% group_by(format(recurring_month, '%Y-%m')) %>%
  count(product,style) 

product <- pro %>% gather(-month, key = 'variables', value = 'value')

ggplot(pro)+
  geom_line(aes(month,n,group = style, color = style))+
  facet_grid(cols = vars(product))
  
  



### need visualization


#b <- accural %>% spread(key = 'style',value ='price')
##mutate(style = if_else(min(recurring_month) == transactionDay, 'new','recurring'))
  





b <- trans %>% group_by(format(transactionDay, '%Y-%m')) 




##we have some repeated unserid, so we have some old customer here

usercount <- trans %>% group_by(userId) %>% count() %>% ungroup()
data_count <- merge(usercount, trans)
##data_count$olduser = as.integer(data_count$n > 1)

##data_count %>% filter(olduser == 1 ) %>% group_by(billingCountry)%>% count()
##data_count %>% filter(olduser == 0 ) %>% group_by(billingCountry)%>% count()

##! conclusion Frans looks don't really like our product. 



##trans %>% group_by(userId) %>%
##mutate(next_date=transactionDay)%>%
##complete(next_date=seq(from = transactionDay, length.out = period, by = 'month'))

##trans[, list(datelist=seq.Date( from = transactionDay, length.out = period, by='month')),by=userId]
##apply(df2, 1, function(x) seq(from = x['transactionDay'], lenth.out = x['period'], by = 'month'))
##map(seq,from = df2[,1],by = 'month',length.out = df2[,2])
##apply(df2,1,function(x) { seq.Date(from=x[,"transactionDay"], by = "month", length.out = x[,"period"])})

##seq.Date(from=trans[["transactionDay"]][1], by = "month", length.out = trans[["period"]][1])