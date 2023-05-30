# Atharva Deshmukh
# Roll no. - 210231

########################################################

library(tidyverse)
library(rvest)
library(dplyr)
library(stringr)

###########################################################

# Question-1 (a)

html <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")

table_all<-html%>% html_table(fill=TRUE)
table<-table_all[[1]]
name<-table[[2]]
cmp<-table[[3]]
price_change<-table[[4]]
market_cap<-table[[5]]
week_high<-table[[6]]
week_low<-table[[7]]
roe<-table[[8]]
pe<-table[[9]]
pbv<-table[[10]]
ebitda<-table[[11]]
sales_gr<-table[[12]]
profit_gr<-table[[13]]

combined_table<- data.frame(name,
                            cmp,
                            price_change,
                            market_cap,
                            week_high,
                            week_low,
                            roe,
                            pe,
                            pbv,
                            ebitda,
                            sales_gr,
                            profit_gr)

combined_table


###########################################################

# Question-1 (b)


html <- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/automobiles/automobiles-passenger-cars/mahindra-mahindra/company-info")
table_all<-html%>% html_table(fill=TRUE)
table<-table_all[[1]]
table<-table[-2,]
table<-table[-2,]
table<-table[-2,]
table<-table[-2,]

terms<-table[[1]]
mar_13<-table[[2]]
mar_14<-table[[3]]
mar_15<-table[[4]]
mar_16<-table[[5]]
mar_17<-table[[6]]
mar_18<-table[[7]]
mar_19<-table[[8]]
mar_20<-table[[9]]
mar_21<-table[[10]]
mar_22<-table[[11]]

combined_table<- data.frame(terms,mar_13,mar_14,mar_15,mar_16,mar_17,mar_18
                            ,mar_19,mar_20,mar_21,mar_22)

table2<-table_all[[3]]

terms<-table2[[1]]

mar_13<-table2[[2]]
mar_14<-table2[[3]]
mar_15<-table2[[4]]
mar_16<-table2[[5]]
mar_17<-table2[[6]]
mar_18<-table2[[7]]
mar_19<-table2[[8]]
mar_20<-table2[[9]]
mar_21<-table2[[10]]
mar_22<-table2[[11]]

combined_table2<- data.frame(terms,mar_13,mar_14,mar_15,mar_16,mar_17,mar_18
                            ,mar_19,mar_20,mar_21,mar_22)

total_combined<-rbind(combined_table,combined_table2)

total_combined


#################################################################


# Question-1 (c)


my.tennis <- function(p)
{
  A <- 0
  B <- 0
  probab_A<-p
  probab_B<-1-p
  for(i in 1:5)
  {
    next_match <- sample(0:1, size = 1, prob = c(probab_A, probab_B))
    if(next_match == 0) B=1+B
    if(next_match == 1) A=1+A
    if(A == 3 || B == 3){
      x <- i 
      break
    }
  }  
  return(x)
}


matches <- numeric(length = 1000)
for(i in 1:1000)
{
  matches[i] <- my.tennis(0.70)
}

ans <- mean(matches)
ans


############################################################

# Question - 1 (d)

doors<-c(1,2,3)


reveal_door <- function(doors, prize, initial){
  if(prize == initial){
    reveal <- sample(x = doors[-prize], size = 1)
  } else {
    reveal <- doors[-c(prize,initial)]
  }  
}

prize <- sample(doors,1)
initial<- 1
reveal <- reveal_door(doors, prize, initial)
final <- doors[-c(initial, reveal)]


win<- 0

for(i in 1:1000){
  prize <- sample(doors,1)
  initial<- 1
  reveal <- reveal_door(doors, prize, initial)
  final<- doors[-c(initial, reveal)]
  
  if(final==prize){
    win=win+1
  }
  
}

print(win/1000)


##############################################################

# Question - 1 (e)


html <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")

ranking <- html %>%  html_elements(".countdown-index") %>% html_text()
name <- html %>%  html_elements(".article_movie_title a") %>% html_text()
tomato_score <- html %>%  html_elements(".tMeterScore") %>% html_text()
year <- html %>%  html_elements(".subtle.start-year") %>% html_text()

table<- data.frame(ranking,name,tomato_score,year)
table

################################################################

