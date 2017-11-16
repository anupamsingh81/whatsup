library(tidyverse)
history= "whatsup.txt"
chat <- readr::read_lines(history, locale = readr::locale(encoding = "UTF-8"))


chat[3]
library(lubridate)


# time extraction

timex= function(x){
  
  raw.time  <- stringr::str_split_fixed(x, "- ", n=2)[1]
  
  mdy_hm(raw.time)}


# content extraction
content = function(x){
  raw.data1 <- stringr::str_split_fixed(x, ": ", n=2)[2]
  
  if(raw.data1=="")(raw.data1=x)
  raw.data1}

# author extraction

authorize= function(x){
  gsub(".*- |:.*", "", x)
}

moment = map(chat,timex)

time = as.data.frame(moment) %>% gather()

summary(as.factor(time$value))

time= time %>%select(value) %>% rename(time=value)


time$day =wday(time$time,label = TRUE)

time$month =month(time$time,label = TRUE)
time$year = year(time$time)
?mon

time$hour = hour(time$time)

time$content= map(chat,content)

time$ym = paste(time$month,time$year,sep="")

time= time %>% mutate( author = map_chr(chat,authorize)) %>% mutate( author = case_when(
  is.na(day)~NA_character_,
  TRUE~author
  
)) # in case all RHS of same type, is.na better substitute instead of day==NA




# hour_wise plot
time %>% filter(time!=is.na(time)) %>% select(author,hour) %>% filter(author=="Satish"|author=="anupam singh") %>%group_by(author,hour) %>% count() %>%  ggplot(aes(x=hour,y=n,group=author,shape=author,colour=author))+geom_line()

time %>% filter(time!=is.na(time)) %>% mutate(authlength= str_count(author)) %>% filter(authlength<15,authlength>=6) %>% select(author,hour) %>%group_by(author,hour) %>% count() %>%  ggplot(aes(x=hour,y=n,group=author,shape=author,colour=author))+geom_line()

time %>%filter(time!=is.na(time)) %>%  pull(author) %>% mutate(author=as.factor(author)) %>% count()

f= time %>%filter(time!=is.na(time)) %>%  count(author) %>% top_n(15) %>% pull(author)  # select top 15 author n plot
f[7]<- NULL

f <- f[! f %in% c("http")] # remove http , # https://stackoverflow.com/questions/9665984/how-to-delete-multiple-values-from-a-vector
f
time %>% filter(time!=is.na(time)) %>% select(author,hour) %>%filter(author%in% f) %>% group_by(author,hour) %>% count() %>%  ggplot(aes(x=hour,y=n,group=author,shape=author,colour=author))+geom_line()

author[812]

library(stringr)

 
 
 
 time$author[1] = NA # set first to NA
 
 
patte= str_c("www","http",sep = "|")
summary(str_detect(time$content,patte))
str_detect(content,patte)
time$links = case_when(str_detect(time$content,patte)==TRUE~1,
TRUE~0) # never plain = instead of ==

time$media = case_when(str_detect(time$content,"Media ")==TRUE~1,
                       TRUE~0) # never plain = instead of ==

summary(as.factor(time$links))



summary(str_detect(time$content,"Media "))
summary(str_detect(time$content,"pdf"))


patter = ".*- | :*"
str_replace(chat[2],patter, "")

?str_replace




time %>% filter(time!=is.na(time)) %>% ggplot(aes(x=day))+geom_bar()

time %>% filter(time!=is.na(time)) %>% ggplot(aes(x=month))+geom_bar()

time$ym = paste(time$month,time$year,sep="")

time %>% filter(time!=is.na(time)) %>% ggplot(aes(x=ym))+geom_bar() +coord_flip()
# for order of appaearance

time %>% filter(time!=is.na(time)) %>% ggplot(aes(x=factor(ym,levels=unique(ym))))+geom_bar(fill="steel blue")+coord_flip() + labs(x= "month", y= "messages")



time %>% select(month,year,day) %>% group_by(month,year) %>% count()
timex(chat[4])

time$ym

chat[812]
summary()


time %>% filter(author=="Satish"|author=="anupam singh") %>%group_by(author,month) %>% count() %>%  ggplot(aes(x=month,y=n,fill=author))+geom_bar(stat="identity")

# bar plot
time %>% filter(author%in% f)  %>%group_by(author,month) %>% count() %>%  ggplot(aes(x=month,y=log(n),group=author,shape=author,colour=author))+geom_line()

# line plot
time %>% filter(author=="Satish"|author=="anupam singh") %>%group_by(author,month) %>% count() %>%  ggplot(aes(x=month,y=n,group=author,shape=author,colour=author))+geom_line()

# line plot hack
time %>% mutate(authlength= str_count(author)) %>% filter(authlength<15,authlength>=6) %>%group_by(author,month) %>% count() %>%  ggplot(aes(x=month,y=n,group=author,shape=author,colour=author))+geom_line()


str_count(time$author[12])

library(zoo)

replace(a, !is.na(a), tapply(b, zoo::na.locf(a), paste, collapse = " "))

t1= tapply(as.character(time$content),zoo::na.locf(time$day),paste,collapse=" ")
str(t1)
str(t1)

a1 = time %>% select(day,content)


rm(content1)
output= c("a","b c d",NA,NA,"e f g",NA,NA,"h")

na.locf(a)

rm(t1)

satish= time

a <- as.character(satish$day)
b= as.character(satish$content)

a= as.character(satish$day)
b = as.character(satish$content)
x = as.character(satish$day)
x[!is.na(a)] <- tapply(b, cumsum(!is.na(a)), paste, collapse=" ")

satish$content1= x
library(tidyverse)
satish = satish %>% filter(!(is.na(satish$time)))
library(tidytext)

satish1 = satish %>% select(time,author,content1) %>% mutate(linenumber=row_number()) %>% rename(text=content1)

tidy_books <- satish1 %>%
  unnest_tokens(word, text)

data("stop_words")
cleaned_books <- tidy_books %>%
  anti_join(stop_words)

cleaned_books %>%
  count(word, sort = TRUE) %>% top_n(n,n=50)

nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

tidy_books %>%
  filter(author == "Satish") %>%
  semi_join(nrcjoy) %>%
  count(word, sort = TRUE)
library(wordcloud)

# general word cloud

cleaned_books %>% filter(!(word=="media"|word=="omitted"|word=="http"|word=="https")) %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# satish

cleaned_books %>% filter(!(word=="media"|word=="omitted"|word=="http"|word=="https"|word=="html"|word=="www"|word==".com")) %>% filter(author=="Satish") %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


# anupam

cleaned_books %>% filter(!(word=="media"|word=="omitted"|word=="http"|word=="https"|word=="html"|word=="www"|word==".com")) %>% filter(author=="anupam singh") %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


library(tidyr)
bing <- get_sentiments("bing")

janeaustensentiment <- tidy_books %>%
  inner_join(bing) %>%
  count(time,author, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

janeaustensentiment %>% filter(author=="Satish"|author=="anupam singh") %>% ggplot( aes(index, sentiment, fill = author)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~author, ncol = 2, scales = "free_x")

library(lubridate)
  
janeaustensentiment= janeaustensentiment %>% mutate(month= month(time,label=TRUE))

janeaustensentiment %>% filter(author=="Satish"|author=="anupam singh") %>% ggplot( aes(month, sentiment, fill = author)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~author, ncol = 2, scales = "free_x")
