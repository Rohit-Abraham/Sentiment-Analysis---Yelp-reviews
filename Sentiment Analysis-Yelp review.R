library   ('tidyverse') #  data manipulation and graphs
library   ('stringr') #  string manipulation
library   ('lubridate') #  date manipulation
library   ('wordcloud') #  wordcloud
library   ('tidytext') # tidy implementation of NLP methods
library   ('DT')       # table format display of data
library   ('igraph') #  graphs
library   ('ggraph') #  graphs

library   ('topicmodels') # for LDA topic modelling 
library   ('tm') # general text mining functions, making document term matrixes
library   ('SnowballC') # for stemming
library   ('GGally')
library   ('textcat')
library   ('wordcloud')
library   ('text2vec')
library   ("data.table")
library ('SentimentAnalysis')

text_data=read.csv(file.choose(),header=1,sep=";")
d_use=text_data
View(d_use)
glimpse(text_data)


#Plotting graph of stars with number of ratings
d_use %>%
  group_by(stars) %>%
  summarise(Count = n()) %>%
  #arrange(desc(Count)) %>%
  #ungroup() %>%
  #mutate(Stars = reorder(stars,Count)) %>%
  #head(10)%>%
  
  ggplot(aes(x = stars,y = Count)) +
  geom_bar(stat='identity',colour="white", fill ="light blue") +
  geom_text(aes(x = stars, y = 1, label = paste0("(",round(Count/1e3)," K )",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Stars', y = 'Count', 
       title = 'Stars Count') +
   theme_bw()

d_lang=as.data.frame(d_use$text)
d_lang$id=d_use$business_id

View(d_use)

#Creating a new variable, assigning rating- 4-5- Good (1), 3- Neutral(0), 1-2- Bad(-1)
d_use$Rating=lapply(d_use$stars,function(stars){ifelse(stars>4,1,ifelse(stars<3,-1,0))})

#creating new dataframe
df=d_use %>%
  select(useful, cool, funny,  stars)
summary(df)

df <- as.data.frame(lapply(df, unlist))
par(mfrow=c(1,3))

for(i in 1:3){
plot(df[,i],as.numeric(df[,4]),xlim = c(0,80),ylim=c(0,5),
     col.axis = "blue",xlab=colnames(df)[i],font=10,ylab=colnames(df)[4],
       col="red",cex.lab=1.8,cex.axis=1.4)
  title(main = "Relationship with Rating",cex.main=2,font.main=4,col.main="blue")
 
}

#making a dataframe with restaurants having high ratings , mean rating 3.75
b1 <- d_use %>% 
  group_by(text) %>% 
  #summarize(m=mean(stars)) %>% 
  filter(stars > 3.75) %>% 
   arrange(desc(stars))


b1$text=as.character(b1$text)
#checking language
b1$lang=textcat(b1$text)
#write.csv(b1,"modified positive data.csv")
#taking only english language, removing german and french
lang_ind_german=which(b1$lang=='german')
lang_ind_french=which(b1$lang=='french')
data_positive=b1[-lang_ind_german,]
data_positive=data_positive[-lang_ind_french,]
data_positive$text=as.character(data_positive$text)
# data_positive$id=matrix(1:nrow(data_positive)) #addding ID
#token_positive = itoken(data_positive$text, 
      #                  preprocessor = prep_fun, 
  #                      tokenizer = tok_fun, 
       #                 ids = data_positive$X, 
      #                  progressbar = FALSE)
#
#vocab_positive = create_vocabulary(token_positive)
#View(vocab_positive)
#?stop
data_positive$text = tolower(data_positive$text) #make it lower case
data_positive$text = gsub('[[:punct:]]', '', data_positive$text) #remove punctuation
#pruned_vocab = prune_vocabulary(vocab_positive, 
                          #      term_count_min = 10, 
                           #     doc_proportion_max = 0.5,
                           #     doc_proportion_min = 0.001)

positive_Corpus <- Corpus(VectorSource(data_positive$text)) 
View(DTM_positive)
DTM_positive <- DocumentTermMatrix(positive_Corpus)
# convert the document term matrix to a tidytext corpus
DTM_positive_tidy <- tidy(DTM_positive)

DTM_positive_tidy_cleaned <- DTM_positive_tidy %>% # take our tidy dtm and...
  anti_join(stop_words, by = c("term" = "word")) # remove English stopwords 

wordcloud (DTM_positive_tidy_cleaned$term, DTM_positive_tidy_cleaned$count, 
           random.order=FALSE, colors=brewer.pal(8, "Dark2"))



#Similarly for negative words
#making a dataframe with restaurants having high ratings , mean rating 3.75
b2 <- d_use %>% 
  group_by(text) %>% 
 # summarize(m=mean(stars)) %>% 
  filter(stars < 3.75) %>% 
  arrange(stars)
#b2=read.csv(file.choose(),header = 1)
b2$text=as.character(b2$text)
#View(b2)
#checking language
#b2$lang=textcat(b2$text)
#write.csv(b1,"modified positive data.csv")
#taking only english language, removing german and french
lang_ind_german=which(b2$lang=='german')
lang_ind_french=which(b2$lang=='french')
data_negative=b2[-lang_ind_german,]
data_negative=data_negative[-lang_ind_french,]
data_negative$text=as.character(data_negative$text)
# data_positive$id=matrix(1:nrow(data_positive)) #addding ID
#token_positive = itoken(data_positive$text, 
#                  preprocessor = prep_fun, 
#                      tokenizer = tok_fun, 
#                 ids = data_positive$X, 
#                  progressbar = FALSE)
#
#vocab_positive = create_vocabulary(token_positive)
#View(vocab_positive)
#?stop
data_negative$text = tolower(data_negative$text) #make it lower case
data_negative$text = gsub('[[:punct:]]', '', data_negative$text) #remove punctuation

##########################################################################################################
########################################################################################################
#Making 3 as neutral



#Similarly for negative words
#making a dataframe with restaurants having high ratings , mean rating 3.75
View(b2)
b2 <- b2 %>% 
  group_by(text) %>% 
  # summarize(m=mean(stars)) %>% 
  filter(m < 2.75) %>% 
  arrange(m)
#b2=read.csv(file.choose(),header = 1)
b2$text=as.character(b2$text)
View(b2)
#checking language
b2$lang=textcat(b2$text)
#write.csv(b1,"modified positive data.csv")
#taking only english language, removing german and french
lang_ind_german=which(b2$lang=='german')
lang_ind_french=which(b2$lang=='french')
data_negative=b2[-lang_ind_german,]
data_negative=data_negative[-lang_ind_french,]
data_negative$text=as.character(data_negative$text)
# data_positive$id=matrix(1:nrow(data_positive)) #addding ID
#token_positive = itoken(data_positive$text, 
#                  preprocessor = prep_fun, 
#                      tokenizer = tok_fun, 
#                 ids = data_positive$X, 
#                  progressbar = FALSE)
#
#vocab_positive = create_vocabulary(token_positive)
#View(vocab_positive)
#?stop
data_negative$text = tolower(data_negative$text) #make it lower case
data_negative$text = gsub('[[:punct:]]', '', data_negative$text) 

###################################################################################
negative_Corpus <- Corpus(VectorSource(data_negative$text)) 
DTM_negative <- DocumentTermMatrix(negative_Corpus)

# convert the document term matrix to a tidytext corpus
DTM_negative_tidy <- tidy(DTM_negative)

DTM_negative_tidy_cleaned <- DTM_negative_tidy %>% # take our tidy dtm 
  anti_join(stop_words, by = c("term" = "word")) # remove English stopwords 
 #anti_join(custom_stop_words, by = c("term" = "word"))
wordcloud (DTM_negative_tidy_cleaned$term, DTM_negative_tidy_cleaned$count, 
           random.order=FALSE, colors=brewer.pal(8, "Dark2"))

View(DTM_negative_tidy)

#Since many words of food items are similar in both, removing common food items

custom_stop_words <- tibble(word = c("dish","meat","chicken","fish","pizza","burger"
                                     ,'rice','food',"pho","steak",'sushi','pork','cake','donuts','dog','restaurant'
                                     ,'beef','bradsticks','eggs','waffles','rice','cheese','potato','soup','noodles'
                                     ,'doughnuts','pretzel','menu','lunch','dinner','breakfast','noodle','rolls','cookies','chai','bun'
                                     ,'dogs','egg','fries','sandwich','taco','brunch','burritoes','salad'
                                     ,'curry','sauce','dumplings','pita','tea','coffee','cream','chocolate','teriyaki','pasta','tacos'
                                     ,'japanese','indian','chinese','thai','italian','stew','wings','fried','clam','filet','dumplings','roll'
                                     ,'wine','beer','congee','shop','diner','toronto','sticks','crab','greek','salad','burgers','ice','donut'
                                     ,'table','tuna','cuban','waffle','smoothie','enchilada','donuts','piccata','eel','bread','bacon','spinach',
                                     'vegan','korean','hummus','clams','detroit','oysters','mexican','candy','wing','burrito','scottish','mitzels',
                                     'clams','pie','potatoes','bbq','hash','breadsticks','chicago','dishes','shabu','bobbie','hibachi','matts','wildflower',
                                     'buffet','drinks','duck','beans','pub','brewery','tokantsu','tables','table','bar','phuong','turkish','hookah','meal',
                                     'foybread','cocktail','shinobu','spanish','furikake','salmon','steaks','ribs','pancake','gluten','bagels','dishes','filipino','vegas','mitzies','ramen','shrimp','pies','bowl'))

DTM_positive_tidy_cleaned <- DTM_positive_tidy %>% # take our tidy dtm and...
  anti_join(stop_words, by = c("term" = "word")) %>%
  anti_join(custom_stop_words, by = c("term" = "word"))# remove English stopwords 
wordcloud (DTM_positive_tidy_cleaned$term, DTM_positive_tidy_cleaned$count, 
           random.order=FALSE, colors=brewer.pal(8, "Dark2"))

View(DTM_negative_tidy_cleaned)
DTM_negative_tidy_cleaned <- DTM_negative_tidy %>% # take our tidy dtm 
  anti_join(stop_words, by = c("term" = "word"))%>% # remove English stopwords 
  anti_join(custom_stop_words, by = c("term" = "word"))
wordcloud (DTM_negative_tidy_cleaned$term, DTM_negative_tidy_cleaned$count, 
           random.order=FALSE, colors=brewer.pal(8, "Dark2"))

str(DTM_negative_tidy_cleaned)
# Uploading Harvard dictionary


positive_corpus <- VCorpus(VectorSource(data_positive$text))
View(positive_corpus)
positive_corpus
tdm <- TermDocumentMatrix(positive_corpus, 
                          control=list(wordLengths=c(1,Inf), 
                                       tokenize=function(x) ngram_tokenize(x, char=FALSE,ngmin=1, ngmax=2)))
dim(tdm)
#creating one column in both data for creating dictionary
data_positive$rating=1
data_negative$rating=-1
#Removing stop words
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')

document_pos = stringr::str_replace_all(data_positive$text, stopwords_regex, '')
document_neg =stringr::str_replace_all(data_negative$text, stopwords_regex, '')
data_positive$text=document_pos
data_negative$text=document_neg

#MAking dictionary from the data 
document=c(data_positive$text,data_negative$text)
star=c(data_positive$rating,data_negative$rating)
dict_original_data <- generateDictionary(document, star)
summary(dict_original_data)

dict_original_data
View(words)

sentiment <- predict(dict_original_data, document)
compareToResponse(sentiment, star)




#creating dictionary from Harvard data




doc_har_positive=read.delim(file.choose(),header= 0)
doc_har_negative=read.delim(file.choose(),header = 0)

doc_har_negative$V1=tolower(doc_har_negative$V1)
doc_har_positive$V1=tolower(doc_har_positive$V1)
doc_har_positive$rank=1
doc_har_negative$rank=-1 
doc_har_pos=c(doc_har_positive$V1)
doc_har_neg=c(doc_har_negative$V1)
doc=c(doc_har_positive$V1,doc_har_negative$V1)
rank=c(doc_har_positive$rank,doc_har_negative$rank)

dict_har=

View(doc_har_negative)

#document_har=c(doc_har_positive$V1,doc_har_negative$V1)
#rating_har=c(doc_har_positive$rating,doc_har_negative$rating)

str(dict_harvard)

dict_harvard=SentimentDictionaryBinary(doc_har_pos,doc_har_neg)

View(dict_harvard)
View(data_positive)
#comparing Dictionary
c=compareDictionaries(dict_harvard,dict_original_data)
?SentimentAnalysis
sentiment <- analyzeSentiment(dict_harvard, document)
compareToResponse(sent, star)
?generateDictionary
#making dictionary for LIu

?Sentiment

doc_liu_positive=read.delim(file.choose(),header= 1)


doc_liu_negative=read.delim(file.choose(),header = 1)

doc_liu_positive$X............................................................................=as.character(doc_liu_positive$X............................................................................)
doc_liu_negative$X..............................................................................=as.character(doc_liu_negative$X..............................................................................)

doc_liu_pos=c(doc_liu_positive$X............................................................................)
doc_liu_neg=c(doc_liu_negative$X..............................................................................)


View(dict_liu)
#document_har=c(doc_har_positive$V1,doc_har_negative$V1)
#rating_har=c(doc_har_positive$rating,doc_har_negative$rating)

dict_liu=SentimentDictionaryBinary(doc_liu_pos[-(1:33)],doc_liu_neg[-(1:33)])

#comparing Dictionary
compareDictionaries(dict_liu,dict_harvard)




#AFINN Dictionary
afinn=read.delim(file.choose(),header= 0)
  str(afinn)
afinn$V1=as.character(afinn$V1)
#adding a new column- for bringing the scor eon same level with that of data
afinn$rank=lapply(afinn$V2,function(x){
                            ifelse(x<(-3),1,ifelse(x<(-1),2,ifelse(x<2,3,ifelse(x<4,4,5))))})


mat1 <- afinn %>% 
  filter(rank == 1) 
mat2=afinn %>% 
  filter(rank == 2) 
mat3=afinn %>% 
  filter(rank == 3) 
mat4=afinn %>% 
  filter(rank == 4) 
mat5=afinn %>% 
  filter(rank == 5) 

View(mat1)
str(mat1)
var_desc <- c(mat1$V1,mat2$V1,mat3$V1,mat4$V1,mat5$V1)

var_type <- c(mat1$V2,mat2$V2,mat3$V2,mat4$V2,mat5$V2)

dict_afinn=SentimentDictionaryWeighted(var_desc,var_type)




#checking accuracy through various dictionaries
s=0
n=0
for(i  in 30328:40527){
sentimen <- analyzeSentiment(document[i],
                              rules=list("SentimentLM"=list(ruleSentiment, dict_liu)))
if(!is.na(sentimen)){
if(sentimen>0){
 s=s+1
}}
if(is.na(sentimen)){
  n=n+1}
#sentiment=matrix(1:40527,nrow = 40527,ncol=1)

}
?analyzeSentiment

plot(sent)
str(document)
View(document)
sent <- predict(dict_afinn, document)
compareToResponse(sent, star)

summary(sent)
star[1:30327]=1
star[30328:48056]=-1
View(data_positive$rating)
compareToResponse(sent, star)


#Testing with Harvard dictionary
sentiment <- predict(dict_harvard, document)
compareToResponse(sentiment, star)
sent=as.data.frame(sent)
View(sent)

#qplot(,sent[,1])+geom_smooth()



##############################################################################################################
#########################################################################################################3#

#combining poitive and negative reviews both
#For the model training purpose, taking rating 3 also

word_Corpus <- Corpus(VectorSource(data_use$text)) 
str(word_Corpus)
DTM_whole <- DocumentTermMatrix(word_Corpus)

# convert the document term matrix to a tidytext corpus
str(DTM_whole)
DTM_whole_tidy <- tidy(DTM_whole)
str(DTM_whole_tidy)
DTM_positive_tidy_cleaned <- DTM_positive_tidy %>% # take our tidy dtm and...
  anti_join(stop_words, by = c("term" = "word")) # remove English stopwords 
View(DTM_whole_tidy)

u=data.frame(unique(DTM_whole_tidy$term))
colnames(u)=c("term")
Dict_har=data.frame(1:3642,nrow=3642)
Dict_har$term=c(dict_harvard$positiveWords,dict_harvard$negativeWords)
har=inner_join(u,Dict_har)                     #no of words matching with Harvard
dict_LIU=data.frame(1:6787,nrow=6787)
dict_LIU$term=c(dict_liu$positiveWords,dict_liu$negativeWords)
LIU=inner_join(u,dict_LIU)                    #no of words matching with LIU
dict_AFINN=data.frame(1:4954,nrow=4954)
dict_AFINN$term=c(dict_afinn$words,dict_afinn$words)
AFINN=inner_join(u,dict_AFINN)                  #no of words matching with AFINN

library ('quanteda')





str(dtm)
str(AFINN)
?dfm
 
liu
str(dict_liu)
View(data_use)

View(har)
str(har)

View(Dict_har)

get_sentiments("afinn")













View(data_use)

data_use=rbind.data.frame(data_positive,data_negative)
data_use$lang=NULL
View(data_use)

set.seed(11790)
all_ids = data_use$X
sample_1 = sample(all_ids, 10000) #taking sample of 10000 from the data
data_sample1=data_use[sample_1,]
#train_ids_1=sample(sample_1,8000)
#test_ids_1=setdiff(sample_1,train_ids_1)


#model training
#library(glmnet)
NFOLDS = 4


for ( i in 1:nrow(data_use)){
  data_use[i,5]=ifelse(data_use[i,3]>3,1,-1)
}
str(data_use$V5)
View(data_use)
data_use$V5=as.factor(data_use$V5)

set.seed(11790)
ind=sample(nrow(data_use),0.7*nrow(data_use))
data_train=data_use[ind,]
data_test=data_use[-ind,]


prep_fun = tolower
tok_fun = word_tokenizer
train_tokens = data_train$text %>% 
  prep_fun %>% 
  tok_fun

it_train = itoken(train_tokens, 
                  ids = data_train$X)


vocab = create_vocabulary(it_train)

#Dict=dictionary(list(positive=c(dict_harvard$positiveWords,dict_liu$positiveWords),
# negative=c(dict_harvard$negativeWords,dict_liu$negativeWords)))


pruned_vocab = prune_vocabulary(vocab, 
                                term_count_min = 10, 
                                doc_proportion_max = 0.7,
                                doc_proportion_min = 0.03)
vectorizer = vocab_vectorizer(pruned_vocab)


dtm_train = create_dtm(it_train, vectorizer)
View(data_train)

glmnet_classifier = cv.glmnet(x = dtm_train, y = data_train[['V5']], 
                              family = 'binomial', 
                              # L1 penalty
                              alpha = 1,
                              # interested in the area under ROC curve
                              type.measure = "auc",
                              # 5-fold cross-validation
                              nfolds = NFOLDS,
                              # high value is less accurate, but has faster training
                              thresh = 1e-3,
                              # again lower number of iterations for faster training
                              maxit = 1e3)


plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))
summary(glmnet_classifier)
View(glmnet_classifier)
#TEsting on test data

# Note that most text2vec functions are pipe friendly!
it_test = data_test$text %>% 
  prep_fun %>% tok_fun %>% 
  itoken(ids = data_test$X)


dtm_test = create_dtm(it_test, vectorizer)

preds = predict(glmnet_classifier, dtm_test, type = 'response')[,1]

summary(preds)
for( i in 1:nrow(data_test)){
  data_test[i,6]=ifelse(data_test[i,5]==1,1,0)
}
glmnet:::auc(data_test$V6, preds)


library('e1071')

svm_classifier=svm(x=dtm_train,y=data_train$V5,kernel='linear')
pred_svm=predict(svm_classifier,dtm_train)
pred_svm_test=predict(svm_classifier,dtm_test)
View(data_test)
table(pred_svm,data_train$V5)
mean(pred_svm==data_train$V5)
install.packages('doMc')
library('naivebayes')
?naive_bayes

df <- as.data.frame(as.matrix(dtm_train))
df_test=as.data.frame(as.matrix(dtm_test))
head(df_test)
naive_model=naiveBayes(df,data_train$V5)
pred_naive=predict(naive_model,df_test)
str(pred_naive)
summary(naive_model)
summary(pred_naive)
table(pred_naive,data_test$V5)
mean(pred_naive==data_test$V5)
plot(pred_naive)

summary(naive_model)
as.numeric(data_train$V5)
data_train$V5=as.numeric(data_train$V5)
str(dtm_train)

plot(svm_classifier,data=data_,data_train$V5~data_train$)

rf_classifier = randomForest(x = dtm_train, y = data_train[['V5']], ntree = 100)
                             
str(dtm_train)


?randomForest




















# DT matrix
?vocab_vectorizer
vectorizer = vocab_vectorizer(Dict)

dtm_train = create_dtm(it_train_dict, vectorizer)









dtm=dfm(data_use$text)
dtm_sample1=dfm(data_sample1$text)
train_dfm_1=dfm_sample(dtm_sample1,size=8000)
test_dfm_1=dtm[setdiff()]
dtm_sample1=dfm()
dict_dtm=dfm_lookup(dtm,Dict)
head(dict_dtm)



#str(test1)
#str(sample_1)
#str(train1)
#View(train1)

train1 = data_sample1[train_ids_1,]
test1 = data_sample1[test_ids_1,]

prep_fun = tolower
tok_fun = word_tokenizer

#Making a combined dictionary from Harvard and Liu 

Dict_comb$positiveWords=rbind(dict_harvard$positiveWords,dict_liu$positiveWords)
Dict_comb$negativeWords=c(dict_harvard$negativeWords,dict_liu$negativeWords)


View(train_tokens)

it_train = itoken(train1$text, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = train1$X, 
                  progressbar = FALSE)

vocab = Dict_comb
str(dict_afinn)
Dict_comb<- as.data.frame(Dict_comb)
dict=data.frame(1:10429,nrow)
dict$positiveWords=Dict_comb$positiveWords
dict$negativeWords=Dict_comb$negativeWords
str(dict_harvard)
dict$
vectorizer = vocab_vectorizer(vocab)
dtm_train_1 = create_dtm(it_train, vectorizer)



it_train = itoken(train1$review, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = train$id, 
                  progressbar = FALSE)
vocab = create_vocabulary(it_train)







it_train = itoken(positive_tokens, 
                  ids = data_positive$X,
                  # turn off progressbar because it won't look nice in rmd
                  progressbar = FALSE)

vocab = create_vocabulary(it_train)
View(vocab)





token_positive = itoken(data_positive$text, 
                  preprocessor = prep_fun, 
                      tokenizer = tok_fun, 
                 ids = data_positive$X, 
                  progressbar = FALSE)

vectorizer = vocab_vectorizer(Har_pos_dict)
t1 = Sys.time()
dtm_train = create_dtm(token_positive, vectorizer)
print(difftime(Sys.time(), t1, units = 'sec'))
View(dtm_train)





View(d_use)

b1 %>% ggplot(aes(business_id,s))+geom_col(aes(fill=s))
+ coord_flip() + labs(fill= 'Mean Rating') + ylab('Highest Reviewed')

?unique


str(d_use)  
d=d_use %>% group_by(business_id)

u=as.data.frame(unique(d_use$business_id))

d[,20]=d_use[,1]
View(d_use)
for(i in 1:nrow(u)){
  for(j in 1:nrow(d_use)){
    if(length(levels(d[j,1]))==length(levels(u[i,1])))
      d_use[j,20]=i
    else
      i=i+1
  }
}
write.csv(d_use,'D M4.csv')

d_use <- apply(d_use,2,as.character)

?levels

View(u)

for( i in 1:nrow(d_use)){
  for (level in unique(d_use[, 1])){
    
      as.numeric(ifelse(data_svm[, i] == level, 1, -1))
  }



str(u)
for( i in 1:nrow(d_use)){
  if(!(u1==u))
    j=j+1
  d_use
  u=d_use[i,1]
  
}



  
  
  