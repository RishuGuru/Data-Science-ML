#####################################  Solution of Q2 ##########################

install.packages("rvest")
install.packages("XML")
install.packages("magritter")

library(rvest)
library(XML)
library(magrittr)


### wonder women 1984 (2020) review 
idmb = "https://www.imdb.com/title/tt7126948/reviews?ref_=tt_ql_3"

wonder_women <- NULL


for (i in 1:30){
  idmburl <- read_html(as.character(paste(idmb,i,sep="=")))
  rev <- idmburl %>% html_nodes(".text") %>% html_text()
  wonder_women <- c(wonder_women,rev)
}

View(wonder_women)

write.table(wonder_women,"E:/Data Science Completed Assignmennt/NLP text mining/wonder women 1984(2020) reivew.txt")  # saving the review
getwd()



# Performinng sentimental analysis

WWR_review = wonder_women

#installing term matrix package

install.packages("tm")
library(tm)


#convert charecter data in to corpus type

r = Corpus(VectorSource(WWR_review))
inspect(r[1])

#now performing data cleansing

r3 = tm_map(r, tolower)  # coverting into lower case
r3 = tm_map(r3, removePunctuation)   # removing puctuation 
r3 =  tm_map(r3, removeNumbers)   #removing numbers
r3 = tm_map(r3, removeWords,stopwords("english"))     #removing stopwords

a = c("movies","movie","wonder","women","film","ive","just","also","going", "coming"," however","back","just","will","using","back"," minutes","get")
r3 = tm_map(r3, removeWords, a)

inspect(r3[1:10])

# Striping white space
r3 = tm_map(r3, stripWhitespace)        

inspect(r3[1:2])

# converting into TermDocumentMatrix

tdm = TermDocumentMatrix(r3)

# To remove sparse entries upon a specific value
corpus.dtm.frequent <- removeSparseTerms(tdm, 0.99) 

tdm = as.matrix(tdm)    # Coverting into matrix
dim(tdm)
View(tdm)
x = rowSums(tdm)   #adding all the row value

View(x)

x_sub = subset(x, x>100) #subsetting those which have more than 100 frequency
View(x_sub)

barplot(x_sub, las=2, col = rainbow(30))   #barplotting of the data

x_sub


#for better visulization doing word cloud
 
#installing word cloud

install.packages("wordcloud")
library(wordcloud)

x_sub0 = sort(x, decreasing = T )
View(x_sub0)

#for better visulization
#Unigram

windows()
?wordcloud
wordcloud(words = names(x_sub0),freq = x_sub0,max.words = 300, random.order = T, colors = rainbow(15), scale = c(2,0.5), rot.per = 0.4 )

#Bigram

library(RWeka)

minfreq = 2
bitoken = NGramTokenizer(r3, Weka_control(min =2 ,max=2)) 
two_word = data.frame(table(bitoken))
sort_two = two_word[order(two_word$Freq, decreasing = TRUE),]

windows()
wordcloud(words = sort_two$bitoken, freq = sort_two$Freq, random.order = F, scale = c(2, 0.35), min.freq = minfreq, colors = brewer.pal(8, "Dark2"), max.words = 150)

############ Positive word And Negative Word from word cloud

positive_word = readLines(file.choose())  #loading Positive word
negative_word = readLines(file.choose())   #loading Negative word

# positive word cloud

pos_matches = match(names(x_sub0),positive_word)
pos_matches = !is.na(pos_matches)
freq_pos = x_sub0[pos_matches]

names0 = names(freq_pos)

windows()
wordcloud(names0, freq_pos, scale=c(4,.5), colors = brewer.pal(8, "Dark2"))

# Negative word cloud

neg_matches = match(names(x_sub0),negative_word)
neg_matches = !is.na(neg_matches)
freq_neg = x_sub0[neg_matches]
names1 = names(freq_neg)

windows()
wordcloud(words = names1,freq_neg, scale=c(5,0.5), max.words=50, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
?wordcloud

#comparing positive word cloud and negative word cloud


write.table(names0,"E:/Data Science Completed Assignmennt/NLP text mining/Task2 pos_neg/pos_match.txt")
write.table(names1,"E:/Data Science Completed Assignmennt/NLP text mining/Task2 pos_neg/neg_match.txt")

data <- Corpus(DirSource("E:/Data Science Completed Assignmennt/NLP text mining/Task2 pos_neg/"))

data = tm_map(data, removeNumbers)
data = tm_map(data, removePunctuation)

data = TermDocumentMatrix(data)
data = as.matrix(data)
View(data)
dim(data)
colnames(data) = c("Neg_word","Pos_word")


windows()
comparison.cloud(data, max.words = 300, title.size = 2,
                 colors = brewer.pal(4,"Set1"))
#########################################################################################################


