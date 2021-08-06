install.packages(c("rvest","XML","magrittr","wordcloud"))
install.packages(c("tm","RWeka","readr","tidytext",
                   "syuzhet","plotly","openNLP"))
install.packages("C:/Users/ASUS/Downloads/openNLPmodels.en_1.5-1.tar.gz", 
                 repos = NULL, type = "source")
library(rvest)
library(XML)
library(magrittr)
library(tm)
library(readr)
library(RWeka)
library(tidytext)
library(syuzhet)
library(plotly)
library(ggplot2)
library(openNLP)
library(wordcloud)
library(openNLPmodels.en)


# Amazon Reviews #############################
aurl <- "https://www.amazon.in/All-new-Echo-Dot-3rd-Gen/product-reviews/B07H6234HJ?pageNumber=1"
amazon_reviews <- NULL
for (i in 1:25){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))  # Use html()
  murl
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text(!is.null(rev))
  amazon_reviews <- c(amazon_reviews,rev)
  View (amazon_reviews)
}
write.table(amazon_reviews,"echodot.txt",row.names = F)


#Loading File and converting into corpus
file <- readLines(file.choose())
corp <- Corpus(VectorSource(file))


#Cleaning of Corpus
corp_clean <- tm_map(corp,(tolower))
corp_clean <- tm_map(corp_clean,removeNumbers)
stop <- c(stopwords("english"),"can","also","far","mini","example","the","in",
                    "out","true","thing","india","yet","biggest","thats",
                    "white","hey","got","etc","just","even","dot","echo")
corp_clean <- tm_map(corp_clean,removeWords,stop)
corp_clean <- tm_map(corp_clean,removePunctuation)    
corp_clean <- tm_map(corp_clean,stripWhitespace)
inspect(corp_clean)

#Making of Term Document Matrix
dtm <- TermDocumentMatrix(corp_clean,control = list(minWordLength=c(1,Inf)))
findFreqTerms(dtm,lowfreq = 2)

#Making of Bar Plot
termfre <- rowSums(as.matrix(dtm))
termfre <- subset(termfre,termfre>=80)
barplot(termfre,las=2,cex.axis =0.85,cex.names = 0.8, col = rainbow(20),
        main = "Maximum Used Terms",ylab = "Frequency")
 
#Making of Word Cloud 
m <- as.matrix(dtm)
wordFreq <- sort(rowSums(m),decreasing = T)        
wordcloud(words = names(wordFreq),freq =wordFreq,min.freq = 10,random.order = F,
          colors = rainbow(50))

#Positive and Negative Word Repository
pos <- scan(file.choose(),what = "character",comment.char = ";")
neg <- scan(file.choose(),what = "character",comment.char = ";")

#Making of Positive Word cloud
pos.matches <- match(names(wordFreq),pos)
pos.matches <- !is.na(pos.matches)
freq_pos <- wordFreq[pos.matches]
wordcloud(names(freq_pos),freq = wordFreq,min.freq = 5,colors = rainbow(40),
          random.order = F)

#Making of Negative Word cloud
neg.matches <- match(names(wordFreq),neg)
neg.matches <- !is.na(neg.matches)
freq_neg <- wordFreq[neg.matches]
wordcloud(names(freq_neg),freq = wordFreq,min.freq = ,random.order = F,
          colors = rainbow(20))

#Bigram
minfreq <- 2
tok_del <- " \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(corp_clean,Weka_control(min=2,max=2,delimiters=tok_del))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq,decreasing = T),]
wordcloud(sort_two$bitoken,sort_two$Freq,scale = c(2,0.35),
          colors = brewer.pal(8,"Dark2"),random.order = F,min.freq = minfreq,max.words = 150)

#Trigram
mintri <- 3
tritoken <- NGramTokenizer(corp_clean,Weka_control(min=3,max=3,delimiters=tok_del))
three_words <- data.frame(table(tritoken))
sort_three <- three_words[order(three_words$Freq,decreasing = T),]
wordcloud(sort_three$tritoken,sort_three$Freq,random.order = F, 
          scale = c(2,0.25),colors =brewer.pal(8,"Dark2"),min.freq = mintri)

#Sentiment Analysis or Emotion Mining
sv <- get_sentences(file)
nrc <- get_sentiment(sv,method = "nrc")
bing <- get_sentiment(sv,method="bing")
afinn <- get_sentiment(sv,method = "afinn")
syuzhet <- get_sentiment(sv,method = "syuzhet")
sentiment <- data.frame(nrc,bing,afinn,syuzhet)

#Most  Positive and  Negative Comment thorugh NRC Method
pos_nrc <- sv[which.max(nrc)]
pos_nrc                               #Positive Comment
neg_nrc <- sv[which.min(nrc)]         
neg_nrc                               #Negative Comment


#Most Positive  and  Negative Comment thorugh Bing Method
pos_bing <- sv[which.max(bing)]
pos_bing                              #Positive Comment
neg_bing <- sv[which.min(bing)]         
neg_bing                              #Negative Comment
                                           
#Most  Positive and  Negative Comment thorugh Afinn Method
pos_afinn <- sv[which.max(afinn)]         
pos_afinn                             #Positive Comment
neg_afinn <- sv[which.min(afinn)]         
neg_afinn                             #Negative Comment
                                       
#Most Positive and Negative Comment thorugh Syuzhet Method
pos_syuzhet <- sv[which.max(syuzhet)]  
pos_syuzhet                           #Positive Comment
neg_syuzhet <- sv[which.min(syuzhet)]         
neg_syuzhet                           #Negative Comment

#Experimenting with NRC Emotions
emotions <- get_nrc_sentiment(sv)
emo_bar <- colSums(emotions)
emo_bar
barplot(emo_bar,las=1,horiz=F,cex.names = 0.85, col = rainbow(30),
        main = "Emotions",ylab="Emotional Valence")


#Visualisation of Sentiments
plot(syuzhet[1:200],type = "h",main = "Syuzhet Sentiment",
     xlab = "Narrative Time",ylab = "Emotional Valence")
abline(h=0,col="red")

plot(nrc[1:200],type = "l",main = "NRC Sentiment",
     xlab = "Narrative Time",ylab = "Emotional Valence")
abline(h=0,col="red")

plot(bing[1:200],type = "l",main = "Bing Sentiment",
     xlab = "Narrative Time",ylab = "Emotional Valence")
abline(h=0,col="red")

plot(afinn[1:200],type = "l",main = "Afinn Sentiment",
     xlab = "Narrative Time",ylab = "Emotional Valence")
abline(h=0,col="red")


#Visualisation with Transformed values of sentiments
fit_nrc <- get_transformed_values(
          nrc,low_pass_size = 3,x_reverse_len = 100,
          scale_vals = FALSE,scale_range = TRUE)
plot(fit_nrc,type = "h",main = "Normalised NRC Sentiment",
     xlab = "Narrative Time",ylab = "Emotional Valence",
     col=brewer.pal(8,"Dark2") )
abline(h=0,col="red")


fit_bing <- get_transformed_values(
  bing,low_pass_size = 3,x_reverse_len = 100,
  scale_vals = FALSE,scale_range = TRUE)
plot(fit_bing,type = "h",main = "Normalised Bing Sentiment",
     xlab = "Narrative Time",ylab = "Emotional Valence",
     col=brewer.pal(8,"Dark2") )
abline(h=0,col="red")


fit_afinn <- get_transformed_values(
  afinn,low_pass_size = 3,x_reverse_len = 100,
  scale_vals = FALSE,scale_range = TRUE)
plot(fit_afinn,type = "h",main = "Normalised Afinn Sentiment",
     xlab = "Narrative Time",ylab = "Emotional Valence",
     col=brewer.pal(8,"Dark2") )
abline(h=0,col="red")


fit_syuzhet <- get_transformed_values(
  syuzhet,low_pass_size = 3,x_reverse_len = 100,
  scale_vals = FALSE,scale_range = TRUE)
plot(fit_syuzhet,type = "h",main = "Normalised Syuzhet Sentiment",
     xlab = "Narrative Time",ylab = "Emotional Valence",
     col=brewer.pal(8,"Dark2") )
abline(h=0,col="red")


