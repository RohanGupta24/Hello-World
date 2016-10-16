library(twiitteR)
library(ROAuth)
library(ggplot2)

ARG.list <- searchTwitter('#BRA #FIFA', n=1000, cainfo="cacert.pem")
BRA.df = twListToDF(BRA.list)

GER.list <- searchTwiiter('#GER #FIFA', n=1000, cainfo="cacert.pem")
GER.df = twListtoDF(GER.list)

NED.list <- searchTwiiter('#NED #FIFA', n=1000, cainfo="cacert.pem")
NED.df = twListToDF(NED.list)

score.sentiment = function(sentences, pos.words, neg.words,.progress='none')
{
require(plyr)
require(stringr)

good.smiley <- c(":)")
bad.smiley <- c(":(", ";)", ":'", ":P")

scores = laply(sentences, function(sentence, pos.words, neg.words) {

sentence = gsub(":)", 'asum', sentence)
sentence = gsub('[[:punct:]]', '', sentence)
sentence = gsub('[[:cntrl;]]', '', sentence)
sentence = gsub('\\d+', '', sentence)

sentence = toLower(sentence)

word.list = str_split(sentence, '\\s+')
words = unlist(word.list)

pos.matches = match(words, pos.words)
neg.matches = match(words, neg.words)

pos.matches = !is.na(pos.matches)
neg.matches = !is.na(neg.matches)

score = sum(pos.matches) - sum(neg.matches)

return(score)

}, pos.words, neg.words, .progress=.progress)
scores.df = data.frame(score=scores, text=sentences)
return(scores.df)
}

hu.liu.pos = scan('C:/temp/positive-words.txt', what='character', comment.char=';')
hu.liu.neg = scan('C:/temp/negative-words.txt', what='character', comment.char=';')

pos.words = c(hu.liu.pos, 'upgrade', 'awsum')
neg.words = c(hu.liu.neg, 'wtf', 'wait', 'waiting', 'epicfail', 'mechanical', "suspension", "no")
team = c("vs.", "vs", "versus")

ARG.df$text<-as.factor(ARG.df$text)
BRA.df$text<-as.factor(BRA.df$text)
NED.df$text<-as.factor(NED.df$text)
GER.df$text<-as.factor(GER.df$text)

ARG.scores = score.sentiment(ARG.df$text,pos.words,neg.words,.progress='text')
BRA.scores = score.sentiment(BRA.df$text, pos.words, neg.words, progress='text')

NED.scores = score.sentiment(NED.df$text,pos.words, neg.words, .progress='text')

GER.scores = score.sentiment(GER.df$text, pos.words, neg.words, .progress='text')

ARG.scores$Team = 'Argentina'
BRA.scores$Team = 'Brazil'
NED.scores$Team = 'Netherland'
GER.scores$Team = 'Germany'

ARG.scores.2 = subset(ARG.scores, ARG.scores$score < 0)
head(ARG.scores.2)

hist(ARG.score$score)

hist(BRA.score$score)

hist(NED.score$sscore)
hist(GER.score$score)

table(ARG.scores$score)
table(BRA.scores$score)
table(NED.scores$score)
table(GER.scores$score)

head(all.scores)
all.scores = rbind(ARG.scores, NED.scores, GER.scores, BRA.scores)

table(all.scores$score, all.scores$Team)

ggplot(data=all.scores) + geom_bar(mapping=aes(x=score, fill=Team), binwidth=1) + facet_grid(Team~.) + theme.bw() + scale_fill_brewer()

