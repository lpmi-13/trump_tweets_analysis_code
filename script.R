# setting dependencies to use

library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(forecast)

# specify some aesthetic settings for fonts and colour palettes used in the figures.

quartzFonts(avenir = c("Avenir Light",
                       "Avenir Medium",
                       "Avenir Light Oblique",
                       "Avenir Medium Oblique"))

bluec <- rgb(90, 75, 215, maxColorValue=255)
redc <- rgb(215, 50, 75, maxColorValue=255)
yellowc <- rgb(225, 190, 30, maxColorValue=255)
orangec <- rgb(230, 130, 10, maxColorValue=255)
greenc <- rgb(60, 170, 130, maxColorValue=255)

# reading in this dataset and ordering it by date. In addition to the 63 categorical linguistic variables, which are stored in columns 3 to 65, columns 1 and 2 contain the unique Twitter ID of each tweet and the total number of words in each tweet, and columns 66 to 71 contain metadata: date, time period (which we coded based on date), time of day, source, retweet count, and like count.

tweets <- read.table("./R_ANALYSIS/TRUMP_DATA.txt", header = TRUE, sep = ",")
dim(tweets)
tweets <- tweets[order(tweets$DATE), ]
head(tweets$DATE)
tail(tweets$DATE)


# measure tweet length (in words) across the entire corpus, as well as before and after the character limit of Twitter was raised from 140 to 280 characters.

summary(tweets)

summary(tweets$WORDCOUNT[1:21234])              # Before 280 Character Limit

summary(tweets$WORDCOUNT[21235:nrow(tweets)])   # After 280 Character Limit

wc <- aggregate(WORDCOUNT ~ DATE, tweets, mean)
tc <- count(tweets,DATE)
cor(tc$n, wc$WORDCOUNT)

# generate visual

plot(tc$n, wc$WORDCOUNT)

summary(tweets$SOURCE )

sum(tweets$SOURCE == "Web Client")/nrow(tweets)

sum(tweets$SOURCE == "Android")/nrow(tweets)

sum(tweets$SOURCE == "iPhone")/nrow(tweets)

sum(tweets$SOURCE == "TweetDeck")/nrow(tweets)

(sum(tweets$SOURCE == "Web Client") +
    sum(tweets$SOURCE == "Android") +
    sum(tweets$SOURCE == "iPhone"))/
  nrow(tweets)

# generate pie chart

pie(table(tweets$SOURCE))

# graphing function to generate time series charts for the meta data, with important dates marked on the x-axis.

tsgraph <- function(day, val, color, ylabel){
  plot(day, val,
       col = color,
       type= "l",
       lwd=.5,
       axes = FALSE,
       frame.plot=TRUE,
       xlab= "Date",
       ylab= ylabel)
  abline(v=as.Date("2010-03-14"), col="grey",lwd=.6)   # Apprentice
    abline(v=as.Date("2011-03-06"), col="grey",lwd=.6) # Apprentice
    abline(v=as.Date("2012-02-19"), col="grey",lwd=.6) # Apprentice
    abline(v=as.Date("2013-03-03"), col="grey",lwd=.6) # Apprentice
    abline(v=as.Date("2015-01-04"), col="grey",lwd=.6) # Apprentice
    abline(v=as.Date("2010-09-16"), col="grey",lwd=.6) # Apprentice
    abline(v=as.Date("2011-04-30"), col="grey",lwd=.6) # Birther Attacks
    abline(v=as.Date("2012-11-06"), col="grey",lwd=.6) # 2012 Election
    abline(v=as.Date("2015-06-16"), col="grey",lwd=.6) # Announces
    abline(v=as.Date("2016-02-01"), col="grey",lwd=.6) # Iowa
    abline(v=as.Date("2016-05-03"), col="grey",lwd=.6) # Presumptive Nominee
    abline(v=as.Date("2016-08-17"), col="grey",lwd=.6) # Bannon
    abline(v=as.Date("2016-11-08"), col="grey",lwd=.6) # Election
    abline(v=as.Date("2017-01-20"), col="grey",lwd=.6) # Inauguration
    abline(v=as.Date("2017-05-17"), col="grey",lwd=.6) # Mueller
    abline(v=as.Date("2017-10-07"), col="grey",lwd=.6) # Twitter Length Increase
  axis(side=2, tick = TRUE)
}

# use this graphing function to generate Figure 1 for the paper, which presents four aligned time series from 2009-2018 together on one grid. These time series show total tweets by day, mean tweet length by day, mean likes and retweets by day, and total tweets by device by day.

tiff("./OUTPUT/FIG/Fig1.tiff", width=2700, height=4200, res=400)

# General Parameters

  par(mfrow=c(4,1),
      oma=c(2, 0, 8, 0),
      family="avenir")

# Panel 1: Tweets by Day

  par(mar=c(1, 5, 1, 2))
  temp <- count(tweets,DATE)
  tsgraph(day = as.Date(temp$DATE),
          val = temp$n,
          color = bluec,
          ylabel= "Tweet Count")
  axis(side=3, tick = TRUE,  las=2, cex.axis=.85, outer=FALSE,
       col.ticks = "gray50",
       col.axis = "gray50", col=NA,
       at=c(as.Date("2010-03-14"),
            as.Date("2010-09-16"),
            as.Date("2011-03-06"),
            as.Date("2011-04-30"),
            as.Date("2012-02-19"),
            as.Date("2012-11-06"),
            as.Date("2013-03-03"),
            as.Date("2015-01-04"),
            as.Date("2015-06-16"),
            as.Date("2016-02-01"),
            as.Date("2016-05-03"),
            as.Date("2016-08-17"),
            as.Date("2016-11-08"),
            as.Date("2017-01-20"),
            as.Date("2017-05-17"),
            as.Date("2017-10-07")),
       labels=c("Apprentice", "Apprentice", "Apprentice",
                "Birther", "Apprentice", "Election",
                "Apprentice", "Apprentice", "Declares",
                "Iowa", "Nominee", "Bannon",
                "Election", "Inaguration","Mueller",
                "280 Chars"))
  mtext(text=expression(bold("Change in Activity on the @realDonaldTrump Twitter Account")),
        side=3, line=6, las=1, cex=1)
  axis(side=1, tick = TRUE, cex.axis=1,
       labels= FALSE,
       at=c(as.Date("2010-01-01"),
            as.Date("2011-01-01"),
            as.Date("2012-01-01"),
            as.Date("2013-01-01"),
            as.Date("2014-01-01"),
            as.Date("2015-01-01"),
            as.Date("2016-01-01"),
            as.Date("2017-01-01"),
            as.Date("2018-01-01")))

# Panel 2: Mean Tweet Length by Day

  par(mar=c(1, 5, 1, 2))
  temp <- aggregate(WORDCOUNT ~ DATE, tweets, mean)
  tsgraph(day = as.Date(temp$DATE),
          val = temp$WORDCOUNT,
          color = bluec,
          ylabel = "Mean Tweet Length in Words")
  axis(side = 1, tick = TRUE, cex.axis = 1,
       labels = FALSE,
       at=c(as.Date("2010-01-01"),
            as.Date("2011-01-01"),
            as.Date("2012-01-01"),
            as.Date("2013-01-01"),
            as.Date("2014-01-01"),
            as.Date("2015-01-01"),
            as.Date("2016-01-01"),
            as.Date("2017-01-01"),
            as.Date("2018-01-01")))

# Panel 3: Like/Retweets by Day

  par(mar=c(1, 5, 1, 2))
  temp <- aggregate(FAV ~ DATE, tweets, mean)
  tsgraph(day = as.Date(temp$DATE),
          val = log10(temp$FAV),
          color = bluec,
          ylabel= "Mean Likes/Retweets Logged")
  temp <- aggregate(RETWEET ~ DATE, tweets, mean)
  lines(as.Date(temp$DATE), log10(temp$RETWEET),
        col=adjustcolor(redc, alpha.f = 0.55))
  legend("topleft",
         legend = c("Likes", "Retweets"),
         lty = c(1,1),
         lwd=c(1,1),
         col = c(bluec, redc),
         text.col = c(bluec, redc),
         bty="n",
         cex=1)
  axis(side=1, tick = TRUE, cex.axis=1,
       labels= FALSE,
       at=c(as.Date("2010-01-01"),
            as.Date("2011-01-01"),
            as.Date("2012-01-01"),
            as.Date("2013-01-01"),
            as.Date("2014-01-01"),
            as.Date("2015-01-01"),
            as.Date("2016-01-01"),
            as.Date("2017-01-01"),
            as.Date("2018-01-01")))

# Panel 4: Device by Day

  temp<-c()
  temp$DATE <- seq.Date(min(as.Date(tweets$DATE)),
                        max(as.Date(tweets$DATE)),
                        by=1)
  temp <- as.data.frame(temp)
  temp.w <- as.data.frame(count(tweets[tweets$SOURCE=="Web Client",],DATE))
    temp.w$DATE <- as.Date(temp.w$DATE)
    temp.w <- merge(temp,temp.w,by="DATE", all.x=TRUE)
    temp.w[is.na(temp.w)] <- 0
  temp.a <- as.data.frame(count(tweets[tweets$SOURCE=="Android",],DATE))
    temp.a$DATE <- as.Date(temp.a$DATE)
    temp.a <- merge(temp,temp.a,by="DATE", all.x=TRUE)
    temp.a[is.na(temp.a)] <- 0
  temp.i <- as.data.frame(count(tweets[tweets$SOURCE=="iPhone",],DATE))
    temp.i$DATE <- as.Date(temp.i$DATE)
    temp.i <- merge(temp,temp.i,by="DATE", all.x=TRUE)
    temp.i[is.na(temp.i)] <- 0
  stackb <- c()
    stackb$W <- temp.w$n
    stackb$A <- temp.a$n
    stackb$I <- temp.i$n
    stackb <- as.data.frame(stackb, row.names=temp.w$DATE)
  par(mar=c(1, 5, 1, 2))
  barplot(t(stackb),
          border=NA,
          col=c(bluec, redc, yellowc),
          space=0,
          axes=FALSE,
          axisnames = FALSE,
          ylab="Tweet Count")
  box()
  legend("topleft",
         legend = c("Web", "Android", "iPhone"),
         lty = c(1,1),
         lwd=c(1,1),
         col = c(bluec, redc ,yellowc),
         text.col = c(bluec, redc, yellowc),
         bty="n",
         cex=1)
  axis(side=2, tick = TRUE)
  axis(side=1, tick = TRUE, cex.axis=1,
       at=c(which(rownames(stackb) %in% "2010-01-01"),
            which(rownames(stackb) %in% "2011-01-01"),
            which(rownames(stackb) %in% "2012-01-01"),
            which(rownames(stackb) %in% "2013-01-01"),
            which(rownames(stackb) %in% "2014-01-01"),
            which(rownames(stackb) %in% "2015-01-01"),
            which(rownames(stackb) %in% "2016-01-01"),
            which(rownames(stackb) %in% "2017-01-01"),
            which(rownames(stackb) %in% "2018-01-01")),
       labels=c("2010", "2011", "2012",
                "2013", "2014", "2015",
                "2016","2017", "2018"))
  abline(v=which(rownames(stackb) %in% "2010-09-16"),
         col="grey",lwd=.6)   # Apprentice
  abline(v=which(rownames(stackb) %in% "2011-04-30"),
         col="grey",lwd=.6) # Birther Attacks
  abline(v=which(rownames(stackb) %in% "2012-11-06"),
         col="grey",lwd=.6) # 2012 Election
  abline(v=which(rownames(stackb) %in% "2015-06-16"),
         col="grey",lwd=.6) # Announces
  abline(v=which(rownames(stackb) %in% "2016-02-01"),
         col="grey",lwd=.6) # Iowa
  abline(v=which(rownames(stackb) %in% "2016-05-03"),
         col="grey",lwd=.6) # Presumptive Nominee
  abline(v=which(rownames(stackb) %in% "2016-08-17"),
         col="grey",lwd=.6) # Bannon
  abline(v=which(rownames(stackb) %in% "2016-11-08"),
         col="grey",lwd=.6) # Election
  abline(v=which(rownames(stackb) %in% "2017-01-20"),
         col="grey",lwd=.6) # Inauguration
  abline(v=which(rownames(stackb) %in% "2017-05-17"),
         col="grey",lwd=.6) # Mueller
  abline(v=which(rownames(stackb) %in% "2017-10-07"),
         col="grey",lwd=.6) # 280 char
  abline(v=which(rownames(stackb) %in% "2010-03-14"),
         col="grey",lwd=.6) # Apprentice
  abline(v=which(rownames(stackb) %in% "2011-03-06"),
         col="grey",lwd=.6) # Apprentice
  abline(v=which(rownames(stackb) %in% "2012-02-19"),
         col="grey",lwd=.6) # Apprentice
  abline(v=which(rownames(stackb) %in% "2013-03-03"),
         col="grey",lwd=.6) # Apprentice
  abline(v=which(rownames(stackb) %in% "2015-01-04"),
         col="grey",lwd=.6) # Apprentice

dev.off()

#  covert the time data from being measured by the second to being measured by the minute.

tweets$TIME_SHORT <- gsub("(\\d\\d):(\\d\\d):(\\d\\d)","\\1:\\2", tweets$TIME)

# make a blank data frame consisting of all the minutes in a day as rows.

temp<-c()
temp$TIME_SHORT <- seq.POSIXt(as.POSIXct("00:00", format="%H:%M"),
                              as.POSIXct("23:59", format="%H:%M"),
                              by=60)
temp <- as.data.frame(temp)
temp$TIME_SHORT <- gsub(
  "\\d\\d\\d\\d-\\d\\d-\\d\\d (\\d\\d):(\\d\\d):(\\d\\d)",
  "\\1:\\2",
  temp$TIME_SHORT)

# count all occurences of the three main sources (web client, android, iphone) by minute across all days in the dataset and then merge these counts with the blank minute-by-minute data frame to get complete and comparable counts for each source over the minutes of the day.

temp.w <- as.data.frame(count(tweets[tweets$SOURCE=="Web Client",],TIME_SHORT))
temp.w <- merge(temp, temp.w, by="TIME_SHORT", all.x=TRUE)
temp.w[is.na(temp.w)] <- 0

temp.a <- as.data.frame(count(tweets[tweets$SOURCE=="Android",],TIME_SHORT))
temp.a <- merge(temp,temp.a,by="TIME_SHORT", all.x=TRUE)
temp.a[is.na(temp.a)] <- 0

temp.i <- as.data.frame(count(tweets[tweets$SOURCE=="iPhone",],TIME_SHORT))
temp.i <- merge(temp,temp.i,by="TIME_SHORT", all.x=TRUE)
temp.i[is.na(temp.i)] <- 0

# combine these counts to construct a new data frame, which we’ll use as the basis for the stacked bar plot in the first part of Figure 2.

stackb <- c()
stackb$W <- temp.w$n
stackb$A <- temp.a$n
stackb$I <- temp.i$n
stackb <- as.data.frame(stackb, row.names=temp$TIME_SHORT)

# read in a second dataset that lists the most common source in every 10 minute block for every day, which is usually only a single device (computed outside of R with a perl script).

ttd <- read.table("./R_ANALYSIS/TRUMP_3D_SHORT.txt", header = TRUE, sep=",")

#  set a colour palette based on the most fequent device for each day/timeblock pair in this dataset, which we’ll use to generate the scatterplot in the second part of Figure 2.

colour <- c()
colour[ttd$R > ttd$B & ttd$R > ttd$G] <- redc
colour[ttd$B > ttd$R & ttd$B > ttd$G] <- bluec
colour[ttd$G > ttd$R & ttd$G > ttd$B] <- yellowc

#generate the second figure for the paper, which consists of two plots aligned by time of day. The first plots frequency and source by time of day and the second plots most common source by time of day and date.

tiff("./OUTPUT/FIG/Fig2.tiff", width=2700, height=2400, res=250)

  par(mfrow=c(2,1),
    oma=c(3, 0, 3, 0),
    mar=c(1, 4, 0, 2),
    family="avenir")

# Panel 1: Posting Frequency and Device by Time of Day

  barplot(t(stackb),
          border=NA,
          col=c(bluec,redc,yellowc),
          axes=FALSE,
          axisnames = FALSE,
          space=0,
          ylab="Tweet Count")
  box()
  legend("topleft",
         legend = c("Web", "Android", "iPhone"),
         col = c(bluec,redc,yellowc),
         text.col = c(bluec,redc,yellowc),
         bty="n",
         cex=1)
  axis(side=2, tick = TRUE)
  mtext(text=expression(bold(
    "Change in Device Use on the @realDonaldTrump Twitter Account")),
        side=3, line=1, las=1, cex=1.5)

# Panel 2: Device by Time of Day and Date

  plot(as.POSIXct(ttd$TIME, format="%H:%M"),as.Date(ttd$DATE),
       col=adjustcolor(colour, alpha.f =.75),
       pch=18, cex=.75,
       ylab="Date",
       xaxt="n")
  axis(side=1, tick = TRUE,
       at=c(as.POSIXct("00:00", format="%H:%M"),
            as.POSIXct("03:00", format="%H:%M"),
            as.POSIXct("06:00", format="%H:%M"),
            as.POSIXct("09:00", format="%H:%M"),
            as.POSIXct("12:00", format="%H:%M"),
            as.POSIXct("15:00", format="%H:%M"),
            as.POSIXct("18:00", format="%H:%M"),
            as.POSIXct("21:00", format="%H:%M"),
            as.POSIXct("24:00", format="%H:%M")),
       labels=c("midnight", "3am", "6am",
                "9 am", "noon", "3pm", "6pm", "9pm", "midnight"))
  axis(side=3, tick = TRUE, labels = FALSE,
       line =.5, col = NA, col.ticks = 1,
       at=c(as.POSIXct("00:00", format="%H:%M"),
            as.POSIXct("03:00", format="%H:%M"),
            as.POSIXct("06:00", format="%H:%M"),
            as.POSIXct("09:00", format="%H:%M"),
            as.POSIXct("12:00", format="%H:%M"),
            as.POSIXct("15:00", format="%H:%M"),
            as.POSIXct("18:00", format="%H:%M"),
            as.POSIXct("21:00", format="%H:%M"),
            as.POSIXct("24:00", format="%H:%M")))
  mtext(text="Time of Day", side=1, line=3, las=1)

dev.off()

# conduct the multiple correspondence analysis (MCA) for the paper.
# First, we extract the linguistic data from the dataset – the 63 categorical linguistic variables plus tweet length in words.

tweets_mat<-tweets[,2:65]

# run the MCA to extract 5 dimensions based on this dataset, but setting tweet length (in column 1) as a supplementary quantitative variable. This means tweet length is not taken into consideration when we build the MCA dimensions, but we can still assess the degree to which it is correlated with each dimension.

mcares <- MCA(tweets_mat, ncp=5, quanti.sup=1)

#  inspect the relationship between tweet length and the dimensions, which shows that Dimension 1 has a strong correlation with tweet length, while the other dimensions do not. This means that the effect of texts length has effectively been isolated on Dimension 1

mcares$quanti.sup

# save select components of the MCA output for further analysis.

eig <- as.data.frame(mcares$eig)
coordind <- as.data.frame(mcares$ind$coord)
coordvar <- as.data.frame(mcares$var$coord)
contribind <- as.data.frame(mcares$ind$contrib)
contribvar <- as.data.frame(mcares$var$contrib)

# output the main results to text files for manual linguistic interpretation.

summary(mcares, nbelements = Inf, nbind=Inf, ncp=5,
        file = "OUTPUT/MCA_SUMMARY.txt")
summary(mcares, nbelements = Inf, nbind=0, ncp=5,
        file = "OUTPUT/FEATURE_SUMMARY.txt")
summary(mcares, nbind = Inf, ncp= 5,
        file = "OUTPUT/TEXT_SUMMARY.txt")
write.table(coordind, "OUTPUT/TEXT_COORDIANTES.txt")
write.table(coordvar, "OUTPUT/FEATURE_COORDIANTES.txt")
write.table(contribind, "OUTPUT/TEXT_CONTRIBUTIONS.txt")
write.table(contribvar, "OUTPUT/FEATURE_CONTRIBUTIONS.txt")

# We chose to focus the first 5 dimensions based on interpretability, which we think is by far and away the most important criteria when selecting the number of dimensions to analyse in exploratory multivariate analysis. But we can also look at the eigen values and the amount of variance explained by the dimensions individually and cumulatively.

eig

# compute pseudo-eigenvalues based on the mean eigenvalue across all dimension and the number of variables (63) in our full analysis.

mean_eig <- mean(eig$eigenvalue)
Q <- nrow(eig)
eig$pseudo <- (Q / (Q - 1)) ^ 2 * (eig$eigenvalue - mean_eig) ^ 2

# compute adjusted percentages of variance explained based on these pseudo-eigenvalues by dividing these values by the sum of all pseudo-eigenvalues.

pseudo_sum <- sum(eig$pseudo)
eig$pseudo_percent <- round(100 * eig$pseudo / pseudo_sum, 2)

# output these values, which shows that the first five dimensions account for almost 80% of variance, with something of a drop in variance explained between Dimensions 5 and 6, for whatever that’s worth.

eig

sum(eig$pseudo_percent[1:5])

# generate the third figure for the paper, which is a heat map that displays the strength and direction of the relationship between the 63 linguistic features and the four stylistic dimension.

# specify the row labels for the heat map.

names <-c("Amplifier", "Analytic Neg.", "Attributive Adj.", "Auxiliary Do",
          "Copula Be", "Parenthese", "Capital", "Coordinating Conj.",
          "Contrastive Conj.", "Colon", "Comma", "Definite Art.",
          "Quantitative Det.", "Exclamation Mark", "First Pers. Pron.",
          "Period", "Gerund", "Hashtag", "Have Main Verb", "Imperative",
          "Indefinite Art.", "Infinitive", "Pronoun It", "Modal of Necessity",
          "Modal of Possibility", "Modal of Prediction", "Non-Initial Ment.",
          "Phrasal Verb", "Nominalisation", "Numeral Det.", "Numeral NP Head",
          "Object Pronoun", "Adverb", "Interjection", "Noun", "Verb", "Passive",
          "Past Tense", "Perception Verb", "Perfect", "Poss. Prop. Noun",
          "Possessive Det.", "Predicative Adjective", "Preposition",
          "Aux. Contraction", "Progressive", "Quantitative Pron.", "Proverb Do",
          "Proper Noun", "Private Verb", "Public Verb", "Question",
          "Rel. Cl. (Subj. Gap)", "Third Pers. -s", "Second Pers. Pron.",
          "Stance Verb", "Subject Pronoun", "Superlative", "Third Pers. Pron.",
          "Time Adverb", "URL", "WH Word", "Initial Mention")

# extract the coordinates for just the presence features, which represents the association between the presence of each linguistic feature and each dimension.

temp <-coordvar[1:5]
temp

temp$name <- row.names(temp)
temp<-filter(temp, !grepl("_A",name))
row.names(temp) <- temp$name
temp <-temp[1:5]
coords <- as.matrix(temp)
coords

# isolate the four main stylistic dimensions (Dimension 2-5). We exclude Dimension 1 from our main stylistic analysis because it essentially represents tweet length. We especially don’t want strong Dimension 1 scores to dominate the heat map.

adjust <- as.data.frame(coords[,2:5])
adjust <- as.matrix(adjust)

# set a colour palette.

pal <- colorRampPalette(c(bluec,"white",redc), bias=1)(20)

# generate a heat map to show which linguistic features are most strongly associated with the positive (red) and negative (blue) pole of each dimension, with rows organised based on a complete-linkage hierachical cluster analysis. Note the style labels on the four dimensions represent our linguistic interpretations of the MCA results and are discussed at length in the paper.

tiff("./OUTPUT/FIG/Fig3.tiff", width=2700, height=2400, res=400)
  par(family="avenir")
  heatmap(adjust,
          Colv=NA,
          cexRow=.5, cexCol=.85,
          col=pal, las=2,
          labRow=names,
          labCol=c("Dimension 2:\nConversational\nStyle",
                   "Dimension 3:\nCampaigning\nStyle",
                   "Dimension 4:\nEngaged\nStyle",
                   "Dimension 5:\nAdvisory\nStyle"),
          main=" ",
          cex=.5)
  mtext(text=expression(bold(
    "Linguistic Feature Coordinates across 4 Stylistic Dimensions")),
        side=3, line=2.5, las=1, cex=1)
dev.off()

# generate plots to let us look at the coordinates and contributions together for each variable on each dimension. These graphs aren’t included in the paper.

# extract the presence contributions like we did for the coordinates

temp <-contribvar[1:5]
temp$name <- row.names(temp)
temp<-filter(temp, !grepl("_A",name))
row.names(temp) <- temp$name
temp <-temp[1:5]
contribs <- as.matrix(temp)
contribs

# convert both the feature coordinates and contributions to data frames.

coords <-as.data.frame(coords)
contribs <-as.data.frame(contribs)

# make a series of plots show both the feature coordinates and contributions for each dimensions. Notably, we see they are fairly similar, aside from the fact that contributions are all positive and thus are not associated with a pole, which is why we focus primarily on presenting coordinates in the paper.

plot(coords$`Dim 2`,contribs$`Dim 2`, type="n",
       xlab="Coordinates", ylab="Contributions",
       main="Dimension 2: Conversational Style")
text(coords$`Dim 2`,contribs$`Dim 2`,labels=names,
       cex=.5, col = bluec)

plot(coords$`Dim 3`,contribs$`Dim 3`, type="n",
       xlab="Coordinates", ylab="Contributions",
       main="Dimension 3: Campaigning Style")
text(coords$`Dim 3`,contribs$`Dim 3`,labels=names,
       cex=.5, col = redc)

plot(coords$`Dim 4`,contribs$`Dim 4`, type="n",
       xlab="Coordinates", ylab="Contributions",
       main="Dimension 4: Engaged Style")
text(coords$`Dim 4`,contribs$`Dim 4`,labels=names,
       cex=.5, col = greenc)

plot(coords$`Dim 5`,contribs$`Dim 5`, type="n",
       xlab="Coordinates", ylab="Contributions",
       main="Dimension 5: Advisory Style")
text(coords$`Dim 5`,contribs$`Dim 5`,labels=names,
       cex=.5, col = orangec)

# make a second time series graphing function, which we’ll use to plot change over time in the 4 stylistic dimensions from 2008-2019, including moving average trend lines.

tsgraph2 <- function(dim, win, lab, colo, miny, maxy){
  ts<-merge(as.Date(tweets$DATE), dim, by="row.names")
  ts<-aggregate(y ~ x, ts, mean)
  sts <- ts
  for (i in 1:ncol(ts)){
    sts[,i] <-  ma(ts[,i], order = win, centre = T)
  }
  plot(ts$x, ts$y, type="n",
       ylim=c(miny, maxy),
       ylab=lab, xlab=" ",
       axes = FALSE, frame.plot=TRUE)
  axis(side=2, at=c(-.1,0,.1),labels=c("-0.1","0","+0.1"))
  abline(v=as.Date("2010-09-16"), col="grey",lwd=.6) # Apprentice
  abline(v=as.Date("2011-04-30"), col="grey",lwd=.6) # Birther Attacks
  abline(v=as.Date("2012-11-06"), col="grey",lwd=.6) # 2012 Election
  abline(v=as.Date("2015-06-16"), col="grey",lwd=.6) # Announces
  abline(v=as.Date("2016-02-01"), col="grey",lwd=.6) # Iowa
  abline(v=as.Date("2016-05-03"), col="grey",lwd=.6) # Presumptive Nominee
  abline(v=as.Date("2016-11-08"), col="grey",lwd=.6) # Election
  abline(v=as.Date("2017-01-20"), col="grey",lwd=.6) # Inauguration
  abline(v=as.Date("2010-03-14"), col="grey",lwd=.6) # Apprentice
  abline(v=as.Date("2011-03-06"), col="grey",lwd=.6) # Apprentice
  abline(v=as.Date("2012-02-19"), col="grey",lwd=.6) # Apprentice
  abline(v=as.Date("2013-03-03"), col="grey",lwd=.6) # Apprentice
  abline(v=as.Date("2015-01-04"), col="grey",lwd=.6) # Apprentice
  abline(a=0, b=0, lty=1, lwd=.5, col="black")
  points(ts$x, ts$y, pch=16, cex=.7,
         col= adjustcolor(colo, alpha.f = 0.2))
  lines(ts$x, sts$y, col = colo, lwd=1)
}

# use this graphing function to generate the fourth figure for the paper, which presents four aligned time series from 2009-2018 together on one grid, one for each set of dimension coordinates, with individual tweets marked as points and with the moving average trend line superimposed. A number of important dates are also marked.

tiff("./OUTPUT/FIG/Fig4.tiff", width=2700, height=4200, res=400)

  par(mfrow=c(4,1),
    oma=c(2, 0, 8, 0),
    family="avenir")

# Panel 1: Dimension 2

  par(mar=c(1, 5, 1, 2))
  tsgraph2(coordind$'Dim 2', 60,
           "Dimension 2: Conversational",
           bluec, min=-.15, max=+.15)
  axis(side=3, tick = TRUE,
       las=2, cex.axis=.85, outer=FALSE,
       col.ticks = "gray50",
       col.axis = "gray50", col=NA,
       at=c(as.Date("2010-03-14"),
            as.Date("2010-09-16"),
            as.Date("2011-03-06"),
            as.Date("2011-04-30"),
            as.Date("2012-02-19"),
            as.Date("2012-11-06"),
            as.Date("2013-03-03"),
            as.Date("2015-01-04"),
            as.Date("2015-06-16"),
            as.Date("2016-02-01"),
            as.Date("2016-05-03"),
            as.Date("2016-11-08"),
            as.Date("2017-01-20")),
       labels=c("Apprentice", "Apprentice", "Apprentice",
                "Birther", "Apprentice", "Election",
                "Apprentice", "Apprentice", "Declares",
                "Iowa", "Nominee",
                "Election", "Inaguration"))
  mtext(text=expression(bold(
    "Stylistic Change on the @realDonaldTrump Twitter Account")),
        side=3, line=6, las=1, cex=1)
  axis(side=1, tick = TRUE, cex.axis=1, labels=FALSE,
       at=c(as.Date("2010-01-01"),
            as.Date("2011-01-01"),
            as.Date("2012-01-01"),
            as.Date("2013-01-01"),
            as.Date("2014-01-01"),
            as.Date("2015-01-01"),
            as.Date("2016-01-01"),
            as.Date("2017-01-01"),
            as.Date("2018-01-01")))

# Panel 2: Dimension 3

  par(mar=c(1, 5, 1, 2))
  tsgraph2(coordind$'Dim 3', 60,
           "Dimension 3: Campaigning",
           redc, min=-.15, max=+.15)
  axis(side=1, tick = TRUE, cex.axis=1, labels=FALSE,
       at=c(as.Date("2010-01-01"),
            as.Date("2011-01-01"),
            as.Date("2012-01-01"),
            as.Date("2013-01-01"),
            as.Date("2014-01-01"),
            as.Date("2015-01-01"),
            as.Date("2016-01-01"),
            as.Date("2017-01-01"),
            as.Date("2018-01-01")))

# Panel 3: Dimension 4

  par(mar=c(1, 5, 1, 2))
  tsgraph2(coordind$'Dim 4', 60,
           "Dimension 4: Engaged",
           greenc, min=-.15, max=+.15)
  axis(side=1, tick = TRUE, cex.axis=1, labels=FALSE,
       at=c(as.Date("2010-01-01"),
            as.Date("2011-01-01"),
            as.Date("2012-01-01"),
            as.Date("2013-01-01"),
            as.Date("2014-01-01"),
            as.Date("2015-01-01"),
            as.Date("2016-01-01"),
            as.Date("2017-01-01"),
            as.Date("2018-01-01")))

# Panel 4: Dimension 5

  par(mar=c(1, 5, 1, 2))
  tsgraph2(coordind$'Dim 5', 60,
           "Dimension 5: Advisory",
           orangec, min=-.15, max=+.15)
  axis(side=1, tick = TRUE, cex.axis=1,
       at=c(as.Date("2010-01-01"),
            as.Date("2011-01-01"),
            as.Date("2012-01-01"),
            as.Date("2013-01-01"),
            as.Date("2014-01-01"),
            as.Date("2015-01-01"),
            as.Date("2016-01-01"),
            as.Date("2017-01-01"),
            as.Date("2018-01-01")),
       labels=c("2010", "2011", "2012",
                "2013", "2014", "2015",
                "2016", "2017", "2018"))
dev.off()

# make a third time series graphing function, which we’ll use to plot change over time in the 4 stylistic dimensions during the campaign, this time with all four trend lines sumperimposed on one graph and no plotting of individual tweets.

tsgraph3 <-
  function(dim, win, lab, colo, minx, maxx, miny, maxy, titlex){
  ts<-merge(as.Date(tweets$DATE), dim, by="row.names")
  ts<-aggregate(y ~ x, ts, mean)
  sts <- ts
  for (i in 1:ncol(ts)){
    sts[,i] <-  ma(ts[,i], order = win, centre = T)
  }
  par(mar=c(3, 5, 5, 2))
  plot(ts$x, ts$y, type="n", ylim=c(miny, maxy),
       xlim = c(minx, maxx), ylab=lab, xlab=" ",
       axes = FALSE,frame.plot=TRUE)
  axis(side=1, tick = TRUE, labels=FALSE,
       at=c(as.Date("2010-01-01"),
            as.Date("2011-01-01"),
            as.Date("2012-01-01"),
            as.Date("2013-01-01"),
            as.Date("2014-01-01"),
            as.Date("2015-01-01"),
            as.Date("2016-01-01"),
            as.Date("2017-01-01"),
            as.Date("2018-01-01")))
  axis(side=2, at=c(-.1,0,.1),labels=c("-0.1","0","+0.1"))
  abline(v=as.Date("2010-09-16"), col="grey",lwd=.6) # Apprentice
  abline(v=as.Date("2011-04-30"), col="grey",lwd=.6) # Birther Attacks
  abline(v=as.Date("2012-11-06"), col="grey",lwd=.6) # 2012 Election
  abline(v=as.Date("2015-06-16"), col="grey",lwd=.6) # Announces
  abline(v=as.Date("2016-02-01"), col="grey",lwd=.6) # Iowa
  abline(v=as.Date("2016-05-03"), col="grey",lwd=.6) # Presumptive Nominee
  abline(v=as.Date("2016-08-17"), col="grey",lwd=.6) # Bannon
  abline(v=as.Date("2016-11-08"), col="grey",lwd=.6) # Election
  abline(v=as.Date("2017-01-20"), col="grey",lwd=.6) # Inauguration
  abline(v=as.Date("2017-05-17"), col="grey",lwd=.6) # Mueller
  abline(v=as.Date("2010-03-14"), col="grey",lwd=.6) # Apprentice
  abline(v=as.Date("2011-03-06"), col="grey",lwd=.6) # Apprentice
  abline(v=as.Date("2012-02-19"), col="grey",lwd=.6) # Apprentice
  abline(v=as.Date("2013-03-03"), col="grey",lwd=.6) # Apprentice
  abline(v=as.Date("2015-01-04"), col="grey",lwd=.6) # Apprentice
  abline(v=as.Date("2015-08-06"), col="grey",lwd=.6) # GOP Debate 1
  abline(v=as.Date("2016-03-15"), col="grey",lwd=.6) # Super Tuesday 2/Rubio Out
  abline(v=as.Date("2016-06-20"), col="grey",lwd=.6) # Manafort
  abline(v=as.Date("2016-07-15"), col="grey",lwd=.6) # Pence VP
  abline(v=as.Date("2016-10-07"), col="grey",lwd=.6) # Access Hollywood
  abline(v=as.Date("2015-10-28"), col="grey",lwd=.6) # GOP Debate 3
  abline(a=0, b=0, lty=1, lwd=.5, col="black")
  points(ts$x, ts$y, pch=16, cex=.7,
         col= adjustcolor(colo, alpha.f = 0.2))
  lines(ts$x, sts$y, col = colo, lwd=1.5)
  axis(side=3, tick = TRUE,  las=2, cex.axis=.85,
       outer=FALSE, col.ticks = "gray50",
       col.axis = "gray50", col=NA,
       at=c(as.Date("2010-03-14"),
            as.Date("2010-09-16"),
            as.Date("2011-03-06"),
            as.Date("2011-04-30"),
            as.Date("2012-02-19"),
            as.Date("2012-11-06"),
            as.Date("2013-03-03"),
            as.Date("2015-01-04"),
            as.Date("2015-06-16"),
            as.Date("2015-08-06"),
            as.Date("2015-10-28"),
            as.Date("2016-02-01"),
            as.Date("2016-03-15"),
            as.Date("2016-05-03"),
            as.Date("2016-06-20"),
            as.Date("2016-07-15"),
            as.Date("2016-08-17"),
            as.Date("2016-10-07"),
            as.Date("2016-11-08"),
            as.Date("2017-01-20"),
            as.Date("2017-05-17")),
       labels=c("Apprentice", "Apprentice", "Apprentice",
                "Birther", "Apprentice", "Election",
                "Apprentice", "Apprentice", "Declares",
                "GOP Deb 1", "GOP Deb 3", "Iowa",
                "Sup Tues 2", "Nominee", "Manafort",
                "Pence VP", "Bannon", "AH Tape",
                "Election", "Inaguration","Mueller"))
  mtext(text=titlex,
        side=3, line=6, las=1, cex=1.5,font = 2)
  axis(side=1, tick = TRUE, cex.axis=1,
       at=c(as.Date("2010-01-01"),
            as.Date("2011-01-01"),
            as.Date("2012-01-01"),
            as.Date("2013-01-01"),
            as.Date("2014-01-01"),
            as.Date("2015-01-01"),
            as.Date("2016-01-01"),
            as.Date("2017-01-01"),
            as.Date("2018-01-01")),
       labels=c("2010", "2011", "2012",
                "2013", "2014", "2015",
                "2016", "2017", "2018"))

}

# use this graphing function to generate the fifth figure for the paper, which presents four aligned time series from 2009-2018 together on one grid, one for each set of dimension coordinates, with individual tweets marked as points and with a 60-day moving average trend line superimposed. A number of important dates are also marked.

win <- 60

tiff("./OUTPUT/FIG/Fig5.tiff", width=2700, height=1800, res=280)
  par(mar=c(3, 5, 8, 2),
      family="avenir")

# Get moving averages for Dimension 2

  ts<-merge(as.Date(tweets$DATE), coordind$'Dim 2', by="row.names")
  ts<-aggregate(y ~ x, ts, mean)
  sts <- ts
  for (i in 1:ncol(ts)){
    sts[,i] <-  ma(ts[,i], order = win, centre = T)
  }

# Plot Dimension 2 (with general lables)

  plot(ts$x, ts$y, type="n",
       ylim=c(-.15, .15),
       xlim = c(as.Date("2015-01-01"), as.Date("2018-01-01")),
       ylab="Dimension Coordinates", xlab=" ",
       axes = FALSE,frame.plot=TRUE)
  lines(ts$x, sts$y, col = bluec, lwd=1.5)

# Get moving averages for Dimension 3 and Plot

  ts<-merge(as.Date(tweets$DATE), coordind$'Dim 3', by="row.names")
  ts<-aggregate(y ~ x, ts, mean)
  sts <- ts
  for (i in 1:ncol(ts)){
    sts[,i] <-  ma(ts[,i], order = win, centre = T)
  }
  lines(ts$x, sts$y, col = redc, lwd=1.5)

# Get moving averages for Dimension 4 and Plot

  ts<-merge(as.Date(tweets$DATE), coordind$'Dim 4', by="row.names")
  ts<-aggregate(y ~ x, ts, mean)
  sts <- ts
  for (i in 1:ncol(ts)){
    sts[,i] <-  ma(ts[,i], order = win, centre = T)
  }
  lines(ts$x, sts$y, col = greenc, lwd=1.5)

 # Get moving averages for Dimension 5 and Plot

  ts<-merge(as.Date(tweets$DATE), coordind$'Dim 5', by="row.names")
  ts<-aggregate(y ~ x, ts, mean)
  sts <- ts
  for (i in 1:ncol(ts)){
    sts[,i] <-  ma(ts[,i], order = win, centre = T)
  }
  lines(ts$x, sts$y, col = orangec, lwd=1.5)

# Add Title

  mtext(text=expression(bold(
    "Stylistic Change during the 2016 Campaign on the @realDonaldTrump Twitter Account")),
        side=3, line=6, las=1, cex=1.25)

# Add Year Axis (X)

  axis(side=1, tick = TRUE, labels=FALSE,
       at=c(as.Date("2010-01-01"),
            as.Date("2011-01-01"),
            as.Date("2012-01-01"),
            as.Date("2013-01-01"),
            as.Date("2014-01-01"),
            as.Date("2015-01-01"),
            as.Date("2016-01-01"),
            as.Date("2017-01-01"),
            as.Date("2018-01-01")))

# Add Coordinate Axis (Y)

  axis(side=2, at=c(-.1,0,.1),labels=c("-0.1","0","+0.1"))

# Add Time Line Splits (X)

  abline(v=as.Date("2010-09-16"), col="grey",lwd=.6)    # Apprentice
  abline(v=as.Date("2011-04-30"), col="grey",lwd=.6)    # Birther Attacks
  abline(v=as.Date("2012-11-06"), col="grey",lwd=.6)    # 2012 Election
  abline(v=as.Date("2015-06-16"), col="grey10",lwd=.6)  # Announces
  abline(v=as.Date("2016-02-01"), col="grey",lwd=.6)    # Iowa
  abline(v=as.Date("2016-05-03"), col="grey",lwd=.6)    # Presumptive Nominee
  abline(v=as.Date("2016-08-17"), col="grey",lwd=.6)    # Bannon
  abline(v=as.Date("2016-11-08"), col="grey10",lwd=.6)  # Election
  abline(v=as.Date("2017-01-20"), col="grey10",lwd=.6)  # Inauguration
  abline(v=as.Date("2017-05-17"), col="grey",lwd=.6)    # Mueller
  abline(v=as.Date("2010-03-14"), col="grey",lwd=.6)    # Apprentice
  abline(v=as.Date("2011-03-06"), col="grey",lwd=.6)    # Apprentice
  abline(v=as.Date("2012-02-19"), col="grey",lwd=.6)    # Apprentice
  abline(v=as.Date("2013-03-03"), col="grey",lwd=.6)    # Apprentice
  abline(v=as.Date("2015-01-04"), col="grey",lwd=.6)    # Apprentice
  abline(v=as.Date("2015-08-06"), col="grey",lwd=.6)    # GOP Debate 1
  abline(v=as.Date("2016-03-15"), col="grey",lwd=.6)    # Super Tuesday 2/Rubio Out
  abline(v=as.Date("2016-06-20"), col="grey",lwd=.6)    # Manafort
  abline(v=as.Date("2016-07-15"), col="grey",lwd=.6)    # Pence VP
  abline(v=as.Date("2016-10-07"), col="grey",lwd=.6)    # Access Hollywood
  abline(v=as.Date("2015-10-28"), col="grey",lwd=.6)    # GOP Debate 3
  abline(v=as.Date("2017-02-13"), col="grey",lwd=.6)    # Flynn
  abline(v=as.Date("2017-07-28"), col="grey",lwd=.6)    # Kelly in
  abline(v=as.Date("2017-08-18"), col="grey",lwd=.6)    # Bannon out
  abline(v=as.Date("2017-09-20"), col="grey",lwd=.6)   # Hurricane

# Add zero line (Y)

  abline(a=0, b=0, lty=1, lwd=.5, col="black")

# Add Time Line Ticks with Labels (X)

  axis(side=3, tick = TRUE,  las=2, cex.axis=.85, outer=FALSE,
       col.ticks = "gray50",
       col.axis = "gray50", col=NA,
       at=c(as.Date("2010-03-14"),
            as.Date("2010-09-16"),
            as.Date("2011-03-06"),
            as.Date("2011-04-30"),
            as.Date("2012-02-19"),
            as.Date("2012-11-06"),
            as.Date("2013-03-03"),
            as.Date("2015-01-04"),
            as.Date("2015-06-16"),
            as.Date("2015-08-06"),
            as.Date("2015-10-28"),
            as.Date("2016-02-01"),
            as.Date("2016-03-15"),
            as.Date("2016-05-03"),
            as.Date("2016-06-20"),
            as.Date("2016-07-15"),
            as.Date("2016-08-17"),
            as.Date("2016-10-07"),
            as.Date("2016-11-08"),
            as.Date("2017-01-20"),
            as.Date("2017-02-13"),
            as.Date("2017-05-17"),
            as.Date("2017-07-28"),
            as.Date("2017-08-18"),
            as.Date("2017-09-20")),
       labels=c("Apprentice", "Apprentice", "Apprentice",
                "Birther", "Apprentice", "Election",
                "Apprentice", "Apprentice", "Declares",
                "GOP Deb 1", "GOP Deb 3", "Iowa",
                "Sup Tue 2", "Nominee", "Manafort",
                "Pence", "Bannon In", "AH Tape",
                "Election", "Inaguration", "Flynn",
                "Mueller", "Kelly", "Bannon Out",
                "Hurricane"))

  # Add Year Ticks with Labels (X)

  axis(side=1, tick = TRUE, cex.axis=1,
       at=c(as.Date("2010-01-01"),
            as.Date("2011-01-01"),
            as.Date("2012-01-01"),
            as.Date("2013-01-01"),
            as.Date("2014-01-01"),
            as.Date("2015-01-01"),
            as.Date("2016-01-01"),
            as.Date("2017-01-01"),
            as.Date("2018-01-01")),
       labels=c("2010", "2011", "2012",
                "2013", "2014", "2015",
                "2016", "2017", "2018"))

# Add Legend

  legend("topright",
         legend = c("Conversational",
                    "Campaigning",
                    "Engaged",
                    "Advisory"),
         col = c(bluec,redc,greenc,orangec),
         text.col = c(bluec,redc,greenc,orangec),
         bty="n",
         cex=1)

dev.off()

# generate the sixth figure for the paper, which plots Dimension 1 against tweet length, showing a very strong correlation (see section 7.1 above), with Tweets sent before and after the length shift highlighted in blue and red.

color <- c()
color[as.Date(tweets$DATE) >=
        as.Date("2017-10-07")] <- redc
color[as.Date(tweets$DATE) <
        as.Date("2017-10-07")] <- bluec


tiff("./OUTPUT/FIG/Fig6.tiff", width=2700, height=2500, res=250)
  par(mar=c(4.5, 5, 6, 2),family="avenir")
  plot(tweets$WORDCOUNT,coordind$`Dim 1`,
       pch="-", cex=3.95, cex.lab = 1.15,
       col=adjustcolor(color, alpha.f =.7),
       ylab="Dimension 1 Coordinate",
       xlab="Total Words")
  mtext(text=expression(bold(
    "Dimension 1 Tweet Coordinate vs Tweet Length")),
        side=3, line=2, las=1, cex=1.5)
  legend("topleft",
         legend = c("140 Characters Max",
                    "280 Characters Max"),
         col = c(bluec,redc),
         text.col = c(bluec,redc),
         bty="n",
         cex=1.15)
dev.off()
