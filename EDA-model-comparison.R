## Network Modeling
library(stringr)
library(tidyverse)
library(igraph)
library(ROCR)
library(amen)
load("Data/bso.rda")


#########
## EDA ##
#########

## Number of unique concerts
length(unique(bso[-which(bso$Season == "2018-2019"), ]$Concert))
length(unique(bso[-which(bso$Season == "2018-2019"), ]$Composer))
length(unique(bso[-which(bso$Season == "2018-2019"), ]$Conductor))



## Counts of info in data
bso %>% group_by(Composer) %>% tally(sort = TRUE) 
length(unique(bso$Composer))

length(unique(bso$Work))

bso %>% group_by(Soloist) %>% tally(sort = TRUE) 
length(unique(bso$Soloist))

bso %>% group_by(Instrument) %>% tally(sort = TRUE) 
length(unique(bso$Instrument))

bso %>% group_by(Conductor) %>% tally(sort = TRUE) 
length(unique(bso$Conductor))

bso %>% group_by(Orchestra) %>% tally(sort = TRUE) 

bso %>% group_by(Venue) %>% tally(sort = TRUE) 


bso %>% group_by(Additional) %>% tally(sort = TRUE) 
length(unique(bso$Additional))

bso %>% group_by(Type) %>% tally(sort = TRUE) 

bso %>% group_by(Nationality) %>% tally(sort = TRUE) 
length(unique(bso$Nationality))

bso %>% group_by(Region) %>% tally(sort = TRUE) 
length(unique(bso$Region))

bso %>% group_by(DOB) %>% tally(sort = TRUE) 
length(unique(bso$DOB))

bso %>% group_by(Era) %>% tally(sort = TRUE) 
length(unique(bso$Era))


## Create i-graph Object

relations.bso <- list() #matrix(0, nrow = 10000, ncol = 4)
count <- 1
for(concert in unique(bso$Concert)){
  
  ## Select relevant rows
  sub.df <- bso[which(bso$Concert == concert), ]
  
  ## Add relevant edges to graph
  c.list <- sub.df$Composer
  n <- length(c.list)
  
  if(n == 1){
    sub.mat <- matrix(0, nrow = n, ncol = 4)
    sub.mat[1, ] <- c(c.list, c.list, 1, as.numeric(str_sub(sub.df$Date[1],-4,-1)))
  }
  
  
  else{
    sub.mat <- matrix(0, nrow = choose(n, 2), ncol = 4)
    count2 <- 1
    for(i in 1:(n-1)){
      for(j in (i+1):n){
        sub.mat[count2, 1] <- c.list[i]
        sub.mat[count2, 2] <- c.list[j]
        count2 <- count2 + 1
      }
    }
    sub.mat[, 3] <- 1
    sub.mat[, 4] <- as.numeric(str_sub(sub.df$Date[1],-4,-1))
  }

  relations.bso[[count]] <- sub.mat
  count <- count + 1
  
  if(count%%100 == 0){
    print(count)
  }
}

rel.bso <- data.frame(do.call("rbind", relations.bso))
colnames(rel.bso) <- c("from", "to", "number", "year")


## Ignore Year
rel.df <- rel.bso[,-4] %>% group_by_all() %>% summarise(COUNT = n())


g <- graph_from_data_frame(rel.df, directed=FALSE)
print(g, e=TRUE, v=TRUE)

plot(g, layout=layout.fruchterman.reingold(g,niter=1000),
     vertex.label = ifelse(degree(g) > 20, V(g), NA), vertex.size = 0)

plot(g, layout=layout_in_circle(g),
     vertex.label = ifelse(degree(g) > 20, V(g), NA), vertex.size = 0)


plot(g, vertex.color="gold", vertex.size=5, 
     vertex.frame.color="gray", vertex.label.color="black", 
     vertex.label.cex=0.5, vertex.label.dist=.2, edge.curved=0.2)

sgdf.copy <- delete.vertices(g, 
                             V(g)[-order(degree(g), decreasing = TRUE)[1:50]])
set.seed(17)
plot(sgdf.copy, vertex.color="gold", vertex.size=5, 
     vertex.frame.color="gray", vertex.label.color="black", 
     vertex.label.cex=0.5, vertex.label.dist=.5, edge.curved=0.2,
     layout = layout.fruchterman.reingold(sgdf.copy),
     rescale = FALSE, ylim=c(-2,2),xlim=c(-2,2), asp = 0)

plot(sgdf.copy, vertex.color="gold", vertex.size=5, 
     vertex.frame.color="gray", vertex.label.color="black", 
     vertex.label.cex=0.5, vertex.label.dist=-.05, edge.curved=0.2,
     layout = layout.graphopt(sgdf.copy))



## Nodes with Highest Degree

sort(degree(g), decreasing = TRUE)[1:50]

V(g)[order(edge_betweenness(g, directed = FALSE))]

sort(closeness(g), decreasing = TRUE)


neighbors(g, v = "Wolfgang Amadeus Mozart")
neighbors(g, v = "Ludwig van Beethoven")
neighbors(g, v = "Pyotr Ilyich Tchaikovsky")
neighbors(g, v = "Antonin Dvorák")
neighbors(g, v = "Maurice Ravel")
neighbors(g, v = "Leonard Bernstein")
neighbors(g, v = "John Harbison")
neighbors(g, v = "Max Bruch")
neighbors(g, v = "Thomas Adès")

eb <- cluster_edge_betweenness(g, directed = FALSE)
lb <- cluster_louvain(g)
le <- cluster_leading_eigen(g)
as.hclust(lb)
as.dendrogram(eb)
plot(as.dendrogram(le))





##############
## Modeling ##
##############

# Train: Seasons 1999-2000 through 2016-2017
# Test: Seasons 2017-2018 and 2018-2019 (up to March)

## (1) Logistic Regression - ignore network structure



relations.bso <- list() #matrix(0, nrow = 10000, ncol = 4)
count <- 1
for(concert in unique(bso$Concert)){
  
  ## Select relevant rows
  sub.df <- bso[which(bso$Concert == concert), ]
  
  ## Add relevant edges to graph
  c.list <- sub.df$Composer
  n <- nrow(sub.df)
  
  if(n == 1){
    sub.mat <- data.frame(matrix(0, nrow = n, ncol = 25))
    sub.mat[1, ] <- c(c.list, sub.df$Work[1], c.list, sub.df$Work[1], 
                      sub.df$Soloist[1], NA, sub.df$Instrument[1], NA,
                      sub.df$Conductor, sub.df$Orchestra, sub.df$Venue,
                      sub.df$Additional[1], NA, 
                      as.numeric(substr(sub.df$Date[1], start = 1, stop = 2)),
                      sub.df$Season, sub.df$Type[1], NA, sub.df$Nationality[1], NA,
                      sub.df$Region[1], NA, sub.df$DOB[1], NA, sub.df$Era[1], NA)
  }
  
  
  else{
    sub.mat <- data.frame(matrix(0, nrow = choose(n, 2), ncol = 25))
    count2 <- 1
    for(i in 1:(n-1)){
      for(j in (i+1):n){
        sub.mat[count2, ] <- c(c.list[i], sub.df$Work[i], c.list[j], 
                               sub.df$Work[j], 
                                sub.df$Soloist[i], sub.df$Soloist[j], 
                                sub.df$Instrument[i], sub.df$Instrument[j],
                                sub.df$Conductor[1], sub.df$Orchestra[1], 
                                sub.df$Venue[1],
                                sub.df$Additional[i], sub.df$Additional[j], 
                                as.numeric(substr(sub.df$Date[1], 
                                                  start = 1, stop = 2)),
                                sub.df$Season[1], 
                                sub.df$Type[i], sub.df$Type[j], 
                                sub.df$Nationality[i], sub.df$Nationality[j],
                                sub.df$Region[i], sub.df$Region[j], 
                                sub.df$DOB[i], sub.df$DOB[j], 
                                sub.df$Era[i], sub.df$Era[j])
        count2 <- count2 + 1
      }
    }
  }
  
  relations.bso[[count]] <- sub.mat
  count <- count + 1
  
  if(count%%100 == 0){
    print(count)
  }
}

lm.df <- data.frame(do.call("rbind", relations.bso))
colnames(lm.df) <- c("Composer1", "Work1", "Composer2", "Work2",
                     "Soloist1", "Soloist2",
                     "Instrument1", "Instrument2", "Conductor", "Orchestra",
                     "Venue", "Additional1", "Additional2", "Month", 
                     "Season", "Type1", "Type2", "Nationality1", "Nationality2",
                     "Region1", "Region2", "DOB1", "DOB2", "Era1", "Era2")
lm.df$Y <- 1

## Binary Random Variables
lm.df$Soloist1 <- if_else(is.na(lm.df$Soloist1), 0, 1)
lm.df$Soloist2 <- if_else(is.na(lm.df$Soloist2), 0, 1)
lm.df$Additional1 <- if_else(is.na(lm.df$Additional1), 0, 1)
lm.df$Additional2 <- if_else(is.na(lm.df$Additional2), 0, 1)

## No instruments to None
lm.df$Instrument1 <- if_else(is.na(lm.df$Instrument1), "NA", lm.df$Instrument1)
lm.df$Instrument2 <- if_else(is.na(lm.df$Instrument2), "NA", lm.df$Instrument2)


## Season to seasons since 1999-2000
lm.df$Season <- as.numeric(substr(lm.df$Season, start = 1, stop = 4)) - 1999

## String Trim
lm.df$Composer1 <-  str_trim(lm.df$Composer1, side = "both")
lm.df$Composer2 <-  str_trim(lm.df$Composer2, side = "both")
lm.df$Work1 <-  str_trim(lm.df$Work1, side = "both")
lm.df$Work2 <-  str_trim(lm.df$Work2, side = "both")
lm.df$Instrument1 <-  str_trim(lm.df$Instrument1, side = "both")
lm.df$Instrument2 <-  str_trim(lm.df$Instrument2, side = "both")
lm.df$Conductor <-  str_trim(lm.df$Conductor, side = "both")
lm.df$Orchestra <-  str_trim(lm.df$Orchestra, side = "both")
lm.df$Venue <-  str_trim(lm.df$Venue, side = "both")
lm.df$Type1 <-  str_trim(lm.df$Type1, side = "both")
lm.df$Type2 <-  str_trim(lm.df$Type2, side = "both")
lm.df$Nationality1 <-  str_trim(lm.df$Nationality1, side = "both")
lm.df$Nationality2 <-  str_trim(lm.df$Nationality2, side = "both")
lm.df$Region1 <-  str_trim(lm.df$Region1, side = "both")
lm.df$Region2 <-  str_trim(lm.df$Region2, side = "both")
lm.df$Era1 <-  str_trim(lm.df$Era1, side = "both")
lm.df$Era2 <-  str_trim(lm.df$Era2, side = "both")
 
## Expand to add 0's by season
s0.df <- lm.df %>%
  filter(Season == 0)
s0.df.full <- lm.df %>%
  filter(Season == 0) %>%
  complete(Work1, Work2) 


for(i in 1:nrow(s0.df.full)){
  if(is.na(s0.df.full[i,3])){ ## Concert that doesn't occur
    ind1 <- which(s0.df$Work1 == s0.df.full[i,]$Work1)[1]
    ind2 <- which(s0.df$Work2 == s0.df.full[i,]$Work2)[1]
    
    ## Fill piece 1
    ii1 <- c(3, 5, 7, 9:12, 14:16, 18, 20,22, 24)
    s0.df.full[i, ii1] <- c(s0.df[ind1, ]$Composer1, s0.df[ind1, ]$Soloist1,
                            s0.df[ind1, ]$Instrument1, s0.df[ind1, ]$Conductor,
                            s0.df[ind1, ]$Orchestra, s0.df[ind1, ]$Venue,
                            s0.df[ind1, ]$Additional1, s0.df[ind1, ]$Month,
                            s0.df[ind1, ]$Season, s0.df[ind1, ]$Type1,
                            s0.df[ind1, ]$Nationality1, s0.df[ind1, ]$Region1,
                            s0.df[ind1, ]$DOB1, s0.df[ind1, ]$Era1)
    
    ## Fill Piece 2
    ii2 <- c(4, 6, 8, 13, 17, 19, 21, 23, 25)
    s0.df.full[i, ii2] <- c(s0.df[ind2, ]$Composer2, s0.df[ind2, ]$Soloist2,
                            s0.df[ind2, ]$Instrument2, s0.df[ind2, ]$Additional2,
                            s0.df[ind2, ]$Type2, s0.df[ind2, ]$Nationality2,
                            s0.df[ind2, ]$Region2, s0.df[ind2, ]$DOB2,
                            s0.df[ind2, ]$Era2)
    
    ## Fill Y Value
    s0.df.full[i, ]$Y <- 0
  }
}


fill.df <- function(full.df, sub.df) {
  for(i in 1:nrow(full.df)){
    if(is.na(full.df[i,3])){ ## Concert that doesn't occur
      ind1 <- which(sub.df$Work1 == full.df[i,]$Work1)[1]
      ind2 <- which(sub.df$Work2 == full.df[i,]$Work2)[1]
      
      ## Fill piece 1
      ii1 <- c(3, 5, 7, 9:12, 14:16, 18, 20,22, 24)
      full.df[i, ii1] <- c(sub.df[ind1, ]$Composer1, sub.df[ind1, ]$Soloist1,
                              sub.df[ind1, ]$Instrument1, sub.df[ind1, ]$Conductor,
                              sub.df[ind1, ]$Orchestra, sub.df[ind1, ]$Venue,
                              sub.df[ind1, ]$Additional1, sub.df[ind1, ]$Month,
                              sub.df[ind1, ]$Season, sub.df[ind1, ]$Type1,
                              sub.df[ind1, ]$Nationality1, sub.df[ind1, ]$Region1,
                              sub.df[ind1, ]$DOB1, sub.df[ind1, ]$Era1)
      
      ## Fill Piece 2
      ii2 <- c(4, 6, 8, 13, 17, 19, 21, 23, 25)
      full.df[i, ii2] <- c(sub.df[ind2, ]$Composer2, sub.df[ind2, ]$Soloist2,
                              sub.df[ind2, ]$Instrument2, sub.df[ind2, ]$Additional2,
                              sub.df[ind2, ]$Type2, sub.df[ind2, ]$Nationality2,
                              sub.df[ind2, ]$Region2, sub.df[ind2, ]$DOB2,
                              sub.df[ind2, ]$Era2)
      
      ## Fill Y Value
      full.df[i, ]$Y <- 0
    }
  }
  return(full.df)
}


## Expand to add 0's by season
s1.df <- lm.df %>%
  filter(Season == 1)
s1.df.full <- lm.df %>%
  filter(Season == 1) %>%
  complete(Work1, Work2) 

s1.df.full <- fill.df(s1.df.full, s1.df) 


seasons.list <- list()
seasons.list[[1]] <- s0.df.full
seasons.list[[2]] <- s1.df.full


for(i in 2:19){
  s.df <- lm.df %>%
    filter(Season == i)
  s.df.full <- lm.df %>%
    filter(Season == i) %>%
    complete(Work1, Work2) 
  seasons.list[[i+1]] <- fill.df(s.df.full, s.df) 
  print(i)
}



bso.lm.df <- data.frame(do.call("rbind", seasons.list))

## Convert Instruments to more generic
bso.lm.df$Instrument1[bso.lm.df$Instrument1 %in% 
                        c("Soprano", "Baritone", "Counter Tenor",
                          "Mezzo Soprano","Bass-Baritone", "Singer",
                          "Tenor")] <- "Voice"
bso.lm.df$Instrument1[bso.lm.df$Instrument1 %in% 
                       c("SATB Chorus", "Ensemble", "Vocal Ensemble",
                         "Chorus", "Women's Chorus", "Children's Choir",
                         "Unknown Tanglewood Music Center Vocal Fellows",
                         "Boys Choir")] <- "Chorus"
bso.lm.df$Instrument1[bso.lm.df$Instrument1 %in% 
                       c("Flute", "Oboe", "Clarinet", "Horn",
                         "Trumpet", "Bassoon", "Horn Shi-Yeon Sung",
                         "English Horn", "Piccolo", "Trombone")] <- "Winds"
bso.lm.df$Instrument1[bso.lm.df$Instrument1 %in% 
                       c("Cello", "Violin", "Contrabass", "Viola")] <- "Strings"
bso.lm.df$Instrument1[bso.lm.df$Instrument1 %in% 
                       c("Harp", "Percussion", "Guitar", 
                         "Harp Shi-Yeon Sung", "Timpani",
                         "Unknown Members of Emmanuel Music", "Jazz Band",
                         "Jazz Trio", "Marimba")] <- "Other Instrument"
bso.lm.df$Instrument1[bso.lm.df$Instrument1 %in% 
                        c("Conductor", "Narrator", "Conductor Ann Hobson Pilot",
                          "Orchestra John Williams", 
                          "Conductor BSO Boston Symphony Orchestra",
                          "Conductor Tanglewood Music Center Orchestra",
                          "Orchestra", "Conductor TMCO TMC Orchestra" )] <- "Conductor"

bso.lm.df$Instrument2[bso.lm.df$Instrument2 %in% 
                        c("Soprano", "Baritone", "Counter Tenor",
                          "Mezzo Soprano","Bass-Baritone", "Singer",
                          "Tenor")] <- "Voice"
bso.lm.df$Instrument2[bso.lm.df$Instrument2 %in% 
                        c("SATB Chorus", "Ensemble", "Vocal Ensemble",
                          "Chorus", "Women's Chorus", "Children's Choir",
                          "Unknown Tanglewood Music Center Vocal Fellows",
                          "Boys Choir")] <- "Chorus"
bso.lm.df$Instrument2[bso.lm.df$Instrument2 %in% 
                        c("Flute", "Oboe", "Clarinet", "Horn",
                          "Trumpet", "Bassoon", "Horn Shi-Yeon Sung",
                          "English Horn", "Piccolo", "Trombone")] <- "Winds"
bso.lm.df$Instrument2[bso.lm.df$Instrument2 %in% 
                        c("Cello", "Violin", "Contrabass", "Viola")] <- "Strings"
bso.lm.df$Instrument2[bso.lm.df$Instrument2 %in% 
                        c("Harp", "Percussion", "Guitar", 
                          "Harp Shi-Yeon Sung", "Timpani",
                          "Unknown Members of Emmanuel Music", "Jazz Band",
                          "Jazz Trio", "Marimba", "Bagpipes",
                          "Horn Jason Snider")] <- "Other Instrument"
bso.lm.df$Instrument2[bso.lm.df$Instrument2 %in% 
                        c("Conductor", "Narrator", "Conductor Ann Hobson Pilot",
                          "Orchestra John Williams", 
                          "Conductor BSO Boston Symphony Orchestra",
                          "Conductor Tanglewood Music Center Orchestra",
                          "Orchestra", "Conductor TMCO TMC Orchestra" ,
                          "Vocalist Andris Nelsons")] <- "Conductor"

## Venue
bso.lm.df$Venue[bso.lm.df$Venue %in% 
                  unique(bso.lm.df$Venue)[4:97]] <- "Other"


## as.numeric
bso.lm.df$Month <- as.numeric(bso.lm.df$Month)
bso.lm.df$Season <- as.numeric(bso.lm.df$Season)
bso.lm.df$DOB1 <- as.numeric(bso.lm.df$DOB1)
bso.lm.df$DOB2 <- as.numeric(bso.lm.df$DOB2)
bso.lm.df$Soloist1 <- as.numeric(bso.lm.df$Soloist1)
bso.lm.df$Soloist2 <- as.numeric(bso.lm.df$Soloist2)
bso.lm.df$Additional1 <- as.numeric(bso.lm.df$Additional1)
bso.lm.df$Additional2 <- as.numeric(bso.lm.df$Additional2)

save(bso.lm.df, file = "Data/bso-lm.rda")

## Split Train-Test (Random)
set.seed(17)

## Select all programmed performances

## Test on 2018-2019 season
inds.test <- which(bso.lm.df$Season == 19)
inds.train <- which(bso.lm.df$Season %in% 0:18)
train.df.full <- bso.lm.df[inds.train, ]
test.df.full <- bso.lm.df[inds.test, ]

inds.yes.train <- which(bso.lm.df[inds.train,]$Y == 1)
inds.no.train <- sample(1:nrow(train.df.full)[-inds.yes.train], 
                        length(inds.yes.train)*10)
inds.yes.test <- which(bso.lm.df[inds.test,]$Y == 1)
inds.no.test <- sample(1:nrow(test.df.full)[-inds.yes.test], 
                        length(inds.yes.test)*10)

test.df <- test.df.full[, 5:26]
train.df <- train.df.full[c(inds.yes.train, inds.no.train), 5:26]
nrow(test.df)/nrow(train.df)

length(which(train.df$Y == 1))/nrow(train.df)



## Perform Frequentist Logistic Regression
log.lm <- glm(Y ~ ., data = train.df, family = "binomial")
summary(log.lm)



## Check test set performance
test.df <- test.df[-which(test.df$Conductor == "Hannu Lintu"), ]
#test.df <- test.df[-which(test.df$Nationality1 == "Other"), ]
#test.df <- test.df[-which(test.df$Nationality2 == "Other"), ]
lm.prob <- predict(log.lm, test.df, type="response")


pred <- prediction(lm.prob, test.df$Y);

# Recall-Precision curve             
RP.perf <- performance(pred, "prec", "rec");

plot(RP.perf);

# ROC curve
ROC.perf <- performance(pred, "tpr", "fpr");
plot (ROC.perf);

p.df <- data.frame(FPR = ROC.perf@x.values[[1]], TPR = ROC.perf@y.values[[1]])
ggplot(p.df, aes(x = FPR, y = TPR)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = 2)

# ROC area under the curve
auc.tmp <- performance(pred,"auc");
auc <- as.numeric(auc.tmp@y.values)
print(auc)


#########################
## (2) Network Models ##
########################


## Ignore Year

rel.df.train <- rel.bso[which(rel.bso$year %in% c(2000:2018)),-4] %>% 
  group_by_all() %>% summarise(COUNT = n())
g.train <- graph_from_data_frame(rel.df.train, directed=FALSE)
g.adj.train <- as_adjacency_matrix(g.train, sparse = FALSE)

rel.df.test <- rel.bso[which(rel.bso$year == 2019),-4] %>% 
  group_by_all() %>% summarise(COUNT = n())
g.test <- graph_from_data_frame(rel.df.test, directed=FALSE)
g.adj.test <- as_adjacency_matrix(g.test, sparse = FALSE)

## Form nodal covariates dataframe
Xn.train <- data.frame(matrix(0, nrow = dim(g.adj.train)[1], ncol = 8))

count <- 1
for(comp in colnames(g.adj.train)){
  Xn.train[count, 1:4] <- bso[which(bso$Composer == comp)[1], 13:16]
  types.music <- unique(bso[which(bso$Composer == comp), ]$Type)
  if("Symphony" %in% types.music){
    Xn.train[count, 5] <- 1
  }
  if("Concerto" %in% types.music){
    Xn.train[count, 6] <- 1
  }
  if("Overture" %in% types.music){
    Xn.train[count, 7] <- 1
  }
  if("Other" %in% types.music){
    Xn.train[count, 8] <- 1
  }
  count <- count + 1 
}
colnames(Xn.train) <- c("Nationality", "Region", "DOB", "Era", 
                  "Symphony", "Concerto", "Overture", "Other")


## Test
Xn.test <- data.frame(matrix(0, nrow = dim(g.adj.test)[1], ncol = 8))

count <- 1
for(comp in colnames(g.adj.test)){
  Xn.test[count, 1:4] <- bso[which(bso$Composer == comp)[1], 13:16]
  types.music <- unique(bso[which(bso$Composer == comp), ]$Type)
  if("Symphony" %in% types.music){
    Xn.test[count, 5] <- 1
  }
  if("Concerto" %in% types.music){
    Xn.test[count, 6] <- 1
  }
  if("Overture" %in% types.music){
    Xn.test[count, 7] <- 1
  }
  if("Other" %in% types.music){
    Xn.test[count, 8] <- 1
  }
  count <- count + 1 
}
colnames(Xn.test) <- c("Nationality", "Region", "DOB", "Era", 
                        "Symphony", "Concerto", "Overture", "Other")


#Xn.train.lm <- Xn.train
#Xn.train.lm$Y <- rnorm(nrow(Xn.train.lm))
#Xn.train.lm$DOB <- as.numeric(Xn.train.lm$DOB )
#train.lm <- lm(Y ~.-1, data = Xn.train.lm)
#model.matrix(train.lm)


Xn.train[, 1] <- as.factor(Xn.train[, 1])
Xn.train[, 2] <- as.factor(Xn.train[, 2])
Xn.train[, 4] <- as.factor(Xn.train[, 4])
Xn.train <- data.matrix(Xn.train)
Xn.train[which(is.na(Xn.train))] <- 0 ## Set NA DOB to 0

#Xn.train$DOB <- as.numeric(Xn.train$DOB)
#Xn.train[which(is.na(Xn.train))] <- 0 ## Set NA DOB to 0
#Xn.train <- data.frame(Xn.train, stringsAsFactors = TRUE)
#rownames(Xn.train) <- colnames(g.adj.train)

## Model Progressions
fit_SRM <- ame(g.adj.train, model="bin", symmetric = TRUE)
fit_SRG <- ame(g.adj.train, model="bin", rvar=FALSE, cvar=FALSE, dcor=FALSE,
               symmetric = TRUE)
fit_SRRM <- ame(g.adj.train, Xr=Xn.train,  model="bin", symmetric = TRUE)
summary(fit_SRRM)

## Assume lm model, no network structyre
fit_rm<-ame(g.adj.train, Xr=Xn.train,  model="bin", symmetric = TRUE,
            rvar=FALSE,cvar=FALSE,dcor=FALSE)



## Random Effects for nodes
fit_SRRM.RF <- ame(g.adj.train, Xr=Xn.train,  model="bin", symmetric = TRUE,
                nvar = TRUE)
summary(fit_SRRM.RF)


## Multiplicative Effects
fit_AME<-ame(g.adj.train, Xr=Xn.train,  model="bin", symmetric = TRUE, R = 3)
summary(fit_AME)

fit_AME.RF <- ame(g.adj.train, Xr=Xn.train, symmetric = TRUE,
                       model="bin",  R = 3, nvar = TRUE)
summary(fit_AME.RF)

#fit_AME2<-ame(g.adj.train, Xr=Xn.train,  model="bin", symmetric = TRUE, R = 5)
#summary(fit_AME2)

#################################
## 3. Asymmetric Network Model ##
#################################

## Create i-graph Object

relations.bso <- list() #matrix(0, nrow = 10000, ncol = 4)
count <- 1
for(concert in unique(bso$Concert)){
  
  ## Select relevant rows
  sub.df <- bso[which(bso$Concert == concert), ]
  
  ## Add relevant edges to graph
  c.list <- sub.df$Composer
  n <- length(c.list)
  
  if(n == 1){
    sub.mat <- matrix(0, nrow = n, ncol = 4)
    sub.mat[1, ] <- c(c.list, c.list, 1, as.numeric(str_sub(sub.df$Date[1],-4,-1)))
  }
  
  
  else{
    sub.mat <- matrix(0, nrow = n-1, ncol = 4)
    count2 <- 1
    for(i in 1:(n-1)){
      sub.mat[count2, 1] <- c.list[i]
      sub.mat[count2, 2] <- c.list[i+1]
      count2 <- count2 + 1
    }
    sub.mat[, 3] <- 1
    sub.mat[, 4] <- as.numeric(str_sub(sub.df$Date[1],-4,-1))
  }
  
  relations.bso[[count]] <- sub.mat
  count <- count + 1
  
  if(count%%100 == 0){
    print(count)
  }
}

rel.bso <- data.frame(do.call("rbind", relations.bso))
colnames(rel.bso) <- c("from", "to", "number", "year")


## Ignore Year
rel.df <- rel.bso[,-4] %>% group_by_all() %>% summarise(COUNT = n())


g <- graph_from_data_frame(rel.df, directed=TRUE)
print(g, e=TRUE, v=TRUE)

## Ignore Year

rel.df.train <- rel.bso[which(rel.bso$year %in% c(2000:2018)),-4] %>% 
  group_by_all() %>% summarise(COUNT = n())
g.train.asym <- graph_from_data_frame(rel.df.train, directed=TRUE)
g.adj.train.asym <- as_adjacency_matrix(g.train.asym, sparse = FALSE)

rel.df.test <- rel.bso[which(rel.bso$year == 2019),-4] %>% 
  group_by_all() %>% summarise(COUNT = n())
g.test.asym <- graph_from_data_frame(rel.df.test, directed=TRUE)
g.adj.test.asym <- as_adjacency_matrix(g.test.asym, sparse = FALSE)


## Form nodal covariates dataframe
Xn.train.asym <- data.frame(matrix(0, nrow = dim(g.adj.train.asym)[1], ncol = 8))

count <- 1
for(comp in colnames(g.adj.train)){
  Xn.train.asym[count, 1:4] <- bso[which(bso$Composer == comp)[1], 13:16]
  types.music <- unique(bso[which(bso$Composer == comp), ]$Type)
  if("Symphony" %in% types.music){
    Xn.train.asym[count, 5] <- 1
  }
  if("Concerto" %in% types.music){
    Xn.train.asym[count, 6] <- 1
  }
  if("Overture" %in% types.music){
    Xn.train.asym[count, 7] <- 1
  }
  if("Other" %in% types.music){
    Xn.train.asym[count, 8] <- 1
  }
  

  count <- count + 1 
}
colnames(Xn.train.asym) <- c("Nationality", "Region", "DOB", "Era", 
                        "Symphony", "Concerto", "Overture", "Other")


## Test
Xn.test.asym <- data.frame(matrix(0, nrow = dim(g.adj.test.asym)[1], ncol = 8))

count <- 1
for(comp in colnames(g.adj.test)){
  Xn.test.asym[count, 1:4] <- bso[which(bso$Composer == comp)[1], 13:16]
  types.music <- unique(bso[which(bso$Composer == comp), ]$Type)
  if("Symphony" %in% types.music){
    Xn.test.asym[count, 5] <- 1
  }
  if("Concerto" %in% types.music){
    Xn.test.asym[count, 6] <- 1
  }
  if("Overture" %in% types.music){
    Xn.test.asym[count, 7] <- 1
  }
  if("Other" %in% types.music){
    Xn.test.asym[count, 8] <- 1
  }
  count <- count + 1 
}
colnames(Xn.test.asym) <- c("Nationality", "Region", "DOB", "Era", 
                       "Symphony", "Concerto", "Overture", "Other")


#Xn.train.lm <- Xn.train
#Xn.train.lm$Y <- rnorm(nrow(Xn.train.lm))
#Xn.train.lm$DOB <- as.numeric(Xn.train.lm$DOB )
#train.lm <- lm(Y ~.-1, data = Xn.train.lm)
#model.matrix(train.lm)

XN.train.asym <- Xn.train.asym[,-1]
XN.train.asym$Region[grep("South America|Asia|Austrailia", 
                          Xn.train.asym$Region)] <- "Other"
XN.train.asym$Era[grep("Renaissance|Baroque|Austrailia", 
                          Xn.train.asym$Era)] <- "Other"

XN.train.asym$Region <- factor(XN.train.asym$Region, 
                               levels = c("Other", "Europe", "North America"))
XN.train.asym <- data.frame(model.matrix(~Era + Region-1,XN.train.asym), 
                            XN.train.asym[, c(2,4:7)])


#XN.train.asym$Region <- as.factor(Xn.train.asym$Region)
#XN.train.asym$Region <- factor(XN.train.asym$Era, 
#                            levels = c("Renaissance", "Baroque",
#                                       "Classical", "Romantic", "Other",
#                                       "Modern"))
#XN.train.asym$Era <- factor(XN.train.asym$Era, 
#                                levels = c("Renaissance", "Baroque",
#                                           "Classical", "Romantic", "Other",
#                                           "Modern"))
#XN.train.asym$DOB <- as.numeric(XN.train.asym$DOB) - 1888
#XN.train.asym$DOB[which(is.na(XN.train.asym$DOB))] <- -1 ## Set NA DOB to 0
#XN.train.asym$DOB <- XN.train.asym$DOB/sd(XN.train.asym$DOB)
XN.train.asym <- data.matrix(XN.train.asym)#[,-1]
XN.train.asym[which(is.na(XN.train.asym))] <- 0 ## Set NA DOB to 0


#test <- XN.train.asym
#test <- cbind(test, rnorm(nrow(test)))
#solve(t(XN.train.asym)%*%XN.train.asym)
#solve(XN.train.asym%*%t(XN.train.asym))
#eigen(t(XN.train.asym)%*%XN.train.asym)$values


## Model Progressions
fit_SRM.asym <- ame(g.adj.train.asym, model="bin")
fit_SRG.asym <- ame(g.adj.train.asym, model="bin", rvar=FALSE, cvar=FALSE, dcor=FALSE)
fit_SRRM.asym <- ame(g.adj.train.asym, Xr=Xn.train.asym,  Xc = Xn.train.asym,
                     model="bin")
summary(fit_SRRM.asym)

## Assume lm model, no network structyre
fit_rm.asym <- ame(g.adj.train.asym, Xr=Xn.train.asym,  Xc=Xn.train.asym,
                   model="bin", rvar=FALSE,cvar=FALSE,dcor=FALSE)

## Random Effects for nodes
fit_SRRM.RF.asym <- ame(g.adj.train.asym, Xr=Xn.train.asym,  Xc=Xn.train.asym,
                        model="bin", rvar = TRUE, cvar = TRUE)
summary(fit_SRRM.RF.asym)


## Multiplicative Effects
fit_AME.asym <- ame(g.adj.train.asym, Xr = Xn.train.asym, Xc = Xn.train.asym,
                    model="bin",  R = 3)
summary(fit_AME.asym)

## Multiplicative Effects and Random Effects


fit_AME.RF.asym <- ame(g.adj.train.asym, Xr = XN.train.asym, 
                       Xc = XN.train.asym,
                    model="bin",  R = 3, rvar = TRUE, cvar = TRUE)
summary(fit_AME.RF.asym)




#####################
## Goodness of Fit ##
#####################
obs <- fit_SRG$GOF[1,]

sd.rowmean <- data.frame(SRG = fit_SRG$GOF[-1, 1],
                         SRM = fit_SRM$GOF[-1, 1],
                         SRRM = fit_SRRM$GOF[-1, 1],
                         LM = fit_rm$GOF[-1, 1],
                         AME = fit_AME.RF$GOF[-1, 1])
sd.colmean <- data.frame(SRG = fit_SRG$GOF[-1, 2],
                         SRM = fit_SRM$GOF[-1, 2],
                         SRRM = fit_SRRM$GOF[-1, 2],
                         LM = fit_rm$GOF[-1, 2],
                         AME = fit_AME.RF$GOF[-1, 2])
dyad.dep <- data.frame(SRG = fit_SRG$GOF[-1, 3],
                         SRM = fit_SRM$GOF[-1, 3],
                         SRRM = fit_SRRM$GOF[-1, 3],
                         LM = fit_rm$GOF[-1, 3],
                         AME = fit_AME.RF$GOF[-1, 3])
triad.dep <- data.frame(SRG = fit_SRG$GOF[-1, 4],
                         SRM = fit_SRM$GOF[-1, 4],
                         SRRM = fit_SRRM$GOF[-1, 4],
                         LM = fit_rm$GOF[-1, 4],
                         AME = fit_AME.RF$GOF[-1, 4])

dat.row <- melt(sd.rowmean)
colnames(dat.row) <- c("Model", "sd.rowmean")
p1 <- ggplot(dat.row, aes(x = Model, y = sd.rowmean, fill = Model)) +
  geom_boxplot() +
  geom_hline(yintercept = obs[1], linetype = 2) +
  ggtitle("Sd Row Mean")


dat.col <- melt(sd.colmean)
colnames(dat.col) <- c("Model", "sd.colmean")
p2 <- ggplot(dat.col, aes(x = Model, y = sd.colmean, fill = Model)) +
  geom_boxplot() +
  geom_hline(yintercept = obs[2], linetype = 2) +
  ggtitle("Sd Col Mean")

dat.dyad <- melt(dyad.dep)
colnames(dat.dyad) <- c("Model", "dyad.dep")
p3 <- ggplot(dat.dyad, aes(x = Model, y = dyad.dep, fill = Model)) +
  geom_boxplot() +
  geom_hline(yintercept = obs[3], linetype = 2) +
  ggtitle("Dyad Dependence")

dat.triad <- melt(triad.dep)
colnames(dat.triad) <- c("Model", "triad.dep")
p4 <- ggplot(dat.triad, aes(x = Model, y = triad.dep, fill = Model)) +
  geom_boxplot() +
  geom_hline(yintercept = obs[4], linetype = 2) +
  ggtitle("Triad Dependence")
grid.arrange(p1, p2, p3, p4, nrow = 2)

## Asymetric

obs.asym <- fit_SRG.asym$GOF[1,]

sd.rowmean.asym <- data.frame(SRG = fit_SRG.asym$GOF[-1, 1],
                         SRM = fit_SRM.asym$GOF[-1, 1],
                         SRRM = fit_SRRM.asym$GOF[-1, 1],
                         LM = fit_rm.asym$GOF[-1, 1],
                         AME = fit_AME.RF.asym$GOF[-1, 1])
sd.colmean.asym <- data.frame(SRG = fit_SRG.asym$GOF[-1, 2],
                         SRM = fit_SRM.asym$GOF[-1, 2],
                         SRRM = fit_SRRM.asym$GOF[-1, 2],
                         LM = fit_rm.asym$GOF[-1, 2],
                         AME = fit_AME.RF.asym$GOF[-1, 2])
dyad.dep.asym <- data.frame(SRG = fit_SRG.asym$GOF[-1, 3],
                       SRM = fit_SRM.asym$GOF[-1, 3],
                       SRRM = fit_SRRM.asym$GOF[-1, 3],
                       LM = fit_rm.asym$GOF[-1, 3],
                       AME = fit_AME.RF.asym$GOF[-1, 3])
triad.dep.asym <- data.frame(SRG = fit_SRG.asym$GOF[-1, 4],
                        SRM = fit_SRM.asym$GOF[-1, 4],
                        SRRM = fit_SRRM.asym$GOF[-1, 4],
                        LM = fit_rm.asym$GOF[-1, 4],
                        AME = fit_AME.RF.asym$GOF[-1, 4])



dat.row <- melt(sd.rowmean.asym)
colnames(dat.row) <- c("Model", "sd.rowmean")
p1 <- ggplot(dat.row, aes(x = Model, y = sd.rowmean, fill = Model)) +
  geom_boxplot() +
  geom_hline(yintercept = obs.asym[1], linetype = 2) +
  ggtitle("Sd Row Mean")


dat.col <- melt(sd.colmean.asym)
colnames(dat.col) <- c("Model", "sd.colmean")
p2 <- ggplot(dat.col, aes(x = Model, y = sd.colmean, fill = Model)) +
  geom_boxplot() +
  geom_hline(yintercept = obs.asym[2], linetype = 2) +
  ggtitle("Sd Col Mean")

dat.dyad <- melt(dyad.dep.asym)
colnames(dat.dyad) <- c("Model", "dyad.dep")
p3 <- ggplot(dat.dyad, aes(x = Model, y = dyad.dep, fill = Model)) +
  geom_boxplot() +
  geom_hline(yintercept = obs.asym[3], linetype = 2) +
  ggtitle("Dyad Dependence")

dat.triad <- melt(triad.dep.asym)
colnames(dat.triad) <- c("Model", "triad.dep")
p4 <- ggplot(dat.triad, aes(x = Model, y = triad.dep, fill = Model)) +
  geom_boxplot() +
  geom_hline(yintercept = obs.asym[4], linetype = 2) +
  ggtitle("Triad Dependence")
grid.arrange(p1, p2, p3, p4, nrow = 2)

fit_SRG$GOF
fit_SRM
fit_SRRM
fit_rm
fit_SRRM.RF
fit_AME
summary(fit_AME.RF.asym)


library(scales)
show_col(hue_pal()(4))

## Analyze AME.RF Asymmetric fit
p1 <- ggplot(data.frame(row.RF = fit_AME.RF.asym$APM), aes(x = row.RF))+
        geom_histogram(bins = 50, fill = "#F8766D") +
        labs(x = "Posterior Means") + 
        xlim(-1,1.5) + 
        geom_vline(xintercept = 0, linetype = 2) + 
        ggtitle("Row Random Effects")
p2 <- ggplot(data.frame(col.RF = fit_AME.RF.asym$BPM), aes(x = col.RF))+
  geom_histogram(bins = 50, fill = "#00BFC4") +
  labs(x = "Posterior Means") + 
  geom_vline(xintercept = 0, linetype = 2) +
  xlim(-1,1.5) +
  ggtitle("Column Random Effects")

grid.arrange(p1, p2, nrow = 2)


row.large <- sort(fit_AME.RF.asym$APM, decreasing = TRUE)[1:10]
row.small <- sort(fit_AME.RF.asym$APM, decreasing = FALSE)[1:10]

col.large <- sort(fit_AME.RF.asym$BPM, decreasing = TRUE)[1:10]
col.small <- sort(fit_AME.RF.asym$BPM, decreasing = FALSE)[1:10]

row.df <- data.frame(composers.large = names(row.large), 
                     row.large = round(row.large, 3),
                     composers.small = names(row.small), 
                     row.small = round(row.small, 3))
rownames(row.df) <- 1:10
col.df <- data.frame(composers.large = names(col.large), 
                     col.large = round(col.large, 3),
                     composers.small = names(col.small), 
                     col.small = round(col.small, 3))
rownames(col.df) <- 1:10

top.c <- bso %>% group_by(Composer) %>% tally(sort = TRUE) %>% 
  select(Composer) 

inds <- rep(0, 20)
for(i in 1:20){
  inds[i] <- grep(top.c[i,], colnames(fit_AME.RF.asym$UVPM))
}



ggplot(data = melt(fit_AME.RF.asym$UVPM[inds[1:10], inds[1:10]]), 
       aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_gradient2(low="darkgreen", high="darkblue", guide="colorbar") +
  labs(x = "Composers - Senders", y = "Composers - Receivers",
       fill = "UV Posterior Mean")


## Cluster multiplicative effects
library(factoextra)
k.U <- kmeans(fit_AME.RF.asym$U, centers = 5, nstart = 25)
k.V <- kmeans(fit_AME.RF.asym$V, centers = 5, nstart = 25)


fviz_cluster(k.U, data = fit_AME.RF.asym$U, geom = "point") #geom = "point"
c1.U <- names(which(k.U$cluster == 1))
c2.U <- names(which(k.U$cluster == 2))
c3.U <- names(which(k.U$cluster == 3))
c4.U <- names(which(k.U$cluster == 4))
c5.U <- names(which(k.U$cluster == 5))

fviz_cluster(k.V, data = fit_AME.RF.asym$V, geom = "point") #geom = "point"
c1.V <- names(which(k.V$cluster == 1))
c2.V <- names(which(k.V$cluster == 2))
c3.V <- names(which(k.V$cluster == 3))
c4.V <- names(which(k.V$cluster == 4))
c5.V <- names(which(k.V$cluster == 5))


## Variance Parameters
library('latex2exp')
post.var <- data.frame(fit_AME.RF.asym$VC[,-5])
dat.var <- melt(post.var)
colnames(dat.var) <- c("Parameter", "Samples")
ggplot(dat.var, aes(x = Parameter, y = Samples, fill = Parameter))+
  geom_boxplot() +
  labs(y = "Posterior Variance Samples") +
  scale_x_discrete(labels=c('va'=parse(text = TeX('$\\sigma^2_a$')),
                            'cab'=parse(text = TeX('$\\sigma_{ab}$')),
                            'vb'=parse(text = TeX('$\\sigma^2_b$')),
                            'rho'=parse(text = TeX('$\\rho$')))) +
  guides(fill=FALSE)


## Regression Coefficients

post.beta <- data.frame(fit_AME.RF.asym$BETA[,-1])
dat.beta <- melt(post.beta)
colnames(dat.beta) <- c("Coefficient", "Samples")
ggplot(dat.beta, aes(x = Coefficient, y = Samples, fill = Coefficient))+
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = 2) + 
  labs(y = "Posterior  Samples") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill=FALSE)


summary(fit_AME.RF.asym)



paste0(c1.U[c(1,4,13,14,15,16,17,19,24, 35, 38,47)], collapse = ", ")
paste0(c2.U[c(8,10,11,16,20,37,38,45,49,50,52,54,56, 57,58,59)], collapse = ", ")
paste0(c3.U[c(5, 7, 8, 9,11, 12, 27, 31,32,33,34,35,38, 39,58,65,66,67,75)], 
       collapse = ", ")
paste0(c4.U[c(2, 23, 24, 29, 30, 33)], collapse = ", ")
paste0(c5.U[c(9, 20,42, 64, 81, 82, 83, 86, 87, 88, 89)], collapse = ", ")
print(c(length(c1.U), length(c2.U), length(c3.U), length(c4.U), length(c5.U)))


## Barplot

pop.composers <- data.frame(Composer = names(sort(rowSums(g.adj.train), 
                                                  decreasing = TRUE)[1:20]),
                            Count = sort(rowSums(g.adj.train), 
                                         decreasing = TRUE)[1:20])
pop.composers$Composer <- c("Mozart",  "Beethoven", "Tchaikovsky",
                            "Ravel",  "Dvorák", 'John Williams' , "Debussy",
                            "Stravinsky", "Prokofiev", "Bach",  "Brahms",
                             "Shostakovich", "Berlioz", "Richard Strauss",
                             "Mendelssohn", "Copland", "Schumann",
                            "Haydn", "Rachmaninoff", "Bartók" )

era <- c("Classical", "Classical", "Romantic", "Late Romantic", "Romantic",
         "Modern", "Late Romantic", "Modern", "Modern", "Baroque", "Romantic",
         "Modern", "Romantic", "Late Romantic", "Romantic", "Modern",
         "Romantic", "Classical", "Modern", "Modern")
era <- factor(era, levels = c('Baroque', 'Classical', 'Romantic', 'Late Romantic', 'Modern'))
comp.df <- data.frame(Composer = rep(pop.composers$Composer, pop.composers$Count),
                      Era = rep(era, pop.composers$Count))
ggplot(comp.df, aes(Composer, fill = Era))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=16))
ggsave(filename = "Plots/EDA-comps.png", height = 12, width = 20, units = "cm")




reg.df <- data.frame(Region = rep(c("Europe", "North America", "South America",
                                    "Other", "Asia", "Austrailia"), 
                                  c(7179, 1048, 46, 41, 37, 20)))
era.df <- data.frame(Era = rep(c("Romantic", "Modern", "Classical",
                                    "Baroque", "Other", "Renaissance"), 
                                  c(3810, 3435, 853, 217, 37, 19)))

p1 <- ggplot(reg.df, aes(Region))+
  geom_bar(fill = "#F8766D")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Composer Nationality Region")
p2 <- ggplot(era.df, aes(Era))+
  geom_bar(fill = "#00BFC4")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Composer Era")

grid.arrange(p1, p2, nrow = 1)

