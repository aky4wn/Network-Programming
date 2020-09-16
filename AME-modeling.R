library(stringr)
library(tidyverse)
library(igraph)
library(ROCR)
library(amen)
library(gridExtra)
library(reshape2)
load("Data/bso.rda")

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


## No train/test split, include all data

## Ignore Year

rel.df.train <- rel.bso[,-4] %>% 
  group_by_all() %>% summarise(COUNT = n())
g.train.asym <- graph_from_data_frame(rel.df.train, directed=TRUE)
g.adj.train.asym <- as_adjacency_matrix(g.train.asym, sparse = FALSE)

#rel.df.test <- rel.bso[which(rel.bso$year == 2019),-4] %>% 
#  group_by_all() %>% summarise(COUNT = n())
#g.test.asym <- graph_from_data_frame(rel.df.test, directed=TRUE)
#g.adj.test.asym <- as_adjacency_matrix(g.test.asym, sparse = FALSE)


## Plot sample graph
plot.df <- rel.bso[which(rel.bso$year %in% c(2017, 2018)),-4] %>% 
  group_by_all() %>% summarise(COUNT = n())
g.plot <- graph_from_data_frame(plot.df, directed=TRUE)
plot(g.test.asym, edge.arrow.size=.2, vertex.color="gold", vertex.size=15, 
     
     vertex.frame.color="gray", vertex.label.color="black", 
     
     vertex.label.cex=0.8, vertex.label.dist=0, edge.curved=0.2) 



## Form nodal covariates dataframe
Xn.train.asym <- data.frame(matrix(0, nrow = dim(g.adj.train.asym)[1], ncol = 8))

count <- 1
for(comp in colnames(g.adj.train.asym)){
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




nrow(Xn.train.asym)
nrow(distinct(Xn.train.asym))
dub <- Xn.train.asym %>% 
  group_by(Region, Era, DOB, Symphony, 
           Concerto, Overture, Other) %>% filter(n() > 1)

#####################################################
## Modeling

XN.train.asym <- Xn.train.asym[,-1]
XN.train.asym$DOB[which(Xn.train.asym$DOB %in% c("Other", "Unknown"))] <- 0
XN.train.asym$DOB <- as.numeric(XN.train.asym$DOB)
DOB.mean <- mean(XN.train.asym$DOB)
DOB.sd <- sd(XN.train.asym$DOB)
XN.train.asym$DOB <- (XN.train.asym$DOB - DOB.mean)/DOB.sd

XN.train.asym$Region <- factor(XN.train.asym$Region, 
                               levels = c("Other", "Asia", "Austrailia", "Europe",
                                          "North America", "South America"))

XN.train.asym <- data.frame(model.matrix(~Region,XN.train.asym), 
                            XN.train.asym[, c(2, 4:7)])

train <- data.matrix(XN.train.asym[, -1])
## Multiplicative Effects and Random Effects

#train <- data.matrix(XN.train.asym[, c(2,3,5:8)])

fit_AME.RF.asym <- ame(g.adj.train.asym, Xr = train, 
                       Xc = train,
                       model="bin",  R = 3, rvar = TRUE, cvar = TRUE)
summary(fit_AME.RF.asym)


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


## Only column effects
beta.col <- post.beta[, 11:20]
library(latex2exp)
colnames(beta.col) <- c("Region-Asia", "Region-Austrailia", "Region-Europe",       
                        "Region-North America", "Region-South America", "Year of Birth",
                        "Symphony", "Concerto", "Overture", "Other")
dat.beta <- melt(beta.col)
colnames(dat.beta) <- c("Coefficient", "Samples")
ggplot(dat.beta, aes(x = Coefficient, y = Samples, fill = Coefficient))+
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = 2) + 
  labs(y = TeX("$\\beta_c$"), x = "Column Coefficient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=20)) +
  guides(fill=FALSE)
ggsave(filename = "Plots/beta-cols.png", height = 12, width = 20, units = "cm")


## Multiplicative Effects

top.c <- bso %>% group_by(Composer) %>% tally(sort = TRUE) %>% 
  select(Composer) 

inds <- rep(0, 20)
for(i in 1:20){
  inds[i] <- grep(top.c[i,], colnames(fit_AME.RF.asym$UVPM))
}


#cn <- colnames(fit_AME.RF.asym$UVPM[inds[1:10], inds[1:10]])
cn <- colnames(fit_AME.RF.asym$UVPM[inds[1:5], inds[1:5]])
strsplit(cn, "\\s+")

sapply(strsplit(cn, "\\s+"), tail, 1)

#df <- fit_AME.RF.asym$UVPM[inds[1:10], inds[1:10]]
df <- fit_AME.RF.asym$UVPM[inds[1:5], inds[1:5]]
colnames(df) <- sapply(strsplit(cn, "\\s+"), tail, 1)
rownames(df) <- sapply(strsplit(cn, "\\s+"), tail, 1)

ggplot(data = melt(df), 
       aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(colour = "white", size = 1) +
  labs(x = "Composers - Senders", y = "Composers - Receivers",
       fill = "Posterior Mean") +
  geom_text(aes(label = round(value, 2)), size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=24))+
  scale_fill_distiller(palette = "Spectral", direction = -1)
  #scale_fill_distiller(palette = "RdPu", direction=-1)
ggsave(filename = "Plots/heatmap-v2.png", height = 15, width = 22, units = "cm")

## Analyze AME.RF Asymmetric fit
p1 <- ggplot(data.frame(row.RF = fit_AME.RF.asym$APM), aes(x = row.RF))+
  geom_histogram(bins = 50, fill = "#F8766D") +
  labs(x = "Row Random Effects") + 
  xlim(-1,1.5) + 
  theme(text = element_text(size=20))+
  geom_vline(xintercept = 0, linetype = 2) 
p2 <- ggplot(data.frame(col.RF = fit_AME.RF.asym$BPM), aes(x = col.RF))+
  geom_histogram(bins = 50, fill = "#00BFC4") +
  labs(x = "Column Random Effects") + 
  geom_vline(xintercept = 0, linetype = 2) +
  theme(text = element_text(size=20))+
  xlim(-1,1.5) 

## Sort composers
sort(fit_AME.RF.asym$APM, decreasing = FALSE)[1:20]
sort(fit_AME.RF.asym$APM, decreasing = TRUE)[1:20]


inds <- which(colnames(g.adj.train.asym) %in% names(sort(fit_AME.RF.asym$APM, 
                                                         decreasing = FALSE)[1:10]))
Xn.train.asym$DOB[inds]

sort(fit_AME.RF.asym$BPM, decreasing = FALSE)[1:20]
sort(fit_AME.RF.asym$BPM, decreasing = TRUE)[1:20]

p <- grid.arrange(p1, p2, nrow = 2)
ggsave(p, filename = "Plots/row-res.png", height = 12, width = 20, units = "cm")

## Hist of most performed composers
## Barplot

pop.composers <- data.frame(Composer = names(sort(rowSums(g.adj.train.asym), 
                                                  decreasing = TRUE)[1:20]),
                            Count = sort(rowSums(g.adj.train.asym), 
                                         decreasing = TRUE)[1:20])


pop.composers$Composer <- c("Mozart",  "Beethoven", "Ravel",
                            "Haydn",  "J.S. Bach", 'Brahms' , "Tchaikovsky",
                            "Debussy", "Prokofiev", "Stravinsky",  "Dvorák",
                            "John Williams", "Schumann", "Mendelssohn",
                            "Sibelius", "Copland", "Shostakovich",
                            "Berlioz", "Barber", "R. Strauss" )

era <- c("Classical", "Classical", "Late Romantic", "Classical", "Baroque",
         "Romantic", "Romantic", "Late Romantic", "Modern", "Modern", "Romantic",
         "Modern", "Romantic", "Romantic", "Late Romantic", "Modern",
         "Modern", "Romantic", "Modern", "Late Romantic")
era <- factor(era, levels = c('Baroque', 'Classical', 'Romantic', 'Late Romantic', 'Modern'))
comp.df <- data.frame(Composer = rep(pop.composers$Composer, pop.composers$Count),
                      Era = rep(era, pop.composers$Count))
ggplot(comp.df, aes(Composer, fill = Era))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=16))

ggsave(filename = "Plots/EDA-comps.png", height = 12, width = 20, units = "cm")


## How often composers performed


## Hist of Type of Pieces

df.type <- data.frame(Type = c(rep("Symphony", sum(Xn.train.asym[,5])),
             rep("Concerto", sum(Xn.train.asym[,6])),
             rep("Overture", sum(Xn.train.asym[,7])),
             rep("Other", sum(Xn.train.asym[,8]))))
df.type$Type <- factor(df.type$Type, levels = c("Overture", "Concerto",
                                                   "Symphony", "Other"))

ggplot(df.type, aes(Type, fill = Type))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=16)) +
  labs(x = "Piece Type", fill = "Piece Type")
ggsave(filename = "Plots/EDA-type.png", height = 12, width = 18, units = "cm")


## Cluster multiplicative effects

hc1 <- hclust(as.dist(fit_AME.RF.asym$UVPM[inds[1:20], inds[1:20]]), method = "complete" )


png(filename="Plots/hclust.png", units="in", width=11, height=8, res=300)
par(cex=1.5)
plot(hc1, main ="Volume", xlab = '', sub="")
dev.off()




library(Rtsne)

top.c <- bso %>% group_by(Composer) %>% tally(sort = TRUE) %>% 
  select(Composer) 

inds <- rep(0, 200)
for(i in 1:200){
  inds[i] <- grep(top.c[i,], colnames(fit_AME.RF.asym$UVPM))
}

tsne <- Rtsne(fit_AME.RF.asym$V[inds[1:100],], dims = 2, perplexity=30, verbose=TRUE, 
              max_iter = 500)
df.plot <- data.frame(tsne$Y)
df.plot$Era <- Xn.train.asym$Era[inds[1:100]]
df.plot$Region <- Xn.train.asym$Region[inds[1:100]]


ggplot(melt(df.plot[,1:3], id = c("X1", "X2"))) +
  geom_point(aes(x = X1, y = X2, color = value),
             size = 2.5) +
  labs(color = "Era", x = "X1", y = "X2") 
#ggsave(filename = "Plots/tempo-X-tsne.png")

ggplot(melt(df.plot[,c(1,2,4)], id = c("X1", "X2"))) +
  geom_point(aes(x = X1, y = X2, color = value),
             size = 2.5) +
  labs(color = "Region", x = "X1", y = "X2") 
#ggsave(filename = "Plots/tempo-X-tsne.png")

## Plot first two dimensions of U matrix
df.plot <- data.frame(fit_AME.RF.asym$U[,1:2])
df.plot$Era <- Xn.train.asym$Era
df.plot$Region <- Xn.train.asym$Region


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

## Covariates
c1.subset <- Xn.train.asym[which(colnames(g.adj.train.asym) %in% c1.U), ]
rownames(c1.subset) <- c1.U
length(which(c1.subset$Nationality %in% c('Russian', 'French')))
dim(c1.subset)

c2.subset <- Xn.train.asym[which(colnames(g.adj.train.asym) %in% c2.U), ]
rownames(c2.subset) <- c2.U
length(which(c2.subset$Nationality == 'American'))
dim(c2.subset)
length(which(Xn.train.asym$Era == 'Baroque'))
length(which(c2.subset$Era == 'Baroque'))

c3.subset <- Xn.train.asym[which(colnames(g.adj.train.asym) %in% c3.U), ]
rownames(c3.subset) <- c3.U
dim(c3.subset)

c4.subset <- Xn.train.asym[which(colnames(g.adj.train.asym) %in% c4.U), ]
rownames(c4.subset) <- c4.U
dim(c4.subset)

c5.subset <- Xn.train.asym[which(colnames(g.adj.train.asym) %in% c5.U), ]
rownames(c5.subset) <- c5.U
dim(c5.subset)

#fviz_cluster(k.V, data = fit_AME.RF.asym$V, geom = "point") #geom = "point"
#c1.V <- names(which(k.V$cluster == 1))
#c2.V <- names(which(k.V$cluster == 2))
#c3.V <- names(which(k.V$cluster == 3))
#c4.V <- names(which(k.V$cluster == 4))
#c5.V <- names(which(k.V$cluster == 5))









ggplot(XN.train.asym, aes(x = Region)) +
  geom_bar(fill = '#00BFC4') +
  theme(text = element_text(size=16)) + 
  ggsave(filename = "Plots/EDA-region.png")

ggplot(XN.train.asym, aes(x = Era)) +
  geom_bar(fill = '#C77CFF') +
  theme(text = element_text(size=16)) + 
  ggsave(filename = "Plots/EDA-era.png")


XN.train.asym$Era <- factor(XN.train.asym$Era, 
                            levels = c("Other", "Renaissance", "Baroque", "Classical",
                                       'Romantic', "Modern"))

ggplot(XN.train.asym[which(XN.train.asym$DOB > 0),], aes(x = Era, y = DOB, fill = Era)) +
  geom_boxplot() +
  labs(y = 'Year of Birth') +
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggsave(filename = "Plots/EDA-era-DOB.png")


ggplot(XN.train.asym[which(XN.train.asym$DOB > 0),], aes(x = DOB)) +
  geom_histogram(bins = 40, fill = '#00BA38') +
  labs(x = 'Year of Birth') +
  theme(text = element_text(size=16)) + 
  ggsave(filename = "Plots/EDA-DOB.png")



## Nodes with Highest Degree

sort(degree(g.train.asym, mode = 'out'), decreasing = TRUE)[1:10]
sort(degree(g.train.asym, mode = 'in'), decreasing = TRUE)[1:10]

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




