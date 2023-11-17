##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("tidyverse","stringr","openxlsx")
lapply(pkg, require, character.only=T)
rm(pkg)

setwd("~/Library/CloudStorage/Dropbox/_Teaching/2023/BDML_online_2023_13/SesionesSincronicas/Lecture09/extra")

set.seed(10101)


require("simstudy")

ddef <- defData(varname = "dcbd", formula = "0;1", dist = "uniform")

theta1 <- c(0.9, 0.01, 0.5, 0.8, 0.6, 0.3, 0.8)
knots <- c(0.25, 0.5, 0.75)

viewSplines(knots = knots, theta = theta1, degree = 3)


dt <- genData(1000, ddef)

dt <- genSpline(
  dt = dt, newvar = "price",
  predictor = "dcbd", theta = theta1,
  knots = knots, degree = 3,
  noise.var = .025
)

dt<- dt %>% mutate(price=100000+price*10000)




require("ggformula")
require("splines")
fit<-lm(price ~ bs(dcbd,knots= c(25,40,60),degree=5),data=dt)
dcbdlims<-range(dt$dcbd)
dcbd.grid<-seq(from=dcbdlims[1], to = dcbdlims[2])
dt$spline_i<-predict(fit,newdata = list(dcbd=dt$dcbd))




# Data Generating Process
#https://stackoverflow.com/questions/46453548/regression-tree-with-simulated-data-rpart-package

n=1000

habitaciones<-rpois(n,lambda = 2)
#DCBD<-rnorm(n,mean=3,sd=2)
DCBD<-1+dt$dcbd
e<-rnorm(n,0,3)
price=exp(11.5-.3*I(habitaciones<3)*I(DCBD>1.5)+0.5*I(DCBD<1.5))+e

mydat=as.data.frame(cbind(price,habitaciones,DCBD))
summary(mydat)

library(rpart)
library(rpart.plot)


#mytree<-rpart(log(price)~.,data=mydat, cp=0.0001)
#plotcp(mytree)
mytree<-rpart(log(price)~.,data=mydat)
plot(mytree)

# Find the optimal value of the complexity parameter cp
optcp <- mytree$cptable[which.min(mytree$cptable[,4]),1]
optcp
# Prune the tree using the optial complexity parameter
tree_prune <- prune(mytree,optcp)
tree_prune
plot(tree_prune)
# Plot the cross-validation error vs the complexity parameter


pdf("../figures/trees.pdf",width = 15, height = 12)
prp(tree_prune, under = TRUE, branch.lty = 2, yesno = 2, faclen = 0, varlen=15,tweak=1.2,clip.facs= TRUE,box.palette = "Greens",compress=TRUE,ycompress = TRUE,node.fun=function(x, labs, digits, varlen) paste("Precio \n", format(round(exp(mytree$frame$yval), 0), nsmall=0, big.mark=",")))
dev.off()
system("pdfcrop ../figures/trees.pdf  ../figures/trees.pdf")

ggplot(mydat) +
  geom_point(aes(x=habitaciones,y=DCBD),position=position_jitter(width = .05)) +
  scale_x_continuous(breaks=seq(0,8,1)) +
  theme_classic() +
  xlab("Habitaciones") +
  ylab("Distancia al Centro") +
  theme(legend.position =  "none",
      text=element_text(size=20))
ggsave("../figures/dcbd_hab.pdf",width = 9, height = 5)



#i<-1
MSE_hab<-NA
for(i in 0:8){
  R1<- mydat %>% filter(habitaciones<=i)
  R1<- R1 %>% mutate(c1=mean(price))
  MSEr1<- ifelse(is.na(mean((R1$price-R1$c1)^2)),0,mean((R1$price-R1$c1)^2))
  R2<- mydat %>% filter(habitaciones>i)
  R2<- R2 %>% mutate(c2=mean(price))
  MSEr2<- ifelse(is.na(mean((R2$price-R2$c2)^2)),0,mean((R2$price-R2$c2)^2))
  
  MSE_hab[i+1]<-MSEr1+MSEr2
  
}

MSE_dbcd<-NA
j<-1
for(i in seq(1,2,0.25)){
  R1<- mydat %>% filter(DCBD<=i)
  R1<- R1 %>% mutate(c1=mean(price))
  MSEr1<- ifelse(is.na(mean((R1$price-R1$c1)^2)),0,mean((R1$price-R1$c1)^2))
  R2<- mydat %>% filter(DCBD>i)
  R2<- R2 %>% mutate(c2=mean(price))
  MSEr2<- ifelse(is.na(mean((R2$price-R2$c2)^2)),0,mean((R2$price-R2$c2)^2))
  
  MSE_dbcd[j]<-MSEr1+MSEr2
  print(MSEr1+MSEr2)
  j<-j+1
}

MSE<-c(MSE_hab,MSE_dbcd)
MSE[which.min(MSE)]
MSE

write.csv(mydat,"toy_houses.csv",row.names = FALSE)
