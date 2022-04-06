## Set path (modify for your directory structure)
setwd("/home/flake/Desktop/github")

## CONFIGURATIONS
library(e1071) 
library(apaTables)
library(apa)
library(extrafont)
library(tikzDevice)
font_import()
#loadfonts()
#getOption("tikzLatex")
#options("tikzLatex"='/usr/bin/pdflatex')
loadfonts(device = "pdf")
par(family = "CM Roman")

save_plots <- TRUE
if (save_plots) {
  dir.create("Plots", showWarnings = FALSE)
}
middle_to <- NA #will be recorded as NA or 4
remove_control <- TRUE

## LOAD DATASET

# Documentation on likert:
# 1 = real ... 7 = synthetic; 
# -1 if the slides has been left on the middle (4) position; will be recorded as <middle_to>

ans <- read.csv("Data/ans.csv", sep=";", stringsAsFactors = TRUE)
ans$likert[ans$likert<0] <- middle_to

img <- read.csv("Data/img.csv", sep=";")
img <- data.frame(lapply(img, function(x) {gsub("FFHQ", "REAL", x)}))
img <- data.frame(lapply(img, function(x) {gsub("Control", "CNTRL", x)}))
img$img_id <- factor(img$img_id, levels=levels(ans$img_id))  # ensure that categories match
img$ds <- factor(img$ds)
img$synthetic <- img$synthetic==1 # cast to boolean

omit <- c("s-025", "s-029", "s-070", "s-096", "s-188", "s-328", "s-337", "s-423", "s-462", "s-465", "s-484", "s-509", "s-547", "s-549", "s-552", "s-562", "s-607", "s-024", "s-144", "s-178", "s-322", "s-331", "s-336", "s-575", "s-086", "s-244", "s-412", "s-414", "s-438", "s-605")

#Remove outliers
ans <- droplevels(ans[!ans$subj_id %in% omit,])

## Add convenience columns to answer set (basic joins)

ans$ds <- img$ds[as.numeric(ans$img_id)]
ans$synthetic <- img$synthetic[as.numeric(ans$img_id)]

ans$correct <- ((ans$likert < 4) & !ans$synthetic) | ((ans$likert > 4) & ans$synthetic)

#If needed remove control dataset
if (remove_control) {
  ans <- droplevels(ans[!ans$ds == 'CNTRL',])
  img <- droplevels(img[!img$ds == 'CNTRL',])
}
myds <- c("REAL", "PGGAN","StyleGAN", "StyleGAN2")
ans$ds <- factor(ans$ds, levels = myds)
img$ds <- factor(img$ds, levels = myds)
mycolors=c("#43aa8b", "#f94144", "#f9a34e", "#f9f94e")

ans$as_real <- ((ans$likert > 0) & (ans$likert<4))
img_real_rate <- tapply(ans$as_real==TRUE,ans$img_id,sum,na.rm=TRUE)

img_real_rate2 = img_real_rate/table(ans$img_id)*100
ds_real_rate <- tapply(ans$as_real==TRUE,ans$ds,mean,na.rm=TRUE)

img_confidence <- tapply(c(1,2/3,1/3,0,1/3,2/3,1)[ans$likert],ans$img_id,mean,na.rm=TRUE)*100
img_correct_rate <- tapply(ans$correct,ans$img_id,mean,na.rm=TRUE)*100
med_resp_time <- tapply(ans$resp_time[!is.na(ans$likert)], ans$img_id[!is.na(ans$likert)], median)

#############
## LIKERT
#############

if (save_plots) {
  pdf("Plots/likert.pdf", width = 20, height = 9)
}
par(mgp=c(4,1.5,0))
par(mai = c(1.1, 1.3, 0.3, 0)) #bottom, left, top and right
likertTable <- prop.table(table(ans$likert,ans$ds),2)*100
barplot(likertTable ,bes=TRUE,col=rep(mycolors,each=7),ylim=c(0,40), las=1, ylab="% usage", cex.lab=2,
        cex.axis=2, cex=2, axis.lty=1, names=c(1, 2, 3, 4, 5, 6, 7,1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7,1, 2, 3, 4, 5, 6, 7))
mtext("REAL",side=1,col="black",line=4, at=4.5, cex=2) 
mtext("PGGAN",side=1,col="black",line=4, at=12.5, cex=2) 
mtext("StyleGAN",side=1,col="black",line=4, at=20.5, cex=2) 
mtext("StyleGAN2",side=1,col="black",line=4, at=28.5, cex=2) 
if (save_plots) {
  dev.off()
}


#############
## RR
#############

if (save_plots) {
  pdf("Plots/RR-A.pdf", width = 12, height = 12)
}
par(mgp=c(5,2,0))
par(mai = c(1, 1.5, 0.8, 0))
barplot(ds_real_rate*100, beside=T, ylim=c(0,100), las=1, main="Realism Rate", col=mycolors, cex.lab=3, cex.axis=3, cex=2.5, cex.main=3)
if (save_plots) {
  dev.off()
}


if (save_plots) {
  pdf("Plots/RR-B.pdf", width=12, height = 12)
}
par(mfrow=c(2,2))
par(mai = c(0.4, 0.6, 0.4, 0.2))
shift = c(-2.2, -1.5, -1, -1)
i <- 1
for (L in levels(img$ds))  {
  hist1 <- hist(img_real_rate2[img$ds==L], breaks = c(0,10,20,30,40,50,60,70,80,90,100), plot=FALSE)
  hist1$counts <- hist1$counts/sum(hist1$counts)*100
  plot(hist1, col =mycolors[i],
      main="", xlim=c(0,100), ylim=c(0,40), las=1, ylab="", xlab="", cex.lab=2, cex.axis=2, cex=2, cex.main=3)
  
  mtext(side=1, line=-28, at=0, adj=shift[i], cex=2, bquote(bold(.(L))))
 i <- i+1 
}
if (save_plots) {
  dev.off()
}

for (L in levels(img$ds))
  print(skewness(img_real_rate2[img$ds==L]))

#############
## ACC
#############

if (save_plots) {
  pdf("Plots/ACC.pdf", width = 12, height = 12)
}
par(mgp=c(5,2,0))
par(mai = c(1, 1.5, 0.8, 0))
bilan <- aggregate(img_correct_rate, list(img$ds), mean)
rownames(bilan) <- bilan[,1]
bilan <- as.matrix(bilan[,-1])
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length)
}
stdev <- aggregate(img_correct_rate, list(img$ds), sd)
rownames(stdev) <- stdev[,1]
stdev <- as.matrix(stdev[,-1]) * 1.96 / 10
ze_barplot <- barplot(tapply(img_correct_rate, img$ds, mean), beside=T,col=mycolors , ylim=c(0,100),
                      las=1, main="Mean Accuracy", cex.lab=3, cex.axis=3, cex=2.5, cex.main=3)
error.bar(ze_barplot,bilan, stdev)
if (save_plots) {
  dev.off()
}

#############
## MC
#############

if (save_plots) {
  pdf("Plots/MC.pdf", width = 12, height = 12)
}
par(mgp=c(5,2,0))
par(mai = c(1, 1.5, 0.8, 0))
bilan <- aggregate(img_confidence, list(img$ds), mean)
rownames(bilan) <- bilan[,1]
bilan <- as.matrix(bilan[,-1])
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length)
}
stdev <- aggregate(img_confidence, list(img$ds), sd)
rownames(stdev) <- stdev[,1]
stdev <- as.matrix(stdev[,-1]) * 1.96 / 10
ze_barplot <- barplot(tapply(img_confidence, img$ds, mean), beside=T,col=mycolors , ylim=c(0,100),
                      las=1, main="Mean Confidence", cex.lab=3, cex.axis=3, cex=2.5, cex.main=3)
error.bar(ze_barplot,bilan, stdev)
if (save_plots) {
  dev.off()
}

#############
## MC - CORR
#############

img_confidence_correct <- tapply((c(1,2/3,1/3,0,1/3,2/3,1)[ans$likert])[ans$correct],ans$img_id[ans$correct],mean,na.rm=TRUE)
if (save_plots) {
  pdf("Plots/im-dist-correct-conf.pdf", width=12, height = 12)
}
par(mfrow=c(2,2))
par(mai = c(0.4, 0.6, 0.4, 0.2))
shift = c(-2.2, -1.5, -1, -1)
i <- 1
for (L in levels(img$ds)) {
  hist1 <- hist(img_confidence_correct[img$ds==L],breaks=c(0:20)/20, plot=FALSE)
  hist1$counts <- hist1$counts/sum(hist1$counts)*100
  plot(hist1, col =mycolors[i],
       main="", xlim=c(0,1), ylim=c(0,50), las=1, ylab="", xlab="", cex.lab=2, cex.axis=2, cex=2, cex.main=3)
    mtext(side=1, line=-28, at=0, adj=shift[i], cex=2, bquote(bold(.(L))))
  i <- i+1 
}
if (save_plots) {
  dev.off()
}

for (L in levels(img$ds))
  print(skewness(img_confidence_correct[img$ds==L]))

#############
## MC - INCORR
#############

img_confidence_correct <- tapply((c(1,2/3,1/3,0,1/3,2/3,1)[ans$likert])[!ans$correct],ans$img_id[!ans$correct],mean,na.rm=TRUE)
if (save_plots) {
  pdf("Plots/im-dist-incorrect-conf.pdf", width=12, height = 12)
}
par(mfrow=c(2,2))
par(mai = c(0.4, 0.6, 0.4, 0.2))
shift = c(-2.2, -1.5, -1, -1)
i <- 1
for (L in levels(img$ds)) {
  hist1 <- hist(img_confidence_correct[img$ds==L],breaks=c(0:20)/20, plot=FALSE)
  hist1$counts <- hist1$counts/sum(hist1$counts)*100
  plot(hist1, col =mycolors[i],
       main="", xlim=c(0,1), ylim=c(0,50), las=1, ylab="", xlab="", cex.lab=2, cex.axis=2, cex=2, cex.main=3)
  mtext(side=1, line=-28, at=0, adj=shift[i], cex=2, bquote(bold(.(L))))
  i <- i+1 
}
if (save_plots) {
  dev.off()
}

for (L in levels(img$ds))
  print(skewness(img_confidence_correct[img$ds==L]))



#############
## IMAGE
#############

if (save_plots) {
  pdf("Plots/im-corr-vs-conf.pdf")
}
par(mai = c(1.3, 0.9, 0.1, 0.1))
plot(img_correct_rate, img_confidence, xlim=c(0,100), ylim=c(0,100), pch = 19,
     col=mycolors[as.numeric(img$ds)],
     xlab="Accuracy", ylab="Confidence", las=1, cex.lab=1.5, cex.axis=1.5)
legend("bottom", inset = c(0,-0.25), legend = myds, xpd = TRUE, 
       horiz = TRUE, pch = 19, bty = "n",col=mycolors, cex=1.3)
if (save_plots) {
  dev.off()
}


#############
## SPEARMAN
#############

#overall
cor.test(img_correct_rate, img_confidence, method="spearman", exact=FALSE)
cor.test(img_correct_rate, med_resp_time, method="spearman", exact=FALSE)
cor.test(img_confidence, med_resp_time, method="spearman", exact=FALSE)

#By ds acc/conf
for (L in levels(img$ds)) {
  print(L)
  print(cor.test(img_correct_rate[img$ds==L], img_confidence[img$ds==L], method="spearman", exact=FALSE))
}

#By ds acc/mrs
for (L in levels(img$ds)) {
  print(L)
  print(cor.test(img_correct_rate[img$ds==L], med_resp_time[img$ds==L], method="spearman", exact=FALSE))
}

#By ds acc/mrs
for (L in levels(img$ds)) {
  print(L)
  print(cor.test(img_confidence[img$ds==L], med_resp_time[img$ds==L], method="spearman", exact=FALSE))
}

#subject

subj_correct_rate <- tapply(ans$correct,ans$subj_id,mean,na.rm=TRUE)
subj_confidence <- tapply(c(1,2/3,1/3,0,1/3,2/3,1)[ans$likert],ans$subj_id,mean,na.rm=TRUE)
cor.test(subj_correct_rate, subj_confidence)
cor.test(subj_correct_rate, subj_confidence, method="spearman", exact=FALSE)

