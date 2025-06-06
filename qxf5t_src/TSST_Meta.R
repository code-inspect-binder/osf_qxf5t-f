setwd("~/Dropbox/Studien/2018/MetaAnal_TSST")
library(metafor)
library(multcomp)
library(readxl)

d <- read_excel("Dataset_ext.xlsx")
f <- read_excel("cult value orientation scores.xls")
e <- read_excel("WMHS.xlsx")

#calculate sampling variance
rho <- .5 #assumed correlation between C0 and Cmax
bias <- function(df) 1 - 3/(4*df-1)
d$dv <- (2*(1-rho) / d$N)* ((d$N-1)/(d$N-3))* (1+d$N/(2*(1-rho))*d$d^2) - (d$d^2)/(bias(d$N-1))^2

#generate unique study identifier, format and annotate data
d$id <- paste0(d$First_Author,", ", d$Year)
d$Year <- as.numeric(substr(d$Year,1,4))
d$ids <- NA
for(i in d$id) d[d$id == i,"ids"] <- 1:dim(d[d$id == i,"ids"])[1]
d$ids <- as.character(d$ids)
d$Age <- (as.numeric(d$Age_M) - 30) / 20

d$energy_renew <- d$energy_renew1 + d$energy_renew2
find_cCode <- Vectorize(function(i) which(unlist(Map(function(x) sum(d[i,grepl("SVS",names(d))] - f[x,grepl("SVS",names(f))]), 1:dim(f)[1])) == 0))
d$Ccode <- f$Ccode[find_cCode(1:dim(d)[1])]

#determine manifestations and visualize cultural value dimensions
d$SVS_d1 <- d$SVS_harmony - d$SVS_mastery
d$SVS_d2 <- d$SVS_egalit - d$SVS_hierarchy
d$SVS_d3 <- (d$SVS_autona+d$SVS_autoni)/2 - d$SVS_embed  
cor(d[grepl("_d",names(d))])

f$SVS_d1 <- f$SVS_harmony - f$SVS_mastery
f$SVS_d2 <- f$SVS_egalit - f$SVS_hierarchy
f$SVS_d3 <- (f$SVS_autona+f$SVS_autoni)/2 - f$SVS_embed
cor(f[grepl("_d",names(f))])

pcf <- princomp(f[,3:9], cor=T)
summary(pcf)
loadings(pcf)
#d$SVS_d1 <- predict(pcf, newdata = d[,18:24])[,1]


library(png)
scal <- c(height=3, width=3, cex=0.03, asp=4/3)

#unrotated components
scores <- t(t(pcf$scores[,1:2])/pcf$sdev[1:2])
#plot(scores[,1], (apply(as.matrix(f[,3:9]),2,scale) %*% loadings(pcf))[,1])
loads <- t(t(pcf$loadings[,1:2])*pcf$sdev[1:2])

#rotated components (varimax)
#lam <- apply((scale(as.matrix(f[,3:9])) %*% varimax(loadings(pcf))$rotmat)[,1:2],2,sd)
#scores <- t(t((scale(as.matrix(f[,3:9])) %*% varimax(loadings(pcf))$rotmat)[,1:2])/lam)
#loads <- t(t(varimax(loadings(pcf))$rotmat[,1:3])*lam)

symlink <- paste0("./Flags/",f$Ccode,".png")
flag <- Map(function(x) readPNG(symlink[x]), 1:length(symlink))
flage <- list() #add alpha channel
for(i in 1:length(flag)){
  flage[[i]] <- array(NA, dim=dim(flag[[i]]))
  if(dim(flag[[i]])[3] < 4) flage[[i]] <- array(NA, dim=dim(flag[[i]])+c(0,0,1))
  flage[[i]][,,1:3] <- flag[[i]][,,1:3]
  flage[[i]][,,4] <- 1
  if(i %in% which(!(f$Ccode %in% unique(d$Ccode)))) flage[[i]][,,4] <- 0.1
}
flag <- flage; rm(flage)
flag_pos <- list(
  xleft=scores[,1]-scal["width"]*scal["cex"]*scal["asp"],
  xright=scores[,1]+scal["width"]*scal["cex"]*scal["asp"],
  ybottom=scores[,2]-scal["height"]*scal["cex"],
  ytop=scores[,2]+scal["height"]*scal["cex"]
)

par(mfrow=c(1,1))
plot(scores[,1], scores[,2], xlim=c(-scal["width"],scal["width"]), ylim=c(-scal["height"],+scal["height"]), pch=NA,
     xlab="country scores on 1st component (z-value)", ylab="country scores on 2nd component (z-value)"); polygon(x=c(-4,4,4,-4), y=c(-4,-4,4,4), col = "antiquewhite4"); grid()

par(new = TRUE); dev.hold(); on.exit(dev.flush(), add = TRUE)
plot(loads, axes = FALSE, type = "n", xlab = "", ylab = "", xlim=c(-1,1), ylim=c(-1,1)); axis(3); axis(4)
arrows(0, 0, loads[,1] * 0.85, loads[,2] * 0.85, col = "black", length = 0.1, lty=1)
text(loads, labels = c("Harmony","Embedded-\nness","Hierarchy","Mastery","Affective\nAutonomy","Intellectual\nAutonomy","Egalitarism"), cex = 0.8)

par(new = TRUE); dev.hold(); on.exit(dev.flush(), add = TRUE)
plot(scores, axes = FALSE, type = "n", xlab = "", ylab = "", xlim=c(-scal["width"],scal["width"]), ylim=c(-scal["height"],+scal["height"]))
Map(function(x) rasterImage(flag[[x]], flag_pos$xleft[x], flag_pos$ybottom[x], flag_pos$xright[x], flag_pos$ytop[x], interpolate = T), which(!(f$Ccode %in% unique(d$Ccode))))
Map(function(x) rasterImage(flag[[x]], flag_pos$xleft[x], flag_pos$ybottom[x], flag_pos$xright[x], flag_pos$ytop[x], interpolate = T), which((f$Ccode %in% unique(d$Ccode))))

#d$SVS_d1 <- predict(pcf, newdata = d[,18:24])[,1]
#d$SVS_d2 <- predict(pcf, newdata = d[,18:24])[,2]
#d$SVS_d2 <- predict(pcf, newdata = d[,18:24])[,3]

#basic meta analyses
mod0 <- rma.mv(yi = d, V = dv, mods =~I(dv), data = d, test = "t", random =list(~ ids|id), struct="CS")
summary(mod0)

mod0 <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv), test = "t", data = d, random =list(~ ids|id), struct="CS")
summary(mod0)

mod <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv), test = "t", data = d, random =list(~ ids|id, ~ 1|Country), struct="CS")
anova(mod0,mod)
summary(mod)

wi <- 1/sqrt(d$dv)
size <- 0.5 + 2 * (wi - min(wi))/(max(wi) - min(wi))

par(mfrow=c(1,2))

plot(c(-1,3.5), 1/sqrt(range(d$dv)), type="n", axes=F, ylab="study precision (1/SE)", xlab="standardized cortisol difference (d)")
polygon(c(-3,4,4,-3), c(0,0,15,15), border = NA, col="lightgrey"); abline(h=c(1/sqrt(max(d$dv)), seq(0,15,2), 1/sqrt(min(d$dv))), col="white")
y <- c(1/sqrt(seq(1e-10,max(d$dv), length=1000)), 1/sqrt(seq(max(d$dv),1e-10, length=1000)) )
x95 <- c(sqrt((1/y[1:1000])^2+0) *qnorm(.025), sqrt((1/y[1001:2000])^2+0) *qnorm(.975)) + 0
x99 <- c(sqrt((1/y[1:1000])^2+0) *qnorm(.005), sqrt((1/y[1001:2000])^2+0) *qnorm(.995)) + 0
polygon(x99,y, border = NA, col="grey"); lines(x99[1:1000],y[1:1000], lty=3); lines(x99[1001:2000],y[1001:2000], lty=3)
polygon(x95,y, border = NA, col="white"); lines(x95[1:1000],y[1:1000], lty=3); lines(x95[1001:2000],y[1001:2000], lty=3)
axis(1, at=seq(-3,3,1)); axis(2, at=seq(0,12,2))
points(d$d, 1/sqrt(d$dv), pch=21, bg="white")
mod0 <- rma.mv(yi = d, V = dv, mods =~I(dv), test = "t", data = d, random =list(~ ids|id), struct="CS")
preds <- predict(mod0, newmods = seq(min(d$dv), max(d$dv), length=1000) )
lines(preds$pred, 1/sqrt(seq(min(d$dv), max(d$dv), length=1000)), lty=2, lwd=2 )
lines(preds$cr.lb, 1/sqrt(seq(min(d$dv), max(d$dv), length=1000)), lty=3, lwd=2 )
lines(preds$cr.ub, 1/sqrt(seq(min(d$dv), max(d$dv), length=1000)), lty=3, lwd=2 )
legend("topright", bty="n", lwd=2, lty=c(2,3), legend=c("mean effect estimate", "95% credible region"))
text(-0.75,13,"(A)", cex=1.4)

cols <- apply(colorRamp(c("red","blue"))(d$Male),1,function(x) rgb(x[1],x[2],x[3],150, maxColorValue = 255))
plot(d$Age_M, d$d, pch=19, cex=size, col=cols, las=1, bty="l", xlim=c(0,70),
     ylab="standardized cortisol difference (d)", xlab="mean sample age (years)")
grid()
cols <- apply(colorRamp(c("red","blue"))(c(0,.5,1)),1,function(x) rgb(x[1],x[2],x[3],150, maxColorValue = 255))
legend("topright",bty="n", pch=19, cex=1.1, col=cols, legend=c("0% male","50% male","100% male"))
mod <- rma.mv(yi = d, V = dv, mods =~Year+I(Male-0.5)+Age+I(dv), test="t", data = d, random =list(~ ids|id, ~ 1|Country), struct="CS")
preds <- predict(mod, newmods = cbind(2010, rep(0.5,71), (0:70-30)/20, rep(0,71)))
lines(0:70, preds$pred, col="blue")
polygon(c(0:70,70:0), c(preds$ci.lb,rev(preds$ci.ub)), border = NA, col=rgb(0,0,1,.2))
preds <- predict(mod, newmods = cbind(2010, rep(-0.5,71), (0:70-30)/20, rep(0,71)))
polygon(c(0:70,70:0), c(preds$ci.lb,rev(preds$ci.ub)), border = NA, col=rgb(1,0,0,.2))
lines(0:70, preds$pred, col="red")
text(5,3.35,"(B)", cex=1.4)


#investigate effects of North America vs. Europe / countries
d$CCode <- factor(d$Ccode)

mod <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+CCode, data = d, random =~ ids|id, struct="CS")
preds <- predict(mod, newmods = cbind(rep(0,22), rep(0,22), rep(0,22), rep(0,22), contrasts(d$CCode) ))
preds <- data.frame(preds)
rownames(preds) <- rownames(contrasts(d$CCode))
preds <- preds[order(preds$pred, decreasing = T),]

contrasts(d$CCode) <- contr.sum(length(unique((d$CCode))))
mod <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+CCode, data = d, random =~ ids|id, struct="CS")
summary(mod)
                                
symlink <- paste0("./Flags/",f$Ccode,".png")
symlink <- symlink[match(rownames(preds), f$Ccode)]
scal <- c("height"=1.5, "width"=10, "cex"=0.03, "asp"=4/3)
flag <- Map(function(x) readPNG(symlink[x]), 1:length(symlink))
flag_pos <- list(
  xleft=(1:dim(preds)[1]-0.5)-scal["width"]*scal["cex"]*scal["asp"],
  xright=(1:dim(preds)[1]-0.5)+scal["width"]*scal["cex"]*scal["asp"],
  ybottom=rep(0.1,dim(preds)[1])-scal["height"]*scal["cex"],
  ytop=rep(0.1,dim(preds)[1])+scal["height"]*scal["cex"]
)

par(mfrow=c(1,2))

barplot(preds$pred, width = 1, space = 0, ylab="adj. standardized cortisol difference (d)", horiz = F, las=2, cex.names = .6, ylim=c(0,2), xlab="country / region of TSST-conducting studies")
Hmisc::errbar((1:dim(preds)[1]-0.5), pch=NA, preds$pred, yplus = with(preds, pred + se), yminus = with(preds,pred), add=T)
Map(function(x) rasterImage(flag[[x]], flag_pos$xleft[x], flag_pos$ybottom[x], flag_pos$xright[x], flag_pos$ytop[x], interpolate = T), 1:length(symlink))
lab <- table(d$Ccode)
lab <- paste("k =", as.numeric(lab[rownames(preds)]))
axis(1, at = 1:dim(preds)[1]-0.5, labels = lab, las=3, cex.axis=0.7, col.ticks = "grey")
abline(h = mod$b[1], lty=2, lwd=2)
legend("topright",bty="n",lty=2:4,lwd=2, col=c("black","royalblue","royalblue"),
       legend=c("overall effect","North America","Europe"))
text(1,1.9,"(A)", cex=1.4)

sel <- d$Continent %in% c("North America","Europe")
mod <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+factor(Continent), test="t", data = d, random =~ ids|id, struct="CS", subset = sel, btt = 6)
summary(mod)
preds <- predict(mod, newmods = cbind(c(0,0),c(0,0),c(0,0),c(0,0), contrasts(unique(factor(d$Continent[sel])))))
abline(h = preds$pred, lty=3:4, col="royalblue", lwd=2)

#investigate CIA World Factbook and World Mental Health survey variables
d$GDP <- scale(d$GDP, center = 50000, scale = 5000)
d$edu_expendit <- as.numeric(d$edu_expendit)

e <- data.frame(e)
e[,-1:-2] <- apply(e[,-1:-2],2,as.numeric)
#e[,-1:-2] <- apply(apply(e[,-1:-2],2,as.numeric),2, function(x) log((x/100)/(1-(x/100))))
d$MDD <- e$MDD[match(d$Ccode, e$Ccode)]
d$PD <- e$PD[match(d$Ccode, e$Ccode)]
d$SAD <- e$SAD[match(d$Ccode, e$Ccode)]
d$SP <- e$SP[match(d$Ccode, e$Ccode)]
d$GAD <- e$GAD[match(d$Ccode, e$Ccode)]
d$PTSD <- e$PTSD[match(d$Ccode, e$Ccode)]


names(d)
mods <- list() #all studies
mods[[1]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+pop_growth, data = d, test="t", random =~ ids|id, struct="CS")
mods[[2]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+fertility, data = d, test="t", random =~ ids|id, struct="CS")
mods[[3]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+life_expect, data = d, test="t", random =~ ids|id, struct="CS")
mods[[4]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+obesity, data = d, test="t", random =~ ids|id, struct="CS")
mods[[5]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+GDP, data = d, test="t", random =~ ids|id, struct="CS")
mods[[6]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+taxes, data = d, test="t", random =~ ids|id, struct="CS")
mods[[7]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+health_expendit, data = d, test="t", random =~ ids|id, struct="CS")
mods[[8]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+edu_expendit, data = d, test="t", random =~ ids|id, struct="CS")
mods[[9]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+milit_expendit, data = d, test="t", random =~ ids|id, struct="CS")
mods[[10]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+Gini, data = d, test="t", random =~ ids|id, struct="CS")
mods[[11]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+energy_renew, data = d, test="t", random =~ ids|id, struct="CS")
mods[[12]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+(MDD), data = d, test="t", random =~ ids|id, struct="CS")
mods[[13]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+(PD), data = d, test="t", random =~ ids|id, struct="CS")
mods[[14]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+(SAD), data = d, test="t", random =~ ids|id, struct="CS")
mods[[15]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+(SP), data = d, test="t", random =~ ids|id, struct="CS")
mods[[16]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+(GAD), data = d, test="t", random =~ ids|id, struct="CS")
mods[[17]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+(PTSD), data = d, test="t", random =~ ids|id, struct="CS")


betas <- unlist(lapply(lapply(mods, function(x) unlist(summary(x)["beta"])), function(x) x[6]))
betase <- unlist(lapply(lapply(mods, function(x) unlist(summary(x)["se"])), function(x) x[6]))
cilb <- unlist(lapply(lapply(mods, function(x) unlist(summary(x)["ci.lb"])), function(x) x[6]))
ciub <- unlist(lapply(lapply(mods, function(x) unlist(summary(x)["ci.ub"])), function(x) x[6]))
samp <- unlist(lapply(mods, function(x) summary(x)$k))
pvals <- unlist(lapply(lapply(mods, function(x) unlist(summary(x)["pval"])), function(x) x[6]))
names(betas) <- names(betase) <- names(cilb) <- names(ciub) <- names(samp) <- names(pvals) <- c("pop_growth","fertility","life_expect","obesity","GDP","taxes","health","edu","milit","Gini","energy_renew",
                                                                                                 "MDD","PD","SAD","SP","GAD","PTSD")
write.csv2(round(cbind(samp,betas,betase,cilb,ciub,pvals),3), "Factbook_Anal.csv")

range(d$pop_growth)
range(d$fertility)
range(d$life_expect)
range(d$obesity)
range(d$GDP) + 50000/5000
range(d$taxes)
range(d$health_expendit)
range(d$edu_expendit, na.rm=T)
range(d$milit_expendit)
range(d$Gini)
range(d$energy_renew)
range(d$MDD, na.rm=T)
range(d$PD, na.rm=T)
range(d$SAD, na.rm=T)
range(d$SP, na.rm=T)
range(d$GAD, na.rm=T)
range(d$PTSD, na.rm=T)

#investigate effect of cultural values orientations
mods <- list() #all studies
mods[[1]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+scale(SVS_harmony, center = mean(f$SVS_harmony), scale = sd(f$SVS_harmony)), data = d, test="t", random =~ ids|id, struct="CS")
mods[[2]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+scale(SVS_embed, center = mean(f$SVS_embed), scale = sd(f$SVS_embed)), data = d, test="t", random =~ ids|id, struct="CS")
mods[[3]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+scale(SVS_hierarchy, center = mean(f$SVS_hierarchy), scale = sd(f$SVS_hierarchy)), data = d, test="t", random =~ ids|id, struct="CS")
mods[[4]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+scale(SVS_mastery, center = mean(f$SVS_mastery), scale = sd(f$SVS_mastery)), data = d, test="t", random =~ ids|id, struct="CS")
mods[[5]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+scale(SVS_autona, center = mean(f$SVS_autona), scale = sd(f$SVS_autona)), data = d, test="t", random =~ ids|id, struct="CS")
mods[[6]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+scale(SVS_autoni, center = mean(f$SVS_autoni), scale = sd(f$SVS_autoni)), data = d, test="t", random =~ ids|id, struct="CS")
mods[[7]] <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+scale(SVS_egalit, center = mean(f$SVS_egalit), scale = sd(f$SVS_egalit)), data = d, test="t", random =~ ids|id, struct="CS")

betas <- unlist(lapply(lapply(mods, function(x) unlist(summary(x)["beta"])), function(x) x[6]))
betase <- unlist(lapply(lapply(mods, function(x) unlist(summary(x)["se"])), function(x) x[6]))
zvals <- unlist(lapply(lapply(mods, function(x) unlist(summary(x)["zval"])), function(x) x[6]))
pvals <- unlist(lapply(lapply(mods, function(x) unlist(summary(x)["pval"])), function(x) x[6]))
names(betas) <- names(betase) <- names(zvals) <- names(pvals) <- names(d[,18:24])
round(cbind(betas,betase,zvals,pvals),3)

temp <- round(with(d, cor(cbind(SVS_mastery, SVS_harmony, SVS_embed, SVS_autona, SVS_autoni, SVS_egalit, SVS_hierarchy ))),2)
quantile(abs(temp[upper.tri(temp)]), c(0.25,0.5,0.75))
temp <- round(with(f, cor(cbind(SVS_mastery, SVS_harmony, SVS_embed, SVS_autona, SVS_autoni, SVS_egalit, SVS_hierarchy ))),2)
quantile(abs(temp[upper.tri(temp)]), c(0.25,0.5,0.75))

#investigate culture value dimensions
round(with(d, cor(data.frame(dv,Year,Male,Age,SVS_d1,SVS_d2,SVS_d3))),2)

mod <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv)+
                scale(SVS_d1, center = mean(f$SVS_d1), scale = sd(f$SVS_d1))+
                scale(SVS_d2, center = mean(f$SVS_d2), scale = sd(f$SVS_d2))+
                scale(SVS_d3, center = mean(f$SVS_d3), scale = sd(f$SVS_d3)), btt=6:8, data = d, test="t", random =~ ids|id, struct="CS")
summary(mod)
mod0 <- rma.mv(yi = d, V = dv, mods =~scale(Year,2010,10)+I(Male-0.5)+Age+I(dv), data = d, test="t", random =~ ids|id, struct="CS")
summary(mod0)

sel <- which(scale(d$SVS_d1, center = mean(f$SVS_d1), scale = sd(f$SVS_d1)) > 1.7)
d[sel[1],c("Country","SVS_d1","SVS_d2","SVS_d3")]
sel <- which(scale(d$SVS_d1, center = mean(f$SVS_d1), scale = sd(f$SVS_d1)) < -1.8)
d[sel[1],c("Country","SVS_d1","SVS_d2","SVS_d3")]

predict(mod, newmods = cbind(c(0,0),c(0,0),c(0,0),c(0,0), c(1.8,-1.8), c(0,0), c(0,0)))

cols <- apply(colorRamp(c("red","blue"))(d$Male),1,function(x) rgb(x[1],x[2],x[3],150, maxColorValue = 255))
plot(scale(d$SVS_d1, center = mean(f$SVS_d1), scale = sd(f$SVS_d1)), d$d, pch=19, cex=size, col=cols, xlim=c(-2,2),
     las=1, bty="l", ylab="standardized cortisol difference (d)", xlab="cultural value dimension: harmony vs. mastery (SD)")
preds <- predict(mod, newmods = cbind(rep(0,24), rep(0,24), rep(0,24), rep(0,24), seq(-2,2, length=24), rep(0,24), rep(0,24)  ))
grid()
lines(seq(-2, 2, length=24), preds$pred, lwd=2, lty=2)
polygon(c(seq(-2, 2, length=24),rev(seq(-2, 2, length=24))), c(preds$ci.lb,rev(preds$ci.ub)), border = NA, col=rgb(0,0,0,.2))
cols <- apply(colorRamp(c("red","blue"))(c(0,.5,1)),1,function(x) rgb(x[1],x[2],x[3],150, maxColorValue = 255))
legend(0,3.6, bty="n", pch=19, cex=1.1, col=cols, legend=c("0% male","50% male","100% male"))
text(-1.8,3.35,"(B)", cex=1.4)


