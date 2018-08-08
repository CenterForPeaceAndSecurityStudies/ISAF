#Load data "Oneal&Russett_fromGoenner.dta", data originally from Cullen Goenner

library("foreign")
library("boolean3")   # "boolean" includes Zelig
## DK: boolean is not available for R version 3.5.0. Do I need boolean3?

## DK: load data
OR <- read.csv("russett.csv")

###Column 1, Table 2.  Straight replication of Oneal & Russett 1999.
replic.1 <- glm(onset1 ~ alliesr + lncaprt + smldepnp + lrgdepnp + peaceyr1 + peaceyr2 + peaceyr3 + peaceyr4 + jntdem + contigkb + logdstab + majdyds, data=OR, family=binomial)
summary(replic.1)

###Boolean logit, Table 2.  Boolean analysis w/ regime type and political relevance paths.  Includes good start values based on prior model estimates of coefficients.
#bp2<- boolprep("a&b&c", "onset1", a = ~ alliesr + lncaprt + smldepnp + lrgdepnp + peaceyr1 + peaceyr2 + peaceyr3 + peaceyr4, b = ~ jntdem, c = ~ contigkb + logdstab + majdyds, data = OR)

## DK: Recreate using boolean3
bp2<- boolprep(onset1 ~ (a & b & c), a ~ alliesr + lncaprt + smldepnp +
							lrgdepnp + peaceyr1 + peaceyr2 + peaceyr3 + peaceyr4,
							b ~ jntdem,
							c ~ contigkb + logdstab + majdyds,
							data = OR)

test <-  boolean(bp2, method="nlminb", control=list(trace=1))
summary(test)

## DK: How does this compare to separate GLMs
require(broom)
m1 <- glm(onset1 ~ alliesr + lncaprt + smldepnp + lrgdepnp + peaceyr1 +
					peaceyr2 + peaceyr3 + peaceyr4,
					family = binomial(link = "logit"),
					data = OR)
tidy(m1)

m2 <- glm(onset1 ~ jntdem,
					family = binomial(link = "logit"),
					data = OR)
tidy(m2)

m3 <- glm(onset1 ~ contigkb + logdstab + majdyds,
					family = binomial(link = "logit"),
					data = OR)
tidy(m3)



#No bootstrapped confidence intervals
russettboolean.cg2 <- zelig(bp2, data=Russett.data, model="boolean", link=c("logit","logit","logit"), method="CG", start.values=c(-0.46, -0.42, -0.17, -8.81, 1.9, -0.28, 0.27, 0.03, 0.08, 5.5, -0.02, 4.8, 4.5, -1.06, 2.9), trace=1)
try(summary(russettboolean.cg2))

#With bootstraps
russettboolean.cg2.boot <- zelig(bp2, data=Russett.data, model="boolean", link=c("logit","logit","logit"), method="CG", start.values=c(-0.46, -0.42, -0.17, -8.81, 1.9, -0.28, 0.27, 0.03, 0.08, 5.5, -0.02, 4.8, 4.5, -1.06, 2.9), boot=250)
try(summary(russettboolean.cg2.boot))

#AIC calculation for boolean bootstrapped
AIC.russettboolean.cg2.boot <- 2*(length(russettboolean.cg2.boot$coefficients)/250) - 2*russettboolean.cg2.boot$value
AIC.russettboolean.cg2.boot

###Table 4 [Appendix A].  Traditional interaction term / multiplicative models.
# Column 1
aldemcon<-alliesr*jntdem*contigkb
aldemdist<-alliesr*jntdem*logdstab
aldemmaj<-alliesr*jntdem*majdyds
capdemcon<-lncaprt*jntdem*contigkb
capdemdist<-lncaprt*jntdem*logdstab
capdemmaj<-lncaprt*jntdem*majdyds
lddemcon<-smldepnp*jntdem*contigkb
lddemdist<-smldepnp*jntdem*logdstab
lddemmaj<-smldepnp*jntdem*majdyds
hddemcon<-lrgdepnp*jntdem*contigkb
hddemdist<-lrgdepnp*jntdem*logdstab
hddemmaj<-lrgdepnp*jntdem*majdyds

russettlogitintmin <- glm(onset1 ~ aldemcon + aldemdist + aldemmaj + capdemcon + capdemdist + capdemmaj + lddemcon + lddemdist + lddemmaj + hddemcon + hddemdist + hddemmaj + peaceyr1 + peaceyr2 + peaceyr3 + peaceyr4, family=binomial(link="logit"))
summary(russettlogitintmin)

# Column 2
russettlogitintmed <- glm(onset1 ~ alliesr + lncaprt + smldepnp + lrgdepnp + jntdem + contigkb + logdstab + majdyds + aldemcon + aldemdist + aldemmaj + capdemcon + capdemdist + capdemmaj + lddemcon + lddemdist + lddemmaj + hddemcon + hddemdist + hddemmaj + peaceyr1 + peaceyr2 + peaceyr3 + peaceyr4, family=binomial(link="logit"))
summary(russettlogitintmed)

###Figure 3.  2D graphs showing change in effect of other variables on pr(conflict) in different regime type / relevance scenarios
#Stuff to do to initialize...
#library("RColorBrewer")
#mycols<-brewer.pal(4,"BrBG")
#mycols<-brewer.pal(7,"Reds")[c(2,4,7)]
mycols<-c("lightblue4", "darkred")

preddiff<-function(x,y){    # Calculate reduction in "slope" of predictions
	foo1<-abs(x[1]-x[length(x)])  # i.e. yhatmax - yhatmin
	foo2<-abs(y[1]-y[length(y)])
	1-(foo2/foo1)}

#pdf(file="/Users/bfbraum/Documents/Research/present/book2/paths/StudiesOfInterest/Russett-Schultz-Rus-effects.pdf", width=7, height=7)
par(mar=c(5,3,3,1))
par(mfrow=c(2,2), pty="s")

allypred<-logit(b.rus[1]+b.rus[2]*c(0,1)+b.rus[3]*min(Russett.data$lncaprt)+b.rus[4]*min(Russett.data$smldepnp)+b.rus[5]*max(Russett.data$lrgdepnp)+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*min(Russett.data$jntdem))*logit(b.rus[12]+b.rus[13]*max(Russett.data$contigkb)+b.rus[14]*min(Russett.data$logdstab)+b.rus[15]*max(Russett.data$majdyds))
allypred.dem.hi<-logit(b.rus[1]+b.rus[2]*c(0,1)+b.rus[3]*min(Russett.data$lncaprt)+b.rus[4]*min(Russett.data$smldepnp)+b.rus[5]*max(Russett.data$lrgdepnp)+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*max(Russett.data$jntdem))*logit(b.rus[12]+b.rus[13]*max(Russett.data$contigkb)+b.rus[14]*min(Russett.data$logdstab)+b.rus[15]*max(Russett.data$majdyds))
allypred.rel.min<-logit(b.rus[1]+b.rus[2]*c(0,1)+b.rus[3]*min(Russett.data$lncaprt)+b.rus[4]*min(Russett.data$smldepnp)+b.rus[5]*max(Russett.data$lrgdepnp)+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*min(Russett.data$jntdem))*logit(b.rus[12]+b.rus[13]*min(Russett.data$contigkb)+b.rus[14]*3.849736+b.rus[15]*min(Russett.data$majdyds))
allypred.rel.lo<-logit(b.rus[1]+b.rus[2]*c(0,1)+b.rus[3]*min(Russett.data$lncaprt)+b.rus[4]*min(Russett.data$smldepnp)+b.rus[5]*max(Russett.data$lrgdepnp)+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*min(Russett.data$jntdem))*logit(b.rus[12]+b.rus[13]*min(Russett.data$contigkb)+b.rus[14]*max(Russett.data$logdstab)+b.rus[15]*min(Russett.data$majdyds))
allypred.demhi.rello<-logit(b.rus[1]+b.rus[2]*c(0,1)+b.rus[3]*min(Russett.data$lncaprt)+b.rus[4]*min(Russett.data$smldepnp)+b.rus[5]*max(Russett.data$lrgdepnp)+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*max(Russett.data$jntdem))*logit(b.rus[12]+b.rus[13]*min(Russett.data$contigkb)+b.rus[14]*max(Russett.data$logdstab)+b.rus[15]*min(Russett.data$majdyds))

plot(seq(0,1,by=1), allypred, type="b", lty=1, lwd=2, col=mycols[2], xlim=c(-0.5,1.5), ylim=c(0,0.2), xaxt="n", ylab="Probability of MID Onset", xlab="Alliance")
axis(1,c(0.1,0.9), c("Absent","Present"), tick=FALSE)
lines(seq(0,1,by=1), allypred.rel.min, type="b", lty=2, pch=3, lwd=2, col=mycols[2])
lines(seq(0,1,by=1), allypred.dem.hi, type="b", lty=4, pch=2, lwd=2, col=mycols[1])
lines(seq(0,1,by=1), allypred.rel.lo, type="b", lty=3, pch=4, lwd=2, col=mycols[2])

preddiff(allypred,allypred.rel.min)
preddiff(allypred,allypred.dem.hi)
preddiff(allypred,allypred.rel.lo)
preddiff(allypred,allypred.demhi.rello)
# will be the same for all variables

cappred<-logit(b.rus[1]+b.rus[2]*min(Russett.data$alliesr)+b.rus[3]*seq(min(Russett.data$lncaprt), max(Russett.data$lncaprt), by=0.02*(max(Russett.data$lncaprt)-min(Russett.data$lncaprt)))+b.rus[4]*min(Russett.data$smldepnp)+b.rus[5]*max(Russett.data$lrgdepnp)+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*min(Russett.data$jntdem))*logit(b.rus[12]+b.rus[13]*max(Russett.data$contigkb)+b.rus[14]*min(Russett.data$logdstab)+b.rus[15]*max(Russett.data$majdyds))
cappred.dem.hi<-logit(b.rus[1]+b.rus[2]*min(Russett.data$alliesr)+b.rus[3]*seq(min(Russett.data$lncaprt), max(Russett.data$lncaprt), by=0.02*(max(Russett.data$lncaprt)-min(Russett.data$lncaprt)))+b.rus[4]*min(Russett.data$smldepnp)+b.rus[5]*max(Russett.data$lrgdepnp)+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*max(Russett.data$jntdem))*logit(b.rus[12]+b.rus[13]*max(Russett.data$contigkb)+b.rus[14]*min(Russett.data$logdstab)+b.rus[15]*max(Russett.data$majdyds))
cappred.rel.min<-logit(b.rus[1]+b.rus[2]*min(Russett.data$alliesr)+b.rus[3]*seq(min(Russett.data$lncaprt), max(Russett.data$lncaprt), by=0.02*(max(Russett.data$lncaprt)-min(Russett.data$lncaprt)))+b.rus[4]*min(Russett.data$smldepnp)+b.rus[5]*max(Russett.data$lrgdepnp)+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*min(Russett.data$jntdem))*logit(b.rus[12]+b.rus[13]*min(Russett.data$contigkb)+b.rus[14]*3.849736+b.rus[15]*min(Russett.data$majdyds))
cappred.rel.lo<-logit(b.rus[1]+b.rus[2]*min(Russett.data$alliesr)+b.rus[3]*seq(min(Russett.data$lncaprt), max(Russett.data$lncaprt), by=0.02*(max(Russett.data$lncaprt)-min(Russett.data$lncaprt)))+b.rus[4]*min(Russett.data$smldepnp)+b.rus[5]*max(Russett.data$lrgdepnp)+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*min(Russett.data$jntdem))*logit(b.rus[12]+b.rus[13]*min(Russett.data$contigkb)+b.rus[14]*max(Russett.data$logdstab)+b.rus[15]*min(Russett.data$majdyds))

plot(seq(0,1,by=0.02), cappred, type="l", lwd=2, col=mycols[2], ylim=c(0,0.2), xaxt="n", ylab="Probability of MID Onset", xlab="Capability Ratio")
axis(1,c(0.1,0.9), c("Low","High"), tick=FALSE)
lines(seq(0,1,by=0.02), cappred.rel.min, lty=2, lwd=2, col=mycols[2])
lines(seq(0,1,by=0.02), cappred.dem.hi, lty=4, lwd=2, col=mycols[1])
lines(seq(0,1,by=0.02), cappred.rel.lo, lty=3, lwd=2, col=mycols[2])
legend("topright", legend=c("Nondemocratic, Relevant", "Nondemocratic, Min. Irrelevant", "Democratic, Relevant", "Nondemocratic, Most Irrelevant"), col=mycols[c(2,2,1,2)], lty=c(1,2,4,3), lwd=c(2,2,2,2), cex=.65)

ldpred<-logit(b.rus[1]+b.rus[2]*min(Russett.data$alliesr)+b.rus[3]*min(Russett.data$lncaprt)+b.rus[4]*seq(min(Russett.data$smldepnp), max(Russett.data$smldepnp), by=0.02*(max(Russett.data$smldepnp)-min(Russett.data$smldepnp)))+b.rus[5]*max(Russett.data$lrgdepnp)+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*min(Russett.data$jntdem))*logit(b.rus[12]+b.rus[13]*max(Russett.data$contigkb)+b.rus[14]*min(Russett.data$logdstab)+b.rus[15]*max(Russett.data$majdyds))
ldpred.dem.hi<-logit(b.rus[1]+b.rus[2]*min(Russett.data$alliesr)+b.rus[3]*min(Russett.data$lncaprt)+b.rus[4]*seq(min(Russett.data$smldepnp), max(Russett.data$smldepnp), by=0.02*(max(Russett.data$smldepnp)-min(Russett.data$smldepnp)))+b.rus[5]*max(Russett.data$lrgdepnp)+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*max(Russett.data$jntdem))*logit(b.rus[12]+b.rus[13]*max(Russett.data$contigkb)+b.rus[14]*min(Russett.data$logdstab)+b.rus[15]*max(Russett.data$majdyds))
ldpred.rel.min<-logit(b.rus[1]+b.rus[2]*min(Russett.data$alliesr)+b.rus[3]*min(Russett.data$lncaprt)+b.rus[4]*seq(min(Russett.data$smldepnp), max(Russett.data$smldepnp), by=0.02*(max(Russett.data$smldepnp)-min(Russett.data$smldepnp)))+b.rus[5]*max(Russett.data$lrgdepnp)+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*min(Russett.data$jntdem))*logit(b.rus[12]+b.rus[13]*min(Russett.data$contigkb)+b.rus[14]*3.849736+b.rus[15]*min(Russett.data$majdyds))
ldpred.rel.lo<-logit(b.rus[1]+b.rus[2]*min(Russett.data$alliesr)+b.rus[3]*min(Russett.data$lncaprt)+b.rus[4]*seq(min(Russett.data$smldepnp), max(Russett.data$smldepnp), by=0.02*(max(Russett.data$smldepnp)-min(Russett.data$smldepnp)))+b.rus[5]*max(Russett.data$lrgdepnp)+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*min(Russett.data$jntdem))*logit(b.rus[12]+b.rus[13]*min(Russett.data$contigkb)+b.rus[14]*max(Russett.data$logdstab)+b.rus[15]*min(Russett.data$majdyds))

plot(seq(0,1,by=0.02), ldpred, type="l", lwd=2, col=mycols[2], ylim=c(0,0.2), xaxt="n", ylab="Probability of MID Onset", xlab="Lower Trade Dependency")
axis(1,c(0.1,0.9), c("Low","High"), tick=FALSE)
lines(seq(0,1,by=0.02), ldpred.rel.min, lty=2, lwd=2, col=mycols[2])
lines(seq(0,1,by=0.02), ldpred.dem.hi, lty=4, lwd=2, col=mycols[1])
lines(seq(0,1,by=0.02), ldpred.rel.lo, lty=3, lwd=2, col=mycols[2])

hdpred<-logit(b.rus[1]+b.rus[2]*min(Russett.data$alliesr)+b.rus[3]*min(Russett.data$lncaprt)+b.rus[4]*min(Russett.data$smldepnp)+b.rus[5]*seq(min(Russett.data$lrgdepnp), max(Russett.data$lrgdepnp), by=0.02*(max(Russett.data$lrgdepnp)-min(Russett.data$lrgdepnp)))+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*min(Russett.data$jntdem))*logit(b.rus[12]+b.rus[13]*max(Russett.data$contigkb)+b.rus[14]*min(Russett.data$logdstab)+b.rus[15]*max(Russett.data$majdyds))
hdpred.dem.hi<-logit(b.rus[1]+b.rus[2]*min(Russett.data$alliesr)+b.rus[3]*min(Russett.data$lncaprt)+b.rus[4]*min(Russett.data$smldepnp)+b.rus[5]*seq(min(Russett.data$lrgdepnp), max(Russett.data$lrgdepnp), by=0.02*(max(Russett.data$lrgdepnp)-min(Russett.data$lrgdepnp)))+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*max(Russett.data$jntdem))*logit(b.rus[12]+b.rus[13]*max(Russett.data$contigkb)+b.rus[14]*min(Russett.data$logdstab)+b.rus[15]*max(Russett.data$majdyds))
hdpred.rel.min<-logit(b.rus[1]+b.rus[2]*min(Russett.data$alliesr)+b.rus[3]*min(Russett.data$lncaprt)+b.rus[4]*min(Russett.data$smldepnp)+b.rus[5]*seq(min(Russett.data$lrgdepnp), max(Russett.data$lrgdepnp), by=0.02*(max(Russett.data$lrgdepnp)-min(Russett.data$lrgdepnp)))+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*min(Russett.data$jntdem))*logit(b.rus[12]+b.rus[13]*min(Russett.data$contigkb)+b.rus[14]*3.849736+b.rus[15]*min(Russett.data$majdyds))
hdpred.rel.lo<-logit(b.rus[1]+b.rus[2]*min(Russett.data$alliesr)+b.rus[3]*min(Russett.data$lncaprt)+b.rus[4]*min(Russett.data$smldepnp)+b.rus[5]*seq(min(Russett.data$lrgdepnp), max(Russett.data$lrgdepnp), by=0.02*(max(Russett.data$lrgdepnp)-min(Russett.data$lrgdepnp)))+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*min(Russett.data$jntdem))*logit(b.rus[12]+b.rus[13]*min(Russett.data$contigkb)+b.rus[14]*max(Russett.data$logdstab)+b.rus[15]*min(Russett.data$majdyds))

plot(seq(0,1,by=0.02), hdpred, type="l", lwd=2, col=mycols[2], ylim=c(0,0.2), xaxt="n", ylab="Probability of MID Onset", xlab="Higher Trade Dependency")
axis(1,c(0.1,0.9), c("Low","High"), tick=FALSE)
lines(seq(0,1,by=0.02), hdpred.rel.min, lty=2, lwd=2, col=mycols[2])
lines(seq(0,1,by=0.02), hdpred.dem.hi, lty=4, lwd=2, col=mycols[1])
lines(seq(0,1,by=0.02), hdpred.rel.lo, lty=3, lwd=2, col=mycols[2])

#dev.off(2)


###Figure 2.  3D graphics showing joint effect of increasing dyadic democracy and political irrelevance on upper bound of conflict
###for major power dyads and for non-major power dayads (including comparison to standard loss-of-strength prediction).

#Upper-bounds plots for political relevance and democracy: Russett and Oneal
mypolygon<-function(x1,x2,y1,y2,color="grey85"){
	xx<-c(x1, rev(x2))
	yy<-c(y1, rev(y2))
	polygon(xx,yy,col=color,border=NA)
	}

bouldinglog<-function(x){.535^(log(exp(x)/500)+(10-2.71828183))} # LOS gradient
logdist<-seq(min(Russett.data$logdstab), max(Russett.data$logdstab), by=0.02*(max(Russett.data$logdstab)-min(Russett.data$logdstab)))
lines(seq(0,1,by=0.02),bouldinglog(logdist), col="blue", lty=4)
legend("topright", legend=c("Boulding/BdM LOS Gradient", "Estimated Relationship"), col=c("blue", "darkred"), lty=c(4,1), cex=.5)

#Effects plots for other variables:  Russett and Oneal

#Stuff to do to initialize...
#library("RColorBrewer")
#mycols<-brewer.pal(4,"BrBG")
#mycols<-brewer.pal(7,"Reds")[c(2,4,7)]
mycols<-c("lightblue4", "darkred")

preddiff<-function(x,y){    # Calculate reduction in "slope" of predictions
	foo1<-abs(x[1]-x[length(x)])  # i.e. yhatmax - yhatmin
	foo2<-abs(y[1]-y[length(y)])
	1-(foo2/foo1)}

#Effects plots for other variables in 3D:  Russett and Oneal

logit<-function(x){1/(1+exp((-1)*x))}

dist<-seq(min(Russett.data$logdstab), max(Russett.data$logdstab), by=0.05*(max(Russett.data$logdstab)-min(Russett.data$logdstab)))

dem<-seq(min(Russett.data$jntdem), max(Russett.data$jntdem), by=0.05*(max(Russett.data$jntdem)-min(Russett.data$jntdem)))

#pdf(file="/Users/bfbraum/Documents/Research/present/book2/paths/StudiesOfInterest/Russett-Schultz-Rus-effects3Dc.pdf", width=7, height=3.5)

par(mar=c(4,2,2,1))
par(mfrow=c(1,2), pty="s")
par(cex=.75)
par(cex.axis=.75)
shading<-0.25

init.dem.dist<-function(dem,dist){logit(b.rus[1]+b.rus[2]*min(Russett.data$alliesr)+b.rus[3]*min(Russett.data$lncaprt)+b.rus[4]*min(Russett.data$smldepnp)+b.rus[5]*max(Russett.data$lrgdepnp)+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*dem)*logit(b.rus[12]+b.rus[13]*0+b.rus[14]*dist+b.rus[15]*0)}

Upper.Bound<-outer(dem,dist,init.dem.dist)

colormatB<-matrix("grey80", nr=nrow(Upper.Bound)-1, nc=ncol(Upper.Bound)-1)
colormatB[seq(1,6,by=1),seq(1,4,by=1)]<-"darkred"
colormatB[seq(1,10,by=1),seq(14,20,by=1)]<-"darkred"
colormatB[seq(10,14,by=1),seq(15,20,by=1)]<-"darkred"
colormatB[seq(13,18,by=1),seq(16,20,by=1)]<-"darkred"
colormatB[seq(16,20,by=1),seq(17,20,by=1)]<-"darkred"

persp(dem,dist,Upper.Bound,theta=295,phi=25, col=colormatB,d=0.1,shade=shading, border=NA, r=20, xlab="Dyadic Democracy", ylab="Distance",zlab="Upper Bound on Pr(Init)", ticktype="simple", main="Non-Major Power Dyads", zlim=c(0,max(Upper.Bound))) -> my3dplot
text(trans3d(x=400,y=min(dist)-0.5,z=0,pmat=my3dplot),labels="0.00", cex=.65)
text(trans3d(x=400,y=min(dist)-0.5,z=.05,pmat=my3dplot),labels="0.05", cex=.65)
text(trans3d(x=400,y=min(dist)-0.5,z=.10,pmat=my3dplot),labels="0.10", cex=.65)
text(trans3d(x=400,y=min(dist)-0.5,z=.15,pmat=my3dplot),labels="0.15", cex=.65)

bouldinglog<-function(x){.535^(log(exp(x)/500)+(10-2.71828183))} # LOS gradient
logdist<-seq(min(Russett.data$logdstab), max(Russett.data$logdstab), by=0.05*(max(Russett.data$logdstab)-min(Russett.data$logdstab)))
lines(trans3d(y=dist, 0, z=bouldinglog(logdist), pmat=my3dplot), lty=2, lwd=2)

init.dem.dist<-function(dem,dist){logit(b.rus[1]+b.rus[2]*min(Russett.data$alliesr)+b.rus[3]*min(Russett.data$lncaprt)+b.rus[4]*min(Russett.data$smldepnp)+b.rus[5]*max(Russett.data$lrgdepnp)+b.rus[6]*mean(Russett.data$peaceyr1)+b.rus[7]*mean(Russett.data$peaceyr2)+b.rus[8]*mean(Russett.data$peaceyr3)+b.rus[9]*mean(Russett.data$peaceyr4))*logit(b.rus[10]+b.rus[11]*dem)*logit(b.rus[12]+b.rus[13]*0+b.rus[14]*dist+b.rus[15]*1)}

Upper.Bound<-outer(dem,dist,init.dem.dist)

colormatA<-matrix("grey80", nr=nrow(Upper.Bound)-1, nc=ncol(Upper.Bound)-1)
colormatA[seq(1,8,by=1),seq(1,9,by=1)]<-"darkred"
colormatA[seq(1,7,by=1),20]<-"darkred"

persp(dem,dist,Upper.Bound,theta=295,phi=25, col=colormatA,d=0.1,shade=shading, border=NA,r=20, xlab="Dyadic Democracy", ylab="Distance",zlab="Upper Bound on Pr(Init)", ticktype="simple", main="Major Power Dyads", zlim=c(0,max(Upper.Bound))) -> my3dplot
text(trans3d(x=400,y=min(dist)-0.5,z=0,pmat=my3dplot),labels="0.00", cex=.65)
text(trans3d(x=400,y=min(dist)-0.5,z=.05,pmat=my3dplot),labels="0.05", cex=.65)
text(trans3d(x=400,y=min(dist)-0.5,z=.10,pmat=my3dplot),labels="0.10", cex=.65)
text(trans3d(x=400,y=min(dist)-0.5,z=.15,pmat=my3dplot),labels="0.15", cex=.65)

bouldinglog<-function(x){.535^(log(exp(x)/500)+(10-2.71828183))} # LOS gradient
logdist<-seq(min(Russett.data$logdstab), max(Russett.data$logdstab), by=0.05*(max(Russett.data$logdstab)-min(Russett.data$logdstab)))
lines(trans3d(y=dist, 0, z=bouldinglog(logdist), pmat=my3dplot), lty=2, lwd=2)

#dev.off(2)
