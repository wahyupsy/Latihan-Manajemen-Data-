### R code from vignette source 'amelia-jss.Rnw'

###################################################
### code chunk number 1: amelia-jss.Rnw:439-442
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
options(show.signif.stars = FALSE)
set.seed(12345)


###################################################
### code chunk number 2: amelia-jss.Rnw:447-448 (eval = FALSE)
###################################################
## install.packages("Amelia")


###################################################
### code chunk number 3: amelia-jss.Rnw:454-456 (eval = FALSE)
###################################################
## install.packages("Amelia", repos = "http://r.iq.harvard.edu", type
## = "source")


###################################################
### code chunk number 4: amelia-jss.Rnw:460-461 (eval = FALSE)
###################################################
## update.packages()


###################################################
### code chunk number 5: amelia-jss.Rnw:482-484 (eval = FALSE)
###################################################
## library("Amelia")
## AmeliaView()


###################################################
### code chunk number 6: amelia-jss.Rnw:516-518
###################################################
require("Amelia")
data("freetrade")


###################################################
### code chunk number 7: amelia-jss.Rnw:524-525
###################################################
summary(freetrade)


###################################################
### code chunk number 8: amelia-jss.Rnw:532-534
###################################################
summary(lm(tariff ~ polity + pop + gdp.pc + year + country, 
          data = freetrade)) 


###################################################
### code chunk number 9: amelia-jss.Rnw:571-573
###################################################
a.out <- amelia(freetrade, m = 5, ts = "year", cs = "country")
a.out


###################################################
### code chunk number 10: hist1plot
###################################################
hist(a.out$imputations[[3]]$tariff, col="grey", border="white")


###################################################
### code chunk number 11: amelia-jss.Rnw:597-598
###################################################
options(SweaveHooks = list(fig = function() par(mfrow=c(1,1))))


###################################################
### code chunk number 12: hist1
###################################################
hist(a.out$imputations[[3]]$tariff, col="grey", border="white")


###################################################
### code chunk number 13: amelia-jss.Rnw:617-618 (eval = FALSE)
###################################################
## save(a.out, file = "imputations.RData")


###################################################
### code chunk number 14: amelia-jss.Rnw:627-628 (eval = FALSE)
###################################################
## write.amelia(obj=a.out, file.stem = "outdata")


###################################################
### code chunk number 15: amelia-jss.Rnw:643-644 (eval = FALSE)
###################################################
## write.amelia(obj=a.out, file.stem = "outdata", format = "dta")


###################################################
### code chunk number 16: amelia-jss.Rnw:660-662
###################################################
a.out.more <- amelia(freetrade, m = 10, ts = "year", cs = "country", p2s=0)
a.out.more


###################################################
### code chunk number 17: amelia-jss.Rnw:666-668
###################################################
a.out.more <- ameliabind(a.out, a.out.more)
a.out.more


###################################################
### code chunk number 18: amelia-jss.Rnw:689-692 (eval = FALSE)
###################################################
## b<-round(runif(1,min=1111,max=9999))
## random.name<-paste("am",b,sep="")
## amelia <- write.amelia(obj=a.out, file.stem = random.name)


###################################################
### code chunk number 19: amelia-jss.Rnw:722-723
###################################################
amelia(freetrade, m = 1, ts = "year", cs = "country", p2s = 2)


###################################################
### code chunk number 20: amelia-jss.Rnw:781-782
###################################################
table(round(a.out$imputations[[3]]$polity, digits = 3))


###################################################
### code chunk number 21: amelia-jss.Rnw:799-802
###################################################
a.out1 <- amelia(freetrade, m = 5, ts = "year", cs = "country", ords =
                 "polity", p2s = 0)
table(a.out1$imputations[[3]]$polity)


###################################################
### code chunk number 22: amelia-jss.Rnw:820-821
###################################################
table(round(a.out1$imputations[[3]]$signed, digits = 3))


###################################################
### code chunk number 23: amelia-jss.Rnw:838-841
###################################################
a.out2 <- amelia(freetrade, m = 5, ts = "year", cs = "country", noms =
                 "signed", p2s = 0)
table(a.out2$imputations[[3]]$signed)


###################################################
### code chunk number 24: logshist
###################################################
hist(freetrade$tariff, col="grey", border="white")
hist(log(freetrade$tariff), col="grey", border="white")


###################################################
### code chunk number 25: amelia-jss.Rnw:875-876
###################################################
options(SweaveHooks = list(fig = function() par(mfrow=c(1,2))))


###################################################
### code chunk number 26: hist2
###################################################
hist(freetrade$tariff, col="grey", border="white")
hist(log(freetrade$tariff), col="grey", border="white")


###################################################
### code chunk number 27: amelia-jss.Rnw:890-891
###################################################
options(SweaveHooks = list(fig = function() par(mfrow=c(1,1))))


###################################################
### code chunk number 28: amelia-jss.Rnw:926-927
###################################################
amelia(freetrade, idvars = c("year", "country"))


###################################################
### code chunk number 29: amelia-jss.Rnw:934-935
###################################################
a.out2 <- amelia(freetrade, idvars = c("year"))


###################################################
### code chunk number 30: amelia-jss.Rnw:971-972
###################################################
a.out2 <- amelia(freetrade, ts = "year", cs = "country", polytime = 2)


###################################################
### code chunk number 31: amelia-jss.Rnw:992-994
###################################################
a.out.time <- amelia(freetrade, ts = "year", cs = "country", polytime = 2,
                 intercs = TRUE, p2s = 2)


###################################################
### code chunk number 32: tcomp1
###################################################
tscsPlot(a.out, cs = "Malaysia", var = "tariff", ylim = c(-10,60),
         main = "Malaysia (no time settings)")
         

tscsPlot(a.out.time, cs = "Malaysia", var = "tariff", ylim = c(-10,60), 
         main = "Malaysia (with time settings)")


###################################################
### code chunk number 33: amelia-jss.Rnw:1015-1016
###################################################
options(SweaveHooks = list(fig = function() par(mfrow=c(1,2))))


###################################################
### code chunk number 34: timecompare
###################################################
tscsPlot(a.out, cs = "Malaysia", var = "tariff", ylim = c(-10,60),
         main = "Malaysia (no time settings)")
         

tscsPlot(a.out.time, cs = "Malaysia", var = "tariff", ylim = c(-10,60), 
         main = "Malaysia (with time settings)")


###################################################
### code chunk number 35: amelia-jss.Rnw:1048-1050
###################################################
a.out2 <- amelia(freetrade, ts = "year", cs = "country", lags = "tariff",
                 leads = "tariff")


###################################################
### code chunk number 36: amelia-jss.Rnw:1078-1079
###################################################
a.out.time


###################################################
### code chunk number 37: amelia-jss.Rnw:1102-1105
###################################################
a.out.time2 <- amelia(freetrade, ts = "year", cs = "country", polytime = 2,
                 intercs = TRUE, p2s = 0, empri = .01*nrow(freetrade))
a.out.time2


###################################################
### code chunk number 38: tcomp2
###################################################
tscsPlot(a.out.time, cs = "Malaysia",  var = "tariff", ylim = c(-10, 60), 
         main = "Malaysia (no ridge prior)")

tscsPlot(a.out.time2, cs = "Malaysia", var = "tariff", ylim = c(-10, 60), 
         main = "Malaysia (with ridge prior)")


###################################################
### code chunk number 39: amelia-jss.Rnw:1118-1119
###################################################
options(SweaveHooks = list(fig = function() par(mfrow=c(1,2))))


###################################################
### code chunk number 40: timecomp2
###################################################
tscsPlot(a.out.time, cs = "Malaysia",  var = "tariff", ylim = c(-10, 60), 
         main = "Malaysia (no ridge prior)")

tscsPlot(a.out.time2, cs = "Malaysia", var = "tariff", ylim = c(-10, 60), 
         main = "Malaysia (with ridge prior)")


###################################################
### code chunk number 41: amelia-jss.Rnw:1173-1174
###################################################
freetrade[freetrade$country == "Thailand", c("year","country","tariff")]


###################################################
### code chunk number 42: amelia-jss.Rnw:1178-1179 (eval = FALSE)
###################################################
## #$


###################################################
### code chunk number 43: amelia-jss.Rnw:1187-1189
###################################################
pr <- matrix(c(158,159,160,3,3,3,40,40,40,3,3,3), nrow=3, ncol=4) 
pr


###################################################
### code chunk number 44: amelia-jss.Rnw:1196-1197
###################################################
a.out.pr <- amelia(freetrade, ts = "year", cs = "country", priors = pr)


###################################################
### code chunk number 45: amelia-jss.Rnw:1206-1208
###################################################
pr.2 <- matrix(c(158,159,160,3,3,3,34,34,34,46,46,46,.95,.95,.95), nrow=3, ncol=5)
pr.2


###################################################
### code chunk number 46: amelia-jss.Rnw:1219-1221
###################################################
pr.3 <- matrix(c(158,159,160,0,3,3,3,3,40,40,40,20,3,3,3,5), nrow=4, ncol=4)
pr.3


###################################################
### code chunk number 47: amelia-jss.Rnw:1267-1269
###################################################
bds <- matrix(c(3, 30, 40), nrow = 1, ncol = 3)
bds


###################################################
### code chunk number 48: amelia-jss.Rnw:1274-1276
###################################################
a.out.bds <- amelia(freetrade, ts = "year", cs = "country", bounds = bds,
                    max.resample = 1000)


###################################################
### code chunk number 49: bounds
###################################################
tscsPlot(a.out, cs = "Malaysia", main = "No logical bounds", var =
         "tariff", ylim = c(-10,60))

tscsPlot(a.out.bds, cs = "Malaysia", main = "Bounded between 30 and 40", var =
         "tariff", ylim = c(-10,60))


###################################################
### code chunk number 50: amelia-jss.Rnw:1299-1300
###################################################
options(SweaveHooks = list(fig = function() par(mfrow=c(1,2))))


###################################################
### code chunk number 51: boundscomp
###################################################
tscsPlot(a.out, cs = "Malaysia", main = "No logical bounds", var =
         "tariff", ylim = c(-10,60))

tscsPlot(a.out.bds, cs = "Malaysia", main = "Bounded between 30 and 40", var =
         "tariff", ylim = c(-10,60))


###################################################
### code chunk number 52: plotmeth
###################################################
plot(a.out, which.vars = 3:6)


###################################################
### code chunk number 53: plot1
###################################################
plot(a.out, which.vars = 3:6)


###################################################
### code chunk number 54: amelia-jss.Rnw:1374-1375
###################################################
compare.density(a.out, var = "signed")


###################################################
### code chunk number 55: amelia-jss.Rnw:1378-1379
###################################################
options(SweaveHooks = list(fig = function() par(mfrow=c(1,1))))


###################################################
### code chunk number 56: overimp
###################################################
overimpute(a.out, var = "tariff")


###################################################
### code chunk number 57: amelia-jss.Rnw:1411-1412
###################################################
options(SweaveHooks = list(fig = function() par(mfrow=c(1,1))))


###################################################
### code chunk number 58: oi2
###################################################
overimpute(a.out, var = "tariff")


###################################################
### code chunk number 59: overimp-bad
###################################################
dd <- Amelia:::rmvnorm(50, mu = c(0.5,0.5), vcv =
                       matrix(c(0.25^2,.06, .06,0.25^2),2,2))
ddmiss <- sample(1:50, replace = FALSE, size = 10)
is.na(dd) <- ddmiss
aa.out <- amelia(dd, m= 5)
overimpute(aa.out, var = 2, main = "Observed versus Imputed Values")


###################################################
### code chunk number 60: oi
###################################################
dd <- Amelia:::rmvnorm(50, mu = c(0.5,0.5), vcv =
                       matrix(c(0.25^2,.06, .06,0.25^2),2,2))
ddmiss <- sample(1:50, replace = FALSE, size = 10)
is.na(dd) <- ddmiss
aa.out <- amelia(dd, m= 5)
overimpute(aa.out, var = 2, main = "Observed versus Imputed Values")


###################################################
### code chunk number 61: disp1d
###################################################
disperse(a.out, dims = 1, m = 5)
disperse(a.out, dims = 2, m = 5)


###################################################
### code chunk number 62: amelia-jss.Rnw:1538-1539
###################################################
options(SweaveHooks = list(fig = function() par(mfrow=c(1,2))))


###################################################
### code chunk number 63: disp1dfig
###################################################
disperse(a.out, dims = 1, m = 5)
disperse(a.out, dims = 2, m = 5)


###################################################
### code chunk number 64: amelia-jss.Rnw:1576-1578
###################################################
freetrade2 <- freetrade
freetrade2$tariff2 <- freetrade2$tariff*2+3


###################################################
### code chunk number 65: amelia-jss.Rnw:1584-1586
###################################################
a.out.bad <- amelia(freetrade2, ts = "year", cs = "country")
a.out.bad


###################################################
### code chunk number 66: dispbad
###################################################
disperse(a.out.bad, dims = 1, m = 5)


###################################################
### code chunk number 67: amelia-jss.Rnw:1603-1604
###################################################
options(SweaveHooks = list(fig = function() par(mfrow=c(1,1))))


###################################################
### code chunk number 68: dispbadfig
###################################################
disperse(a.out.bad, dims = 1, m = 5)


###################################################
### code chunk number 69: tsplot1
###################################################
tscsPlot(a.out.time, cs = "Malaysia", var = "tariff", ylim = c(-10, 60), 
         main = "Malaysia (with time settings)")


###################################################
### code chunk number 70: tsplot2
###################################################
tscsPlot(a.out.time, cs = "Malaysia", var = "tariff", ylim = c(-10, 60), 
         main = "Malaysia (with time settings)")


###################################################
### code chunk number 71: mmap1
###################################################
missmap(a.out) 


###################################################
### code chunk number 72: mmap2
###################################################
missmap(a.out) 


###################################################
### code chunk number 73: amelia-jss.Rnw:1742-1745
###################################################
require("Zelig")
z.out <- zelig(tariff ~ polity + pop + gdp.pc + year +country, data =
               freetrade, model = "ls", cite = FALSE)


###################################################
### code chunk number 74: amelia-jss.Rnw:1748-1749
###################################################
summary(z.out)


###################################################
### code chunk number 75: amelia-jss.Rnw:1757-1759
###################################################
z.out.imp <- zelig(tariff ~ polity + pop + gdp.pc + year +country, data =
                   a.out$imputations, model = "ls", cite = FALSE)


###################################################
### code chunk number 76: amelia-jss.Rnw:1762-1763
###################################################
summary(z.out.imp)


###################################################
### code chunk number 77: amelia-jss.Rnw:1814-1816 (eval = FALSE)
###################################################
## library("Amelia")
## AmeliaView()


