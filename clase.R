dato<-read.csv("C://BENJA//clase//Tooth.csv")
attach(dato)
names(dato)

#anova
m<-aov(len~parcela*bloque)
summary(m)
#prueba posthoc
TukeyHSD(m, "parcela", ordered = TRUE)
TukeyHSD(m, "bloque", ordered = TRUE)
##Grafica
d1<-c(mean(len[parcela=="OJ" & bloque=="a"]),mean(len[parcela=="OJ" & bloque=="b"]),mean(len[parcela=="OJ" & bloque=="c"]))
d<-c(mean(len[parcela=="VC" & bloque=="a"]),mean(len[parcela=="VC" & bloque=="b"]),mean(len[parcela=="VC" & bloque=="c"]))
tt<-as.table(cbind(d1,d))
dimnames(tt)<- list(gender=c("a","b","c"), party=c("OJ","VC"))
bar<-barplot(tt,beside = T, las=1, legend.text=T, ylim=c(0,40))

ER<-function(x) {sqrt((var(x))/length(x))}
e1<-c(ER(len[parcela=="OJ" & bloque=="a"]),ER(len[parcela=="OJ" & bloque=="b"]),ER(len[parcela=="OJ" & bloque=="c"]))
e<-c(ER(len[parcela=="VC" & bloque=="a"]),ER(len[parcela=="VC" & bloque=="b"]),ER(len[parcela=="VC" & bloque=="c"]))
err<-c(e1,e)
segments(bar, tt-err, bar, tt+err)
segments(bar+0.1, tt-err, bar-0.1, tt-err)
segments(bar+0.1, tt+err, bar-0.1, tt+err)




####
dat<-read.csv("C://BENJA//clase//ej1.csv")
attach(dat)
names(dat)
m1<-aov(long~sitio*rep)
summary(m1)
#prueba posthoc
TukeyHSD(m1, "sitio", ordered = TRUE)

##Grafica
d1<-c(mean(long[sitio=="a" & rep=="r1"]),mean(long[sitio=="a" & rep=="r2"]),mean(long[sitio=="a" & rep=="r3"]))
d<-c(mean(long[sitio=="b" & rep=="r1"]),mean(long[sitio=="b" & rep=="r2"]),mean(long[sitio=="b" & rep=="r3"]))
tt<-as.table(cbind(d1,d))
dimnames(tt)<- list(gender=c("r1","r2","r3"), party=c("a","b"))
bar<-barplot(tt,beside = T, las=1, legend.text=T, ylim=c(0,10))
ER<-function(x) {sqrt((var(x))/length(x))}

e1<-c(ER(long[sitio=="a" & rep=="r1"]),ER(long[sitio=="a" & rep=="r2"]),ER(long[sitio=="a" & rep=="r3"]))
e<-c(ER(long[sitio=="b" & rep=="r1"]),ER(long[sitio=="b" & rep=="r2"]),ER(long[sitio=="b" & rep=="r3"]))
err<-c(e1,e)
segments(bar, tt-err, bar, tt+err)
segments(bar+0.1, tt-err, bar-0.1, tt-err)
segments(bar+0.1, tt+err, bar-0.1, tt+err)
