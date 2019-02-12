#---------------Cerinta 1--------------------------
#CERINTA 1--- Folosind setul de date X efectuati operatii de statistica descriptiva 
#pentru variabilele din acest set de date(medie, varianta, quartile, boxplot, interpretari).


model <- infert
plot(model)
#variabila education
education <- model$age

medie<-mean(education)
medie
varianta<-var(education) 
varianta
q1=quantile(education,1/4)
q1
q2=quantile(education,2/4)
q2
q3=quantile(education,3/4)
q3
q4=quantile(education,4/4)
q4
mdn = median(education, na.rm = FALSE)
mdn#se poate observa ca mediana este egala cu a doua quanrtila

#variabila age
age <- model$age

medie<-mean(age)
medie
varianta<-var(age) 
varianta
q1=quantile(age,1/4)
q1
q2=quantile(age,2/4)
q2
q3=quantile(age,3/4)
q3
q4=quantile(age,4/4)
q4
mdn = median(age, na.rm = FALSE)
mdn#se poate observa ca mediana este egala cu a doua quartila

#variabila parity
parity <- model$parity

medie<-mean(parity)
medie
varianta<-var(parity) 
varianta
q1=quantile(parity,1/4)
q1
q2=quantile(parity,2/4)
q2
q3=quantile(parity,3/4)
q3
q4=quantile(parity,4/4)
q4
mdn = median(parity, na.rm = FALSE)
mdn#se poate observa ca mediana este egala cu a doua quartila



#Boxplot pentru Varsta si Numarul de nasteri
age2 = sort(age)
age
age2
lg<-length(age2)
boxplot(age~parity,data=infert, main="Varsta-Numarul de nasteri
        (inainte de termen)", 
        xlab="Numarul de nasteri", ylab="Varsta",staplewex = 1, ylim=c(age2[1], age2[lg]))


boxplot(age~parity,data=infert, main="Varsta-Numarul de nasteri(inainte de termen)", 
        xlab="Numarul de nasteri", ylab="Varsta",staplewex = 1, ylim=c(age2[1], age2[lg]))

text(y=fivenum(age~parity), labels =fivenum(age~parity), x=0.25)
#abline(h=quantile(age~parity,0.25),col="red",lty=2)
#abline(h=quantile(age~parity,0.75),col="red",lty=2)

boxplot(age, horizontal = TRUE, axes = FALSE, staplewex = 1)
text(x=fivenum(X), labels =fivenum(X), y=1.25)

#Pentru seturile de date folosite in cadrul analizei de boxplot, 
#observam modul in care este repartizata variabila Parity in functie 
#de variabila Age. 
#Linia orizozntala din mijloc reprezinta mediana(quartila 2). 
#Quartilele 1 si 3 delimiteaza capetele boxplot-ului. 
#Mustata superioara este determinata de valoarea de celei mai mari 
#observatii, care este <= q3+1.5*(q3-q1)
#Mustata inferioara reprezinta valoarea celei mai mici observatii
#(adica cea mai mica varsta), si este >= q1-1.5*(q3-q1).
#Valorile din afara dreptunghiului se numesc valori aberante(extreme).
#Boxplot-ul 6 nu are valori extreme, deoarece quantilele sale sunt:
vec <- c(26,26,26,38,38,39,39,39)
vec

quantile(vec)
#mediana pentru boxplot-ul 6 arata o compacatre a 1/2 din date in 
#jurul intervalului [38-39], in raport cu mai marele ramas sub mediana [26-38]
#quartila 1 da cele mai mici elemente, iar quartila 3 este egala cu 
#quartila 4. si nu avem valori extreme (iesite din box).


#variabila induced
induced <- model$induced

medie<-mean(induced)
medie
varianta<-var(induced) 
varianta
#calcul cuantile (1-4)
q1=quantile(induced,1/4)
q1
q2=quantile(induced,2/4)
q2
q3=quantile(induced,3/4)
q3
q4=quantile(induced,4/4)
q4
mdn = median(induced, na.rm = FALSE)
mdn#se poate observa ca mediana este egala cu a doua quartila

#variabila case
case <- model$case

medie<-mean(case)
medie
varianta<-var(case) 
varianta
q1=quantile(case,1/4)
q1
q2=quantile(case,2/4)
q2
q3=quantile(case,3/4)
q3
q4=quantile(case,4/4)
q4
mdn = median(case, na.rm = FALSE)
mdn#se poate observa ca mediana este egala cu a doua quartila

#variabila spontaneous
spontaneous <- model$spontaneous

medie<-mean(spontaneous)
medie
varianta<-var(spontaneous) 
varianta
q1=quantile(spontaneous,1/4)
q1
q2=quantile(spontaneous,2/4)
q2
q3=quantile(spontaneous,3/4)
q3
q4=quantile(spontaneous,4/4)
q4
mdn = median(spontaneous, na.rm = FALSE)
mdn#se poate observa ca mediana este egala cu a doua quartila

#variabila stratum
stratum <- model$stratum

medie<-mean(stratum)
medie
varianta<-var(stratum) 
varianta
q1=quantile(stratum,1/4)
q1
q2=quantile(stratum,2/4)
q2
q3=quantile(stratum,3/4)
q3
q4=quantile(stratum,4/4)
q4
mdn = median(stratum, na.rm = FALSE)
mdn#se poate observa ca mediana este egala cu a doua quartila

#variabila pooled.stratum
pooled.stratum <- model$pooled.stratum

medie<-mean(pooled.stratum)
medie
varianta<-var(pooled.stratum) 
varianta
q1=quantile(pooled.stratum,1/4)
q1
q2=quantile(pooled.stratum,2/4)
q2
q3=quantile(pooled.stratum,3/4)
q3
q4=quantile(pooled.stratum,4/4)
q4
mdn = median(pooled.stratum, na.rm = FALSE)
mdn#se poate observa ca mediana este egala cu a doua quartila




#---------------Cerinta 2--------------------------

#Varianta 1
# liniar regressions

# simple regression
education <- infert$education
age <- infert$age
#
linMod <- lm(education ~ age, data = infert)
plot(x= age, y = education, col = "red", pch = "*")
abline(linMod, col = "blue")

# multiple regression
age <- infert$age
parity <- infert$parity
education <- infert$education
spontaneous <- infert$spontaneous
#
linMod1 <- lm(age ~ parity + education + spontaneous)
summary(linMod1)
layout(matrix(c(1, 2, 3, 4), 2, 2)) # 4 graphs per page

plot(linMod1)


#Varianta 2:
#var2
#REGERSIE LINIARA simpla cu 1 variabila independenta
#prim mtoda celor mai mici patrate 
age <- model$age #set de date initial #SELECTIE DE REGERSIE LINIARA
length(age) 
yage <-model$age #preluare date de analizat pt SELECTIE DE REGERSIE LINIARA
yrage<-yage*3/4 #propunere SELECTIE DE date pt  REGERSIa LINIARA
yrage
round(yrage, digits = 0)#rotunjim detele noii propuneri de set de date
c=cbind(1,age)
solve(t(c) %*% c, t(c) %*% yrage)


#in boxplot se poate observa ca varstele au fost micsorate cu 25%
#s-a calculat si numarul de nasteri corespunzator
boxplot(yrage~parity,data=infert, main="Regresie liniara penru varsta", 
        xlab="Numarul de nasteri", ylab="Varsta",staplewex = 1)

#var2b regeresie liniara multipla
#REGERSIE LINIARA multipla cu 2 variabila independente
#prim mtoda celor mai mici patrate 
age <- model$age #set1 de date initial #SELECTIE DE REGERSIE LINIARA
length(age) 
parity<-(model$parity)#set2 de date initial #SELECTIE DE REGERSIE LINIARA
c=cbind(1,age,parity)
yrage=(1/4+parity+age)#set combinat al datelor initiale 
round(yrage, digits = 0)
solve(t(c) %*% c, t(c) %*% yrage)
#-------------------interpretare 
#beta0= 0.25 #COEFIECIENTI DE REGRESIE
#beta1= 1 #COEFIECIENTI DE REGRESIE
#beta1=1 #COEFIECIENTI DE REGRESIE
#Constructia setului de date infert,este construit din mai multe variabile
#aleatoare, asta ii creaza utilitatea de a fi folosit cu regresie liniara multipla  varinta regresie multipla e mai potrivita pentru ca poate tine cont

#-------
#-------------------interpretare 
#alfa= -3.352547 #COEFIECIENTI DE REGRESIE
#beta=7.500000 #COEFIECIENTI DE REGRESIE
#Constructia setului de date infert,este construit din mai multe variabile
#aleatoare, asta ii creaza utilitatea de a fi folosit cu regresie liniara multipla  varinta regresie multipla e mai potrivita pentru ca poate tine cont

#-------


#-----------Cerinta 3-----------------------------

#Varianta 1:

# Weibull distribution

#In prima diagrama sunt afisate diferite functii repartizate Weilbull, dar cu valori diferite 
#ale parametrilor
#In diagrama a doua este prezentata functia densitate de repartitie pentru fiecare functie 
#din diagrama anterioara.
#Valoarea maxima ce poate fi atinsa de functiile din prima diagrama este 1.


layout(matrix(c(1, 2), 1, 2))
t <- seq(0, 2.5, 0.0001)
plot(t, pweibull(t, 0.5, 1), pch = ".", col = 2, xlim=c(0, 2.5), ylim=c(0, 1.0))
lines(t, pweibull(t, 1, 1), pch = ".", col = 3)
lines(t, pweibull(t, 1.5, 1), pch = ".", col = 4)
lines(t, pweibull(t, 5, 1), pch = ".", col = 5)
plot(t, dweibull(t, 0.5, 1), pch = ".", col = 2, xlim=c(0, 2.5), ylim=c(0, 2.5))
lines(t, dweibull(t, 1, 1), pch = ".", col = 3)
lines(t, dweibull(t, 1.5, 1), pch = ".", col = 4)
lines(t, dweibull(t, 5, 1), pch = ".", col = 5)
mean1 <- mean(pweibull(t, 0.5, 1))
mean2 <- mean(pweibull(t, 1, 1))
mean3 <- mean(pweibull(t, 1.5, 1))
mean4 <- mean(pweibull(t, 5, 1))
var1 <- var(pweibull(t, 0.5, 1))
var2 <- var(pweibull(t, 1, 1))
var3 <- var(pweibull(t, 1.5, 1))
var4 <- var(pweibull(t, 5, 1))

# Repartitia Weibull este folosita la:
# * prezicerea schibarilor tehnologice
# * in hidrologie, pentru evenimente extreme precum maxima anual a unei zile de precipitatii, debitul unui rau
# * meteorologie, pentru a descrie distributia vanturilor
# * strangerea de informatii de pe un site web - modeleaza timpul de cautare pe un site
# * asigurari generale, pentru a prezice revendicarile de reasigurare