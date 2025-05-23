data=date_curatate_C4
colnames(data)
colnames(data)[9]="interes"
colnames(data)[20]="confort_prof"

#Ipoteza1: Exista o relatie pozitiva intre atitudinea fata de statistica si analiza datelor si relatia cu 
#profesorii de curs si seminar 


#statistica descriptiva 
summary(data$interes)
sd(data$interes)
hist(data$interes)
hist(data$interes,main="Interesul fata de  Statistica si analiza datelor",xlab="scor_interes",ylab="numarul de studenti",ylim=c(0,16),col="red")
ks.test(data$interes, "dnorm")
summary(data$confort_prof)
sd(data$confort_prof)
ks.test(data$confort_prof,"dnorm")

cor.test(data$interes,data$confort_prof, method = "spearman")
#nu exista corelatie intre interesu fata de statistica si relatia cu profesorul de seminar 


#Ipoteza2: Exista o relatie pozitiva intre gandirea superstitiosa si idealurile conspirative
colnames(data)[41]="superstitie"
summary(data$superstitie)
sd(data$superstitie)
hist(data$superstitie)
hist(data$superstitie,main="Gandirea superstitoasa",xlab="scor_superstitie",ylab="nr_studenti",ylim=c(0,9),col="yellow")
ks.test(data$superstitie,"dnorm")
qqnorm(data$superstitie)       
qqline(data$superstitie, col = "red")  


colnames(data)[42]="conspiratie"
summary(data$conspiratie)
sd(data$conspiratie)
hist(data$conspiratie)
hist(data$conspiratie, main="Gandirea conspirativa", xlab="scor_conspiratie",ylab="nr_studenti", ylim=c(0,10), col="green")
ks.test(data$conspiratie,"dnorm")
qqnorm(data$conspiratie)       
qqline(data$conspiratie, col = "red") 

cor.test(data$superstitie,data$conspiratie,method="spearman")
#Conform testului Spearman nu exista corelatie intre cele doua variabile 


#Ipoteza3:Existe o relatie pozitiva intre nota la probabilitati si interesul  
#pentru scoala 

colnames(data)[5]="nota"
summary(data$nota)
sd(data$nota)
hist(data$nota)
hist(data$nota,main="Nota la examen Pob", xlab="nota",ylab="nr_studenti",ylim=c(0,20),col="pink")
ks.test(data$nota,"dnorm")



colnames(data)[39]="interes_scoala"
summary(data$interes_scoala)
sd(data$interes_scoala)
hist(data$interes_scoala)
hist(data$interes_scoala,main="Interesul pt scoala",xlab="interes",ylab="studenti",ylim=c(0,30),col="red")
ks.test(data$interes_scoala,"dnorm")

levels(data$interes_scoala)
levels(data$nota)
cor.test(data$nota,data$interes_scoala,method="spearman")
#Nu există o relație semnificativă între
#notele elevilor și interesul lor pentru școală


#Ipoteza 4: Exista o relatie intre faptul ca studentii au studiat anterior 
#analiza si statistica in trecut si profilul din liceu

colnames(data)[4]="cunostinte_ant"
colnames(data)[7]="liceu"
tabel_contingenta=table(data$cunostinte_ant,data$liceu)
print(tabel_contingenta)
chisq.test(tabel_contingenta)
#P-value = 0.131 > 0.05  Nu există o asociere semnificativă statistic între:
#Faptul că ai studiat anterior statistică / analiză a datelorsi profilul din liceu


#Ipoteza 5:  Există diferențe semnificative statistic între nivelul de încredere 
#în utilizarea R pentru analiza datelor, între persoanele care nu au studiat 
#anterior statistică și cele care au studiat doar elemente de bază.

colnames(data)[28]="incredere_R"
ks.test(data$incredere_R,"dnorm")
#datele  incredere nu sunt normal distribuite

kruskal.test(data$incredere_R ~data$cunostinte_ant)
#nu e semnificativ statistic deci nu exista o diferenta de mediana intre grupe