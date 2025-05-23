#Redenumire coloane
#Merge între cele 3 seturi de date, după codul de înregistrare
#Intrebarile de control - ifelse
#Creat noi coloane
#Scorare scale - Itemi inversati (reversed)
#Scorare scale - ifelse
#Vizualizare
#Stergerea unor coloane
#Scrierea unui nou set de date (salvat din R)


#Redenumire coloane

colnames(Chestionar1)
colnames(Chestionar2)
colnames(Chestionar3)

colnames(Chestionar1)[3] = "cod"
colnames(Chestionar2)[3] = "cod"
colnames(Chestionar3)[3] = "cod"

#Merge între cele 3 seturi de date

data = merge(Chestionar1, Chestionar2, by="cod")
data = merge(data, Chestionar3, by="cod")

#Intrebarile de control - ifelse
colnames(data)
colnames(data)[49] = "control1"
summary(data$control1)
data = subset(data, data$control1 == 2)
colnames(data)[99]="control2"
summary(data$control2)
colnames(data)[150]="control3"
summary(data$control3)
colnames(data)[180]="control4"
summary(data$control4)
colnames(data)

#Prepocesare - Scala SATS

colnames(data) #coloanele 37 - 65 corespund scalei SATS (vezi documentul word cu scalele)
colnames(data)[37] = "st1"
colnames(data)[38] = "st2"
colnames(data)[39] = "st3"
colnames(data)[40] = "st4"
colnames(data)[41:48] = c("st5", "st6", "st7","st8","st9","st10","st11","st12")
colnames(data)[50:65] = c("st13","st14","st15","st16","st17","st18","st19","st20","st21","st22","st23","st24","st25","st26","st27","st28")
colnames(data)

data$SATS = data$st1 +
  (8-data$st2)+
  (8-data$st3)+
  data$st4+
  (8-data$st5)+
  (8-data$st6)+
  data$st7+
  data$st8+
  (8-data$st9)+
  (8-data$st10)+
  (8-data$st11)+
  (8-data$st12)+
  data$st13+
  (8-data$st14)+
  data$st15+
  (8-data$st16)+
  data$st17+
  (8-data$st18)+
  (8-data$st19)+
  (8-data$st20)+
  (8-data$st21)+
  (8-data$st22)+
  data$st23+
  data$st24+
  (8-data$st25)+
  (8-data$st26)+
  (8-data$st27)+
  (8-data$st28)

data$SATS = data$SATS/28

summary(data$SATS)
hist(data$SATS)

#CRT

colnames(data)[66] = "crt1"
colnames(data)[67] = "crt2"
colnames(data)[68] = "crt3"

summary(data$crt1)
summary(as.factor(data$crt1))
summary(as.factor(data$crt2))
summary(as.factor(data$crt3))

data$crt1 = ifelse(data$crt1 == 2.50, 1, 0)
data$crt2 = ifelse(data$crt2 == 5, 1, 0)
data$crt3 = ifelse(data$crt3 == 47, 1, 0)

summary(as.factor(data$crt1))
summary(as.factor(data$crt2))
summary(as.factor(data$crt3))

data$CRT = data$crt1 + data$crt2 + data$crt3

summary(data$CRT)
hist(data$CRT)
barplot(table(data$CRT))

#crt extins
colnames(data)
colnames(data)[69]="crt_extins1"
colnames(data)[70]="crt_extins2"
colnames(data)[71]="crt_extins3"
colnames(data)[72]="crt_extins4"
colnames(data)[73]="crt_moneda"
colnames(data)[74]="crt_linda"
colnames(data)[78]="crt_zar"
colnames(data)[79]="crt_premii"
colnames(data)[80]="crt_acme"
colnames(data)[81]="crt_risc"
colnames(data)[82]="crt_boala"
colnames(data)[83]="crt_persoana"


summary(as.factor(data$crt_extins1))
summary(as.factor(data$crt_extins2))
summary(as.factor(data$crt_extins3))
summary(as.factor(data$crt_extins4))
summary(as.factor(data$crt_moneda))
summary(as.factor(data$crt_linda))
summary(as.factor(data$crt_zar))
summary(as.factor(data$crt_premii))
summary(as.factor(data$crt_acme))
summary(as.factor(data$crt_risc))
summary(as.factor(data$crt_boala))
summary(as.factor(data$crt_persoana))


data$crt_extins1=ifelse(data$crt_extins1==4,1,0)
data$crt_extins2=ifelse(data$crt_extins2==29,1,0)
data$crt_extins3=ifelse(data$crt_extins3==200,1,0)
data$crt_extins4=ifelse(data$crt_extins4=="mai puțin decât avea la început ",1,0)
data$crt_moneda=ifelse(data$crt_moneda=="Capul și pajura au șanse egale să apară la a șasea aruncare",1,0)
data$crt_extins= data$crt_extins1+data$crt_extins2+data$crt_extins3+data$crt_extins4
data$crt_linda=ifelse(data$crt_linda=="Linda este casieră la bancă.",1,0)
data$crt_zar=ifelse(data$crt_zar==500,1,0)
data$crt_premii=ifelse(data$crt_premii==10,1,0)
data$crt_acme=ifelse(data$crt_acme==0.1,1,0)
data$crt_risc=ifelse(data$crt_risc=="1 din 10",1,0)
data$crt_boala=ifelse(data$crt_boala=="0.1",1,0)
data$crt_persoana=ifelse(data$crt_persoana=="2",1,0)


summary(as.factor(data$crt_acme))
summary(data$crt_extins)
summary(data$crt_risc)
summary(data$crt_persoana)
hist(data$crt_extins)
summary(data$crt_moneda)
summary(data$crt_linda)
summary(as.factor(data$crt_zar))
summary(as.factor(data$crt_premii))
summary(as.factor(data$crt_risc))
summary(as.factor(data$crt_boala))
summary(as.factor(data$crt_persoana))



#Sters coloane
data = subset(data, select = -c(1:5))

#Scriere set date
write.csv(data, file ="date_prelucrate")
library(openxlsx)
write.xlsx(data,"date_prelucrare_excel.xlx")
