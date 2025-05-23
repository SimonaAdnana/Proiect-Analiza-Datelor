### Încărcarea datelor și pregătirea variabilelor
data <- date_curatate_C4  

# Redenumirea coloanelor pentru claritate 
colnames(data)[5:6] <- c("nota_prob", "nota_stats")
colnames(data)

# Calculează corelațiile dintre 'sses' și fiecare predictor
cor_sses_nota_prob <- cor(data$sses, data$nota_prob)
#corelatia este 0.407, moderata

cor_sses_nota_stats <- cor(data$sses, data$nota_stats)
#corelatia este 0.48, moderata

cor_sses_mses <- cor(data$sses, data$mses)
#corelatia este 0.36, slaba

cor_sses_school_engagement <- cor(data$sses, data$school_engagement)
#corelatia este 0.46, moderata 

cor_sses_open_minded <- cor(data$sses, data$open_minded)
#corelatia este -0.16, nu exista 

cor_sses_numeracy <- cor(data$sses, data$numeracy)
#corelatia este 0.38, slaba


# Afișează rezultatele
cat("Corelația sses - nota_prob: ", cor_sses_nota_prob, "\n")
cat("Corelația sses - nota_stats: ", cor_sses_nota_stats, "\n")
cat("Corelația sses - mses: ", cor_sses_mses, "\n")
cat("Corelația sses - school_engagement: ", cor_sses_school_engagement, "\n")
cat("Corelația sses - open_minded: ", cor_sses_open_minded, "\n")
cat("Corelația sses - numeracy: ", cor_sses_numeracy, "\n")

###  Modelul de regresie pentru 'sses' 
# Variabile independente selectate :
# - nota_prob, nota_stats: Performanța anterioară în statistică/probabilități
# - mses: Math Self-Efficacy (puternic corelat cu sses)
# - school_engagement: Implicarea în medul academic
# - open_mind: Deschidere mentală (poate influența încrederea)
# - numeracy: Competențe numerice (importante în statistică)

model_sses <- lm(
  sses ~ nota_prob + nota_stats  + school_engagement + open_minded + numeracy,
  data = data
)


summary(model_sses)


model_sses_2 <- lm(
  sses ~ nota_prob  + school_engagement + numeracy,
  data = data
)

summary(model_sses_2)

###  Diagnosticarea modelului
# Verificarea ipotezelor regresiei liniare:
par(mfrow = c(1,1)) 
plot(model_sses)      

 

par(mfrow = c(1, 1)) 
plot(model_sses_2)



# Testarea multicoliniarității (VIF > 5 indică probleme)
library(car)
vif(model_sses) 
vif(model_sses_2)
# Visualizare VIF
vif_values <- vif(model_sses)
barplot(vif_values, 
        main = "Factorii de Inflație a Varianței Model sses  (VIF)", 
        col = "lightblue", 
        ylim = c(0, 5))
abline(h = 5, col = "red", lty = 2) 


 #Visualizare VIF
vif_values_3 <- vif(model_sses_2)
barplot(vif_values_3, 
        main = "Factorii de Inflație a Varianței  model optimizat (VIF)", 
        col = "lightblue", 
        ylim = c(0, 5))
abline(h = 5, col = "red", lty = 2) 








