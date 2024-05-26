#########################################################################################
# Script para análise de fatores de equidade no acesso aos medicamentos no DF e entorno #
# Data: 09/05/2021  Autor: Ivan Zimmermann (ivanzricardo@gmail.com)
########################################################################################
install.packages("corrplot")
install.packages("ggplot2")
install.packages("ggpubr")

#Chamar o pacote para ler a planilha
library(readxl)
library(ggplot2)
library(ggpubr)
library(Hmisc)
library(corrplot)


#Chamar o banco de dados
#bd <- read_excel("C:/Users/Admin/OneDrive/Capacitações/Analise de dados - ENAP/TCC/Dados/Modelos/2021/bd_df_entorno_2017_sem_outlier.xlsx")
bd <- read_excel("bd_df_entorno_2017_sem_outlier.xlsx")


#Estudar a correlação
#Matriz de correlação
mcor <- round(cor(bd[c("popsexofem","pidosos","renda", "supcomp","plano","preta","coef")], use="pairwise.complete.obs", 
                  method = 'spearman'),2)
upper<-mcor
#upper[upper.tri(mcor)]<-"" #Esconde a parte de cima da matriz
#upper<-as.data.frame(upper)
upper

write.csv2(upper, "mcor.csv")

mydata.rcorr = rcorr(as.matrix(mcor))
mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P
corrplot(mcor, type="lower",method="color", tl.col="black", tl.srt=45)

#palette = colorRampPalette(c("green", "white", "red")) (20)
#heatmap(x = mcor, col = palette, symm = TRUE)

#Scatterplots
#plano de saúde
plotplano <- ggscatter(bd, x = "plano", y = "coef", color = "purple",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Moradores com plano de saúde (%)", ylab = "Nº de pacientes por habitantes (100 mil)")

#renda per capita
plotrenda <- ggscatter(bd, x = "renda", y = "coef", color = "blue",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Renda per capita (R$)", ylab = "Nº de pacientes por habitantes (100 mil)")

#cor
plotcor <- ggscatter(bd, x = "preta", y = "coef", color = "green",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Moradores de cor preta (%)", ylab = "Nº de pacientes por habitantes (100 mil)")

#idade
plotidade <- ggscatter(bd, x = "pidosos", y = "coef", color = "red",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Moradores com mais de 60 anos (%)", ylab = "Nº de pacientes por habitantes (100 mil)")

#sexo
plotsexo <- ggscatter(bd, x = "popsexofem", y = "coef", 
                      add = "reg.line", conf.int = TRUE,
                      cor.coef = TRUE, cor.method = "spearman",
                      xlab = "Moradoras do sexo feminino (%)", 
                    ylab = "Nº de pacientes por habitantes (100 mil)")

#escolaridade
plotsup <- ggscatter(bd, x = "supcomp", y = "coef",  
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Moradores com ensino superior completo (%)", ylab = "Nº de pacientes por habitantes (100 mil)")

ggarrange(plotidade + rremove("ylab"), plotrenda+ rremove("ylab"), plotsup + rremove("ylab"), 
          plotcor + rremove("ylab"), plotsexo + rremove("ylab"), plotplano + rremove("ylab"),
          labels = c("A", "B", "C", "D", "E", "F"), 
          ncol = 2, nrow = 3)


#Construir os modelos
modelo1 <- lm(coef ~ popsexofem + pidosos + renda + plano + supcomp + preta, data=bd)
summary(modelo1)

modelo2 <- lm(coef ~ popsexofem + pidosos + renda + plano + supcomp, data=bd)
summary(modelo2)

modelo3 <- lm(coef ~ popsexofem + plano + supcomp, data=bd)
summary(modelo3)

####################### FIM ###########################


