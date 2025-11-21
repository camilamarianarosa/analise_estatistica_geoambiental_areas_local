#Instalar pacotes
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readxl")
#Pacotes necessarios
library(readxl)
library(dplyr)
library(ggplot2)
#Importar os dados
dados<- read_excel("atividade_pratica_minicurso.xlsx")
#Media,mediana e desvio padrão do Ph por grupo
dados%>% group_by(Grupo_Area) %>% summarise(media = mean(pH), mediana = median(pH), desvio= sd(pH))
#Boxplot condutividade e metais
ggplot(dados, aes(x=Grupo_Area, y=Condutividade))+geom_boxplot()+theme_bw()
ggplot(dados,aes(x=Local, y=Metais_Pesados))+ theme_bw()+geom_boxplot() + stat_summary(fun = mean, shape = 8, color = 'red')
#Se p<0,5 rejeitar H0
#Se p>=0,5 não rejeitar H0
#Teste de normalidade
shapiro.test(dados$pH)
shapiro.test(dados$Condutividade)
shapiro.test(dados$Matéria_Orgânica)
shapiro.test(dados$Metais_Pesados)
#t-teste(pH por grupo)
t.test(pH~Grupo_Area, data=dados)
#ANOVA( Materia organica por local)
anova<- aov(Matéria_Orgânica~Local,data=dados)
summary(anova)
#Correlação de Pearson(pH vs Materia organica)
cor.test(dados$pH, dados$Matéria_Orgânica, method="pearson")
#Mann-Whitney(Condutividade por grupo)
wilcox.test(Condutividade~Grupo_Area, data=dados)
#Kruskal-wallis (Metais Pesados vs Condutividade)
kruskal.test(Metais_Pesados~Local, data=dados)
#Correlação de Spearman(Metais Pesados vs Condutividade)
cor.test(dados$Metais_Pesados, dados$Condutividade, method="spearman")
#Grafico de colunas: Calcular média de matéria orgãnica por local
dados%>%group_by(Local) %>% summarise(media_MO = mean(Matéria_Orgânica)) %>% ggplot(aes(x=Local, y=media_MO, fill=Local))+ geom_col()+ labs(title= "Média de Matéria orgânica por Local", x="Local", y="Matéria Orgânica(%)")+theme_minimal()
#Boxplot de Metais Pesados por Local
ggplot(dados, aes(x=Local, y=Metais_Pesados, fill=Local))+geom_boxplot()+labs(title="Distribuição de Metais Pesados por Local", y="Metais Pesados (mg/kg")+theme_bw()+stat_summary(fun=mean, shape=8, color="red")
#Grafico de Linhas: Condutividade por Local
ggplot(dados,aes(x=1:nrow(dados), y=Condutividade, color=Local, group=Local))+geom_line(linewidth = 1)+geom_point(size=2)+ labs(title="Condutividade por Local", x="Amostras", y="Condutividade (uS/cm)", color="Local")+theme_minimal()

       