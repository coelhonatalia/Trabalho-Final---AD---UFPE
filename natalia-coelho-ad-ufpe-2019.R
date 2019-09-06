#####################################
## Trabalho Final de Análise de Dados
## Natália Coêlho de Souza Oliveira
## Data: 
#####################################


# Abrindo pacotes
if(require(magrittr) == F) install.packages('magrittr'); require(magrittr)
if(require(rio) == F) install.packages('rio'); require(rio)
if(require(fields) == F) install.packages('fields'); require(fields)
if(require(car) == F) install.packages('car'); require(car)
if(require(MASS) == F) install.packages('MASS'); require(MASS)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(readxl) == F) install.packages('readxl'); require(readxl)
if(require(psych) == F) install.packages('psych'); require(psych)
if(require(sandwich) == F) install.packages('sandwich'); require(sandwich)
if(require(lmtest) == F) install.packages('lmtest'); require(lmtest)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)



# Definindo o diretório 
setwd("C:/Users/NATHALIA/Documents/CP-Mestrado/Análise de Dados - Davi Moreira/Trabalho Final")

# Abrindo base de dados
ncoelho <- read_xlsx("natalia-coelho-ad-ufpe-2019.xlsx")


# Análise descritiva das variáveis
describe(ncoelho[c("votegrowth","implicated",
              "partidarism","trr", "pibpercapita")])

## Análise gráficas das variáveis

# Histograma de votegrowth
ggplot(data = ncoelho[ncoelho$votegrowth < 150,], aes(votegrowth)) +
  geom_histogram()+
  labs(title = " Gráfico 1 - Histograma da % de crescimento dos votos de 2014 para 2018")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
summary(ncoelho$votegrowth)


# Histograma de partidarism
ggplot(data = ncoelho, aes(partidarism)) +
  geom_histogram()+
  labs(title = "Gráfico 2 - Histograma de partidarismo")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


# Histograma de Total de Recurso Recebidos pelos candidatos
ggplot(data = ncoelho, aes(log(trr))) +
  geom_histogram()+
  labs(title = "Gráfico 3 - Histograma do log de Total de Recursos Recebidos")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


# Histograma de PIB per capita
ggplot(data = ncoelho, aes(log(pibpercapita))) +
  geom_histogram()+
  labs(title = "Gráfico 4 - Histograma do log de PIB Per Capita")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


# Visualizando relação entre as variáveis

# votegrowth e partidarismo
plot(ncoelho$votegrowth ~ ncoelho$partidarism,
     main = 'Gráfico 5 - Relação entre votegrowth e partidarismo',
     ylab = 'votegrowth',
     xlab = "partidarismo")
abline(lm(ncoelho$votegrowth ~ ncoelho$partidarism),
       col = 'red')

# votegrowth e PIB per Capita
# transformando o PIB em log no df
ncoelho$logpibpercapita <- log(ncoelho$pibpercapita)
#gráfico
plot(ncoelho$votegrowth ~ ncoelho$logpibpercapita,
     main = 'Gráfico 6 - Relação entre votegrowth e log de PIB per capita',
     ylab = 'votegrowth',
     xlab = "PIB Per Capita (log)")
abline(lm(ncoelho$votegrowth ~ ncoelho$logpibpercapita),
       col = 'red')

# votegrowth e Total de Recursos Recebidos
# transformando o trr em log no df
ncoelho$logtrr <- log(ncoelho$trr)
#gráfico
plot(ncoelho$votegrowth ~ ncoelho$logtrr,
     main = 'Gráfico 7 - Relação entre votegrowth e log de trr',
     ylab = 'votegrowth',
     xlab = "TRR(log)")
abline(lm(ncoelho$votegrowth ~ ncoelho$logtrr),
       col = 'red')


# Boxplote de votegrowth por implicated
ggplot(data = ncoelho, aes(x= factor(implicated), y= votegrowth)) +
  geom_boxplot()+
  labs(title = "Gráfico 8 - Boxplot de votegrowth por implicated")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


# Histograma de frequência dos partidos
t_part <- table(ncoelho$party)
t_part <- data.frame(t_part)
ggplot(data = t_part, aes(y= Freq, x= Var1)) +
  geom_col() +
  coord_flip() +
  labs(title = "Gráfico 9 - Frequência de Partidos")


# modelo de regressão linear
reg <- lm (votegrowth ~ partidarism + factor(implicated)*partidarism + factor(implicated) + log(pibpercapita) + log(trr) + factor(macror), data = ncoelho)

# resumo do modelo
summary(reg)

# resumo do modelo
plot(reg)

# Utilizando estimador de erros-padrão de White-Huber para corrigir heterocedasticidade
coeftest(reg, vcov = vcovHC(reg, type="HC1"))

# Intervalo de confiança dos betas
confint(reg)

## Checando pressupostos do modelo
# Multicolinearidade
vif(reg)

# Normalidade dos resíduos
hist(residuals(reg),
     main = " Gráfico 10 de Histograma dos Residuos do Modelo 1")

# Heterocedasticidade
ggplot(data = ncoelho, aes(x = fitted(reg), y=residuals(reg))) +
  geom_point() + 
  labs(title="Gráfico 11 - Heteroscedasticidade do Modelo 1", x ="fitted (reg)", y = "residuals(reg)") +
  geom_hline (yintercept = 0, linetype = "dashed", color = "blue") +
  theme_minimal()

## Reestimando modelos sem outliers
# Extraíndo hat values do modelo
hv <- hatvalues(reg)

# Modelo 2 - sem outliers
reg2 <- lm (votegrowth ~ partidarism + factor(implicated)*partidarism + factor(implicated) + log(pibpercapita) + log(trr) + factor(macror), data = ncoelho[hv<4*mean(hv),])

# Resumo do modelo 2
summary(reg2)

## Checando pressupostos do modelo 2
# Multicolinearidade
vif(reg2)

# Normalidade dos resíduos do modelo 2
hist(residuals(reg),
     main = " Gráfico 12 de Histograma dos Residuos do Modelo 2")

# Heterocedasticidade do modelo 2
plot(reg2)


# Tabela do efeito marginal dos modelos 
# Efeito Marginal de partidarism quando implicated = 0 e implicated = 1
me.partidarism <- c ((-0.3491), (-1.1375) + (-0.3491), (-0.4713), (-21.0663) + (-0.4713))
# Erro padrão quando implicated = 0 e implicated = 1 
se <- c(0.7802, 5.1022, 0.7843, 63.5062)
# Criando data frame com os Efeitos Marginais de renda e dos erros padrões 
tabela.em <- data.frame(me.partidarism, se)
head(tabela.em)



