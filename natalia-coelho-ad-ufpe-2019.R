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



# Definindo o diretório 
setwd("C:/Users/NATHALIA/Documents/CP-Mestrado/Análise de Dados - Davi Moreira/Trabalho Final")

# Abrindo base de dados
ncoelho <- read_xlsx("basencoelho.xlsx")

# Análise descritiva das variáveis
describe(ncoelho[c("vote2018","implicated",
              "partidarism","vote2014")])

## Análise gráficas das variáveis

# Histograma de vote2018
ggplot(data = ncoelho, aes(vote2018)) +
  geom_histogram()+
  labs(title = "Histograma da % de votos em 2018")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# Histograma de vote2014
ggplot(data = ncoelho, aes(vote2014)) +
  geom_histogram()+
  labs(title = "Histograma da % de votos em 2014")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# Histograma de partidarism
ggplot(data = ncoelho, aes(partidarism)) +
  geom_histogram()+
  labs(title = "Histograma de partidarismo")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# Histograma de frequência dos partidos
t_part <- table(ncoelho$party)
t_part <- data.frame(t_part)
ggplot(data = t_part, aes(y= Freq, x= Var1)) +
  geom_col() +
  coord_flip() +
  labs(title = "Frequência de Partidos")


# Visualizando relação entre as variáveis
plot(ncoelho$vote2018 ~ ncoelho$vote2014)

boxplot(ncoelho$vote2018 ~ ncoelho$implicated)

boxplot(ncoelho$vote2018 ~ ncoelho$partidarism)

# modelo de regressão linear
reg <- lm (vote2018 ~ partidarism + factor(implicated)*partidarism + factor(implicated) + vote2014 + factor(ufID), data = ncoelho)

# resumo do modelo
summary(reg)

# RMSE do modelo
mean(residuals(reg) ^2) %>% sqrt ()

# Gráfico de efeito marginal do modelo 
beta.hat <- coef(reg) 
cov <- vcov(reg)
z0 <- seq(min(ncoelho$partidarism), max(ncoelho$partidarism), length.out = 1000)
dy.dx <- beta.hat["factor(implicated)1"] + beta.hat["partidarism:factor(implicated)1"]*z0
se.dy.dx <- sqrt(cov["factor(implicated)1", "factor(implicated)1"] + z0^2*cov["partidarism:factor(implicated)1", "partidarism:factor(implicated)1"] + 2*z0*cov["factor(implicated)1", "partidarism:factor(implicated)1"])
upr <- dy.dx + 1.96*se.dy.dx
lwr <- dy.dx - 1.96*se.dy.dx

par(family="serif",bty="l",mar=c(5,5.5,2,2))
plot(x=z0, y=dy.dx,type="n",xlim=c(min(z0),max(z0)),
     ylim=c(min(lwr),max(upr)),
     xlab = "Partidarismo",
     ylab = "Implicado",
     main="Efeito Marginal de implicated*partidarism")
lines(z0, dy.dx, lwd = 3)
lines(z0, lwr)
lines(z0, upr)
abline(h=0,lty=2)

## Checando pressupostos do modelo
# Multicolinearidade
vif(reg)

# Normalidade dos resíduos
hist(residuals(reg$residuals))

# Heterocedasticidade
plot(reg,which = 1)

# Utilizando estimador de erros-padrão de White-Huber para corrigir heterocedasticidade
coeftest(reg,vcov. = vcovHC)

