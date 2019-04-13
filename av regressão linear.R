install.packages("ISLR")
library(ISLR)
> data("Auto")
> head(Auto)

tail(Auto)
summary(Auto)

# (8-A)_I- Os valores p para os coeficientes de regressão são quase zero.
# Isso implica em significância estatística, o que, por sua vez,
# significa que existe um relacionamento
# II- O valor R ^ {2} indica que cerca de 61% da variação na variável
# de resposta (mpg) é devida à variável preditor horsepower.
# III- O coeficiente de regressão para "cavalos-força" é negativo.
# Portanto, o relacionamento é negativo.
# IV- O intervalo de confiança de 95%
# E o intervalo de previsão de 95%
# Como esperado, o intervalo de previsão é mais amplo que o intervalo
# de confiança.

attach(Auto)
plot(mpg~horsepower, main =" MPG vs Horsepower", xlab = " Horsepower", ylab ="MPG")
abline(coef = coef(lm.fit), col ="red")

detach(Auto)

library("ISLR")
pairs(Auto)

cor(Auto[, names(Auto) !="name"])

model = lm(mpg ~. -name, data = Auto)
summary(model)

# (9-C)_I- Sim existe. No entanto, alguns preditores não têm um efeito 
# estatisticamente significativo na resposta. O valor de R-quadrado 
# implica que 82% das mudanças na resposta podem ser explicadas pelos 
# preditores neste modelo de regressão.
# II_ deslocamento, peso, ano, origem.
# III_ Quando cada outro preditor mantido constante, o valor de mpg 
# aumenta a cada ano que passa. Especificamente, o mpg aumenta em 1,43
# a cada ano.

par(mfrow = c(2,2))
plot(model)

#9-D_O primeiro gráfico mostra que existe uma relação não linear entre o respondente e os preditores;
#O segundo gráfico mostra que os resíduos são normalmente distribuídos e inclinados para a direita;
#O terceiro gráfico mostra que a variância constante da suposição de erro não é verdadeira para esse modelo;
#O terceiro gráfico mostra que não há pontos de alavancagem. No entanto, há uma observação que se destaca como um potencial ponto de alavancagem (rotulado 14 no gráfico)

library("ISLR")
?Carseats
head(Carseats)

str(Carseats)

lm.fit = lm(Sales ~ Price+Urban+US, data= Carseats)
summary(lm.fit)

#(10-B)_Quando o preço aumenta em US $ 1.000 e outros preditores são mantidos constantes, as vendas diminuem em 54.459 unidades vendidas. Em outras palavras, quando o preço aumenta em $ 1000, o número de carros vendidos diminui em 54.459.

#A venda de uma loja não é afetada por estar ou não em uma área urbana.

#Uma loja nas vendas dos EUA 1200 mais carros (em média) do que uma loja que está no exterior.

#(10-D)_O preditor "Urbano". Seu valor de p não é estatisticamente 
# significativo com um valor de 0,936.

lm.fit2 = lm(Sales ~ Price+US, data= Carseats)
summary(lm.fit2)

#(10-F)Com base em seus respectivos valores de R-quadrado (em tabelas de resumo), 
# esses dois modelos são medíocres (apenas 24% de mudança na resposta 
# explicada).

confint(lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

#(10-H) Com base no gráfico Normal.q-q e no gráfico Residuals vs Leverage, 
# não há evidências de tais pontos.

x = rnorm(100, mean= 0, sd =1)
eps = rnorm(100, mean =0, sd = 0.25)
y = -1+0.5*x+eps
length(y)
plot(y~x)
#(13-D) O gráfico mostra um relacionamento linear

lm.fit1 = lm(y~x)
summary(lm.fit1)
#(13-E)
# β ^ o e β ^ 1 são praticamente iguais a βo e β1. Como esperado, 
# β ^ o e β ^ 1 são estatisticamente signficantes e R-quadrado = 0,84 mostra 
# que o modelo se ajusta muito bem aos dados.

plot(y~x); abline(lm.fit1, col ="red") 
legend("bottomright", c("Regression line"), lwd=1, col="red",bty ="n")

lm.fit1 = lm(y~poly(x,2))
summary(lm.fit1)
#(13-G) O coeficiente de regressão do termo quadrático não é estatisticamente 
#significativo; Portanto, não há evidências de que o termo quadrático 
#melhore o modelo.

x = rnorm(100, mean= 0, sd =1)
eps = rnorm(100, mean =0, sd = 0.10)
y = -1+0.5*x+eps
lm.fit2 = lm(y~x)
summary(lm.fit2)

plot(y~x); abline(lm.fit2, col ="red") 
legend("bottomright", c("Regression line"), lwd=1, col="red",bty ="n")

lm.fit2 = lm(y~poly(x,2))
summary(lm.fit2)

# R-quadrado = 0,95 mostra que este modelo se ajusta melhor aos dados.
#A observação em (1) é evidenciada graficamente por quão bem a linha de regressão se ajusta aos pontos de dados.
#O termo quadrático é, novamente, estatisticamente insignificante

x = rnorm(100, mean= 0, sd =1)
eps = rnorm(100, mean =0, sd = 0.50)
y = -1+0.5*x+eps
lm.fit3 = lm(y~x)
summary(lm.fit3)

plot(y~x); abline(lm.fit3, col ="red") 
legend("bottomright", c("Regression line"), lwd=1, col="red",bty ="n")

lm.fit3 = lm(y~poly(x,2))
summary(lm.fit3)

# R-squared = 0.4482 is very poor relatively
#Graphically, we can see that the regression line is not fitting the data points quite well.
#The quadratic is still statistically insignificant

lm.fit1 = lm(y~x); lm.fit2 = lm(y~x); lm.fit3 = lm(y~x)
confint(lm.fit1);confint(lm.fit2); confint(lm.fit3)

# Obtemos os mesmos intervalos de confiança para os parâmetros 
# βo e β1 de cada modelo.

install.packages("MASS")
library(MASS)
attach(Boston)
> data("Boston")
> head(Boston)

tail(Boston)
summary(Boston)

?Boston                

reg1 <- lm(crim ~ age)                  
summary(reg1)                  
reg2 <- lm(crim ~ black)           
summary(reg2)              
reg3 <- lm(crim ~ chas)
summary(reg3)
reg4 <- lm(crim ~ dis)
summary(reg4)
reg5 <- lm(crim ~ indus)
summary(reg5)
reg6 <- lm(crim ~ lstat)
summary(reg6)
reg7 <- lm(crim ~ medv)
summary(reg7)
reg8 <- lm(crim ~nox)
summary(reg8)
reg9 <- (crim ~ ptratio)
summary(reg9)
reg10 <- lm(crim ~ rad)
summary(reg10)
reg11 <- lm(crim ~rm)
summary(reg11)
reg12 <- lm(crim ~ tax)
summary(reg12)
reg13 <- lm(crim ~ zn)
summary(reg13)

# 15-B

reg_completa1 <- lm(crim ~ age + black + chas + dis + indus + lstat + medv + nox + ptratio + rad + rm + tax + zn, data = Boston)
summary(reg_completa1)

# outro jeito

lm.all <- lm(crim~.,data=Boston)
summary(lm.all)

# No nível de confiança de 5%, a hipótese nula pode ser rejeitada para os preditores; zn, dis, rad, black e medv.

# 15-C

# n consegui

xs <- c(coef(lm.indus)[2],coef(lm.zn)[2],coef(lm.chas)[2],coef(lm.nox)[2],coef(lm.rm)[2],coef(lm.age)[2],coef(lm.dis)[2],coef(lm.rad)[2],coef(lm.tax)[2],coef(lm.ptratio)[2],coef(lm.black)[2],coef(lm.lstat)[2],coef(lm.medv)[2])
ys <- coef(lm.all)[-1]

data.frame(xs,ys)
plot(xs, ys)

# feito

univcof <- lm(crim ~ zn, data = Boston)$coefficients[2]
univcof <- append(univcof, lm(crim ~ indus, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ chas, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ nox, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ rm, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ age, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ dis, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ rad, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ tax, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ ptratio, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ black, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ lstat, data = Boston)$coefficients[2])
univcof <- append(univcof, lm(crim ~ medv, data = Boston)$coefficients[2])
fooBoston <- (lm(crim ~ . - crim, data = Boston))
fooBoston$coefficients[2:14]
plot(univcof, fooBoston$coefficients[2:14], main = "Univariate vs. Multiple Regression Coefficients", 
     xlab = "Univariate", ylab = "Multiple")

# 15-D

lm.zn3 <- lm(crim~poly(zn,3))
summary(lm.zn3) # No evidence for cubic term.

lm.indus3 <- lm(crim~poly(indus,3))
summary(lm.indus3) # Evidence of non linearity.

# chas é um preditor qualitativo, o polinômio não faz sentido nesse caso.