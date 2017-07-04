library("ggplot2")
require(reshape)

set.seed(1)
vp1 <- rnorm(30,mean = 100,sd = 25)
vp2 <- rnorm(30,mean = 50,sd = 12)
vp3 <- rnorm(40,mean = 10,sd = 5)
vp <- c(vp1,vp2,vp3)

remove(vp1,vp2,vp3)

cra <- vp[1:100] + rnorm(50,mean = 5,sd = 2)
vaa <- vp[1:100] + rnorm(50,-3,30)

cr <- cra[1:50]
va <- vaa[1:50]

dias <- seq(1,100,1)

df <- data.frame(dias = dias, valor = cumsum(vp), GVA = "vp", tp = "Real")
df <- rbind(df,data.frame(dias = 1:50,valor = cumsum(cr), GVA = "cr", tp = "Real"))
df <- rbind(df,data.frame(dias = 1:50,valor = cumsum(va), GVA = "va", tp = "Real"))


df <- rbind(df,data.frame(dias = dias, valor = cumsum(vp), GVA = "vp", tp = "Predito"))
fit1 <- lm(df$valor[51:100]~c(51:100)+I(c(51:100)^2))
df <- rbind(df,data.frame(dias = 51:100, valor = fit1$fitted.values, GVA = "VP", tp = "Predito"))

df <- rbind(df,data.frame(dias = 1:50,valor = cumsum(cr), GVA = "cr", tp = "Predito"))
fit2 <- lm(cra[51:100]~c(51:100)+I(c(51:100)^2))
df <- rbind(df,data.frame(dias = 51:100, valor = cumsum(cr)[50]+cumsum(fit2$fitted.values), GVA = "CR", tp = "Predito"))

df <- rbind(df,data.frame(dias = 1:50,valor = cumsum(va), GVA = "va", tp = "Predito"))
fit3 <- lm(vaa[51:100]~c(51:100)+I(c(51:100)^2))
df <- rbind(df,data.frame(dias = 51:100, valor = cumsum(va)[50]+cumsum(fit3$fitted.values)+vaa[50], GVA = "VA", tp = "Predito"))

ggplot(data = df, aes(x = dias, y = valor, color = GVA)) + 
        geom_line(data = subset(df, GVA %in% c("vp", "cr","va", "VP","CR","VA")), 
                  aes(group = GVA), size = 0.8) + ylab("Valor (R$)") +
        facet_grid( ~ tp)
        scale_color_manual("GVA", 
                           values = c("vp" = "black", "cr" = "blue", "va" = "red", 
                                      "VP" = "grey", "CR" = "lightblue", "VA" = "magenta"))






df2 <- data.frame(dias = dias, vp = vp, cr = cr, va = va)
df2[51:100,c("va","cr")] <- NA

df2 <- melt(df2 ,  id.vars = 'dias', variable.name = 'series')
colnames(df2) <- c("dias","variable", "Valor")
ggplot(data = df2, aes(x = dias, y = Valor, color = variable)) + 
        geom_point(data = subset(df2, variable %in% c("vp", "cr","va")), 
                   aes(group = variable)) + 
        scale_color_manual("Valor", values = c("vp" = "darkgreen", "cr" = "blue", "va" = "red"))

plot(dias[1:50],va)


#0000000000000000000000000000000000000000

base <- read.csv("base.txt",header = TRUE,sep = ";")
head(base)
library(party)
fit <- ctree(tp ~ ., data=base)
plot(fit, main="Conditional Inference Tree for Kyphosis")
c <- rep("Construir",10)
m <- data.frame(tp = rep("Modernizar",11), vme = seq(36,46))
base <- data.frame(tp = c)
base <- rbind(base,data.frame( tp=m))
fit <-rpart(tp ~ ., data=base, method="anova")
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit)
rpart.plot(fit)


