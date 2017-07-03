library("ggplot2")

set.seed(1)
vp1 <- rnorm(30,mean = 100,sd = 25)
vp2 <- rnorm(30,mean = 50,sd = 12)
vp3 <- rnorm(40,mean = 10,sd = 5)
vp <- c(vp1,vp2,vp3)

remove(vp1,vp2,vp3)

cr <- vp[1:50] + rnorm(50,mean = 5,sd = 2)
va <- vp[1:50] + rnorm(50,-3,30)

dias <- seq(1,100,1)
m <- max(c(vp,va,cr))

df <- data.frame(dias = dias, valor = cumsum(vp), class = "vp")
df <- rbind(df,data.frame(dias = 1:50,valor = cumsum(cr), class = "cr"))
df <- rbind(df,data.frame(dias = 1:50,valor = cumsum(va), class = "va"))

fit <- glm(df$valor[51:100]~I(c(51:100)*c(51:100)))

df <- rbind(df,data.frame(dias = 51:100, valor = fit$fitted.values, class = "VP"))

ggplot(data = df, aes(x = dias, y = valor, color = class)) + 
        geom_line(data = subset(df, class %in% c("vp", "cr","va", "VP")), 
                  aes(group = class)) +
        scale_color_manual("Curva S", values = c("vp" = "black", "cr" = "blue", "va" = "red", "VP" = "brown"))



df2 <- data.frame(dias = dias, vp = vp, cr = cr, va = va)
df2[51:100,c("va","cr")] <- NA
require(reshape)
df2 <- melt(df2 ,  id.vars = 'dias', variable.name = 'series')
colnames(df2) <- c("dias","variable", "Valor")
ggplot(data = df2, aes(x = dias, y = Valor, color = variable)) + 
        geom_point(data = subset(df2, variable %in% c("vp", "cr","va")), 
                   aes(group = variable)) + 
        scale_color_manual("Valor", values = c("vp" = "darkgreen", "cr" = "blue", "va" = "red"))

plot(dias[1:50],va)

