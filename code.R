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

ggplot(data = df, aes(x = dias, y = valor, color = class)) + 
        geom_line(data = subset(df, class %in% c("vp", "cr","va")), aes(group = class)) +
        scale_color_manual("Curva S", values = c("vp" = "darkgreen", "cr" = "blue", "va" = "red"))

fit <- lm(vp[1:50]~cr[1:50]+va[1:50])
summary(fit)
plot(predict())
