library("ggplot2")
set.seed(1)
vp1 <- rnorm(30,mean = 100,sd = 25)
vp2 <- rnorm(30,mean = 50,sd = 12)
vp3 <- rnorm(40,mean = 10,sd = 5)
vp <- c(vp1,vp2,vp3)
remove(vp1,vp2,vp3)
cr <- vp[1:50] + rnorm(50,mean = 5,sd = 2)
va <- vp[1:50] + rnorm(50,0,20)

dias <- seq(1,100,1)
m <- max(c(vp,va,cr))

df <- data.frame(dias = dias, val = cumsum(vp), class = "vp")
df <- rbind(df,data.frame(dias = 1:50,val = cumsum(cr), class = "cr"))
df <- rbind(df,data.frame(dias = 1:50,val = cumsum(va), class = "va"))

ggplot(data = df, aes(x = dias, y = val, color = class)) + 
        geom_point(data = subset(df, class %in% c("vp", "cr","va")), aes(group = class)) +
        scale_color_manual("Curva S", values = c("vp" = "darkgreen", "cr" = "blue", "va" = "red"))

fit <- lm(df$val[1:50]~v)
summary(fit)
plot(predict(fit))
