base <- read.csv("base.txt",header = TRUE,sep = ";")
library("partykit")
ct <- ctree(retorno  ~ vme + investimento + demanda , data = base)
plot(ct)
