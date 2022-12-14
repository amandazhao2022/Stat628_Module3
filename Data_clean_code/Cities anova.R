install.packages("readxl")
library(readxl)
data = read_xlsx("SAGE.xlsx")
data.frame(data)
attach(data)
data
cor.test(Fall17R,Fall17C,method = c("pearson"))
cor.test(Spring17R,Spring17C,method = c("pearson"))
cor.test(Fall18R,Fall18C,method = c("pearson"))
cor.test(Spring18R,Spring18C,method = c("pearson"))
cor.test(Fall19R,Fall19C,method = c("pearson"))
cor.test(Spring19R,Spring19C,method = c("pearson"))
cor.test(Fall20R,Fall20C,method = c("pearson"))
cor.test(Spring20R,Spring20C,method = c("pearson"))
plot(Fall17R,Fall17C)
scatter.smooth(Fall17R, Fall17C)

install.packages(ggpubr)
t.test(Fall17R,Fall17C)
t.test(Fall18R,Fall18C)
t.test(Fall19R,Fall19C)
t.test(Fall20R,Fall20C)
