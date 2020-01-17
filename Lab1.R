library(tidyverse)
mpg
p <- ggplot(data = mpg,
            mapping = aes(x = displ, y = hwy, color = class))
p + geom_point()
p + geom_smooth(method = "lm") + scale_x_log10()
#scale_x_log10 reduces the size of the grpah. Makes the lines of best fit more compact and easier to see.




bank_data <- read.table(file = "clipboard",sep = "\t",header = TRUE)


b <- ggplot(data = bank_data,
            mapping = aes(x=y,color = y))
b + geom_bar()

b2 <- ggplot(data = bank_data,
            mapping = aes(x=default,color = y))
b2 + geom_bar()

b3 <- ggplot(data = bank_data,
             mapping = aes(x=education,y = balance))
b3 + geom_point()

