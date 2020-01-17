library(tidyverse)
mpg
p <- ggplot(data = mpg,
            mapping = aes(x = displ, y = hwy, color = class))
p + geom_point()
p + geom_smooth(method = "lm") + scale_x_log10()
#scale_x_log10 reduces the size of the grpah. Makes the lines of best fit more compact and easier to see.




bank_data <- read.table(file = "clipboard",sep = "\t",header = TRUE)
#duration vs y
#total y vs n
#
#memo shoud be about whether the campain was a success you can tell this by looking at the assemlbed graphs
#looking at duration vs y you can tell the customers that you had longer conversations with teneded to end up subscribing
#this led to a 