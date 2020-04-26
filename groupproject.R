library(tidyverse)
library(ggplot2)
library(rUnemploymentData)
library(usmap)
library(tibble)
library(lattice)
library(directlabels)

Crimes <- CrimeStatebyState
Crimes$TotalCrimes = Crimes$Property_crimes+Crimes$Violent_crimes


Crimesplot <- ggplot(data = Crimes, aes(x=Year)) + 
  geom_line(aes(y=TotalCrimes, colour = "Total Crimes"), size = 2) +
  geom_line(linetype = "longdash", aes(y = Violent_crimes, colour = "Violent Crimes"), size = 2) +
  geom_line(linetype = "dotted", aes(y = Property_crimes, colour = "Property Crimes"), size = 2) +
  scale_colour_manual("",
                      values = c("Total Crimes"="red", 
                                 "Property Crimes" = "blue", 
                                 "Violent Crimes" = "green")) +
  theme(axis.title=element_text(face='bold', size=12),    
        legend.position = 'right',
        legend.title = element_text(face='bold'),
        axis.text = element_text(size=12),
        axis.ticks = element_blank()
  ) +
  labs(
    x= "Years",
    y= "Crimes",
    title = "Crimes in United States")

Propertycrimes <- ggplot(data = Crimes, aes(x=Year)) + 
  geom_line(aes(y= Property_crimes, colour = "Property Crimes Total"), size = 2) +
  geom_line(linetype = "longdash", aes(y = Motor_theft, colour = "Motor Theft"), size = 2) +
  geom_line(linetype = "dotted", aes(y = Burglary, colour = "Burglary"), size = 2)+
  geom_line(linetype = "dashed", aes(y = Larceny_theft, colour = "Larceny Theft"), size = 2)+
  scale_colour_manual("",
                      values = c("Property Crimes Total"="red", 
                                 "Motor Theft" = "blue", 
                                 "Burglary" = "green",
                                 "Larceny Theft" = "yellow")) +
  theme(axis.title=element_text(face='bold', size=12),    
        legend.position = 'right',
        legend.title = element_text(face='bold'),
        axis.text = element_text(size=12),
        axis.ticks = element_blank()
  ) +
  labs(
    x= "Years",
    y= "Property Crimes",
    title = "Property Crimes in United States")

Violentcrimes <- ggplot(data = Crimes, aes(x=Year)) + 
  geom_line(aes(y= Violent_crimes, colour = "Violent Crimes Total"), size = 2) +
  geom_line(linetype = "longdash", aes(y = Murder, colour = "Murder"), size = 2) +
  geom_line(linetype = "dotted", aes(y = Robbery, colour = "Robbery"), size = 2)+
  geom_line(linetype = "dashed", aes(y = Aggravated_assault, colour = "Aggravated Assault"), size = 2)+
  geom_line(linetype = "twodash", aes(y = rape...5, colour = "Rape"), size = 2) +
  scale_colour_manual("",
                      values = c("Violent Crimes Total"="red", 
                                 "Murder" = "blue", 
                                 "Robbery" = "green",
                                 "Aggravated Assault" = "yellow",
                                 "Rape" = "orange")) +
  theme(axis.title=element_text(face='bold', size=12),    
        legend.position = 'right',
        legend.title = element_text(face='bold'),
        axis.text = element_text(size=12),
        axis.ticks = element_blank()
  ) +
  labs(
    x= "Years",
    y= "Violent Crimes",
    title = "Violent Crimes in United States")


CPIplot <- ggplot() + 
  geom_line(data = Dallas, aes(x = Year, y= Annual, colour = "Dallas Annual"), size = 2) +
  geom_line(data = SanDiego, linetype = "longdash", aes(x = Year, y = Annual, colour = "San Diego Annual"), size = 2) +
  scale_colour_manual("",
                      values = c("Dallas Annual"="red", 
                                 "San Diego Annual" = "blue")) +
  theme(axis.title=element_text(face='bold', size=12),    
        legend.position = 'right',
        legend.title = element_text(face='bold'),
        axis.text = element_text(size=12),
        axis.ticks = element_blank()
  ) +
  labs(
    x= "Years",
    y= "CPI Change",
    title = "1995-2015 CPI Change for San Diego & Dallas")

data("df_state_unemployment")
unemployment <- df_state_unemployment

fips <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,
          39,40,41,42,43,44,45,46,47,48,49,50,51)

unemploymentdata <- unemployment
unemploymentdata$fips <- fips

plot_usmap(data = unemploymentdata, values = "2005", color = "white") + 
  scale_fill_continuous(low= "orange", high = "blue",name = "Unemployment % (2005)", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = unemploymentdata, values = "2006", color = "white") + 
  scale_fill_continuous(low= "orange", high = "blue",name = "Unemployment % (2006)", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = unemploymentdata, values = "2007", color = "white") + 
  scale_fill_continuous(low= "orange", high = "blue",name = "Unemployment % (2007)", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = unemploymentdata, values = "2008", color = "white") + 
  scale_fill_continuous(low= "orange", high = "blue",name = "Unemployment % (2008)", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = unemploymentdata, values = "2009", color = "white") + 
  scale_fill_continuous(low= "orange", high = "blue",name = "Unemployment % (2009)", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = unemploymentdata, values = "2010", color = "white") + 
  scale_fill_continuous(low= "orange", high = "blue",name = "Unemployment % (2010)", label = scales::comma) + 
  theme(legend.position = "right")



inflation_rate_base <- SanDiego[1,14] #((CPI2015-CPI1995)/CPI1995)*100
inflation_rate_2015 <- SanDiego[21,14]
inflation_rate_SanDiego <- ((inflation_rate_2015-inflation_rate_base)/inflation_rate_base)*100


inflation_rate_base <- Dallas[1,14]
inflation_rate_2015 <- Dallas[21,14]
inflation_rate_Dallas <- ((inflation_rate_2015-inflation_rate_base)/inflation_rate_base)*100