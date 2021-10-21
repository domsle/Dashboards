library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(plotly)

a = 0.19
b = -183
c = 0.062
d = 0.29

unc1 <- 0.05
unc2 <- 0.1
unc <- sqrt(unc1^2 + unc2^2)

cal <- function(x) { (a - (b / ((c * x/2 + 1)^(1/d))))*3.454897}



# Pierwszy pik

# CAPS to odległości między magnesami
caps <- seq(95, 5, -5)
### Największy pik

# WLs to długości fali
# Nie robimy tutaj dłgości fali bo się nie zmieniają, zmienia się tylko transmisja
# wls <- c()
pows <- data %>% filter(x1 == 1566.9) %>% select(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19) %>% as_vector()
# Pows to moc transmisji
pows2 <- c(-74.28, -74.33, -74.21, -74.16, -74.08, -74.01, -73.96, -73.82, -73.79, -73.64, -73.50, -73.47, -73.39, -73.53, -71.79, -70.91, -70.29, -69.61, -68.21)

pows3 <- data %>% filter(x1 == 1594.4) %>% select(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19) %>% as_vector()

pows4 <- data %>% filter(x1 == 1526.3) %>% select(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19) %>% as_vector()
# plot(1:23, pows)
length(pows2)
df <- data.frame(wl = wls, pow = pows, len = caps)

df$delta <- wls - min(wls)
df$mag <- cal(caps)
df$magUL <- cal(caps-unc)
df$magUH <- cal(caps+unc)
