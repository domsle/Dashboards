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
caps <- c(150, 97.5, 87, 80.4, 73.8, 65, 54.3, 47, 42.2, 37, 31.2, 23.7, 14.7, 6)
### Największy pik

# WLs to długości fali
# Nie robimy tutaj dłgości fali bo się nie zmieniają, zmienia się tylko transmisja
wls <- c(1320.8, 1320.7, 1320.6, 1320.6, 1320.4, 1320.5, 1320.2, 1320.2, 1320.2, 1320.1,  1320.1, 1320.1, 1320, 1319.9)

pows <- c(-72.15, -71.8, -71.6, -71.48, -71.22, -71.38, -71.26, -71.02, -70.85, -70.77, -70.57, -70.55, -70.34, -70.06)

pows2 <- data %>% filter(x1 == 1324) %>% select(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14) %>% as_vector()
# Pows to moc transmisji

pows3 <- data %>% filter(x1 == 1342.4) %>% select(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19) %>% as_vector()



# plot(1:23, pows)
length(pows2)
df <- data.frame(wl = wls, pow = pows, len = caps)

df$delta <- wls - min(wls)
df$mag <- cal(caps)
df$magUL <- cal(caps-unc)
df$magUH <- cal(caps+unc)

#dopasowanie liniowe, dla zakresu punktów
lin_start <- 1
lin_stop <- 17

line1 <- lm(pow ~ mag, df[lin_start:lin_stop,])
df$lineX <- seq(from = df$mag[lin_start], to = df$mag[lin_stop], by = ((df$mag[lin_stop] - df$mag[lin_start])/(length(df$wl) - 1)))
df$lineY <- line1$coefficients[1] + line1$coefficients[2] * df$lineX

gg <- ggplot(data = df) + geom_point(aes(x = mag, y = pow)) + 
  geom_errorbarh(aes(y = pow, xmin=magUL, xmax=magUH),  height = 0.8) +
  geom_smooth(aes(x = lineX, y = lineY, color = )) +
  labs(title='Zmiana mocy transmisji piku pod wpływem indukcji pola magnetycznego',
       x='natężenie pola magnetycznego[mT]',
       y='moc[dBm]',
       caption = 'Pomiary dokonane w laboratorium')  + 
  scale_color_manual(name = "Wartość indukcji\npola magnetycznego") +
  theme(legend.text.align = 1, legend.title.align = 1, legend.text = element_text(family = "FuraCode Nerd Font Mono")) 
gg
# przesunięcie długości fali
poly1 <- lm(delta ~ poly(mag, 2, raw = T), df)
# poly1 <- lm(delta ~ log(mag), df)
summary(poly1)
df$polyWls <- poly1$fitted.values
gg <- ggplot(data = df) + geom_point(aes(x = mag, y = delta)) + 
  geom_errorbarh(aes(y = delta, xmin=magUL, xmax=magUH),  height = 0.8) +
  geom_smooth(aes(x = mag, y = polyWls, color = )) +
  labs(title='Zmiana długości fali piku pod wpływem indukcji pola magnetycznego',
       x='natężenie pola magnetycznego[mT]',
       y='względne przesunięcie piku[nm]',
       caption = 'Pomiary dokonane w laboratorium')  + 
  scale_color_manual(name = "Wartość indukcji\npola magnetycznego") +
  theme(legend.text.align = 1, legend.title.align = 1, legend.text = element_text(family = "FuraCode Nerd Font Mono")) 
gg


ggplotly(gg)

# Górny pik

wls <- c(1315.5, 1317.7, 1318.4, 1319.6, 1319.7, 1319.8, 1319.9, 1320.2, 1320.5, 1320.9, 1321.2, 1322, 1323, 1324.0, 1324.8, 1325.3, 1325.4, 1325.8, 1326.3,  1329.4, 1327.3, 1328.9, 1325.6 ) 
length(wls)
pows <- c(-63.9, -63.3, -62.7, -61.8, -61, -61, -61.2, -61.8, -61.6, -61.3, -61.1, -61.5, -60.5, -60.3, -60, -60.4, -60, -62.3, -61.1,  -61.8, -61.9, -61.3, -60.5)
length(pows)
plot(caps[-1], wls)

df <- data.frame(wl = wls, pow = pows, len = caps[-1])
df$delta <- wls - min(wls)
df$mag <- cal(caps[-1])

df$mag <- cal(caps[-1])
df$magUL <- cal(caps[-1]-unc)
df$magUH <- cal(caps[-1]+unc)


gg <- ggplot(data = df) + geom_point(aes(x = mag, y = pow)) + 
  geom_errorbarh(aes(y = pow, xmin=magUL, xmax=magUH),  height = 0.1) +
  # geom_smooth(aes(x = lineX, y = lineY, color = )) +
  labs(title='Zmiana mocy transmisji piku pod wpływem indukcji pola magnetycznego',
       x='natężenie pola magnetycznego[mT]',
       y='moc[dBm]',
       caption = 'Pomiary dokonane w laboratorium')  + 
  scale_color_manual(name = "Wartość indukcji\npola magnetycznego") +
  theme(legend.text.align = 1, legend.title.align = 1, legend.text = element_text(family = "FuraCode Nerd Font Mono")) 
gg

poly1 <- lm(delta ~ poly(mag, 2, raw = T), df)
# poly1 <- lm(delta ~ log(mag), df)
summary(poly1)
df$polyWls <- poly1$fitted.values

gg <- ggplot(data = df) + geom_point(aes(x = mag, y = delta)) + 
  geom_errorbarh(aes(y = delta, xmin=magUL, xmax=magUH),  height = 0.8) +
  geom_smooth(aes(x = mag, y = polyWls, color = )) +
  labs(title='Zmiana długości fali piku pod wpływem indukcji pola magnetycznego',
       x='natężenie pola magnetycznego[mT]',
       y='względne przesunięcie piku[nm]',
       caption = 'Pomiary dokonane w laboratorium')  + 
  scale_color_manual(name = "Wartość indukcji\npola magnetycznego") +
  theme(legend.text.align = 1, legend.title.align = 1, legend.text = element_text(family = "FuraCode Nerd Font Mono")) 
gg
