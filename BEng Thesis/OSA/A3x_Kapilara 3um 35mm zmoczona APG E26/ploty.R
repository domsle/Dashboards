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
caps <- c(70.3, 59.9, 51.4, 45.9, 38.3, 34.8, 32.3, 28.3, 24.1, 21.1, 18.7, 15.6, 14.1, 13.0, 12.0, 10.3, 9.8, 9.5, 8.2, 6.8, 5.5, 4.9, 3.5, 2.5)
### Największy pik

# WLs to długości fali
wls <- c(1334, 1336, 1337, 1339, 1339.7, 1340.3, 1339.6, 1339.2, 1339.4, 1340.8, 1340.8, 1341.1, 1342.3, 1343.3, 1344.1, 1344.8, 1344.9, 1345.2, 1344.6, 1346.3, 1347.5, 1346.4, 1347.9, 1346)

# Pows to moc transmisji
pows <- c(-69.6, -69.8, -69.2, -69.5, -70.8, -72, -73.4, -74.9, -76, -78.6, -78.9, -81.3, -86.5, -91.4, -93.8, -97.4, -97.2, -86.4, -78.6, -76.4, -78.4, -81.9, -80.1, -75.6)
# plot(1:23, pows)
length(pows)
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
