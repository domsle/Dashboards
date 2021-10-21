 
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
wls <- c(1567.9, 1567.8, 1567.7, 1567.7, 1567.8, 1567.5, 1567.2, 1566.6, 1566.6, 1566.5, 1566.5, 1566.3, 1566, 1565.2, 1565.2, 1565.1, 1565, 1564.8, 1564.7)
wls <- wls - min(wls)

# Pows to moc transmisji
pows <- c(-67, -67.1, -67.2, -67.2, -67.3, -67.5, -67.5, -67.8, -68, -68.2, -68.3, -68.4, -68.5, -68.4, -69.2, -69.3, -69.4, -69.5, -69.6)
pows <- pows - min(pows)
# plot(1:23, pows)
length(pows)
df <- data.frame(wl = wls, pow = pows, len = caps)

# df$delta <- wls - min(wls)
df$mag <- cal(caps)
df$magUL <- cal(caps-unc)
df$magUH <- cal(caps+unc)

#dopasowanie liniowe, dla zakresu punktów
lin_start <- 1
# lin_stop <- 19
lin_stop <- length(df[,1])

line1 <- lm(pow ~ mag, df[lin_start:lin_stop,])
# line1 <- lm(pow ~ mag, df)
df$lineX <- seq(from = df$mag[lin_start], to = df$mag[lin_stop], by = ((df$mag[lin_stop] - df$mag[lin_start])/(length(df$wl) - 1)))
df$lineY <- line1$coefficients[1] + line1$coefficients[2] * df$lineX

gg <- ggplot(data = df) + geom_point(aes(x=mag, y = wls)

gg <- ggplot(data = df) + geom_point(aes(x = mag, y = wls, col = '7mm')) + 
  geom_errorbarh(aes(y = wls, xmin=magUL, xmax=magUH),  height = 0.1) +
  # geom_smooth(aes(x = lineX, y = lineY, color = )) +
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
