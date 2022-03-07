## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## span {

##   display: inline-block;

## }


## ---- eval=FALSE, echo=TRUE--------------------------------------------------------------------------------------------------------------------------
## pkgs <- c("tidyverse",      # Die tidyverse-Pakete
##           "palmerpenguins", # Pinguin-Daten
##           "scales",         # Transparente Farben: alpha()
##           "RColorBrewer",   # Hübsche Farben
##           "mclust",         # Schätzung/Verwendung
##                             # Gaußschen Mischverteilungen
##           "MASS")           # Erzeugung von Zufallszahlen aus
##                             # einer multiv. Normalverteilung
## install.packages(pkgs)


## ----pinguine, include=knitr::is_html_output(), echo=FALSE, out.width='70%', fig.cap="Frecher Pinguin bei der Tat."----------------------------------
knitr::include_graphics("images/penguins.gif")


## ---- fig.align='center', out.width="100%", echo=TRUE, eval=TRUE-------------------------------------------------------------------------------------
library("palmerpenguins") # Pinguin-Daten
library("RColorBrewer")   # Hübsche Farben
library("scales")         # Für transparente Farben: alpha()

col_v <- RColorBrewer::brewer.pal(n = 3, name = "Set2")

## Vorbereitung der Daten:
Pinguine <- palmerpenguins::penguins %>% # Pinguin-Daten
  tidyr::as_tibble() %>%                 # Datenformat: 'tibble'-dataframe
  dplyr::filter(species!="Adelie") %>%   # Pinguin-Art 'Adelie' löschen 
  droplevels() %>%                       # Lösche das nicht mehr benötigte Adelie-Level
  tidyr::drop_na() %>%                   # NAs löschen
  dplyr::mutate(Art    = species,        # Variablen umbenennen
                Flosse = flipper_length_mm) %>% 
  dplyr::select(Art, Flosse)             # Variablen auswählen

##  
n      <- nrow(Pinguine)                 # Stichprobenumfang

## Variable 'Penguine_Art' aus Pinguine-Daten "herausziehen"
Penguine_Art    <- dplyr::pull(Pinguine, Art)

## Variable 'Penguine_Flosse' aus Pinguine-Daten "herausziehen"
Penguine_Flosse <- dplyr::pull(Pinguine, Flosse)

## Plot
## Histogramm:
hist(x = Penguine_Flosse, freq = FALSE, 
     xlab="Flossenlänge (mm)", main="Pinguine\n(Zwei Gruppen)",
     col=gray(.65,.5), border=gray(.35,.5), ylim=c(0.0003, 0.039))
## Stipchart hinzufügen:
stripchart(x = Penguine_Flosse, method = "jitter", 
           jitter = .0005, at = .001,
           pch = 21, col=alpha(col_v[3],.5), 
           bg=alpha(col_v[3],.5), cex=1.3, add = TRUE)


## ----GMM-plot1, fig.align='center', out.width="100%", echo=FALSE, fig.cap="Clusteranalyse basierend auf einer  Mischverteilung mit zwei gewichteten Normalverteilungen."----
## mclust R-Paket:
## Clusteranalyse mit Hilfe von Gaußschen Mischmodellen
suppressMessages(library("mclust"))

## Anzahl der Gruppen
G <- 2 

## Schätzung des Gaußschen Mischmodells (per EM Algorithmus)
## und Clusteranalyse
mclust_obj <- mclust::Mclust(data = Penguine_Flosse, G=G, 
                              modelNames = "V", 
                              verbose = FALSE)

# summary(mclust_obj)
# str(mclust_obj)

## Geschätzte Gruppen-Zuordnungen
class <- mclust_obj$classification

## Anteil der korrekten Zuordnungen:
# cbind(class, Penguine_Art)
# round(sum(class == as.numeric(Penguine_Art))/n, 2)

## Geschätzte Mittelwerte 
mean_m <- t(mclust_obj$parameters$mean)

## Geschätzte Varianzen (und evtl. Kovarianzen) 
cov_l  <- list("Cov1" = mclust_obj$parameters$variance$sigmasq[1], 
               "Cov2" = mclust_obj$parameters$variance$sigmasq[2])

## Geschätzte Gewichte (a-priori-Wahrscheinlichkeiten) 
prop_v <- mclust_obj$parameters$pro

## Auswerten der Gaußsche Mischungs-Dichtefunktion
np      <- 100 # Anzahl der Auswertungspunkte
xxd     <- seq(min(Penguine_Flosse)-3, max(Penguine_Flosse)+5, length.out = np)
## Mischungs-Dichte
yyd     <- dnorm(xxd, mean_m[1], sqrt(cov_l[[1]]))*prop_v[1] +
           dnorm(xxd, mean_m[2], sqrt(cov_l[[2]]))*prop_v[2]
## Einzel-Dichten
yyd1    <- dnorm(xxd, mean_m[1], sqrt(cov_l[[1]]))*prop_v[1]
yyd2    <- dnorm(xxd, mean_m[2], sqrt(cov_l[[2]]))*prop_v[2]

## Plot
hist(x = Penguine_Flosse, xlab="Flossenlänge (mm)", main="Pinguine\n(Zwei Gruppen)",
     col=gray(.65,.5), border=gray(.35,.5), freq = FALSE, ylim=c(0, 0.04))
lines(x = xxd, y=yyd, lwd=2, col=gray(.35,.75))
lines(x = xxd, y=yyd1, lwd=2, col=gray(.35,.75), lty=2)
lines(x = xxd, y=yyd2, lwd=2, col=gray(.35,.75), lty=2)
abline(v=203.1, lty=3)
stripchart(Penguine_Flosse[class==1], method = "jitter", jitter = .0005, at = .001,
           pch = 21, col=alpha(col_v[1],.5), bg=alpha(col_v[1],.5), cex=1.3, add = TRUE)
stripchart(Penguine_Flosse[class==2], method = "jitter", jitter = .0005, at = .001,
           pch = 21, col=alpha(col_v[2],.5), bg=alpha(col_v[2],.5), cex=1.3, add = TRUE)


## ---- eval=my_output == "html", results='asis', echo=FALSE-------------------------------------------------------------------------------------------
## cat('<hr style="background-color:#3C6690;height:2px">')


## ---- echo=TRUE, eval=FALSE--------------------------------------------------------------------------------------------------------------------------
## ## mclust R-Paket:
## ## Clusteranalyse mit Hilfe von Gaußschen Mischmodellen
## suppressMessages(library("mclust"))
## 
## ## Anzahl der Gruppen
## G <- 2
## 
## ## Schätzung des Gaußschen Mischmodells (per EM Algorithmus)
## ## und Clusteranalyse
## mclust_obj <- mclust::Mclust(data = Penguine_Flosse, G=G,
##                               modelNames = "V",
##                               verbose = FALSE)
## 
## # summary(mclust_obj)
## # str(mclust_obj)
## 
## ## Geschätzte Gruppen-Zuordnungen (Cluster-Resultat)
## class <- mclust_obj$classification
## 
## ## Anteil der korrekten Zuordnungen:
## # cbind(class, Penguine_Art)
## round(sum(class == as.numeric(Penguine_Art))/n, 2)
## 
## ## Geschätzte Mittelwerte
## mean_m <- t(mclust_obj$parameters$mean)
## 
## ## Geschätzte Varianzen (und evtl. Kovarianzen)
## cov_l  <- list("Cov1" = mclust_obj$parameters$variance$sigmasq[1],
##                "Cov2" = mclust_obj$parameters$variance$sigmasq[2])
## 
## ## Geschätzte Gewichte (a-priori-Wahrscheinlichkeiten)
## prop_v <- mclust_obj$parameters$pro
## 
## ## Auswerten der Gaußsche Mischung-Dichtefunktion
## np      <- 100 # Anzahl der Auswertungspunkte
## xxd     <- seq(min(Penguine_Flosse)-3, max(Penguine_Flosse)+5, length.out = np)
## ## Mischungs-Dichte
## yyd     <- dnorm(xxd, mean_m[1], sqrt(cov_l[[1]]))*prop_v[1] +
##            dnorm(xxd, mean_m[2], sqrt(cov_l[[2]]))*prop_v[2]
## ## Einzel-Dichten
## yyd1    <- dnorm(xxd, mean_m[1], sqrt(cov_l[[1]]))*prop_v[1]
## yyd2    <- dnorm(xxd, mean_m[2], sqrt(cov_l[[2]]))*prop_v[2]
## 
## ## Plot
## hist(x = Penguine_Flosse, xlab="Flossenlänge (mm)", main="Pinguine\n(Zwei Gruppen)",
##      col=gray(.65,.5), border=gray(.35,.5), freq = FALSE, ylim=c(0, 0.04))
## lines(x = xxd, y=yyd, lwd=2, col=gray(.35,.75))
## lines(x = xxd, y=yyd1, lwd=2, col=gray(.35,.75), lty=2)
## lines(x = xxd, y=yyd2, lwd=2, col=gray(.35,.75), lty=2)
## stripchart(Penguine_Flosse[class==1], method = "jitter", jitter = .0005, at = .001,
##            pch = 21, col=alpha(col_v[1],.5), bg=alpha(col_v[1],.5), cex=1.3, add = TRUE)
## stripchart(Penguine_Flosse[class==2], method = "jitter", jitter = .0005, at = .001,
##            pch = 21, col=alpha(col_v[2],.5), bg=alpha(col_v[2],.5), cex=1.3, add = TRUE)


## ----dirac1, include=knitr::is_html_output(), animation.hook="gifski", interval=0.1, fig.align="center", echo=FALSE, fig.cap="Normalverteilung mit $\\mu_g=x_i$ für $\\sigma_g\\to 0$."----
np  <- 1000
rep <- 30
xxd <- seq(-3, 3, length.out = np)
sd_v <- seq(.5,0.05, len=rep)
for(i in 1:length(sd_v)){
  plot(x=xxd, y=dnorm(xxd, 0, sd_v[i]), type="l", ylim=c(0,max(dnorm(xxd, 0, sd_v[rep-3]))), xlim = c(-1.5,1.5),
       ylab="",xlab="", lwd=2, col="darkblue", axes = FALSE)
  axis(2); axis(1, at=0, labels = expression(x[i])); box()
}


## ----dirac2, include=knitr::is_latex_output(), fig.align="center", echo=FALSE, fig.cap="Normalverteilung mit $\\mu_g=x_i$ für $\\sigma_g\\to 0$."----
np  <- 1000
rep <- 30
xxd <- seq(-3, 3, length.out = np)
sd_v <- seq(.5,0.05, len=rep)
plot(x=xxd, y=dnorm(xxd, 0, sd_v[i]), ylim=c(0,max(dnorm(xxd, 0, sd_v[rep]))), xlim = c(-1.5,1.5),
       ylab="",xlab="", lwd=2, col="darkblue", axes = FALSE, type="n")
axis(2); axis(1, at=0, labels = expression(x[i])); box()
##
for(i in 1:rep){
  lines(x=xxd, y=dnorm(xxd, 0, sd_v[i]), type="l", ylim=c(0,max(dnorm(xxd, 0, sd_v[rep]))), xlim = c(-1.5,1.5), lwd=2, col=gray(.5, 0.5))
}
  lines(x=xxd, y=dnorm(xxd, 0, sd_v[rep]), type="l", ylim=c(0,max(dnorm(xxd, 0, sd_v[rep]))), xlim = c(-1.5,1.5), lwd=2, col=alpha("darkblue", 1))


## ---- echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------
library("MASS")
library("mclust")

## Daten:
x <- cbind(Penguine_Flosse) # Daten [n x d]-Dimensional. 
d <- ncol(x)                # Dimension (d=1: univariat)
n <- nrow(x)                # Stichprobenumfang
G <- 2                      # Anzahl Gruppen

## Weitere Deklarationen:
llk       <- matrix(NA, n, G)
p         <- matrix(NA, n, G)  
loglikOld <- 1e07
tol       <- 1e-05
it        <- 0
check     <- TRUE 


## EM Algorithmus

## 1. Startwerte für pi, mu und sigma:
pi    <- rep(1/G, G)              # Naive pi
sigma <- array(diag(d), c(d,d,G)) # Varianz = 1
mu    <- t(MASS::mvrnorm(G, colMeans(x), sigma[,,1]*4) )

while(check){
  
  ## 2.a Expectation-Schritt 
  for(g in 1:G){
    p[,g] <- pi[g] * mclust:::dmvnorm(x, mu[,g], sigma[,,g])
  }
  p <- sweep(p, 1, STATS = rowSums(p), FUN = "/")
  
  ## 2.b Maximization-Schritt
  par   <- mclust::covw(x, p, normalize = FALSE)
  mu    <- par$mean
  sigma <- par$S
  pi    <- colMeans(p)
  
  ## 3. Prüfung der Konvergenz
  for(g in 1:G) {
    llk[,g] <- pi[g] * mclust:::dmvnorm(x, mu[,g], sigma[,,g])
  }
  loglik <- sum(log(rowSums(llk))) # aktueller max. Log-Likelihood Wert
  ##
  diff      <- abs(loglik - loglikOld)/abs(loglik) # Änderungsrate
  loglikOld <- loglik
  it        <- it + 1
  ## Änderungsrate noch groß genug (> tol)?
  check     <- diff > tol
}

## Schätz-Resultate:
results <- matrix(c(pi, mu, sqrt(sigma)), 
                  nrow = 3, ncol = 2, byrow = TRUE,
                  dimnames = list(
            c("Gewichte", "Mittelwerte", "Standardabweichungen"),
            c("Gruppe 1", "Gruppe 2"))) 
##
results %>% round(., 2)


## ----EMGif, include=knitr::is_html_output(), animation.hook="gifski", interval=0.15, fig.align="center", echo=FALSE, fig.cap="Iterative Schätzung mit Hilfe des EM Algorithmuses."----
library("MASS")
library("mclust")

## Daten:
x <- cbind(Penguine_Flosse) # Daten [n x d]-Dimensional. 
d <- ncol(x)                # Dimension (d=1: univariat)
n <- nrow(x)                # Stichprobenumfang
G <- 2                      # Anzahl Gruppen

## Weitere Deklarationen:
llk       <- matrix(NA, n, G)
p         <- matrix(NA, n, G)  
loglikOld <- 1e07
tol       <- 1e-06
it        <- 0
check     <- TRUE 


## EM Algorithmus

## 1. Startwerte für pi, mu und sigma:
pi    <- c(.5,.5)              # Naive pi
sigma <- array(diag(d), c(d,d,G)) # Varianz = 1
mu    <- matrix(c(205,225), nrow=1, byrow=TRUE)
#t(MASS::mvrnorm(G, colMeans(x), sigma[,,1]*4) )

while(check){
  
  ## 2.a Expectation-Schritt 
  for(g in 1:G){
    p[,g] <- pi[g] * mclust:::dmvnorm(x, mu[,g], sigma[,,g])
  }
  p <- sweep(p, 1, STATS = rowSums(p), FUN = "/")
  
  ## 2.b Maximization-Schritt
  par   <- mclust::covw(x, p, normalize = FALSE)
  mu    <- par$mean
  sigma <- par$S
  pi    <- colMeans(p)
  
  ## 3. Prüfung der Konvergenz
  for(g in 1:G) {
    llk[,g] <- pi[g] * mclust:::dmvnorm(x, mu[,g], sigma[,,g])
  }
  loglik <- sum(log(rowSums(llk))) # aktueller max. Log-Likelihood Wert
  ##
  diff      <- abs(loglik - loglikOld)/abs(loglik) # Änderungsrate
  loglikOld <- loglik
  it        <- it + 1
  ## Änderungsrate noch groß genug (> tol)?
  check     <- diff > tol
  
  ## Plot 
  ##
  xxd     <- seq(min(Penguine_Flosse)-3, max(Penguine_Flosse)+5, length.out = np)
  ## Mischungs-Dichte
  yyd     <- dnorm(xxd, mu[1,1], sqrt(sigma)[,,1])*pi[1] +
             dnorm(xxd, mu[1,2], sqrt(sigma)[,,2])*pi[2]
  ## Einzel-Dichten
  yyd1    <- dnorm(xxd, mu[1,1], sqrt(sigma)[,,1])*pi[1]
  yyd2    <- dnorm(xxd, mu[1,2], sqrt(sigma)[,,2])*pi[2]

  loc_line <- which.min(diff(sign(yyd1 - yyd2)))
  
  ## Classification
  cl <- as.numeric(
    dnorm(x, mu[1,1], sqrt(sigma)[,,1])*pi[1] < dnorm(x, mu[1,2], sqrt(sigma)[,,2])*pi[2])

  #class
  
  hist(x = Penguine_Flosse, xlab="Flossenlänge (mm)", main="Pinguine\n(Zwei Gruppen)",
       col=gray(.65,.5), border=gray(.35,.5), freq = FALSE, ylim=c(0, 0.04))
  lines(x = xxd, y=yyd, lwd=2, col=gray(.35,.75))
  lines(x = xxd, y=yyd1, lwd=2, col=gray(.35,.75), lty=2)
  lines(x = xxd, y=yyd2, lwd=2, col=gray(.35,.75), lty=2)
  text(x = 175, y=0.035, labels = paste("Wiederholung k =",it), pos = 4)
  ##
  set.seed(1)
  stripchart(Penguine_Flosse[cl==0], method = "jitter", jitter = .0005, at = .001,
           pch = 21, col=alpha(col_v[1],.5), bg=alpha(col_v[1],.5), cex=1.3, add = TRUE)
  set.seed(1)
  stripchart(Penguine_Flosse[cl==1], method = "jitter", jitter = .0005, at = .001,
           pch = 21, col=alpha(col_v[2],.5), bg=alpha(col_v[2],.5), cex=1.3, add = TRUE)
  ##
  abline(v=xxd[loc_line-1], lty=3)
}


## ---- echo=TRUE, include=knitr::is_latex_output()----------------------------------------------------------------------------------------------------
## Auswerten der Gaußsche Mischungs-Dichtefunktion
np      <- 100 # Anzahl der Auswertungspunkte
xxd     <- seq(min(Penguine_Flosse)-3, max(Penguine_Flosse)+5, length.out = np)
## Mischungs-Dichte
yyd     <- dnorm(xxd, mu[1,1], sqrt(sigma)[,,1])*pi[1] +
           dnorm(xxd, mu[1,2], sqrt(sigma)[,,2])*pi[2]
## Einzel-Dichten
yyd1    <- dnorm(xxd, mu[1,1], sqrt(sigma)[,,1])*pi[1]
yyd2    <- dnorm(xxd, mu[1,2], sqrt(sigma)[,,2])*pi[2]

## Plot
hist(x = Penguine_Flosse, xlab="Flossenlänge (mm)", main="Pinguine\n(Zwei Gruppen)",
     col=gray(.65,.5), border=gray(.35,.5), freq = FALSE, ylim=c(0, 0.04))
lines(x = xxd, y=yyd, lwd=2, col=gray(.35,.75))
lines(x = xxd, y=yyd1, lwd=2, col=gray(.35,.75), lty=2)
lines(x = xxd, y=yyd2, lwd=2, col=gray(.35,.75), lty=2)
abline(v=203.1, lty=3)
stripchart(Penguine_Flosse[class==1], method = "jitter", jitter = .0005, at = .001,
           pch = 21, col=alpha(col_v[1],.5), bg=alpha(col_v[1],.5), cex=1.3, add = TRUE)
stripchart(Penguine_Flosse[class==2], method = "jitter", jitter = .0005, at = .001,
           pch = 21, col=alpha(col_v[2],.5), bg=alpha(col_v[2],.5), cex=1.3, add = TRUE)


## ----pinguinattack, include=knitr::is_html_output(), echo=FALSE, fig.cap="Pinguinforschung am Limit."------------------------------------------------
knitr::include_graphics("images/penguin_attack.gif")

