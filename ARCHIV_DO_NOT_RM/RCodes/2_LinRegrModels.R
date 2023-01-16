## ----setup-lin-reg, include=FALSE--------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## span {

##   display: inline-block;

## }


## ----Fig-BikeSharing, include=knitr::is_html_output(), echo=FALSE, out.width='70%', fig.cap="Abgase am Morgen (Foto: David Lee)."--------------------
knitr::include_graphics("images/Car_Pollution.jpg")
# https://unsplash.com/photos/RhVqPKp4va4 # free pic adress


## ---- eval=FALSE, echo=TRUE--------------------------------------------------------------------------------------------------------------------------
## ## Installieren
## install.packages("tidyverse")
## install.packages("GGally")
## install.packages("ISLR")
## ## Laden
## library("tidyverse") # Viele nützliche Pakete zur Datenverarbeitung
## library("GGally")    # Pairs-Plot
## library("ISLR")      # Enthält die Auto-Daten
## data(Auto)           # Auto-Daten abrufbar machen


## ---- echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------
## Laden
library("ISLR")      # Enthält die Auto-Daten
library("tidyverse") # Viele nützliche Pakete zur Datenverarbeitung
library("GGally")    # Pairs-Plot
data(Auto)           # Daten abrufbar machen


## ---- echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------
## Auswahl und Aufbereitung der Variablen 
Auto_df <- Auto %>% 
  mutate(Verbrauch = mpg * (1.60934/3.78541), # Verbrauch (km/Liter)
         Gewicht   = weight * 0.45359,        # Gewicht (kg)
         PS        = horsepower,              # Pferdestärken (PS)
         Hubraum   = displacement * 2.54^3    # Hubraum (ccm)
         ) %>%   
 dplyr::select("Verbrauch", "Gewicht", "PS", "Hubraum") 

n <- nrow(Auto_df) # Stichprobenumfang 


## ---- eval=TRUE--------------------------------------------------------------------------------------------------------------------------------------
knitr::kable(head(Auto_df), digits = 2, 
             col.names = c("Verbrauch (km/Liter)",
                           "Gewicht (kg)",
                           "Pferdestärken (PS)",
                           "Hubraum (ccm)"))


## ----pairsplot, echo=TRUE, out.width="100%", out.height="100%", fig.cap="Pairs-Plot zur Veranschaulichung der paarweisen Zusammenhänge zwischen den Variablen."----
ggpairs(Auto_df,
upper = list(continuous = "density", combo = "box_no_facet"),
lower = list(continuous = "points", combo = "dot_no_facet"))


## ----fakedata, fig.cap="Simulierte (künstlich erzeugte) Daten zur Veranschaulichung einer allgemeinen, univariaten Regressionsbeziehung."------------
## Erzeugung von "Fake-Daten":
set.seed(1234)
n   <- 50
X   <- runif(n = n, min = 10, max = 25)
f   <- function(x){20 + 60 * pnorm(((x-10)/15)*4-2)}
eps <- rnorm(n, mean = 1, sd = 4)
Y   <- f(X) + eps
fake_data <- tibble("Y"=Y, "X"=X)

## Plot
plot(x = X, y = Y, pch=21, bg="red", col="red",
     xlab="Jahre in Ausbildung (Faked X)",
     ylab="Einkommen (Faked Y)", xlim = c(10,25))
curve(f, 10, 25, col="darkblue", add = TRUE)
with(fake_data, segments(X, Y, X, f(X), lty=2))
legend("topleft", legend = c("Datenpunkte (X,Y)", "f(X)", "Fehlerterme"), 
       lty=c(NA,1,2), pch=c(21,NA,NA), pt.bg="red", 
       col=c("red", "darkblue","black"), bty="n")


## ----plot3d, fig.cap="Veranschaulichung einer allgemeinen, bivariaten Regressionsbeziehung.", out.width="500%", out.height="500%"--------------------
# create sample dataset - you have this already,,,
Auto %>% lm(mpg ~ weight + I(weight^3) + displacement + I(displacement^3), data = .) -> fit

grid.lines = 26 #vis parameter

x.pred = seq(min(Auto$weight), 
             max(Auto$weight), length.out= grid.lines)
y.pred = seq(min(Auto$displacement), 
             max(Auto$displacement), length.out = grid.lines)
xy = expand.grid(weight = x.pred, 
                 displacement = y.pred)

z.pred = matrix(predict(fit, 
                        newdata = data.frame("weight"=xy$weight, 
                                             "I(weight^3)"=xy$weight^2,
                                             "displacement"=xy$displacement,
                                             "I(displacement^3)"=xy$displacement^2)), 
                nrow = grid.lines, ncol = grid.lines)

fitpoints = predict(fit)
library("scatterplot3d") 
library("plot3D")
scatter3D(Auto$weight, Auto$displacement, Auto$mpg, pch = 21, cex = .9,
          zlab="Y", xlab="X1", ylab="X2", col="gray",
                     theta = 30, phi = 20, #ticktype = "detailed",
                     surf = list(x = x.pred, y = y.pred, z = z.pred,
                                 facets = NA, fit = fitpoints,
                                 NAcol = "grey", shade = 0.1))


## ----polynom, echo=TRUE, fig.cap="Polynom Regression bei verschiedenen Polynomgraden $p$.", out.width="100%", out.height="100%"----------------------
## Polynom Regressionen
polreg_1 <- lm(Verbrauch ~ poly(PS, degree = 1, raw=TRUE), data = Auto_df)
polreg_2 <- lm(Verbrauch ~ poly(PS, degree = 2, raw=TRUE), data = Auto_df)
polreg_5 <- lm(Verbrauch ~ poly(PS, degree = 5, raw=TRUE), data = Auto_df)
## Data-Frame zum Abspeichern der Prädiktionen
plot_df       <- tibble("PS" = seq(45, 250, len=50))
## Abspeichern der Prädiktionen
plot_df$fit_1 <- predict(polreg_1, newdata = plot_df)
plot_df$fit_2 <- predict(polreg_2, newdata = plot_df)
plot_df$fit_5 <- predict(polreg_5, newdata = plot_df)
## Ploten
plot(Verbrauch ~ PS, data = Auto_df, ylim=c(2,20),
     xlab="Leistung (PS)", pch=21, col="gray", bg="gray", cex=1.5)
with(plot_df, lines(x = PS, y = fit_1, lwd=2, col="orange"))
with(plot_df, lines(x = PS, y = fit_2, lwd=2, col="blue"))
with(plot_df, lines(x = PS, y = fit_5, lwd=2, col="darkgreen"))
legend("topright", lty=c(NA,1,1,1), pch=c(21,NA,NA,NA), 
       col=c("gray","orange","blue","darkgreen"), pt.bg="gray", pt.cex=1.5,
       legend=c("Datenpunkte", "Grad 1", "Grad 2", "Grad 5"), bty="n")


## ----------------------------------------------------------------------------------------------------------------------------------------------------
## Polynom Regressionen
p_m    <- 100
RSS_v    <- numeric(p_m)
polreg_p <- vector("list", p_m)
fitted_m <- matrix(NA, 50, p_m)
for(p in 1:p_m){
 polreg_p[[p]] <- lm(Verbrauch ~ poly(PS, degree = p, raw=TRUE), data = Auto_df)
 RSS_v[p]      <- sum(resid(polreg_p[[p]])^2)
 plot_df       <- tibble("PS" = seq(min(Auto_df$PS), max(Auto_df$PS), len=50))
 suppressWarnings(fitted_m[,p]  <- predict(polreg_p[[p]], newdata = plot_df))
}


## ----RSSPoly1, include=knitr::is_html_output(), animation.hook="gifski", interval=0.1, fig.align="center", echo=FALSE--------------------------------
pal    <- colorRampPalette(c("blue", "red"))
cols_v <- pal(p_m)
for(p in 2:p_m){
par(mfrow=c(1,2))
  plot(Verbrauch ~ PS, data = Auto_df, ylim=c(2,20),
     xlab="Leistung (PS)", pch=21, col="gray", bg="gray", cex=1.5)
  lines(x = plot_df$PS, y = fitted_m[,p], lwd=2, col=cols_v[p])
  ##
  plot(2:p_m, RSS_v[-1], log="y", type="b", ylab = "RSS", xlab="Polynomgrad p", col="gray")
  points(x=p, y=RSS_v[p], pch=21, col=cols_v[p], bg=cols_v[p], cex=1.3)
par(mfrow=c(1,1))
}


## ----RSSPoly2, fig.align="center", echo=FALSE, fig.cap="Polynom Regression und die Wahl des Polynomgrades $p$ durch Minimierung der Trainingsdaten-RSS. (Eine schlechte Idee)."----
pal    <- colorRampPalette(c("blue", "red"))
cols_v <- pal(p_m)
par(mfrow=c(1,2))
  plot(Verbrauch ~ PS, data = Auto_df, ylim=c(2,20),
     xlab="Leistung (PS)", pch=21, col="gray", bg="gray", cex=1.5)
for(p in 2:p_m){
    lines(x = plot_df$PS, y = fitted_m[,p], lwd=2, col=cols_v[p])
}
    plot(2:p_m, RSS_v[-1], log="y", type="b", ylab = "RSS", xlab="Polynomgrad p", pch=21, col="black", bg="black")
  for(p in 2:p_m){
  points(x=p, y=RSS_v[p], pch=21, col=cols_v[p], bg=cols_v[p], cex=1)
  }
par(mfrow=c(1,1))


## ---- eval=FALSE, echo=TRUE--------------------------------------------------------------------------------------------------------------------------
## n        <- nrow(Auto_df) # Stichprobenumfang
## n_Train  <- 200           # Stichprobenumfang der Trainingsdaten
## n_Valid  <- n - n_Train   # Stichprobenumfang der Validierungsdaten
## 
## ## Index-Mengen zur Auswahl der
## ## Trainings- und Validierungsdaten
## I_Train  <- sample(x = 1:n, size = n_Train, replace = FALSE)
## I_Valid  <- c(1:n)[-I_Train]
## 
## ## Trainingsdaten
## Auto_Train_df <- Auto_df[I_Train, ]
## ## Validierungsdaten
## Auto_Valid_df <- Auto_df[I_Valid, ]


## ---- eval=FALSE, echo=TRUE--------------------------------------------------------------------------------------------------------------------------
## Train_polreg <- lm(Verbrauch ~ poly(PS, degree = p, raw=TRUE), data = Auto_Train_df)


## ---- eval=FALSE, echo=TRUE--------------------------------------------------------------------------------------------------------------------------
## y_fit_Valid   <- predict(Train_polreg, newdata = Auto_Valid_df)
## RSS_Valid     <- sum( (Auto_Valid_df$Verbrauch - y_fit_Valid)^2 )
## MSPE          <- RSS_Valid / n_Valid


## ---- echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------
set.seed(31)
##
n        <- nrow(Auto_df) # Stichprobenumfang
n_Train  <- 200           # Stichprobenumfang der Trainingsdaten
n_Valid  <-n - n_Train    # Stichprobenumfang der Validierungsdaten

## Index-Mengen zur Auswahl der 
## Trainings- und Validierungsdaten
I_Train  <- sample(x = 1:n, size = n_Train, replace = FALSE)
I_Valid  <- c(1:n)[-I_Train]

## Trainingsdaten 
Auto_Train_df <- Auto_df[I_Train, ]
## Validierungsdaten 
Auto_Valid_df <- Auto_df[I_Valid, ]

p_max         <- 6
MSPE          <- numeric(p_max)
fit_plot      <- matrix(NA, 50, p_max)
for(p in 1:p_max){
  ## Schritt 1
  Train_polreg <- lm(Verbrauch ~ poly(PS, degree = p, raw=TRUE), 
                     data = Auto_Train_df)
  ## Schritt 2
  y_fit_Valid   <- predict(Train_polreg, newdata = Auto_Valid_df)
  RSS_Valid     <- sum( (Auto_Valid_df$Verbrauch - y_fit_Valid)^2 )
  MSPE[p]       <- RSS_Valid / n_Valid
  ## Daten für's plotten
  fit_plot[,p] <- predict(Train_polreg, newdata = plot_df)
}


## ----RSSPoly3, fig.align="center", echo=FALSE, fig.cap="Polynom Regression und die Wahl des Polynomgrades $p$ durch Minimierung des mittleren quadratischen Prädiktionsfehler MSPE."----
par(mfrow=c(1,2))
  plot(Verbrauch ~ PS, data = Auto_df, ylim=c(2,20), ylab = "Verbrauch (km/Liter)",
     xlab="Leistung (PS)", pch=21, col="gray", bg="gray", cex=1.5)
for(p in 2:p_max){lines(x = plot_df$PS, y = fit_plot[,p], lwd=2, col="black")}
  lines(x = plot_df$PS, y = fit_plot[,which.min(MSPE)], lwd=2, col="red")
plot(1:p_max, MSPE,  log="y", type="b", ylab = "MSPE", xlab="Polynomgrad p", pch=21, col="black", bg="black",  
     ylim = c(min(MSPE),quantile(MSPE,p=1)))
points(y = MSPE[which.min(MSPE)], 
       x = c(1:p_max)[which.min(MSPE)], 
       col = "red", bg = "red", pch = 21)
par(mfrow=c(1,1))


## ----MSPE, fig.align="center", echo=FALSE, fig.cap="Zehn verschiedene MSPE-Berechnungen basierend auf zehn verschiedenen, zufälligen Aufteilungen der Daten in Trainings- und Validierungsdaten."----
set.seed(3)
n        <- nrow(Auto_df) # Stichprobenumfang
n_Train  <- 200           # Stichprobenumfang der Trainingsdaten
n_Valid  <-n - n_Train    # Stichprobenumfang der Validierungsdaten
##
p_max         <- 6
R             <- 10
MSPE          <- matrix(NA, R, p_max)

for(r in 1:R){
## Index-Mengen zur Auswahl der 
## Trainings- und Validierungsdaten
I_Train  <- sample(x = 1:n, size = n_Train, replace = FALSE)
I_Valid  <- c(1:n)[-I_Train]

## Trainingsdaten 
Auto_Train_df <- Auto_df[I_Train, ]
## Validierungsdaten 
Auto_Valid_df <- Auto_df[I_Valid, ]

for(p in 1:p_max){
  ## Schritt 1
  Train_polreg <- lm(Verbrauch ~ poly(PS, degree = p, raw=TRUE), 
                     data = Auto_Train_df)
  ## Schritt 2
  y_fit_Valid   <- predict(Train_polreg, newdata = Auto_Valid_df)
  RSS_Valid     <- sum( (Auto_Valid_df$Verbrauch - y_fit_Valid)^2 )
  MSPE[r,p]       <- RSS_Valid / n_Valid
}
}

matplot(t(MSPE), type="b", lty=1, ylab="MSPE", xlab="Polynomgrad p", pch=21, col="black", bg="black",
        main="")
for(r in 1:R){
  points(y = MSPE[r,][which.min(MSPE[r,])], 
       x = c(1:p_max)[which.min(MSPE[r,])], 
       col = "red", bg = "red", pch = 21)
}


## ----kfoldcv, include=knitr::is_html_output(), echo=FALSE, out.width='70%', fig.cap="Datenaufteilung in Trainings- und Validierungsdaten bei der $5$-fachen Kreuzvalidierung."----
knitr::include_graphics("images/5-fold_cross-validation.png")


## ---- echo=TRUE, eval=FALSE--------------------------------------------------------------------------------------------------------------------------
## n      <- nrow(Auto_df) # Stichprobenumfang
## k      <- 5             # 5-fache Kreuzvalidierung
## 
## ## Index zur Auswahl k verschiedener
## ## Trainings- und Validierungsdaten:
## folds  <- sample(x = 1:k, size = n, replace=TRUE)
## 
## ## Trainingsdaten im j-ten (j=1,2,...,k) Durchgang
## Auto_df[folds != j,]
## ## Validierungsdaten im j-ten (j=1,2,...,k) Durchgang
## Auto_df[folds == j,]


## ----AutoCV, echo=TRUE, eval=TRUE--------------------------------------------------------------------------------------------------------------------
set.seed(8)             # Seed für den Zufallsgenerator

n      <- nrow(Auto_df) # Stichprobenumfang
k      <- 5             # 5-fache Kreuzvalidierung
p_max  <- 5             # Maximaler Polynomgrad

folds     <- sample(x = 1:k, size = n, replace=TRUE)

## Container für die MSPE-Werte 
## für alle j=1,...,k Kreuzvalidierungen und 
## für alle p=1,...,p_max Polynomgrade
MSPE <- matrix(NA, nrow = k, ncol = p_max,
                    dimnames=list(NULL, paste0("p=",1:p_max)))

for(p in 1:p_max){
  for(j in 1:k){
  ## Modelschätzung auf Basis j-ten Traininsdaten Auto_df[folds != j,]
  poly_fit <- lm(Verbrauch ~
                   poly(Gewicht,        degree = p, raw = TRUE) +
                   poly(PS,             degree = p, raw = TRUE) +
                   poly(Hubraum,        degree = p, raw = TRUE),
                 data=Auto_df[folds != j,])
    ## Prädiktion  auf Basis j-ten Validierungsdaten Auto_df[folds == j,]
    pred          <- predict(poly_fit, newdata = Auto_df[folds == j,])
    ## 
    MSPE[j,p] <- mean( (Auto_df$Verbrauch[folds==j] - pred)^2 )
  }
}

## CV-Wert für alle p=1,...,p_max Polynomgrade 
CV_k <- colMeans(MSPE)

## Plotten
plot(y = CV_k, x = 1:length(CV_k), pch=21, col="black", bg="black", 
     type='b', xlab="Polynomgrad p", ylab=expression(CV[(5)]), log="y")
points(y = CV_k[which.min(CV_k)],
       x = c(1:length(CV_k))[which.min(CV_k)],
       col = "red", bg = "red", pch = 21)


## ----AutoCV2, fig.align="center", echo=FALSE, fig.cap="Zehn verschiedene $\\operatorname{CV}_{(k)}$-Berechnungen basierend auf zehn verschiedenen, zufälligen Wiederholungen der $5$-fachen Kreuzvalidierung."----
set.seed(8)             # Seed für den Zufallsgenerator

n      <- nrow(Auto_df) # Stichprobenumfang
k      <- 5             # 5-fache Kreuzvalidierung
p_max  <- 5             # Maximaler Polynomgrad

R      <- 10
CV_k   <- matrix(NA, R, p_max)

for(r in 1:R){

folds     <- sample(x = 1:k, size = n, replace=TRUE)

## Container für die MSPE-Werte 
## für alle j=1,...,k Kreuzvalidierungen und 
## für alle p=1,...,p_max Polynomgrade
MSPE <- matrix(NA, nrow = k, ncol = p_max,
                    dimnames=list(NULL, paste0("p=",1:p_max)))

for(p in 1:p_max){
  for(j in 1:k){
  ## Modelschätzung auf Basis j-ten Traininsdaten Auto_df[folds != j,]
  poly_fit <- lm(Verbrauch ~
                   poly(Gewicht,        degree = p, raw = TRUE) +
                   poly(PS,             degree = p, raw = TRUE) +
                   poly(Hubraum,        degree = p, raw = TRUE),
                 data=Auto_df[folds != j,])
    ## Prädiktion  auf Basis j-ten Validierungsdaten Auto_df[folds == j,]
    pred          <- predict(poly_fit, newdata = Auto_df[folds == j,])
    ## 
    MSPE[j,p] <- mean( (Auto_df$Verbrauch[folds==j] - pred)^2 )
  }
}

## CV-Wert für alle p=1,...,p_max Polynomgrade 
CV_k[r,] <- colMeans(MSPE)
}

## Plotten
matplot(t(CV_k), type="b", lty=1, ylab=expression(CV[k]), xlab="Polynomgrad p", pch=21, col="black", bg="black",
        main="")
for(r in 1:R){
  points(y = CV_k[r,][which.min(CV_k[r,])], 
       x = c(1:p_max)[which.min(CV_k[r,])], 
       col = "red", bg = "red", pch = 21)
}


## ----mazda, fig.align="center", echo=TRUE, fig.cap="Polynomregression im Anwendungsbeispiel zum Benzinverbrauch. Die größte negative Abweichung ist der Mazda RX-3 von 1973."----

p <- 2
poly_fit <- lm(Verbrauch ~
                   poly(Gewicht,        degree = p, raw = TRUE) +
                   poly(PS,             degree = p, raw = TRUE) +
                   poly(Hubraum,        degree = p, raw = TRUE),
                 data=Auto_df)

par(mar=c(5.1, 5.1, 4.1, 2.1))
plot(y = resid(poly_fit), x = fitted(poly_fit), 
     ylab = expression("Residuen:"~y[i] - hat(y)[i]), 
     xlab = expression("Prädiktionen:"~hat(y)[i]),
     main="Größte negative Abweichung der Verbrauchsangabe")
slct <- order(abs(resid(poly_fit)), decreasing = TRUE)[4]
points(y = resid(poly_fit)[slct], x = fitted(poly_fit)[slct], 
       col = "red", bg = "red", pch = 21)
text(y = resid(poly_fit)[slct], x = fitted(poly_fit)[slct], 
     labels = "Mazda RX-3 (1973)", pos = 2)
par(mar=c(5.1, 4.1, 4.1, 2.1))


## ----mazda2, include=knitr::is_html_output(), echo=FALSE, out.width='70%', fig.cap="Mazda RX-3 hatte einen Wankelmotor."-----------------------------
knitr::include_graphics("images/mazda_rx3.jpg")
# https://unsplash.com/photos/RhVqPKp4va4 # free pic adress


## ----ENDE, include=knitr::is_html_output(), echo=FALSE, out.width='70%', fig.cap="Curve-Fitting nach (xkcd)[https://xkcd.com/2048/]."----------------
knitr::include_graphics("images/curve_fitting.png")
# https://unsplash.com/photos/RhVqPKp4va4 # free pic adress

