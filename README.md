# stochasticscenarios

[![Documentation](https://img.shields.io/badge/documentation-is_here-green)](https://techtonique.github.io/stochasticscenarios/index.html)

A Package for Asset Projection Using Stochastic Simulation

```R
library(stochasticscenarios)
data(ZC)
```

```R
# A function to compute cashflows
computeCfs <- function(scenariosRates, scenariosAsset, txStructurel, txConjoncturel)
{
  #Projection des flux
  L <- matrix(data=1, nrow=nrow(scenariosRates), ncol=ncol(scenariosRates))
  flux <- matrix(data=0, nrow=nrow(scenariosRates), ncol=ncol(scenariosRates))
  actu <- matrix(data=1, nrow=nrow(scenariosRates), ncol=ncol(scenariosRates))
  T <- ncol(scenariosRates) - 1
  for (t in 1:T)
  {
    if (t==T)
    {
      tauxRachat <- 1
    }else
    {
      tauxRachat <- txStructurel + txConjoncturel*(scenariosAsset[, t] < 1)
    }
    L[, t+1] <- L[, t]*scenariosAsset[, t+1]/scenariosAsset[,t]
    flux[, t+1] <- L[, t+1]*tauxRachat
    L[, t+1] <- L[, t+1]-flux[, t+1]
    actu[, t+1] <- actu[, t]*exp(-scenariosRates[, t+1])
  }
  res <- list()
  res[["L"]] <- L
  res[["flux"]] <- flux
  res[["actu"]] <- actu
  return(res)
}
```

```R
k <- 0.12              #Vitesse de retour ? la moyenne du taux court
sTaux <- 0.05          #Volatilit? du processus de taux court
sUC <- .16             #Volatilit? de l'UC
H <- 10                #Horizon de projection
nSimulations <- 500L   #Nombre de simulations
tauxRachatS <- .02
tauxRachatC <- .05

set.seed(123)

# Trajectoire UC et taux. Fonction rStock du package ESG
traj <- rStock(horizon=H, nScenarios=nSimulations, ZC=ZC, vol=sTaux, k=k, volStock=sUC, stock0=1,
              rho=.5)
# Simulation de taux courts
trajectoiresTaux <- traj$shortRatePaths
# Simulation du cours de actions
trajectoiresUC <- traj$stockPaths

par(mfrow=c(1, 2))
matplot(trajectoiresTaux, type='l')
matplot(trajectoiresUC, type='l')
par(mfrow=c(1, 1))

# Flux futurs
futureCfs <- computeCfs(trajectoiresTaux, trajectoiresUC ,tauxRachatS, tauxRachatC)

# Taux d'actualisation
ActufutureCfs <- futureCfs$flux*futureCfs$actu

# Best Estimate calculation
BEempirique <- sum(ActufutureCfs)/nSimulations
BEempirique

## Best Estimate Liabilities 

# Vecteur des moyennes empiriques
moyemp <- rep(0, nSimulations)

# Calcul des moyennes empiriques successives
w <- rep(1, nSimulations)/(1:nSimulations)
temp_moyemp <- apply(ActufutureCfs,2,cumsum)*w
moyemp <- apply(temp_moyemp, 1, sum)

# Graphique de convergence
plot(x=(1:nSimulations), y=moyemp,type="l",xlab="Nombre de simulations",ylab="Best Estimate")
lines(moyemp,col="blue", lwd=2)
titre = paste("Best Estimate Liabilities", "\n", "en fonction du nombre de simulations")
title(titre)
lines(x=(1:nSimulations), y=rep(1, nSimulations), col='red', lwd=2, lty='dashed')
print(paste("Valeur de la moyenne des flux futurs actualis?s par simulation : ", BEempirique, sep=""))
```