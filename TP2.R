# ==================== TP2 (Mirana RAKOTOMAVO) ====================


# ---------- Question 1 ----------

# Construction des nuages de points

#install.packages("rgl")
#install.packages("plot3D")
#install.packages("plot3Drgl")
#install.packages("psych")
#install.packages("scatterplot3d")

library(rgl)
library(plot3D)
library(plot3Drgl)
library(psych)
library(scatterplot3d)
A <- read.table("C:/Users/miran/Desktop/Analyse_Donnees/data1TP2.txt", header = TRUE)
#plot3d(A)


# ---------- Question 2 ----------

# Tableau centré B : xij = xij - moy_j
# Centrage : permet de ramener toutes les colonnes de A à la même origine, 0

centrer <- function(X){
  lignes <- nrow(X);
  col <- ncol(X);
  res<-matrix(rep(0,lignes*col),lignes,col);
  moy <- apply(X,2,mean);
  for (i in 1:lignes){
    for (j in 1:col){
      res[i,j] = X[i,j] - moy[j];
    }
  }
  res;
}

B <- centrer(A)

# Matrice de covariance V
# La matrice de covariance ets la matrice carrée dont le terme générique est :
# aij = cov(X[i],X[j])

matrice_covariance <- function(X){
  col <- ncol(X);
  res<-matrix(rep(0,col*col),col,col);
  for (i in 1:col){
    for (j in 1:col){
      res[i,j] <- cov(X[i],X[j]);
    }
  }
  res;
}

V <- matrice_covariance(A)


# ---------- Question 3 ----------

# Valeurs propres et vecteurs propres
ev <- eigen(V)
valeurs_propres <- ev$values
vecteurs_propres <- ev$vectors


# ---------- Question 4 ----------

# Axes principaux
# Les valeurs propres indiquent l'ordre des axes principaux
# La stature a la valeur propre la plus élevée, c'est donc le 1er axe principal
# Le 2e axe est le poids et e 3e est la taille


# ---------- Question 5 ----------

C <- B %*% vecteurs_propres
# Vérification : Fonction princomp
C_off <- princomp(A)$scores


# ---------- Question 6 ----------

#Tracé du premier axe principal
scatter3D(x=c(0,-300*vecteurs_propres[1,1]), y=c(0,-300*vecteurs_propres[2,1]), z=c(0,-300*vecteurs_propres[3,1]), type='l')

# ---------- Question 7 ----------

plot(C[,1],C[,2])


