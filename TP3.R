# ==================== TP3 (Mirana RAKOTOMAVO) ====================


# ---------- Question 1 ----------

# Génération de x et y uniformes sur [0,1]
X1 <- runif(100,0,1)
Y1 <- runif(100,0,1)
#plot(X1,Y1)

# Génération de x et y gaussiennes (indépendantes) de variance 1
# avec x de moyenne 4 et y centrée
X2 <- rnorm(100,4,1)
Y2 <- rnorm(100,0,1)
#plot(X2,Y2)

# Génération de x et y gaussiennes (indépendantes) de variance 2
# avec x de moyenne 0.5 et y de moyenne 6
X3 <- rnorm(100,0.5,sqrt(2))
Y3 <- rnorm(100,6,sqrt(2))
#plot(X3,Y3)

#Affichage de l'ensemble des points
X <- c(X1,X2,X3)
Y <- c(Y1,Y2,Y3)
plot(X,Y)


# ---------- Question 2 ----------

nuage <- cbind(X,Y)

# Fonction matrice distance
matrice_distance <- function(T){
  lignes <- nrow(T);
  
  res<-matrix(rep(0,lignes*lignes),lignes,lignes);
  
  for (i in 1:lignes){
    for (j in i:lignes){
      c1 <- (T[i,1]-T[j,1])^2;
      c2 <- (T[i,2]-T[j,2])^2;
      c <- c1+c2;
      res[j,i] <- sqrt(c);
    }
  }
  
  res;
}

# Fonction recherche min
# Calcul matrice distance avec fonction dist
# Calcul matrice distance avec fonction matrice_distance
# Recherche le min de dist dans matrice_distance
# Retourne les coordonnées du min dans matrice_distance
recherche_min <- function(T,Distance){
  lignes <- nrow(Distance);
  
  min_ref <- min(dist(T,method = "euclidean"));
  
  res <- which(Distance==min_ref,arr.ind=TRUE);
  res;
}


classification_ascendante_hierarchique <- function(tableau,K,I,distance){
  nb_classes <- nrow(tableau);
  count <- 1;
  
  while (nb_classes != K){
    # Matrice identité
    I <- diag(nrow(nuage));
    # Matrice distance
    distance <- matrice_distance(nuage);
    # Min (x[1,1],x[1,2])
    xmin <- recherche_min(nuage,distance);
    i <- xmin[1,2];
    j <- xmin[1,1];
    
    # Ligne i = Somme des lignes
    I[i,] <- I[i,]+I[j,];
    # Ligne j = 0
    I[j,] <- rep(0);
    
    # Moyenne des points à fusionner = Nouvelle classe
    nuage[i,] <-(nuage[i,]*count+nuage[j,])/(count+1);
    nuage <- nuage[-j,];
    count <- count+1;
    nb_classes <- nb_classes-1;
  }
  
}

# Matrice identité
I <- diag(nrow(nuage));
# Matrice distance
distance <- matrice_distance(nuage);

classification_ascendante_hierarchique(nuage,3,I,distance)
#distance <- dist(nuage,method = "euclidean",diag=TRUE)
#print(distance)

# Vérification programme


# Calculer la distance Euclidienne
#D <- dist(nuage,method = "euclidean")

# Fonction de classification ascendate hiérarchique
#AscHierarchique <- hclust(D,method = "complete") # complete ou ward.D

# Visualisation par Dendrogramme
#plot(AscHierarchique, cex = 0.6, hang = -1)

# Récupérer les résultats
#cluster = cutree(AscHierarchique,3)



# ---------- Test Exemple cours ----------

X <- c(0,0,0.25,4,4.5,5)
Y <- c(1,1.5,1,2,3,1)
nuage <- cbind(X,Y)
#print(nuage[2,2])
plot(nuage)

# Fonction matrice distance
matrice_distance <- function(T){
  lignes <- nrow(T);
  
  res<-matrix(rep(0,lignes*lignes),lignes,lignes);
  
  for (i in 1:lignes){
    for (j in i:lignes){
      c1 <- (T[i,1]-T[j,1])^2;
      c2 <- (T[i,2]-T[j,2])^2;
      c <- c1+c2;
      res[j,i] <- sqrt(c);
    }
  }
  
  res;
}

# Fonction recherche min
# Calcul matrice distance avec fonction dist
# Calcul matrice distance avec fonction matrice_distance
# Recherche le min de dist dans matrice_distance
# Retourne les coordonnées du min dans matrice_distance
recherche_min <- function(T,Distance){
  lignes <- nrow(Distance);
  
  min_ref <- min(dist(T,method = "euclidean"));
  
  res <- which(Distance==min_ref,arr.ind=TRUE);
  res;
}



# ---------- Etape 1 ----------

# Matrice identité
I <- diag(nrow(nuage))
# Matrice distance
distance <- matrice_distance(nuage)
# Min (x[1,1],x[1,2]) (j,i)
xmin <- recherche_min(nuage,distance)

# Ligne i = Somme des lignes
I[1,] <- I[1,]+I[3,]
# Ligne j = 0
I[3,] <- rep(0)

# Moyenne des points à fusionner = Nouvelle classe
nuage[1,] <-(nuage[1,]+nuage[3,])/2
nuage <- nuage[-3,]


# ---------- Etape 2 ----------

# Matrice distance
distance <- matrice_distance(nuage)
# Min (x[1,1],x[1,2]) (j,i)
xmin <- recherche_min(nuage,distance)

# Ligne i = Somme des lignes
I[1,] <- I[1,]+I[2,]
# Ligne j = 0
I[2,] <- rep(0)

# Moyenne des points à fusionner = Nouvelle classe
nuage[1,] <-(nuage[1,]+nuage[2,])/2
nuage <- nuage[-2,]




#print(which.max(distance))
#print(distance[15])


