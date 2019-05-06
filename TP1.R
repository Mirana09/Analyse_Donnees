# ==================== TP1 (Mirana RAKOTOMAVO) ====================


# ---------- Question 1 ----------

# Les différentes variables semblent être liées à la variable Y (mise à part la variable C)
# En effet, les nuages de points représentent des courbes :
# Linéaires croissantes : B et D
# Linéaire décroissante : A
# Non-linéaires : E et C

# Construction des nuages de points
Database <- read.table("C:/Users/miran/Desktop/Analyse_Donnees/data1TP1.txt", header = TRUE)
x1 <- data.frame(Database[1],Database[6])
x2 <- data.frame(Database[2],Database[6])
x3 <- data.frame(Database[3],Database[6])
x4 <- data.frame(Database[4],Database[6])
x5 <- data.frame(Database[5],Database[6])
plot(x1)
plot(x2)
plot(x3)
plot(x4)
plot(x5)


# ---------- Question 2 ----------

# Fonction qui calcule le coefficient r de Pearson
pearson <- function(X,Y){
  ex <- apply(X,2,sd);
  ey <- apply(Y,2,sd);
  e <- ex*ey;
  
  c <- cov(X,Y);
  
  res <- c/e;
  res;
}

# Comparaison entre la fonction précédente et la fonction cor
# On retrouve bien les mêmes coefficients
coefpearson <- pearson(Database,Database[6])
coefoffpearson <- cor(Database,Database[6])

# Les résultats obtenus sont (pour les variables de A à E) :
# -0.9722452 0.9815886 0.4119462 0.7513686 0.2103020

# Le coefficient de corrélation peut avoir une valeur comprise entre -1 et +1.
# Plus la valeur absolue du coefficient est importante, plus la relation linéaire
# entre les variables est forte.

# Le signe du coefficient indique la direction de la relation.
# Si les deux variables ont tendance à augmenter ou à diminuer ensemble,
# le coefficient est positif, et la ligne qui représente la corrélation s'incline
# vers le haut. Si une variable a tendance à augmenter lorsque l'autre diminue, 
# le coefficient est négatif, et la ligne représentant la corrélation s'incline vers le bas.

# Ainsi, la variable qui a la plus petite corrélation est la variable E car elle possède
# le plus petit coeffcicient de corrélation e valeur absolue.


# ---------- Question 3 ----------

# Fonction qui calcule le coefficient de Spearman
myspearman <- function(X,Y){
  n <- nrow(X);
  
  rx <- rank(X);
  ry <- rank(Y);
  
  sum <- 0;
  for (i in 1:n) {
    r <- rx[i]-ry[i];
    rc <- r^2;
    sum <- sum + rc;
  }
  
  rho <- 1 - ( (6*sum) / ((n^3)-n) );
  #print(rho);
  
}

# Comparaison entre la fonction précédente et la fonction cor (avec la méthode de Spearman)
# Cette fois-ci, on a une petite différence entre les 2 :
# myspearman --> -0.9973214 0.9982143 0.4169643 1 0.3419643
# fonction cor --> -0.9991067 0.9982127 0.4164434 1.0000000 0.3413764
coefspear <- myspearman(Database[5],Database[6])
coefoffspear <- cor(Database,Database[6], method="spearman")

# Pour la plupart des variables, le coefficient de Spearman est plus élevé que le
# coefficient de Pearson.


# ---------- Question 5 ----------

# Hypothèse nulle : L'inflation n'a pas affecté le cout de la vie à Marseille
# degré de liberté = 14 / Intervalle de confiance a 95% (alpha = 5%)
# Valeur critique v = 2.145
# Si la valeur absolue de t (|t|) est supérieure à la valeur critique,
# alors la différence est significative. Dans le cas contraire, elle, ne l'est pas.
# Ici, t = 2.177 est supérieur a la valeur critique, la différence est significative.
# L'hypothèse nulle est rejetée : L'inflation a affecté le cout de la vie à Marseille.

Database2 <- read.table("C:/Users/miran/Desktop/Analyse_Donnees/data2TP1.txt", header = TRUE)
xx <- data.frame(Database2[1])

# Fonction qui calcule le score t pour le test d'indépendance d'une variable quantitative
calcul_t <- function(X,moy_theo){
  n <- nrow(X);
  moy <- apply(X,2,mean);
  h <- moy-moy_theo;
  
  e <- apply(X,2,sd);
  r <- sqrt(n);
  b <- e/r;
  
  hh <- abs(h);
  t <- hh/b;
  t;
}

t <- calcul_t(xx,19)
#print (t)


# ---------- Question 6 ----------

# Hypothèse nulle : Il n'y a pas de dépendance significative entre Marseille et Aix-en-Provence
# degré de liberté = 15 + 15 - 2 = 28 / Intervalle de confiance a 95% (alpha = 5%)
# Valeur critique v = 2.048
# Si la valeur absolue de t (|t|) est supérieure à la valeur critique,
# alors la différence est significative. Dans le cas contraire, elle, ne l’est pas.
# Ici, t = 2.321 est supérieur a la valeur critique, la différence est significative.
# L'hypothèse nulle est rejetée : Il y a une dépendance significative entre Marseille et Aix-en-Provence.


# On refait le test avec alpha = 2%.
# Cette fois la valeur critique v = 2.468
# Ici, t = 2.321 est inférieur a la valeur critique, la différence n'est pas significative.
# L'hypothèse nulle est acceptée : Il n'y a pas de dépendance significative entre Marseille et Aix-en-Provence

xx2 <- data.frame(Database2[1],Database2[2])

# Fonction qui calcule le score t pour le test d'indépendance de 2 variables quantitatives
calcul_t2 <- function(X){
  n <- nrow(X);
  moy <- apply(X,2,mean);
  h <- moy[1]-moy[2];
  hh <- abs(h);
  
  s <- apply(X,2,sd);
  ss1 <- s[1]^2;
  ss2 <- s[2]^2;
  
  b1 <- ss1/n;
  b2 <- ss2/n;
  sum <- b1+b2;
  bb <- sqrt(sum);
  
  res <- hh/bb;
  res;
  
}

t2 <- calcul_t2(xx2)
#print (t2)


# ---------- Question 7a ----------

ratio <- data.frame(VioletLong = c(9,1528), VioletRond = c(3,106), RougeLong = c(3,117), RougeRond = c(1,381))

# Fonction qui calcule les valeurs théoriques selon la formule : (ratio / somme_ratios) * n
valeur_theo7 <- function(r){
  a<-1:4;
  res<-numeric(length(a));
  sum <- apply(r,1,sum);
  somme_ratios <- sum[1];
  for (i in 1:4) {
    h <- r[1,i]/somme_ratios;
    res[i] <- h*r[2,i];
  }
  res <- data.frame(VioletLong = res[1], VioletRond = res[2], RougeLong = res[3], RougeRond = res[4])
  #print(res);
}

val_theo <- valeur_theo7(ratio)
#print(val_theo)

# ---------- Question 7b ----------

# Fonction du Khi-Deux
khi_deux <- function(r,v){
  a<-1:4;
  rr<-numeric(length(a));
  for (i in 1:4) {
    h <- r[1,i]-v[i];
    hh <- h^2;
    rr[i] <- hh/v[i];
  }
  sum(unlist(rr));
}

k2 <- khi_deux(ratio,val_theo)
#print(k2)


# ---------- Question 7c ----------

# La fonction du Khi-Deux nous donne un résultat égal à 894.1243.
# H0 : Le vrai ratio est 9:3:3:1
# Avec alpha = 0.05 et un degré de liberté = k - 1 = 3, la valeur critique est : 7.81
# Le coefficient que nous avons calculé est nettement supérieur.
# H0 est rejetée : 9:3:3:1 n'est pas le vrai ratio.


# ---------- Question 8 ----------

melform <- data.frame(FormAbs = c(29,40,18), FormAtyp = c(5,32,22), FormTyp = c(46,8,0))
melcolor <- data.frame(ColorAbs= c(20,29,12), ColorPres= c(60,51,28))

# Hypothèse nulle : Deux variables sont indépendantes et alpha = 5%

# Cas Diagnostic-Forme (3 classes différentes)
# Degré de liberté : k-1 = 2 --> Valeur critique : 5.99
# Valeur khi-deux : 75.1564
# La valeur du khi-deux est supérieure à la valeur critique
# Donc on rejette l'hypothèse nulle

# Cas Diagnostic-Couleur (2 classes différentes)
# Degré de liberté : k-1 = 1 --> Valeur critique : 2.71
# Valeur khi-deux : 2.39415
# La valeur du khi-deux est inférieure à la valeur critique
# Donc on accepte l'hypothèse nulle 

# Fonction qui calcule les valeurs théoriques selon la formule :
# (somme_col * somme_lignes) / n (pour chaque valeur du tableau)
valeur_theo8 <- function(x){
  tot <- sum(x);
  lignes <- nrow(x);
  col <- ncol(x);
  res<-matrix(rep(0,lignes*col),lignes,col);
  
  for(i in 1:lignes){
    for(j in 1:col){
      res[i,j] <- (sum(x[i,])*sum(x[,j])) / tot;
    }
  }
  res;
}

val_theoform <- valeur_theo8(melform)
val_theocolor <- valeur_theo8(melcolor)
#print(val_theoform)
#print(val_theocolor)

# Fonction du khi-deux pour une matrice
khi_deux2 <- function(r,v){
  lignes <- nrow(r);
  col <- ncol(r);
  res<-matrix(rep(0,lignes*col),lignes,col);
  
  for (i in 1:lignes){
    for (j in 1:col){
      h <- r[i,j]-v[i,j];
      hh <- h^2;
      res[i,j] <- hh/v[i,j];
    }
  }
    

  sum(unlist(res));
}

# Comparaison des 2 cas
k2_form <- khi_deux2(melform,val_theoform)
k2_color <- khi_deux2(melcolor,val_theocolor)
print(k2_form)
print(k2_color)


# ---------- Question 9 ----------

# Les tests paramétriques se basent sur des distributions statistiques supposées
# dans les données. Par conséquent, certaines conditions de validité doivent être
# vérifiées pour que le résultat d’un test paramétrique soit fiable.
# Par exemple, le test t de Student pour échantillons indépendants n’est fiable que
# si les données associées à chaque échantillon suivent une distribution normale et
# si les variances des échantillons sont homogènes.

# Les tests paramétriques sont, eux, plus puissants en général que leurs équivalents
# non-paramétriques. En d’autres termes, un test paramétrique sera plus apte à aboutir
# à un rejet de H0, si ce rejet est justifié.
# La plupart du temps, la p-value calculée par un test paramétrique sera inférieure à
# la p-value calculée par un équivalent non-paramétrique exécuté sur les mêmes données.

# Les tests non-paramétriques (comme le test du Lhi-Deux) ne se basent pas sur des
# distributions statistiques.
# Ils peuvent donc être utilisés même si les conditions de validité des tests
# paramétriques ne sont pas vérifiées.
# Les tests non-paramétriques sont plus robustes que les tests paramétriques.


# ---------- Question 10 ----------

# Les coefficients de Pearson et Spearman ne peuvent s'ppliquer que dans le cas de
# variables quantitatives.
