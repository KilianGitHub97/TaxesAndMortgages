##### Main #####
################

# Setup -------------------------------------------------------------------

#setwd to current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#source other R-files needed
source("packages.R")


#Data
mortgage<-fread("..//data//mortgage.csv")
Switzerland_Population<-fread("..//data//Switzerland_Population.csv")
Taxes_Cantons<-fread("..//data//Taxes_Cantons.csv")

# Task 1 ------------------------------------------------------------------

#Scale dataset & make first column to rownames
Switzerland_Population<-Switzerland_Population %>%
  rename(rowname = V1) %>%
  column_to_rownames() %>%
  scale(center = TRUE,
        scale = TRUE)

###########################################################################
# Based on the Switzerland_Population.csv dataset, which other canton is
# most similar to BS in terms of ...
# a) age (3 features: Age0.19, Age20.64, Age64Plus)
###########################################################################

#select needed columns
SPage<-Switzerland_Population[,c("Age0-19", "Age20-64", "Age64plus")]

#Are there NAs
table(
  sapply(SPage, is.na)
)

#distance matrix
Dage<-dist(SPage,
           method = "euclidian")

#hierarchical clustering
ag_fit<-agnes(Dage,
              method = "average")

#plot dendrogram
plot(as.dendrogram(ag_fit)) #TI
plot(ag_fit, main = "Dendrogram for Age")

#Full distance matrix
Dage<-as.matrix(Dage)

#Which canton has the smallest euclidean distance to BS?
which.min(Dage["BS",-5])#TI

#what is the actual distance?
min(Dage["BS",-5])

#remove unnecessary objects
rm(ag_fit, Dage, SPage)

###########################################################################
# b) religion (3 features: RomCath, Protestant, noConfession;
# other confessions = gap to 100%)
###########################################################################

#select needed columns
SPrel<-Switzerland_Population[, c("RomCath", "Protestant", "noConfession")]

#Distance matrix
Drel<-dist(SPrel,
           method = "euclidean")

#agglomerative nesting
ag_fit<-agnes(Drel,
              method = "average")

#plot the dendrogram
plot(as.dendrogram(ag_fit), main = "Dendrogram for Religion")
plot(ag_fit, main = "Dendrogram for Religion")


#Full distance matrix
Drel<-as.matrix(Drel)

#What is the closest Distance to BS
min(Drel["BS",-5])

#Which canton is it?
which.min(Drel["BS",-5])

#delete unnecessary objects
rm(Drel, ag_fit, SPrel)

###########################################################################
# c) political parties (11 features: FDP, ..., OtherParties)
###########################################################################

#select needed columns
SPpar<-Switzerland_Population[,16:26]

Dpar<-dist(SPpar,
           method = "euclidean")

#agglomerative nesting
ag_fit<-agnes(Dpar,
              method = "average")

#plot the dendrogram
plot(as.dendrogram(ag_fit))
plot(ag_fit, main = "Dendrogram for political party")

#Full distance matrix
Dpar<-as.matrix(Dpar)

#What is the closest distance to BS
min(Dpar["BS",-5])

#Which canton is it?
which.min(Dpar["BS",-5])

#delete unnecessary objects
rm(Dpar, ag_fit, SPpar)


# Task 2 ------------------------------------------------------------------

###########################################################################
# Perform a hierarchical cluster analysis on the Taxes_Cantons.csv dataset.
# Choose one method you consider most appropriate for this task. Pay special
# attention to pre-processing of data, the choice of distance measure; and
# the linkage method and argue your decisions.
###########################################################################

#shorten name of dataset & make data.frame
X<-Taxes_Cantons
X<-setDF(X)

#inspection
head(X)
names(X)
dim(X)

#NAs
table(X==0)

#Missing value; print columns with at least 1 missing value + number of NAs
for (i in 2:length(X)) {
  zeros<-table(X[,i]==0)
  if(zeros["FALSE"]<nrow(X)){
    print(paste0("*",
                 names(X)[i],
                 "*",
                 " has ",
                 as.double(zeros["TRUE"]),
                 " missing values"))
    }
}

#Convert 0 to NA
X[X==0]<-NA

#did NA transformation work? If yes, there must be 17 TRUEs
table(is.na(X))

#### MICE imputation

# plot NA values
aggr(X,
     numbers = TRUE,
     sortVars = TRUE)

#save random entries from the columns with missing values
twoland<-X[2, "land"]
oneinheritance<-X[1, "inheritence"]
oneprop_trans<-X[1, "property_transfer"]

#make these entries NA
X[2, "land"]<-NA
X[1, "inheritence"]<-NA
X[1, "property_transfer"]<-NA

#Multiple imputation by chained equations
set.seed(2020)
imputed_X<-mice(X,
                m = 5,
                method = "pmm")
summary(imputed_X)

#subtract the predicted value from the actual value and check for the absolute minimum
which.min(abs(twoland - imputed_X$imp$land[2,]))
which.min(abs(oneinheritance - imputed_X$imp$inheritence[1,]))
which.min(abs(oneprop_trans - imputed_X$imp$property_transfer[1,]))

#insert the predicted values over the NAs
Xi<-complete(imputed_X, 1)

#put the original values back in place
Xi[2, "land"]<-twoland
Xi[1, "inheritence"]<-oneinheritance
Xi[1, "property_transfer"]<-oneprop_trans

#drop the land columns since there were to many NAs initially and scale
Xi<-Xi %>%
  select(-land) %>%
  rename(rowname = Canton) %>%
  column_to_rownames() %>%
  scale(center = TRUE,
        scale = TRUE)

#check for outliers
boxplot(Xi)

#make distance matrix with the euclidean distance
DXi<-dist(Xi,
          method = "euclidean")

#agglomerative nesting
ag_fit<-agnes(DXi,
              method = "average")

#plot the dendrogram
plot(ag_fit, main = "Similarity of all 26 Cantons Regarding Taxes")

#delete unnecessary objects
rm(ag_fit, imputed_X, X, Xi, DXi, i, 
   oneinheritance, oneprop_trans, twoland, zeros)

# Task 3 ------------------------------------------------------------------

###########################################################################
# The dataset mortgage.csv provides transformed and anonymised data of a
# subsample of a bank's mortgage customers.
# How many different types of customers are there? And how can these types
# be characterized? Again, choose one (at most two) method(s) you consider
# most appropriate for this task, and describe your reasoning.
###########################################################################

#shorten name of data
X<-as.data.frame(scale(mortgage))

#inspection
names(X)
summary(X)
dim(X)

#NAs
table(is.na(X))

#plot data
plot(X)
boxplot(X)

#PCA and biplot
biplot(prcomp(X))

#MDS and plot
X_mds<-cmdscale(dist(X))
plot(X_mds)

#Position Around Medoids
pam_fit<-pam(X, k = 3)
plot(pam_fit)
plot(X, col=pam_fit$clustering, main = "Position Around Medoids")
plot(X_mds,  col=pam_fit$clustering, main = "Position Around Medoids")

#Medoids: red = 2, green = 3, black = 1
pam_fit$medoids

#fuzzy clustering
fuzzy_fit<-fanny(X, k = 3)
plot(X, col = rgb(fuzzy_fit$membership))
plot(X_mds, col = rgb(fuzzy_fit$membership))

#delete everything
rm(list=ls())