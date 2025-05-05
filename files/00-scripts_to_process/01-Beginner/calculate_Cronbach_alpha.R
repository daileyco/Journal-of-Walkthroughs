# Cronbach's Alpha calculation - manual
## Author = Cody Dailey, daileyco@uga.edu

## jargon / definitions
### scales == composite scores == aggregate score (e.g., sum) of values across a bunch of variables
### scale items == the individual variables used in the scale

### utility of a composite score
#### dimension reduction 

### when to use a composite score
#### dimension reduction is sought
#### scale items are consistent (aka, reliable)
##### consistent measures each characterize an alike latent variable
###### a latent variable is like a concept that cannot be directly measured (e.g., intelligence)
##### a measure of consistency is Cronbach's alpha
###### only appropriate when scale is unidimensional
####### a single dimension ~= a single latent variable
####### dimensionality assessed by factor analysis or principal component analysis (princomp())
######## essentially want to see scale items condensed similarly (e.g., factor loadings) 
######## and a big dropoff on explained variance after first factor / principal component





## Function to calculate Cronbach's alpha
### similar to psych::alpha(), but I created just to see inner workings and learn it myself

### Arguments 
#### a matrix of observations for scale items (can simply use as.matrix() on a dataframe)
##### matrix should be numeric values
#### a logical indicator whether to calculate the standardized form 
##### standardization needed when the scale items have different ranges
###### item 1 is 1/0 while item 2 is 1-5
####### won't see too drastic of difference unless wildly different scales, but important to understand that it can affect covariance and thus alpha calculations

### Calculation
#### k == number of items in the scale 
#### "relatedness" of items
##### uses either variance-covariance matrix or correlation matrix (standardized)
###### avg.cov == average inter-item covariance (avg.var == average variance also included) 
###### avg.rho == the average inter-item correlation 

### Return
#### a numeric value [0,1]


### Additional notes
#### more items == more reliability (given related) == higher Cronbach's alpha
#### other packages use ?bootstrapping? to calculate confidence intervals
#### useful resources
##### https://stats.idre.ucla.edu/spss/faq/what-does-cronbachs-alpha-mean/#:~:text=Cronbach's%20alpha%20is%20a%20measure,a%20measure%20of%20scale%20reliability.&text=As%20the%20average%20inter%2Ditem,the%20number%20of%20items%20constant).
##### to pursue later
###### dimensionality coefficient via PCA eigenvalues
####### https://www.rasch.org/rmt/rmt263c.htm



cronbachs.alpha <- function(my.matrix, standardized=F){
  k <- ncol(my.matrix)
  
  if(standardized){
    mat <- cor(my.matrix, use="pairwise.complete.obs")
    avg.rho <- mean(mat[lower.tri(mat)])
    ca <- (k*avg.rho) / (1+(k-1)*avg.rho)
  }else{
    mat <- cov(my.matrix, use="pairwise.complete.obs")
    avg.cov <- mean(mat[lower.tri(mat)])
    avg.var <- mean(diag(mat))
    ca <- (k*avg.cov) / (avg.var+(k-1)*avg.cov)
  }
  
  return(ca)
  
}