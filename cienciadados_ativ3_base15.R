base_15 <- floor(runif(15,min=0,max=100))
sort(base_15)

gera_media <- function(vetor) {
  media <- sum(vetor)/length(vetor)
  return (media)
  }

gera_mediana <- function(vetor) {
  index <- length(vetor)%/%2
  mediana <- 0
  if (length(vetor)%%2 == 0){
    mediana <- (vetor[index] + vetor[index - 1])/2
  } else {
      mediana <- vetor[index + 1]
  }
  return(mediana)
}

gera_variancia <- function(vetor){
  med = gera_media(vetor)
  soma <- 0
  aux <- 0
  for (i in vetor){
    aux <- (i - med)^2
    soma <- aux + soma
  variancia <- soma/length(vetor)
  }
  return(variancia)
}

gera_desvpad <- function(vetor){
  desvpad <- sqrt(gera_variancia(vetor))
  return(desvpad)
}

gera_coef <- function(vetor){
  coef <- gera_desvpad(vetor)/gera_media(vetor)*100
  return(coef)
}

gera_q1 <- function(vetor){
  index <- gera_mediana(vetor)
  q1 <- index %% 2
  if (length(vetor)%%2 == 0){
    q1 <- (vetor[index] + vetor[index - 1])/2
  } else {
    q1 <- vetor[index + 1]
}}

gera_q1 <- function(vetor){
    index <- len(vetor) - gera_mediana(vetor)
    q3 <- index %% 2
    if (length(vetor)%%2 == 0){
      q3 <- (vetor[index] + vetor[index - 1])/2
    } else {
      q3 <- vetor[index + 1]
    }}
    
media_15 <- gera_media(base_15)
mediana_15 <- gera_mediana(base_15)
variancia_15 <- gera_variancia(base_15)
desvpad_15 <- gera_desvpad(base_15)
coef_15 <- gera_coef(base_15)


avg <- mean(base_15)
mdn <- median(base_15)
vr <- var(base_15)
std <- sd(base_15)
cv <-  sd(base_15) / mean(base_15) * 100
qt <-quantile(base_15, probs = c(0,0.25,0.5,0.75,1))
hist(base_15)
boxplot(base_15)