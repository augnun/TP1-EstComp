comparacoes <- 0
movimentacoes <- 0

#' Title quicksort
#'
#' @param vetor vetor a ser ordenado
#' @param esquerda primeiro índice a partir do qual desejamos ordenar (default = 1)
#' @param direita último índice até onde deseja-se ordenar (default = comprimento do vetor)
#'
#' @return Para n > 1: Lista com data frame tab.resumo com o C(n) e M(n) para o vetor selecionado
#'                     e o head (10 primeiras posições) do vetor ordenado
#'         Para n = 1: o próprio vetor
#' @export
#'
#' @examples> quicksort(c(2,3,1))
#'            [[1]]
#'            resultado
#'            Número de Comparações: C(n)           1
#'            Número de movimentações: M(n)         6
#'            [[2]]
#'            [1] 1 2 3
quicksort <- function(vetor, esquerda = 1, direita = length(vetor)){
  if(length(vetor) > 1){

  vetor <- quicksort.helper(vetor, esquerda, direita)
  tab.resumo <- data.frame(resultado = c(comparacoes,movimentacoes),
                           row.names = c("Número de Comparações: C(n)", 
                                         "Número de movimentações: M(n)"))
  return(list(tab.resumo, head(vetor)))
  }
  else{
    return(vetor)
    }
}

#' Title quicksort.helper helper da função quicksort acima. Notar que esta função é recursiva
#'
#' @param vetor vetor a ser ordenado
#' @param esquerda primeiro índice a partir do qual desejamos ordenar (default = 1)
#' @param direita último índice até onde deseja-se ordenar (default = comprimento do vetor)
#'
#' @return vetor ordenado
#' @export quicksort()
#'
#' @examples
quicksort.helper <- function(vetor,esquerda=1,direita=length(vetor)){
  pivo = vetor[floor(esquerda+direita)/2]
  i=esquerda
  j=direita
  while(i <= j){
    while(vetor[i]<pivo){
      i = i + 1
      comparacoes <<- comparacoes + 1
    }
    while(vetor[j]>pivo){
      j = j - 1
      comparacoes <<- comparacoes + 1
    }
    if( i <= j){
      buffer <- vetor[i]
      vetor[i] <- vetor[j]
      vetor[j] <- buffer
      i = i+1
      j = j-1
      movimentacoes <<- movimentacoes + 3
    }
  }
if(esquerda < j){
  vetor <- qs2(vetor,esquerda, j)
}
if(i < direita){
  vetor <- qs2(vetor,i,direita)
}
  
  return(vetor)
}
