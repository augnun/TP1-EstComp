#' Title quicksort
#'
#' @param vetor: um vetor de dados unidimensional
#' @param esquerda: índice à esquerda a partir do qual deseja-se aplicar a ordenação
#' @param direita índice à direita até onde deseja-se aplicar a ordenação
#'
#' @return Para n > 1: Lista com data frame tab.resumo com o C(n) e M(n) para o vetor selecionado
#'                     e o head (10 primeiras posições) do vetor ordenado
#'         Para n = 1: o próprio vetor
#' @export
#'
#' @examples
quicksort <- function(vetor, indice_esquerda = 1, indice_direita = length(vetor)){
  if(indice_esquerda < indice_direita){
    indice_pivo <- particionar(vetor,indice_esquerda, indice_direita)
    quicksort(vetor, indice_esquerda, indice_pivo-1)
    quicksort(vetor, indice_pivo + 1, indice_direita)
  }
  return(vetor)
}

particionar <- function(vetor, indice_esquerda, indice_direita){
  buffer <- 0 
  superior <- vetor[indice_direita]
  i = indice_esquerda - 1
  for(j in indice_esquerda:(indice_direita-1)){
    if(vetor[j]<=superior){
      i <- i+1
      vetor[i] <- buffer
      vetor[i] <- vetor[j]
      vetor[j] <- buffer
    }
  }
  vetor[i+1] <- buffer
  vetor[i+1] <- vetor[indice_direita]
  vetor[indice_direita] <- vetor[i+1]
  return(i+1)
}