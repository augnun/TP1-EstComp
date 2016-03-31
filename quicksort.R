#' Title quicksort
#'
#' @param vetor: um vetor de dados unidimensional
#' @param esquerda: índice à esquerda a partir do qual deseja-se aplicar a ordenação
#' @param direita: índice à direita até onde deseja-se aplicar a ordenação
#'
#' @return Para n > 1: Lista com data frame tab.resumo com o C(n) e M(n) para o vetor selecionado
#'                     e o head (10 primeiras posições) do vetor ordenado
#'         Para n = 1: o próprio vetor
#' @export
#'
#' @examples
#' 
comparacoes <- 0L
movimentacoes <- 0L
quicksort <- function(vetor,esq=1, dir=length(vetor),comparacoes=0, movimentacoes=0){
  if(length(vetor)<2){
    tab.resumo <- data.frame(resultado = c(comparacoes,movimentacoes),
                             row.names = c("Número de Comparações: C(n)", "Número de movimentações: M(n)"))
    return(list(tab.resumo))
    # return(vetor)
    
  } 
  pivo <- vetor[sample(length(vetor),1)]
  return(c(quicksort(vetor[vetor<pivo],comparacoes = comparacoes+1, movimentacoes = movimentacoes+1),vetor[vetor==pivo],quicksort(vetor[vetor>pivo],comparacoes = comparacoes+1,movimentacoes = movimentacoes+1)))
  
  
}