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
quicksort <- function(vetor){
  comparacoes <- 0
  movimentacoes <- 0
  if(length(vetor) <= 1){
    return(vetor)
  }
    pivo <- vetor[1] #toma como pivô o elemento na 1a posição
    particao <- vetor[-1] #particiona no último elemento (pos -1) do vetor
    
    vetor_esquerda <- particao[particao < pivo]
    comparacoes <- comparacoes + 1
    vetor_direita <- particao[particao >= pivo]
    comparacoes <- comparacoes + 1
    
    vetor_esquerda <- quicksort(vetor_esquerda)
    vetor_direita <- quicksort(vetor_direita)
    movimentacoes <- movimentacoes + 2
    
    
    return(c(vetor_esquerda, pivo, vetor_direita))
  
}