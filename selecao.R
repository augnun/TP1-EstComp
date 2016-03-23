
#' Title seleção
#'
#' @param vetor de dados unidimensional
#'
#' @return Para n > 1: Lista com o data frame tab.resumo com o C(n) e M(n) para o vetor selecionado
#'         e o head (10 primeiras posições) do vetor ordenado
#'         Para n = 1: o próprio vetor
#' @export 
#'
#' @examples >selecao(c("x","j","j","n","m","k","a")) 
#'            [[1]]
#'                                          resultado
#'            Número de Comparações: C(n)          13
#'            Número de movimentações: M(n)        39
#'
#'            [[2]]
#'            [1] "a" "j" "j" "k" "m" "n"
#'            
#'           >selecao("x")
#'           [1] "x"
selecao <- function(vetor){
  tamanho = length(vetor)
  comparacoes = 0 
  movimentacoes = 0 
if(length(vetor) > 1){
  for(i in 1:(tamanho - 1)){
    for(j in i:tamanho){
      if(vetor[i] > vetor[j]){
        minimo <- vetor[j]
        vetor[j] = vetor[i]
        vetor[i] = minimo
        comparacoes <- comparacoes + 1
        movimentacoes <- movimentacoes + 3
      }
    }
  }
  
}
  else{
    return(vetor)
  }
  tab.resumo <- data.frame(resultado = c(comparacoes,movimentacoes),
                           row.names = c("Número de Comparações: C(n)", "Número de movimentações: M(n)"))
  return(list(tab.resumo, head(vetor)))}