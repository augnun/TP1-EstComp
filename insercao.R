#' Title insercao
#'
#' @param vetor de dados unidimensional
#'
#' @return Para n > 1: Lista com data frame tab.resumo com o C(n) e M(n) para o vetor selecionado
#'                     e o head (10 primeiras posições) do vetor ordenado
#'         Para n = 1: o próprio vetor
#' @export
#'
#' @examples > insercao(c("x","j","j","n","m","k","a"))
#'            [[1]]
#'                                          resultado
#'            Número de Comparações: C(n)          14
#'            Número de movimentações: M(n)        28
#'
#'            [[2]]
#'            [1] "a" "j" "j" "k" "m" "n"
#'           > insercao("x")
#'            [1] "x"
insercao <- function(vetor){
  tamanho = length(vetor)
  comparacoes = 0 
  movimentacoes = 0 
  
  if(length(vetor) > 1){
    for(j in 2:length(vetor)){
      pivo <- vetor[j]
      i <- j-1
      while(i>0 && vetor[i] > pivo){
        vetor[i+1] <- vetor[i]
        i<-i-1
        comparacoes <- comparacoes + 1
        movimentacoes <- movimentacoes + 2
      }
        vetor[i+1] <- pivo
        
    }
    tab.resumo <- data.frame(resultado = c(comparacoes,movimentacoes),
                             row.names = c("Número de Comparações: C(n)", 
                                           "Número de movimentações: M(n)"))
    return(list(tab.resumo, head(vetor)))
    }
  

  else{
    return(vetor)
  }
  
}