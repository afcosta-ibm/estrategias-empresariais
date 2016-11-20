#' Lista no. 1 – Qual produto produzir
#' 
#' Grupo 1: Alexandre, Eduardo e Marcelo
#' 
#' Produto | Preço/unidade | Custo/unidade | Contribuição/unidade
#' A | 2.5 | 1.5 | 1
#' B | 6 | 4 | 2
#' C | 3.75 | 2.25 | 1.5
#'
#' Vendas | Probabilidade
#' Produto A:
#' 2000 | 0.1
#' 3000 | 0.1
#' 4000 | 0.2
#' 5000 | 0.6
#' 
#' Produto B:
#'    0 | 0.1
#' 1000 | 0.2
#' 2000 | 0.2
#' 3000 | 0.4
#' 4000 | 0.1
#'
#' Produto C:
#'    0 | 0.1
#' 1000 | 0.3
#' 2000 | 0.3
#' 3000 | 0.2
#' 4000 | 0.1

lista1 <- function(){
  # sorteia os valores do produto A
  produtoA <- sample( 
    # multiplica o nro de vendas pelo lucro (contribuicao) do produto A 
    c(2000 * 1, 3000 * 1, 4000 * 1, 5000 * 1),
    size=3000,
    replace=TRUE,
    # probabilidades
    prob=c(0.1, 0.1, 0.2, 0.6)
  )

  # sorteia os valores do produto B
  produtoB <- sample(
    # multiplica o nro de vendas pelo lucro (contribuicao) do produto B
    c(0 * 2, 1000 * 2, 2000 * 2, 3000 * 2, 4000 * 2), 
    size=3000, 
    replace=TRUE, 
    # probabilidades
    prob=c(0.1, 0.2, 0.2, 0.4, 0.1)
  )
  
  # sorteia os valores do produto C
  produtoC <- sample(
    # multiplica o nro de vendas pelo lucro (contribuicao) do produto C
    c(0 * 1.5, 1000 * 1.5, 2000 * 1.5, 3000 * 1.5, 4000 * 1.5), 
    size=3000,
    replace=TRUE, 
    # probabilidades
    prob=c(0.1, 0.3, 0.3, 0.2, 0.1)
  )
  
  print(produtoA)
  print(produtoB)
  print(produtoC)
  
  hist(
    x=produtoA,
    main="Produto A", col = "green",
    xlab = "Lucro Gerado Produto A", ylab = "Quantidade",
    xlim = c(0, 10000), ylim = c(0, 2000)
  )

  hist(
    x=produtoB,
    main="Produto B", col = "blue",
    xlab = "Lucro Gerado Produto B", ylab = "Quantidade",
    xlim = c(0, 10000), ylim = c(0, 2000)
  )

  hist(
    x=produtoC,
    main="Produto C", col = "yellow",
    xlab = "Lucro Gerado Produto C", ylab = "Quantidade",
    xlim = c(0, 10000), ylim = c(0, 2000)
  )
  
  cumulativaProdutoA <- ecdf(produtoA)
  cumulativaProdutoB <- ecdf(produtoB)
  cumulativaProdutoC <- ecdf(produtoC)
  
  legenda <- "Produto A - Verde, Produto B - Azul, Produto C - Amarelo"

  plot(
    main = "Cumulativas dos Produtos A, B e C",
    produtoA, type="l", 
    xlim=c(0, 10000), ylim=c(0, 1), 
    xlab=legenda, ylab="Probabilidades"
  )
  
  lines(cumulativaProdutoA, col = "green")
  lines(cumulativaProdutoB, col = "blue")
  lines(cumulativaProdutoC, col = "yellow")
  
  print("lista 1 entregue")
}