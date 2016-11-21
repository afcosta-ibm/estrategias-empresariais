lucrosProdutoA <- c(2000 * 1, 3000 * 1, 4000 * 1, 5000 * 1)
lucrosProdutoB <- c(0 * 2, 1000 * 2, 2000 * 2, 3000 * 2, 4000 * 2)
lucrosProdutoC <- c(0 * 1.5, 1000 * 1.5, 2000 * 1.5, 3000 * 1.5, 4000 * 1.5)

probabilidadeA <- c(0.1, 0.1, 0.2, 0.6)
probabilidadeB <- c(0.1, 0.2, 0.2, 0.4, 0.1) 
probabilidadeC <- c(0.1, 0.3, 0.3, 0.2, 0.1)

acumuladaA = ecdf(lucrosProdutoA)
acumuladaB = ecdf(lucrosProdutoB)
acumuladaC = ecdf(lucrosProdutoC)

plot(
  x = lucrosProdutoA, y = probabilidadeA,
  xlim = range(c(0,10000)), ylim = c(0, 1),
  col = "blue", type = "l",
  xlab = "Lucro dos Produtos", ylab = "Probabilidades",
  main = "Análise de Produção"
)
points(x = lucrosProdutoB, y = probabilidadeB, col = "red",  type = "l")
points(x = lucrosProdutoC, y = probabilidadeC, col = "green",  type = "l")
text(x = 5000, y = 0.65, labels = "A", col = "blue")
text(x = 8000, y = 0.15, labels = "B", col = "red")
text(x = 6000, y = 0.15, labels = "C", col = "green")

plot(
  x = 1, y = 1, type="l",
  xlim = c(0,10000), ylim=c(0, 1),
  pch=21, bg="blue",
  xlab="Lucro dos Produtos", ylab="Probabilidades",
  main="Análise de Produção")

lines(acumuladaA, col = "blue")
lines(acumuladaB, col = "red")
lines(acumuladaC, col = "green")