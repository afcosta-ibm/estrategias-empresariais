# Exemplo de Dominancia Estocastica

x1 <- 1:3
y1 <- c(1/6, 2/6, 3/6)
x2 <- c(0.5, 1.5)
y2 <- c(0.5, 0.5)
p = ecdf(x1)
p2 = ecdf(x2)
plot(x1,y1, xlim = range(c(0,5)), ylim = c(0, 1), col = "blue", xlab = "Ganhos", ylab = "Probabilidades", main = "Exemplo Dominância estocástica")
points(x2,y2, col = "red")
lines(p2, col = "red")
lines(p, col = "blue")

#Exercicio do Eber

ax = c(2000, 3000, 4000, 5000)
ay = c(0.1, 0.1, 0.2, 0.6)

bx = c(0, 1000, 2000, 3000, 4000)
by = c(0.1, 0.2, 0.2, 0.4, 0.1)

cx = c(0, 1000, 2000, 3000, 4000)
cy = c(0.1, 0.3, 0.3, 0.2, 0.1)

contribuicao = c(1, 2, 1.5)
names(contribuicao)[1] <- "A"
names(contribuicao)[2] <- "B"
names(contribuicao)[3] <- "C"

lucrosAx <- ax * contribuicao["A"]
lucrosBx <- bx * contribuicao["B"]
lucrosCx <- cx * contribuicao["C"]

pA = ecdf(lucrosAx)
pB = ecdf(lucrosBx)
pC = ecdf(lucrosCx)
plot(ax,ay, xlim = range(c(-1000,7000)), ylim=c(0, 1), pch=21, bg="blue", xlab="Lucros", ylab="Probabilidades", main="Exercicio de Dominância estocástica")
points(bx,by, pch=21, bg="red")
points(cx,cy, pch=21, bg="green4")


plot(5, 5, type="n", xlim = range(c(-1000,7000)), ylim=c(0, 1), pch=21, bg="blue", xlab="Lucros", ylab="Probabilidades", main="Exercicio de Dominância estocástica")
lines(pA, col = "red")
lines(pB, col = "blue")
lines(pC, col = "green4")

