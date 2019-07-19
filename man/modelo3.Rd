\name{modelo3}
\alias{modelo3}
\docType{package}
\title{
Modelo 3
}
\description{
Alguns exemplos de parâmetros para explorar o modelo 3.
}
\examples{
Exploração 1: e se só houver uma população?
Neste caso temos duas equações atreladas, mas e se uma população estiver extinta?
Para verificar o que acontece, iremos mexer nas populações iniciais, enquanto man-
temos os outros parâmetros constantes (por exemplo, tmax=30, K₁=K₂=100, r₁=r₂=0.5,
α=1.2, β=0.8 e a caixa desmarcada). Comece colocando N₀₁=10 e N₀₂=0 e aperte apply.
Agora tente N₀₁=0 e N₀₂=10. Lembra algo?



Exploração 2: o que representam α e β?
Em resumo, estes parâmetros, juntamente com os Ks, definem quatro situações pos-
síveis previstas pelo modelo. Para simplificar e poder focar no α e no β, deixare-
mos k₁=K₂=100. Mantenha constantes também tmax=50 e r₁=r₂=0.5. Marque
a caixa que diz "Diagrama de fases" e comece a exploração.

Lembre-se: a trajetória termina no pontinho preto!


2.1 Situação A
Coloque α e β de forma que α>1 e β<1. Quaisquer valores que respeitem essas con-
ções servem! Tente alterar as populações iniciais para ver se faz diferença. E aí?


2.2 Situação B
Agora coloque α e β de forma que α<1 e β>1. Novamente tente alterar as populações
iniciais para ver se faz diferença. E aí?


2.3 Situação C
Agora coloque α e β de forma que α<1 e β<1. Comece com ambos iguais, por exemplo
α=β=0.8. Agora deixe eles desiguais com α>β, por exemplo α=0.9 e β=0.5. Agora in-
verta, α=0.5 e β=0.9. Este caso ajuda bastante a entender o que representam α e β!


2.4 Situação D
Agora coloque α e β de forma que α>1 e β>1. Este caso é mais delicado. Comece com
α=β=1.2 e N₀₁=20 e N₀₂=10. Agora mude apenas as condições iniciais para N₀₁=10 e
N₀₂=20. Esquisito né?


Desafio: se tiver sobrado um tempinho, tente entender o papel dos Ks. Olhe pra as
equações, manuseie seus valores nos gráficos e veja se faz algum sentido!



Estas explorações devem ser suficientes para capturar as principais característi-
cas do modelo, porém sinta-se à vontade para explorar o que mais quiser!
}
