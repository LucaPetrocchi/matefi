
library(dplyr)
library(ggplot2)

db = read.csv('.\\QTAXTOTALQTAXCAT1USNO.csv')

colnames(db) = c('Fecha', 'Impuestos')
db = transform(db, Impuestos = as.numeric(Impuestos))
db = transform(db, Fecha = as.Date(Fecha))
db = na.omit(db)
Fecha = db$Fecha
Impuestos = db$Impuestos

ggplot(as.data.frame(Fecha), aes(x = Fecha)) + 
  geom_line(aes(y = Impuestos, color = "Recaudaciones (1M USD)")) + 
  scale_y_continuous(
    labels = scales::comma,
    limits = c(100000, 700000), 
    breaks = seq(100000, 1000000, 100000), 
    expand = c(0, 0)
  ) +
  labs(x = "Anio",
       y = "Valor",
       color = "Leyenda")

cor(Impuestos, as.numeric(Fecha))

modelo = lm(Impuestos ~ as.numeric(Fecha))
summary(modelo)

p = predict(modelo, as.data.frame(Fecha))

ggplot(as.data.frame(Fecha), aes(x = Fecha)) + 
  geom_line(aes(y = Impuestos, color = "Impuestos (1M USD)")) + 
  geom_line(aes(y = p, color = "Predicción")) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(100000, 700000), 
    breaks = seq(100000, 1000000, 100000), 
    expand = c(0, 0)
  ) +
  labs(x = "Anio",
       y = "Valor",
       color = "Leyenda")

dbAjuste = read.csv('.\\QTAXTOTALQTAXCAT1USYES.csv')
colnames(dbAjuste) = c('Fecha', 'Impuestos')
dbAjuste = transform(dbAjuste, Impuestos = as.numeric(Impuestos))
dbAjuste = transform(dbAjuste, Fecha = as.Date(Fecha))
dbAjuste = na.omit(dbAjuste)
FechaAjuste = dbAjuste$Fecha
ImpuestosAjuste = dbAjuste$Impuestos

modeloAjuste = lm(ImpuestosAjuste ~ FechaAjuste, data = dbAjuste)
summary(modeloAjuste)

pAjuste = predict(modeloAjuste, as.data.frame(FechaAjuste))

ggplot(as.data.frame(FechaAjuste), aes(x = FechaAjuste)) + 
  geom_line(aes(y = ImpuestosAjuste, color = "Impuestos Ajustados (1M USD)")) + 
  geom_line(aes(y = pAjuste, color = "Predicción Ajustada")) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(100000, 700000), 
    breaks = seq(100000, 1000000, 100000), 
    expand = c(0, 0)
  ) +
  labs(x = "Anio",
       y = "Valor",
       color = "Leyenda")

segmentoImpuestos = db[c(85:126), 2]
segmentoFechas = db[c(85:126), 1]
modeloSesgado = lm(segmentoImpuestos ~ as.numeric(segmentoFechas))
summary(modeloSesgado)

pSesgada = predict(modeloSesgado, segmentoFechas)

ggplot(as.data.frame(segmentoFechas), aes(x = segmentoFechas)) + 
  geom_line(aes(y = segmentoImpuestos, color = "Impuestos (1M USD)")) + 
  geom_line(aes(y = p[85:126], color = "Modelo Original")) +
  geom_line(aes(y = pSesgada, color = "Modelo Sesgado")) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(100000, 700000), 
    breaks = seq(100000, 1000000, 100000), 
    expand = c(0, 0)
  ) +
  labs(x = "Anio",
       y = "Valor",
       color = "Leyenda")

iAjuste = dbAjuste[c(17:58), 2]
fAjuste = as.numeric(dbAjuste[c(17:58), 1])
ajuste = data.frame(ImpuestosAjuste=unlist(iAjuste), FechasAjuste=fAjuste)

modeloSesgadoAjustado = lm(ImpuestosAjuste ~ FechasAjuste, data=ajuste)
summary(modeloSesgadoAjustado)

pAjusteSesgada = predict(
  modeloSesgadoAjustado,
  newdata=(FechasAjuste=ajuste)
)

segmentoFechasAjuste = ajuste$FechasAjuste

ggplot(as.data.frame(segmentoFechasAjuste), aes(x = segmentoFechasAjuste)) + 
  geom_line(aes(y = ImpuestosAjuste[17:58], color = "Impuestos Ajustados (1M USD)")) + 
  geom_line(aes(y = pAjuste[17:58], color = "Predicción Ajustada")) +
  geom_line(aes(y = pAjusteSesgada, color = "Predicción Sesgada Ajustada")) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(100000, 700000), 
    breaks = seq(100000, 1000000, 100000), 
    expand = c(0, 0)
  ) +
  labs(x = "Anio",
       y = "Valor",
       color = "Leyenda")


FechaPrediccion = seq(
  as.Date(max(fAjuste)),
  by="3 month",
  length = 42,
)

ajusteFinal = predict(modeloAjuste, newdata = (FechaAjuste = FechaPrediccion))

ggplot(as.data.frame(FechaPrediccion), aes(x = as.Date(FechaPrediccion))) + 
  geom_line(aes(y = ajusteFinal, color = "Predicción Ajustada")) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(400000, 700000), 
    breaks = seq(100000, 1000000, 100000), 
    expand = c(0, 0)
  ) +
  labs(x = "Anio",
       y = "Valor",
       color = "Leyenda")


