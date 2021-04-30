# Investigacion: Política monetaria y el ciclo económico de Nicaragua (2006-2020)
# Autor: Marcos Tinoco


# Cargamos los paquetes y la base de datos.
#---------------------------------------------------------------------------

if (!require('readxl')) install.packages('readxl'); library(readxl)
if (!require('tseries')) install.packages('tseries'); library(tseries)
if (!require('zoo')) install.packages('zoo'); library(zoo)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('mFilter')) install.packages('mFilter'); library(mFilter)
if (!require('lmtest')) install.packages('lmtest'); library(lmtest)
if (!require('vars')) install.packages('vars'); library(vars)
if (!require('gridExtra')) install.packages('gridExtra'); library(gridExtra)
if (!require('tidyr')) install.packages('tidyr'); library(tidyr)
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('purrr')) install.packages('purrr'); library(purrr)
if (!require('RCurl')) install.packages('RCurl'); library(RCurl)
if (!require('funr')) install.packages('funr'); library(funr)


url_database <- "https://github.com/Mrtinoco/Pol-tica-monetaria-y-el-ciclo-econ-mico-de-Nicaragua-2006-2020-/blob/main/Base%20de%20datos_PM%20y%20el%20%20CE%20de%20NIC.xlsx?raw=true"

directory <- funr::get_script_path()

setwd(directory)   

download.file(url = url_database,destfile = "Base de datos_PM y el  CE de NIC.xlsx")

Base_de_datos_DI3 <- read_excel("Base de datos_PM y el  CE de NIC.xlsx", 
                                col_types = c("date", "numeric", "numeric", 
                                              "numeric", "numeric"))


# Preparamos los datos
#---------------------------------------------------------------------------

# PIB REAL
pib_real <- zooreg(log(Base_de_datos_DI3$PIB_Real), start = as.yearqtr("2006-1"), frequency = 4)

# INFLACION
inf <- zooreg(Base_de_datos_DI3$inf_tim, start = as.yearqtr("2006-1"), frequency = 4)

# TIPO DE CAMBIO REAL
tc_real <- zooreg(log(Base_de_datos_DI3$tc_real), start = as.yearqtr("2006-1"), frequency = 4)

# BASE MONETARIA
bm <- zooreg(log(Base_de_datos_DI3$bm), start = as.yearqtr("2006-1"), frequency = 4)

# Creamos el data frame
datos <- data.frame(pib_real,inf,tc_real,bm)
View(datos)


# Graficamos las series
#---------------------------------------------------------------------------

#PIB REAL
linea_pib_real <- ggplot(data=datos, aes(x=time(pib_real),y=pib_real, group=1)) +
  geom_line(color="blue")

#Inflacion
linea_inf <- ggplot(data=datos, aes(x=time(inf),y=inf, group=1)) +
  geom_line(color="orange")

#TC real
linea_tc_real <- ggplot(data=datos, aes(x=time(tc_real),y=tc_real, group=1)) +
  geom_line(color="purple")

#BM
linea_bm <- ggplot(data=datos, aes(x=time(bm),y=bm, group=1)) +
  geom_line(color="red")

# Graficamos cuatro graficos y exportamos
png("plot1.png",width = 866,height = 540)

grid.arrange(linea_pib_real+ labs(y = "PIB Real", x = "Tiempo (Trimestre)"),
             linea_bm+ labs(y = "Base Monetaria", x = "Tiempo (Trimestre)"),
             linea_inf+ labs(y = "Inflacion", x = "Tiempo (Trimestre)"),
             linea_tc_real+ labs(y = "Tipo de cambio real", x = "Tiempo (Trimestre)"),
             nrow=2 , ncol=2)

dev.off()


# Calculamos el componente ciclico de las series necesarias.
#---------------------------------------------------------------------------

# PIB REAL

c_pib_real <- hpfilter(datos$pib_real,type='lambda', freq=1600, drift=TRUE)
datos$c_pib_real <- c_pib_real$cycle

linea_c_pib_real <- ggplot(data=datos, aes(x=time(c_pib_real),y=c_pib_real, group=1)) +
  geom_line(color="blue")+
  geom_point()

# BM

c_bm <- hpfilter(datos$bm,type='lambda', freq=1600, drift=TRUE)
datos$c_bm <- c_bm$cycle

linea_c_bm <- ggplot(data=datos, aes(x=time(c_bm),y=c_bm, group=1)) +
  geom_line(color="blue")+
  geom_point()

# Tipo de cambio real

c_tc_real <- hpfilter(datos$tc_real,type='lambda', freq=1600, drift=TRUE)
datos$c_tc_real <- c_tc_real$cycle

linea_c_tc_real <- ggplot(data=datos, aes(x=time(c_tc_real),y=c_tc_real, group=1)) +
  geom_line(color="blue")+
  geom_point()


# Graficamos los Ciclos
#---------------------------------------------------------------------------

datos_copia = data.frame(datos$c_pib_real,datos$c_bm,datos$c_tc_real,time(inf))

names(datos_copia) <- c('Ciclo del PIB Real', 'Ciclo de la Base Monetaria','Ciclo del Tipo de cambio real','Fecha')

mdata <- gather(data = datos_copia, key = Ciclos, value = Valores,-Fecha)

rm(datos_copia)

fluctuacion_ciclos <- ggplot(data=mdata) + 
  geom_line(aes(x=Fecha,y=Valores, group=Ciclos,color = Ciclos)) 

png("plot2.png",width = 944,height = 540)

fluctuacion_ciclos

dev.off()


# Test de granger
#---------------------------------------------------------------------------

namevar <- function(v1) {
  deparse(substitute(v1))
}


grangertable <- function(series1,series2,name_s1,name_s2) {
  
  series1_name <- name_s1
  series2_name <- name_s2
  
  table_d <- matrix(c("","",""),ncol=3,byrow=TRUE)
  colnames(table_d) <- c("Hipotesis Nula","Rezagos","P-value")
  
  
  for(index in c(1,2,3,4,5,6)){
    ps1 <- grangertest(series1 ~ series2, order = index, data = datos)
    ps1 <- ps1$`Pr(>F)`
    ps2 <- grangertest(series2 ~ series1, order = index, data = datos)
    ps2 <- ps2$`Pr(>F)`
    name_first <- paste(series2_name,"-->",series1_name)
    name_second <- paste(series1_name,"-->",series2_name)
    table_d <- rbind(table_d,c(name_first,index,round(ps1[2],digits = 4)))
    table_d <- rbind(table_d,c(name_second,index,round(ps2[2],digits = 4)))
    table_d <- rbind(table_d,c("","",""))
  }
  
  return(table_d)
} 

# CPIB INF
gt_pib_inf <- grangertable(datos$c_pib_real,datos$inf,namevar(datos$c_pib_real),namevar(datos$inf))

# CPIB C_TC_REAL
gt_pib_tc_real <- grangertable(datos$c_pib_real,datos$c_tc_real,namevar(datos$c_pib_real),namevar(datos$c_tc_real))

# CPIB C_BM
gt_pib_bm <- grangertable(datos$c_pib_real,datos$c_bm,namevar(datos$c_pib_real),namevar(datos$c_bm))


# Raiz unitaria
#---------------------------------------------------------------------------

uroot_c_pib_real <- adf.test(datos$c_pib_real,"stationary",k=0)

uroot_c_bm <- adf.test(datos$c_bm,"stationary",k=0)

uroot_inf <- adf.test(datos$inf,"stationary",k=0)

uroot_c_tc_real <- adf.test(datos$c_tc_real,"stationary",k=8)

# Creamos la matriz a exportar

table_uroot <- table_uroot <- data.frame(Variable=character(0),Dickey.Fuller.Stat=numeric(0),P.value=numeric(0))

table_uroot[nrow(table_uroot)+1,] <- c("CPIBR",round(uroot_c_pib_real$statistic,digits = 4),uroot_c_pib_real$p.value)

table_uroot[nrow(table_uroot)+1,] <- c("CBM",round(uroot_c_bm$statistic,digits = 4),uroot_c_bm$p.value)

table_uroot[nrow(table_uroot)+1,] <- c("INF",round(uroot_inf$statistic, digits = 4),uroot_inf$p.value)

table_uroot[nrow(table_uroot)+1,] <- c("CTCR",round(uroot_c_tc_real$statistic,digits = 4),round(uroot_c_tc_real$p.value,digits = 2))

write.csv(table_uroot,file="tabler1.csv",row.names=FALSE)


# Calculamos modelo VAR
#---------------------------------------------------------------------------

datos$pib_real <- NULL
datos$tc_real <- NULL
datos$bm <- NULL
datos <- datos[,c(2,3,1,4)]

# Primer modelo VAR
var1 <- VAR(datos,p=2)


# Calculamos el numero de rezagos optimos mediante una prueba de razon de verosimilitud
#---------------------------------------------------------------------------

# Usamos el HQ IC

lag_len <- VARselect(datos,lag.max = 8)


sink(file = "laglen1.txt")
lag_len
sink(file = NULL)


# Resumen de la estimacion
#---------------------------------------------------------------------------

resumen_var1 <- summary(var1)

# Estabilidad del modelo VAR

estabilidad_roots <- resumen_var1$roots

sink(file = "estabilidad1.txt")
estabilidad_roots
sink(file = NULL)

# Residuos del modelo
residuos <- resid(var1)

# Prueba de normalidad

normalidad_var1 <- normality.test(var1, multivariate.only = TRUE)

sink(file = "normalidad_var1.txt")
normalidad_var1
sink(file = NULL)

# Autocorrelacion

autocorr_var1 <- serial.test(var1, lags.pt = 8, lags.bg = 8, type = "ES" )

sink(file = "autocorrelacion_var1.txt")
autocorr_var1
sink(file = NULL)


# Prueba de granger del modelo var
#---------------------------------------------------------------------------

granger_var1_cpibr <- causality(var1, cause = "c_pib_real", vcov.=NULL, boot=FALSE, boot.runs=100)

granger_var1_cpibr <- granger_var1_cpibr$Granger

sink(file = "grangertest1.txt")
granger_var1_cpibr
sink(file = NULL)

granger_var1_cbm <- causality(var1, cause = "c_bm", vcov.=NULL, boot=FALSE, boot.runs=100)

granger_var1_cbm <- granger_var1_cbm$Granger

granger_var1_inf <- causality(var1, cause = "inf", vcov.=NULL, boot=FALSE, boot.runs=100)

granger_var1_inf <- granger_var1_inf$Granger

granger_var1_tcr <- causality(var1, cause = "c_tc_real", vcov.=NULL, boot=FALSE, boot.runs=100)

granger_var1_tcr <- granger_var1_tcr$Granger


#  Descomposicion de Choleski 
#---------------------------------------------------------------------------

fir_var1_chol <- irf(var1, impulse = c("c_pib_real","c_bm","inf","c_tc_real"), response = "c_pib_real" , n.ahead = 10,
                     ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.95,
                     runs = 100, seed = NULL)


# Grafica

png("plotfirvar1cpr.png",width = 878,height = 328)

plot(fir_var1_chol, names = "c_pib_real")
on.exit(TRUE);

dev.off()

png("plotfirvar1cbm.png",width = 878,height = 328)

plot(fir_var1_chol, names = "c_bm")
on.exit(TRUE);

dev.off()


png("plotfirvar1cinf.png",width = 878,height = 328)

plot(fir_var1_chol, names = "inf")
on.exit(TRUE);

dev.off()

png("plotfirvar1ctr.png",width = 878,height = 328)

plot(fir_var1_chol, names = "c_tc_real")
on.exit(TRUE);

dev.off()


# Tabla para resumir la descomposicion de Varianza del VAR1
#---------------------------------------------------------------------------

chol_var1_pibr <- fir_var1_chol$irf$c_pib_real[1:4,1]

chol_var1_bm <- fir_var1_chol$irf$c_bm[1:4,1]

chol_var1_inf <- fir_var1_chol$irf$inf[1:4,1]

chol_var1_tcr <- fir_var1_chol$irf$c_tc_real[1:4,1]

table_chol_var1 <- data.frame(Periodo=numeric(0),CPIBR=numeric(0),CBM=numeric(0),INF=numeric(0),CTCR=numeric(0))

table_chol_var1[nrow(table_chol_var1)+1,] <- c(1,round(chol_var1_pibr[1],digits = 4),round(chol_var1_bm[1],digits = 4),round(chol_var1_inf[1],digits = 4),round(chol_var1_tcr[1],digits = 4))

table_chol_var1[nrow(table_chol_var1)+1,] <- c(2,round(chol_var1_pibr[2],digits = 4),round(chol_var1_bm[2],digits = 4),round(chol_var1_inf[2],digits = 4),round(chol_var1_tcr[2],digits = 4))

table_chol_var1[nrow(table_chol_var1)+1,] <- c(3,round(chol_var1_pibr[3],digits = 4),round(chol_var1_bm[3],digits = 4),round(chol_var1_inf[3],digits = 4),round(chol_var1_tcr[3],digits = 4))

table_chol_var1[nrow(table_chol_var1)+1,] <- c(4,round(chol_var1_pibr[4],digits = 4),round(chol_var1_bm[4],digits = 4),round(chol_var1_inf[4],digits = 4),round(chol_var1_tcr[4],digits = 4))

write.csv(table_chol_var1,file="tabler2.csv",row.names=FALSE)


# En este caso, los residuos no estan correlacionados

mcov_chol <- resumen_var1$corres


# Descomposicion de varianza
#---------------------------------------------------------------------------

fevd_var1 <- fevd(var1, n.ahead=12)


desv_var1 <- map_df(fevd_var1, ~as.data.frame(.x), .id="id") %>%
  mutate(Horizonte = rep(1:12, 4)) %>%
  pivot_longer(names_to = "Variable", cols = c(c_pib_real, c_bm,inf,c_tc_real))

png("plot3.png",width = 1920,height = 1080)

ggplot(data = desv_var1, mapping = aes(x = Horizonte, y = value, fill = Variable)) +
  facet_wrap(~id) +
  geom_bar(stat = "identity") +
  theme_bw()

dev.off()


# Estimacion VAR y Descomposicion de Choleski con el cambio en el orden de variables
#---------------------------------------------------------------------------

datos_var2 <- datos[,c(1,3,4,2)]

var2 <- VAR(datos_var2,p=2)

fir_var2_chol <- irf(var2, impulse = c("c_pib_real","c_bm","inf","c_tc_real"), response = "c_pib_real" , n.ahead = 10,
                     ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.95,
                     runs = 100, seed = NULL)


# Graficas

png("plotfirvar2cpr.png",width = 878,height = 328)

plot(fir_var2_chol, names = "c_pib_real")
on.exit(TRUE);

dev.off()

png("plotfirvar2cbm.png",width = 878,height = 328)

plot(fir_var2_chol, names = "c_bm")
on.exit(TRUE);

dev.off()


png("plotfirvar2cinf.png",width = 878,height = 328)

plot(fir_var2_chol, names = "inf")
on.exit(TRUE);

dev.off()

png("plotfirvar2ctr.png",width = 878,height = 328)

plot(fir_var2_chol, names = "c_tc_real")
on.exit(TRUE);

dev.off()


# Tabla para resumir la descomposicion de Varianza del VAR2

chol_var2_pibr <- fir_var2_chol$irf$c_pib_real[1:4,1]

chol_var2_inf <- fir_var2_chol$irf$inf[1:4,1]

chol_var2_tcr <- fir_var2_chol$irf$c_tc_real[1:4,1]

chol_var2_bm <- fir_var2_chol$irf$c_bm[1:4,1]


table_chol_var2 <- data.frame(Periodo=numeric(0),CPIBR=numeric(0),INF=numeric(0),CTCR=numeric(0),CBM=numeric(0))

table_chol_var2[nrow(table_chol_var2)+1,] <- c(1,round(chol_var2_pibr[1],digits = 4),round(chol_var2_inf[1],digits = 4),round(chol_var2_tcr[1],digits = 4),round(chol_var2_bm[1],digits = 4))

table_chol_var2[nrow(table_chol_var2)+1,] <- c(2,round(chol_var2_pibr[2],digits = 4),round(chol_var2_inf[2],digits = 4),round(chol_var2_tcr[2],digits = 4),round(chol_var2_bm[2],digits = 4))

table_chol_var2[nrow(table_chol_var2)+1,] <- c(3,round(chol_var2_pibr[3],digits = 4),round(chol_var2_inf[3],digits = 4),round(chol_var2_tcr[3],digits = 4),round(chol_var2_bm[3],digits = 4))

table_chol_var2[nrow(table_chol_var2)+1,] <- c(4,round(chol_var2_pibr[4],digits = 4),round(chol_var2_inf[4],digits = 4),round(chol_var2_tcr[4],digits = 4),round(chol_var2_bm[4],digits = 4))

write.csv(table_chol_var2,file="tabler3.csv",row.names=FALSE)


fevd_var2 <- fevd(var2, n.ahead=12)

resumen_var2 <- summary(var2)

desv_var2 <- map_df(fevd_var2, ~as.data.frame(.x), .id="id") %>%
  mutate(Horizonte = rep(1:12, 4)) %>%
  pivot_longer(names_to = "Variable", cols = c(c_pib_real, c_bm,inf,c_tc_real))

png("plot4.png",width = 1920,height = 1080)

ggplot(data = desv_var2, mapping = aes(x = Horizonte, y = value, fill = Variable)) +
  facet_wrap(~id) +
  geom_bar(stat = "identity") +
  theme_bw()

dev.off()


#FIN