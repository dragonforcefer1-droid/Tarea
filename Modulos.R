## CODIGO DE VALIDACION ENVE 2026 ####


#Limpiar la consola
rm(list=ls())

#Esto es para cargar las librer�as
library("openxlsx")
library("foreign")
library("sqldf")
library("stringr")
library(spanish)

#Establecer directorio de trabajo
setwd("D:/diego.solis/Documents/Respaldo/ENVE/2024/Base datos")

#Se carga la informaci�n en la base de Prueba
#Tr_2 <- read.xlsx(xlsxFile = "TR_ENVE_2024.xlsx", skipEmptyRows = FALSE, skipEmptyCols = FALSE, sheet = "TR_ENVE_D_1")
#deli <- read.xlsx(xlsxFile = "TR_ENVE_2024.xlsx", skipEmptyRows = FALSE, skipEmptyCols = FALSE, sheet = "TR_ENVE_MODULOS")
#Bd_admin <-  read.xlsx(xlsxFile = "BASE ADMINISTRADOR.xlsx", skipEmptyRows = FALSE, skipEmptyCols = FALSE, sheet = "Hoja1")

Tr <- read.xlsx(xlsxFile= "Base módulos 30340 registros captura.xlsx")
Tr_pri <- read.xlsx(xlsxFile = "Base principal 30340 registros captura.xlsx")


Tr_del <- sqldf("
  SELECT *
  FROM Tr AS t1
  LEFT JOIN Tr_pri AS t2
    ON t1.CVE_UNICA = t2.CVE_UNICA
")

names(Tr_del)
###Convertir variables numericas

M1_1_33 = c(grep("^M",names(Tr_del),value=TRUE))
M1_1_33 = M1_1_33

M1_1_33_character = c(grep("X$",M1_1_33,value = TRUE))
M1_1_33_character

M1_1_33_numeric = setdiff(M1_1_33,M1_1_33_character)
M1_1_33_numeric

for (i in M1_1_33_numeric) {
	Tr_del[[i]] <- as.numeric(Tr_del[[i]])
}

#Funcion para contar letras sin espacios
Letras <- function(x) nchar(gsub(" ", "", x))

#Funcion para numeros con letra
Letras_num<-function(x) {
	grepl("^[[:alpha:], /]+$", x) & !grepl("  +", x)
}


##### BASES POR MODULO ####
Tr_mod2 <- sqldf("SELECT * 
			FROM Tr AS t1 LEFT JOIN Tr_pri AS t2 
			ON t1.CVE_UNICA = t2.CVE_UNICA
			WHERE t1.M2_1 IS NOT NULL")

Tr_mod3 <- sqldf("SELECT * 
			FROM Tr AS t1 LEFT JOIN Tr_pri AS t2 
			ON t1.CVE_UNICA = t2.CVE_UNICA 
			WHERE t1.M3_1_1 IS NOT NULL")

Tr_mod4 <- sqldf("SELECT * 
			FROM Tr AS t1 LEFT JOIN Tr_pri AS t2 
			ON t1.CVE_UNICA = t2.CVE_UNICA 
			WHERE t1.M4_1 IS NOT NULL")

Tr_mod5 <- sqldf("SELECT * 
			FROM Tr AS t1 LEFT JOIN Tr_pri AS t2 
			ON t1.CVE_UNICA = t2.CVE_UNICA 
			WHERE t1.M5_1 IS NOT NULL")

Tr_mod6 <- sqldf("SELECT * 
			FROM Tr AS t1 LEFT JOIN Tr_pri AS t2 
			ON t1.CVE_UNICA = t2.CVE_UNICA 
			WHERE t1.M6_1 IS NOT NULL")

# CAMBIAR EN ENVE 2026 a P4_4_16
Tr_mod7 <- sqldf("SELECT * 
			FROM Tr AS t1 LEFT JOIN Tr_pri AS t2 
			ON t1.CVE_UNICA = t2.CVE_UNICA 
			WHERE t1.M7_1 IS NOT NULL")

#RANGOS

Ran_M1_1 <- ifelse(Tr_del$M1_1 %in% c(1:12),"OK","MAL")
x=paste0("0",1:9)
x1=as.character(c(10:33))
Ran_M1_2 <- ifelse(Tr_del$M1_2 %in% c(1:33),"OK","MAL")
Ran_M1_3 <- ifelse(Tr_del$M1_3 %in% c(NA,1:4,9),"OK","MAL")
Ran_M1_4 <- ifelse(Tr_del$M1_4 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_5 <- ifelse(Tr_del$M1_5 %in% c(NA,1:5,9),"OK","MAL")
Ran_M1_5A_1 <- ifelse(Tr_del$M1_5A_1 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_5A_2 <- ifelse(Tr_del$M1_5A_2 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_5A_3 <- ifelse(Tr_del$M1_5A_3 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_5A_4 <- ifelse(Tr_del$M1_5A_4 %in% c(NA,1,2,9),"OK","MAL")


Ran_M1_6 <- ifelse(Tr_del$M1_6 %in% c(NA,1,2,9),"OK","MAL")

Ran_M1_7 <- ifelse(Tr_del$M1_7 %in% c(NA,1,2,9),"OK","MAL")

Ran_M1_8 <- ifelse(Tr_del$M1_8 %in% c(NA,1:6,9),"OK","MAL")

Ran_M1_9A <- ifelse((Tr_del$M1_9A > 0 & Tr_del$M1_9A <= 99) | is.na(Tr_del$M1_9A),"OK","MAL")
Ran_M1_9B <- ifelse((Tr_del$M1_9B > 0 & Tr_del$M1_9B <= 99) | is.na(Tr_del$M1_9B),"OK","MAL")

Ran_M1_10 <- ifelse(Tr_del$M1_10 %in% c(NA,1:9),"OK","MAL")

Ran_M1_11_1 <- ifelse(Tr_del$M1_11_1 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_11_2 <- ifelse(Tr_del$M1_11_2 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_11_3 <- ifelse(Tr_del$M1_11_3 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_11_4 <- ifelse(Tr_del$M1_11_4 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_11_5 <- ifelse(Tr_del$M1_11_5 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_12 <- ifelse(Tr_del$M1_12 %in% c(NA,1,2),"OK","MAL")
Ran_M1_13 <- ifelse(Tr_del$M1_13 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_14_1 <- ifelse(Tr_del$M1_14_1 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_14_2 <- ifelse(Tr_del$M1_14_2 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_14_3 <- ifelse(Tr_del$M1_14_3 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_14_4 <- ifelse(Tr_del$M1_14_4 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_15 <- ifelse(Tr_del$M1_15 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_15A <- ifelse(Tr_del$M1_15A %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_16 <- ifelse(Tr_del$M1_16 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_17_1 <- ifelse(Tr_del$M1_17_1 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_17_2 <- ifelse(Tr_del$M1_17_2 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_17_3 <- ifelse(Tr_del$M1_17_3 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_17_4 <- ifelse(Tr_del$M1_17_4 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_17_5 <- ifelse(Tr_del$M1_17_5 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_17_6 <- ifelse(Tr_del$M1_17_6 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_17_7 <- ifelse(Tr_del$M1_17_7 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_17_8 <- ifelse(Tr_del$M1_17_8 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_17A <- ifelse(Tr_del$M1_17A %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_18 <- ifelse(Tr_del$M1_18 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_19 <- ifelse(Tr_del$M1_19 %in% c(NA,1:9,99),"OK","MAL")
Ran_M1_20 <- ifelse(Tr_del$M1_20 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_21 <- ifelse(Tr_del$M1_21 %in% c(NA,1:7,9),"OK","MAL")
Ran_M1_22 <- ifelse(Tr_del$M1_22 %in% c(NA,1:6,9),"OK","MAL")
Ran_M1_23 <- ifelse(Tr_del$M1_23 %in% c(NA,1:7,9),"OK","MAL")
Ran_M1_24 <- ifelse(Tr_del$M1_24 %in% c(NA,1:5,9),"OK","MAL")
Ran_M1_25 <- ifelse(Tr_del$M1_25 %in% c(NA,1:4,9),"OK","MAL")
Ran_M1_26 <- ifelse(Tr_del$M1_26 %in% c(NA,1,2,9),"OK","MAL")

Ran_M1_27_1 <- ifelse(Tr_del$M1_27_1 %in% c(NA,1,2),"OK","MAL")
Ran_M1_27_2 <- ifelse(Tr_del$M1_27_2 %in% c(NA,1,2),"OK","MAL")
Ran_M1_27_3 <- ifelse(Tr_del$M1_27_3 %in% c(NA,1,2),"OK","MAL")
Ran_M1_27_4 <- ifelse(Tr_del$M1_27_4 %in% c(NA,1,2),"OK","MAL")
Ran_M1_27_5 <- ifelse(Tr_del$M1_27_5 %in% c(NA,1,2),"OK","MAL")
Ran_M1_27_6 <- ifelse(Tr_del$M1_27_6 %in% c(NA,1,2),"OK","MAL")
Ran_M1_27_7 <- ifelse(Tr_del$M1_27_7 %in% c(NA,1,2),"OK","MAL")
Ran_M1_27_8 <- ifelse(Tr_del$M1_27_8 %in% c(NA,1,2),"OK","MAL")
Ran_M1_27_9 <- ifelse(Tr_del$M1_27_9 %in% c(NA,1,2),"OK","MAL")
Ran_M1_27_11 <- ifelse(Tr_del$M1_27_11 %in% c(NA,1,2),"OK","MAL")

Ran_M1_28_1 <- ifelse(Tr_del$M1_28_1 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_28_2 <- ifelse(Tr_del$M1_28_2 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_28_3 <- ifelse(Tr_del$M1_28_3 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_28_4 <- ifelse(Tr_del$M1_28_4 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_28_5 <- ifelse(Tr_del$M1_28_5 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_28_6 <- ifelse(Tr_del$M1_28_6 %in% c(NA,1,2,9),"OK","MAL")

Ran_M1_29_1 <- ifelse(Tr_del$M1_29_1 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_29_2 <- ifelse(Tr_del$M1_29_2 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_29_3 <- ifelse(Tr_del$M1_29_3 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_29_4 <- ifelse(Tr_del$M1_29_4 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_29_5 <- ifelse(Tr_del$M1_29_5 %in% c(NA,1,2,9),"OK","MAL")
Ran_M1_29_6 <- ifelse(Tr_del$M1_29_6 %in% c(NA,1,2,9),"OK","MAL")

#CM7
Ran_M1_30<-ifelse(Tr_del$M1_30 %in% c(NA,1:7,9),"OK","MAL")
Ran_M1_31<-ifelse((Tr_del$M1_31>0 & Tr_del$M1_31 <=9999999999) | (is.na(Tr_del$M1_31)),"OK","MAL")

Ran_M1_32_1 <- ifelse(Tr_del$M1_32_1 %in% c(NA,1,2),"OK","MAL") 
Ran_M1_32_2 <- ifelse(Tr_del$M1_32_2 %in% c(NA,1,2),"OK","MAL") 
Ran_M1_32_3 <- ifelse(Tr_del$M1_32_3 %in% c(NA,1,2),"OK","MAL") 
Ran_M1_32_4 <- ifelse(Tr_del$M1_32_4 %in% c(NA,1,2),"OK","MAL") 
Ran_M1_32_5 <- ifelse(Tr_del$M1_32_5 %in% c(NA,1,2),"OK","MAL") 
Ran_M1_32_6 <- ifelse(Tr_del$M1_32_6 %in% c(NA,1,2),"OK","MAL") 
Ran_M1_32_7 <- ifelse(Tr_del$M1_32_7 %in% c(NA,1,2),"OK","MAL") 
Ran_M1_32_8 <- ifelse(Tr_del$M1_32_8 %in% c(NA,1,2),"OK","MAL") 
Ran_M1_32_9 <- ifelse(Tr_del$M1_32_9 %in% c(NA,1,2),"OK","MAL") 
Ran_M1_32_10 <- ifelse(Tr_del$M1_32_10 %in% c(NA,1,2),"OK","MAL") 
Ran_M1_32_11 <- ifelse(Tr_del$M1_32_11 %in% c(NA,1,2),"OK","MAL") 

#MODULO 2
Ran_M2_1<- ifelse((Tr_del$M2_1 >= 0 & Tr_del$M2_1 <= 99) | is.na(Tr_del$M2_1),"OK","MAL")
Ran_M2_2<- ifelse((Tr_del$M2_2 >= 0 & Tr_del$M2_2 <= 99) | is.na(Tr_del$M2_2),"OK","MAL")

#MODULO 3
Ran_M3_1_1 <- ifelse(Tr_del$M3_1_1 %in% c(NA,1,2,9),"OK","MAL")
Ran_M3_1_2 <- ifelse(Tr_del$M3_1_2 %in% c(NA,1,2,9),"OK","MAL")
Ran_M3_1_3 <- ifelse(Tr_del$M3_1_3 %in% c(NA,1,2,9),"OK","MAL")
Ran_M3_1_4 <- ifelse(Tr_del$M3_1_4 %in% c(NA,1,2,9),"OK","MAL")
Ran_M3_1_5 <- ifelse(Tr_del$M3_1_5 %in% c(NA,1,2,9),"OK","MAL")
Ran_M3_1_6 <- ifelse(Tr_del$M3_1_6 %in% c(NA,1,2,9),"OK","MAL")
Ran_M3_1_7 <- ifelse(Tr_del$M3_1_7 %in% c(NA,1,2,9),"OK","MAL")
Ran_M3_1_8 <- ifelse(Tr_del$M3_1_8 %in% c(NA,1,2,9),"OK","MAL")

#MODULO 4
Ran_M4_1<-ifelse(Tr_del$M4_1 %in% c(NA,1:8),"OK","MAL")

#MODULO 5
Ran_M5_1<-ifelse(Tr_del$M5_1 %in% c(NA,1:8),"OK","MAL")

Ran_M5_1A_1<-ifelse(Tr_del$M5_1A_1 %in% c(NA,1,2,9),"OK","MAL")
Ran_M5_1A_2<-ifelse(Tr_del$M5_1A_2 %in% c(NA,1,2,9),"OK","MAL")
Ran_M5_1A_3<-ifelse(Tr_del$M5_1A_3 %in% c(NA,1,2,9),"OK","MAL")
Ran_M5_1A_4<-ifelse(Tr_del$M5_1A_4 %in% c(NA,1,2,9),"OK","MAL")

Ran_M5_2_1<-ifelse(Tr_del$M5_2_1 %in% c(NA,1,2,9),"OK","MAL")
Ran_M5_2_2<-ifelse(Tr_del$M5_2_2 %in% c(NA,1,2,9),"OK","MAL")
Ran_M5_2_3<-ifelse(Tr_del$M5_2_3 %in% c(NA,1,2,9),"OK","MAL")
Ran_M5_2_4<-ifelse(Tr_del$M5_2_4 %in% c(NA,1,2,9),"OK","MAL")

Ran_M5_3<-ifelse(Tr_del$M5_3 %in% c(NA,1,2,9),"OK","MAL")
Ran_M5_4<-ifelse(Tr_del$M5_4 %in% c(NA,1,2,9),"OK","MAL")

#MODULO 6
Ran_M6_1<-ifelse(Tr_del$M6_1 %in% c(NA,1:6,9),"OK","MAL")
Ran_M6_2<-ifelse(Tr_del$M6_2 %in% c(NA,1:4,9),"OK","MAL")
Ran_M6_3<-ifelse(Tr_del$M6_3 %in% c(NA,1:3,9),"OK","MAL")
Ran_M6_4<-ifelse(Tr_del$M6_4 %in% c(NA,1:2,9),"OK","MAL")

#MODULO 7
Ran_M7_1<-ifelse(Tr_del$M7_1 %in% c(NA,1:5,9),"OK","MAL")


### VALIDACIONES MODULO 1 ###
Val_M1_1 <- ifelse((Tr_del$ID_DELITO %in% c("04","11") & Tr_del$M1_1 %in% c(1:12)) |
(Tr_del$ID_DELITO == "11" & Tr_del$M1_1 %in% c(1:13)) ,"OK","MAL")

Val_M1_2X <- ifelse((Letras(Tr_del$M1_2X)>=4 & Letras_num(Tr_del$M1_2X)) | is.na(Tr_del$M1_2X),"OK","MAL")

Val_M1_5_5X <- ifelse((Tr_del$M1_5 == 5 & Letras(Tr_del$M1_5_5X) >= 6 & Letras_num(Tr_del$M1_5_5X)) |
(Tr_del$M1_5 %in% c(1,2,3,4,9) & is.na(Tr_del$M1_5_5X)) |
(Tr_del$M1_2 == 33 & is.na(Tr_del$M1_5) & is.na(Tr_del$M1_5_5X)),"OK","MAL")

Val_M1_5A_4X <- ifelse((Tr_del$M1_5A_4 == 1 & Letras(Tr_del$M1_5A_4X) >= 6 & Letras_num(Tr_del$M1_5A_4X)) |
(Tr_del$M1_5A_4 %in% c(2,9) & is.na(Tr_del$M1_5A_4X)) | 
((!Tr_del$ID_DELITO %in% c("07","08","09") | Tr_del$M1_2 == 33) & is.na(Tr_del$M1_5A_4) & is.na(Tr_del$M1_5A_4X))
,"OK","MAL")

Val_M1_5A_1 <- ifelse(((Tr_del$P4_4_7 == 1 | Tr_del$P4_4_8 == 1 | Tr_del$P4_4_9 == 1) & 
(Tr_del$M1_5A_1 == 1 | Tr_del$M1_5A_2 == 1 | Tr_del$M1_5A_3 == 1 | Tr_del$M1_5A_4 == 1)) | 
((Tr_del$P4_4_7 == 2 & Tr_del$P4_4_8 == 2 & Tr_del$P4_4_9 == 2) &
(is.na(Tr_del$M1_5A_1) & is.na(Tr_del$M1_5A_2) & is.na(Tr_del$M1_5A_3) & is.na(Tr_del$M1_5A_4))),"OK","MAL")


Tr_del$Sum_M1_9 <- rowSums(Tr_del[,c("M1_9A","M1_9B")],na.rm = TRUE)

Val_M1_9_1 <- ifelse((Tr_del$M1_8 == 1 & Tr_del$Sum_M1_9 %in% c(1,99)) | (Tr_del$M1_8 == 2 & Tr_del$Sum_M1_9 %in% c(2,99)) |
(Tr_del$M1_8 == 3 & Tr_del$Sum_M1_9 %in% c(3,99))| (Tr_del$M1_8 == 4 & Tr_del$Sum_M1_9 %in% c(4,99)) |
(Tr_del$M1_8 == 5 & Tr_del$Sum_M1_9 %in% c(5,99)) | (Tr_del$M1_8 == 6 & Tr_del$Sum_M1_9 >= 6) |
(Tr_del$M1_8 == 9 & Tr_del$Sum_M1_9 == 0) | (is.na(Tr_del$M1_8) & Tr_del$Sum_M1_9 == 0)
,"OK","MAL")

Val_M1_11 <- ifelse((Tr_del$M1_11_1 == 1 | Tr_del$M1_11_2 == 1 | Tr_del$M1_11_3 == 1 | Tr_del$M1_11_4 == 1) |
(is.na(Tr_del$M1_11_1) & is.na(Tr_del$M1_11_2) & is.na(Tr_del$M1_11_3) & is.na(Tr_del$M1_11_4)),"OK","MAL")

Val_M1_11_5 <- ifelse((Tr_del$M1_11_5 == 1 & !is.na(Tr_del$M1_12)) | (Tr_del$M1_11_5 != 1 & is.na(Tr_del$M1_12)) |
(Tr_del$M1_2 == 33 & is.na(Tr_del$M1_11_5) & is.na(Tr_del$M1_12)) | (is.na(Tr_del$M1_11_5) & is.na(Tr_del$M1_12)),"OK","MAL")

Val_M1_14_4X <- ifelse((Tr_del$M1_14_4 == 1 & Letras(Tr_del$M1_14_4X) >= 6 & Letras_num(Tr_del$M1_14_4X)) |
(Tr_del$M1_14_4 %in% c(NA,2) & is.na(Tr_del$M1_14_4X)),"OK","MAL")

Val_M1_14 <- ifelse((Tr_del$M1_14_1 == 1 | Tr_del$M1_14_2 == 1 | Tr_del$M1_14_3 == 1 | Tr_del$M1_14_4 == 1) |
(is.na(Tr_del$M1_14_1) & is.na(Tr_del$M1_14_2) & is.na(Tr_del$M1_14_3) & is.na(Tr_del$M1_14_4)),"OK","MAL")

Val_M1_17_7_1 <- ifelse((Tr_del$M1_13 == 1 & Tr_del$M1_14_1 == 1 & Tr_del$M1_15 == 1 & Tr_del$M1_17_7 == 1) | 
                          (Tr_del$M1_17_7 == 2 & Tr_del$M1_15 == 1 & Tr_del$M1_14_1 == 1 & Tr_del$M1_13 == 1) | 
                          (Tr_del$M1_15 == 2 & is.na(Tr_del$M1_17_7)) | (is.na(Tr_del$M1_15) & is.na(Tr_del$M1_17_7)),"OK","MAL")


Val_M1_17_7_2 <- ifelse((Tr_del$M1_17_7 %in% c(1,2) & Tr_del$M1_13 == 1 & Tr_del$M1_14_1 == 1 & Tr_del$M1_15 == 1) |
          (Tr_del$M1_17_7 == 2 & Tr_del$M1_13 %in% c(2, 9) & Tr_del$M1_16 == 1) |
			    (Tr_del$M1_17_7 == 2 & Tr_del$M1_13 == 1  & Tr_del$M1_14_1 == 2 & (Tr_del$M1_15 == 1 | Tr_del$M1_16 == 1)) |
          (Tr_del$M1_17_7 == 2 & Tr_del$M1_13 == 1 & Tr_del$M1_14_1 == 1 & Tr_del$M1_15 %in% c(2, 9) & Tr_del$M1_16 == 1) |
			    (is.na(Tr_del$M1_17_7) & Tr_del$M1_13 == 1 & Tr_del$M1_14_1 %in% c(2, 9) & Tr_del$M1_15 %in% c(2, 9) & Tr_del$M1_16 %in% c(2, 9)) |			  
			    (is.na(Tr_del$M1_17_7) & Tr_del$M1_13 == 1 & Tr_del$M1_14_1 == 1 & Tr_del$M1_15 %in% c(2, 9)) |
			    (is.na(Tr_del$M1_17_7) & Tr_del$M1_13 %in% c(2, 9) & Tr_del$M1_16 %in% c(2, 9)) |
			    (is.na(Tr_del$M1_17_7) & is.na(Tr_del$M1_13) & is.na(Tr_del$M1_14_1) & is.na(Tr_del$M1_15)),"OK","MAL")

Val_M1_17_8X <- ifelse((Tr_del$M1_17_8 == 1 & Letras(Tr_del$M1_17_8X) >= 6 & Letras_num(Tr_del$M1_17_8X)) |
(Tr_del$M1_17_8 == 2 & is.na(Tr_del$M1_17_8X)) | (is.na(Tr_del$M1_17_8) & is.na(Tr_del$M1_17_8X)),"OK","MAL")

Val_M1_17_1 <- ifelse((Tr_del$M1_17_1 == 1 | Tr_del$M1_17_2 == 1 | Tr_del$M1_17_3 == 1 | Tr_del$M1_17_4 == 1
| Tr_del$M1_17_5 == 1 | Tr_del$M1_17_6 == 1 | Tr_del$M1_17_7 == 1 | Tr_del$M1_17_8 == 1) | (is.na(Tr_del$M1_17_1) &
is.na(Tr_del$M1_17_2) & is.na(Tr_del$M1_17_3) & is.na(Tr_del$M1_17_4) & is.na(Tr_del$M1_17_5) & is.na(Tr_del$M1_17_6)
& is.na(Tr_del$M1_17_7) & is.na(Tr_del$M1_17_8)),"OK","MAL")


Val_M1_19_9X <- ifelse((Tr_del$M1_19 == 9 & Letras(Tr_del$M1_19_9X) >= 6 & Letras_num(Tr_del$M1_19_9X)) |
(Tr_del$M1_19 %in% c(1:8) & is.na(Tr_del$M1_19_9X)) | (is.na(Tr_del$M1_19) & is.na(Tr_del$M1_19_9X)),"OK","MAL")

Val_M1_21_7X <- ifelse((Tr_del$M1_21 == 7 & Letras(Tr_del$M1_21_7X) >= 6 & Letras_num(Tr_del$M1_21_7X)) | 
(Tr_del$M1_27 %in% c(NA,1:6,9) & is.na(Tr_del$M1_21_7X)),"OK","MAL")

Val_M1_22_6X <- ifelse((Tr_del$M1_22 == 6 & Letras(Tr_del$M1_22_6X) >= 6 & Letras_num(Tr_del$M1_22_6X)) |
(Tr_del$M1_22 %in% c(NA,1:5,9) & is.na(Tr_del$M1_22_6X)),"OK", "MAL")

Val_M1_23_7X <- ifelse((Tr_del$M1_23 == 7 & Letras(Tr_del$M1_23_7X) >=6 & Letras_num(Tr_del$M1_23_7X)) |
(Tr_del$M1_23 %in% c(NA,1:6,9) & is.na(Tr_del$M1_23_7X)),"OK", "MAL")

Val_M1_27_9X <- ifelse((Tr_del$M1_27_9 == 1 & Letras(Tr_del$M1_27_9X) >=6 & Letras_num(Tr_del$M1_27_9X)) |
(Tr_del$M1_27_9 %in% c(NA,2) & is.na(Tr_del$M1_27_9X)),"OK", "MAL")

Val_M1_27 <- ifelse((Tr_del$M1_27_11 == 1 & Tr_del$M1_27_1 == 2 & Tr_del$M1_27_2 == 2 & Tr_del$M1_27_3 == 2 
& Tr_del$M1_27_4 == 2 & Tr_del$M1_27_5 == 2 & Tr_del$M1_27_6 == 2 & Tr_del$M1_27_7 == 2 & Tr_del$M1_27_8 == 2
& Tr_del$M1_27_9 == 2) | 
((Tr_del$M1_27_1 == 1 | Tr_del$M1_27_2 == 1 | Tr_del$M1_27_3 == 1 | Tr_del$M1_27_4 == 1 | Tr_del$M1_27_5 == 1 
| Tr_del$M1_27_6 == 1 | Tr_del$M1_27_7 == 1 | Tr_del$M1_27_8 == 1 | Tr_del$M1_27_9 == 1) & Tr_del$M1_27_11 == 2) |
(is.na(Tr_del$M1_27_1) & is.na(Tr_del$M1_27_2) & is.na(Tr_del$M1_27_3) & is.na(Tr_del$M1_27_4)
& is.na(Tr_del$M1_27_5) & is.na(Tr_del$M1_27_6) & is.na(Tr_del$M1_27_7) & is.na(Tr_del$M1_27_8)
& is.na(Tr_del$M1_27_9) & is.na(Tr_del$M1_27_11)),"OK","MAL")

Val_M1_28_5X <- ifelse((Tr_del$M1_28_5 == 1 & Letras(Tr_del$M1_28_5X) >=6 & Letras_num(Tr_del$M1_28_5X)) |
(Tr_del$M1_28_5 %in% c(NA,2,9) & is.na(Tr_del$M1_28_5X)),"OK", "MAL")

Val_M1_29_5X <- ifelse((Tr_del$M1_29_5 == 1 & Letras(Tr_del$M1_29_5X) >=6 & Letras_num(Tr_del$M1_29_5X)) |
(Tr_del$M1_29_5 %in% c(2,9) & is.na(Tr_del$M1_29_5X)) | 
(Tr_del$M1_2 == 33 & is.na(Tr_del$M1_29_5) & is.na(Tr_del$M1_29_5X)),"OK", "MAL")

Val_M1_29 <- ifelse((Tr_del$M1_29_1 == 1 | Tr_del$M1_29_2 == 1 | Tr_del$M1_29_3 == 1 | Tr_del$M1_29_4 == 1 |
Tr_del$M1_29_5 == 1 | Tr_del$M1_29_6 == 1) | (is.na(Tr_del$M1_29_1) & is.na(Tr_del$M1_29_2) & 
is.na(Tr_del$M1_29_3) & is.na(Tr_del$M1_29_4) & is.na(Tr_del$M1_29_5) & is.na(Tr_del$M1_29_6)),"OK","MAL")

Val_M1_30 <- ifelse((Tr_del$M1_29_1 == 1 & Tr_del$M1_30 != 7) | (Tr_del$M1_29_1 == 2 & Tr_del$M1_30 == 7) |
(is.na(Tr_del$M1_29_1) & is.na(Tr_del$M1_30)),"OK","MAL")

Val_M1_31 <- ifelse((Tr_del$M1_30 == 1 & ((Tr_del$M1_31 >= 1 & Tr_del$M1_31 <= 50000) | (Tr_del$M1_31 == 9))) |
(Tr_del$M1_30 == 2 & ((Tr_del$M1_31 >= 50001 & Tr_del$M1_31 <= 500000) | (Tr_del$M1_31 == 9))) | 
(Tr_del$M1_30 == 3 & ((Tr_del$M1_31 >= 500001 & Tr_del$M1_31 <= 2000000) | (Tr_del$M1_31 == 9))) | 
(Tr_del$M1_30 == 4 & ((Tr_del$M1_31 >= 2000001 & Tr_del$M1_31 <= 10000000) | (Tr_del$M1_31 == 9))) |
(Tr_del$M1_30 == 5 & ((Tr_del$M1_31 >= 10000001 & Tr_del$M1_31 <= 50000000) | (Tr_del$M1_31 == 9))) |
(Tr_del$M1_30 == 6 & ((Tr_del$M1_31 >= 50000001) | (Tr_del$M1_31 == 9))) |
(Tr_del$M1_30 %in% c(NA,7,9) & is.na(Tr_del$M1_31)),"OK","MAL")
 
Val_M1_31X <- ifelse((Tr_del$M1_31 > 0 & Letras(Tr_del$M1_31X) >= 3 & Letras_num(Tr_del$M1_31X)) |
(is.na(Tr_del$M1_31X) & is.na(Tr_del$M1_31)),"OK","MAL")

Val_M1_32_10X <- ifelse((Tr_del$M1_32_10 == 1 & Letras(Tr_del$M1_32_10X) >= 6 & Letras_num(Tr_del$M1_32_10X)) |
(Tr_del$M1_32_10 == 2 & is.na(Tr_del$M1_32_10X)),"OK","MAL")

## VALIDACIONES MODULO 2 ###
Val_M2 <- ifelse((Tr_mod2$P4_4_1 == 1 & Tr_mod2$ID_DELITO == "01" & !is.na(Tr_mod2$M2_1) & !is.na(Tr_mod2$M2_2)) |
(Tr_mod2$P4_4_1 %in% c(2,9) & Tr_mod2$ID_DELITO != "01" & is.na(Tr_mod2$M2_1) & is.na(Tr_mod2$M2_2)) |
(Tr_mod2$M1_2 == 33 & is.na(Tr_mod2$P4_4_1) & is.na(Tr_mod2$M2_1) & is.na(Tr_mod2$M2_2)),"OK","MAL")

Val_M2_1 <- ifelse((Tr_mod2$M2_1 != 99 & Tr_mod2$M2_1 <= Tr_mod2$P4_1B) |
                   (Tr_mod2$M2_1 == 99) |
			(is.na(Tr_mod2$M2_1) == TRUE), "OK", "MAL")

Val_M2_1.1 <- ifelse((Tr_mod2$P4_1A - Tr_mod2$P4_1B == 0 & Tr_mod2$M2_1 > 0 & Tr_mod2$ID_DELITO == "01" & is.na(Tr_mod2$M2_1) == FALSE) |
			   (Tr_mod2$P4_1A - Tr_mod2$P4_1B != 0 & Tr_mod2$M2_1 >= 0 & Tr_mod2$ID_DELITO == "01" & is.na(Tr_mod2$M2_1) == FALSE) |
		         (!( Tr_mod2$ID_DELITO == "01") & is.na(Tr_mod2$M2_1) == TRUE), "OK", "MAL")


Val_M2_2 <- ifelse((Tr_mod2$M2_2 <= Tr_mod2$M2_1) | 
(Tr_mod2$M2_1 %in% c(0:98) & Tr_mod2$M2_2 == 99) ,"OK","MAL")

### VALIDACIONES MODULO 3 ###
Val_M3 <- ifelse(((Tr_mod3$P4_4_5 == 1 | Tr_mod3$P4_4_6 == 1) & !is.na(Tr_mod3$M3_1_1) & !is.na(Tr_mod3$M3_1_2)
& !is.na(Tr_mod3$M3_1_3) & !is.na(Tr_mod3$M3_1_4) & !is.na(Tr_mod3$M3_1_5) & !is.na(Tr_mod3$M3_1_6) & 
!is.na(Tr_mod3$M3_1_7) & !is.na(Tr_mod3$M3_1_8)),"OK","MAL") 

Val_M3_1_8X <- ifelse((Tr_mod3$M3_1_8 == 1 & Letras(Tr_mod3$M3_1_8X) >= 6 & Letras_num(Tr_mod3$M3_1_8X)) |
(Tr_mod3$M3_1_8 == 2 & is.na(Tr_mod3$M3_1_8X)) | 
(!Tr_mod3$ID_DELITO %in% c("05","06") & is.na(Tr_mod3$M3_1_8) & is.na(Tr_mod3$M3_1_8X)),"OK","MAL")

Val_M3_1_1 <- ifelse((Tr_mod3$M3_1_1 == 1 | Tr_mod3$M3_1_2 == 1 | Tr_mod3$M3_1_3 == 1 | Tr_mod3$M3_1_4 == 1
| Tr_mod3$M3_1_5 == 1 | Tr_mod3$M3_1_6 == 1 | Tr_mod3$M3_1_7 == 1 | Tr_mod3$M3_1_8 == 1) | (is.na(Tr_mod3$M3_1_1) &
is.na(Tr_mod3$M3_1_2) & is.na(Tr_mod3$M3_1_3) & is.na(Tr_mod3$M3_1_4) & is.na(Tr_mod3$M3_1_5) & is.na(Tr_mod3$M3_1_6)
& is.na(Tr_mod3$M3_1_7) & is.na(Tr_mod3$M3_1_8)),"OK","MAL")

### VALIDACIONES MODULO 4 ###

Val_M4 <- ifelse(((Tr_mod4$P4_4_7 == 1 | Tr_mod4$P4_4_8 == 1 | Tr_mod4$P4_4_9 == 1) & !is.na(Tr_mod4$M4_1)) | 
((Tr_mod4$P4_4_7 == 2 & Tr_mod4$P4_4_8 == 2 & Tr_mod4$P4_4_9 == 2) & is.na(Tr_mod4$M4_1)),"OK","MAL")

Val_M4_1_8X <- ifelse((Tr_mod4$M4_1 == 8 & Letras(Tr_mod4$M4_1_8X) >= 6 & Letras_num(Tr_mod4$M4_1_8X)) |
(Tr_mod4$M4_1 != 8 & is.na(Tr_mod4$M4_1_8X)) | (is.na(Tr_mod4$M4_1) & is.na(Tr_mod4$M4_1_8X)),"OK","MAL")

Val_M4_1.1 <- ifelse((Tr_mod4$ID_DELITO == "07" & Tr_mod4$M4_1 %in% c(1,2,8)) | 
(Tr_mod4$ID_DELITO == "08" & Tr_mod4$M4_1 %in% c(2,4,5,6,7,8)) | 
(Tr_mod4$ID_DELITO == "09" & Tr_mod4$M4_1 %in% c(2,3,8)) |
((!Tr_mod4$ID_DELITO %in% c("07","08","09") | Tr_mod4$M1_2 == 33) & is.na(Tr_mod4$M4_1)),"OK","MAL")

### VALIDACIONES MODULO 5 ###

Val_M5 <- ifelse(Tr_mod5$P4_4_11 == 1 & !is.na(Tr_mod5$M5_1) & Tr_mod5$M5_1A_1 %in% c(NA,1,2,9) &
Tr_mod5$M5_1A_2 %in% c(NA,1,2,9) & Tr_mod5$M5_1A_3 %in% c(NA,1,2,9) & Tr_mod5$M5_1A_4 %in% c(NA,1,2,9) &
Tr_mod5$M5_2_1 %in% c(1,2,9) & Tr_mod5$M5_2_2 %in% c(1,2,9) & Tr_mod5$M5_2_3 %in% c(1,2,9) & 
Tr_mod5$M5_2_4 %in% c(1,2,9) & Tr_mod5$M5_3 %in% c(NA,1,2,9) & Tr_mod5$M5_4 %in% c(NA,1,2,9),"OK","MAL")


Val_M5.1 <- ifelse((Tr_mod5$P4_4_11 == 1 & Tr_mod5$ID_DELITO == "11" & Tr_mod5$M1_5A_3 == 1 & 
!is.na(Tr_mod5$M5_1) & !is.na(Tr_mod5$M5_1A_1) & !is.na(Tr_mod5$M5_1A_2) & !is.na(Tr_mod5$M5_1A_3) &
!is.na(Tr_mod5$M5_1A_4) & Tr_mod5$M5_2_1 != 2 & Tr_mod5$M5_2_2 != 2 & Tr_mod5$M5_2_3 == 1 & 
Tr_mod5$M5_2_4 != 2 & is.na(Tr_mod5$M5_3) & is.na(Tr_mod5$M5_4)) 
| 
(Tr_mod5$P4_4_11 == 1 & Tr_mod5$ID_DELITO == "11" & Tr_mod5$M1_5A_3 == 1 & !is.na(Tr_mod5$M5_1) & 
!is.na(Tr_mod5$M5_1A_1) & !is.na(Tr_mod5$M5_1A_2) & !is.na(Tr_mod5$M5_1A_3) & !is.na(Tr_mod5$M5_1A_4) &
(Tr_mod5$M5_2_1 == 1 | Tr_mod5$M5_2_2 == 1 | Tr_mod5$M5_2_4 == 1) & Tr_mod5$M5_2_3 == 2 & Tr_mod5$M5_3 != 2 &
is.na(Tr_mod5$M5_4))
|
(Tr_mod5$P4_4_11 == 1 & Tr_mod5$ID_DELITO == "11" & Tr_mod5$M1_5A_3 == 1 & !is.na(Tr_mod5$M5_1) & 
!is.na(Tr_mod5$M5_1A_1) & !is.na(Tr_mod5$M5_1A_2) & !is.na(Tr_mod5$M5_1A_3) & !is.na(Tr_mod5$M5_1A_4) &
(Tr_mod5$M5_2_1 == 1 | Tr_mod5$M5_2_2 == 1 | Tr_mod5$M5_2_4 == 1) & Tr_mod5$M5_2_3 == 2 & Tr_mod5$M5_3 == 2 &
!is.na(Tr_mod5$M5_4))
|
(Tr_mod5$P4_4_11 == 1 & Tr_mod5$ID_DELITO == "11" & Tr_mod5$M1_5A_3 == 2 & 
is.na(Tr_mod5$M5_1) & is.na(Tr_mod5$M5_1A_1) & is.na(Tr_mod5$M5_1A_2) & !is.na(Tr_mod5$M5_1A_3) &
is.na(Tr_mod5$M5_1A_4) & Tr_mod5$M5_2_1 != 2 & Tr_mod5$M5_2_2 != 2 & Tr_mod5$M5_2_3 == 1 & 
Tr_mod5$M5_2_4 != 2 & is.na(Tr_mod5$M5_3) & is.na(Tr_mod5$M5_4)) 
|
(Tr_mod5$P4_4_11 == 1 & Tr_mod5$ID_DELITO == "11" & Tr_mod5$M1_5A_3 == 2 & !is.na(Tr_mod5$M5_1) & 
is.na(Tr_mod5$M5_1A_1) & is.na(Tr_mod5$M5_1A_2) & is.na(Tr_mod5$M5_1A_3) & is.na(Tr_mod5$M5_1A_4) &
(Tr_mod5$M5_2_1 == 1 | Tr_mod5$M5_2_2 == 1 | Tr_mod5$M5_2_4 == 1) & Tr_mod5$M5_2_3 == 2 & Tr_mod5$M5_3 != 2 &
is.na(Tr_mod5$M5_4))
|
(Tr_mod5$P4_4_11 == 1 & Tr_mod5$ID_DELITO == "11" & Tr_mod5$M1_5A_3 == 2 & !is.na(Tr_mod5$M5_1) & 
is.na(Tr_mod5$M5_1A_1) & is.na(Tr_mod5$M5_1A_2) & is.na(Tr_mod5$M5_1A_3) & is.na(Tr_mod5$M5_1A_4) &
(Tr_mod5$M5_2_1 == 1 | Tr_mod5$M5_2_2 == 1 | Tr_mod5$M5_2_4 == 1) & Tr_mod5$M5_2_3 == 2 & Tr_mod5$M5_3 == 2 &
!is.na(Tr_mod5$M5_4))
|
((Tr_mod5$M2_1 == 33 | Tr_mod5$ID_DELITO != "11") & is.na(Tr_mod5$M5_1) & 
is.na(Tr_mod5$M5_1A_1) & is.na(Tr_mod5$M5_1A_2) & is.na(Tr_mod5$M5_1A_3) & is.na(Tr_mod5$M5_1A_4) &
is.na(Tr_mod5$M5_2_1) & is.na(Tr_mod5$M5_2_2) & is.na(Tr_mod5$M5_2_3) & is.na(Tr_mod5$M5_2_4) & 
is.na(Tr_mod5$M5_3) & is.na(Tr_mod5$M5_4)),"OK","MAL")


Val_M5_1_8X <- ifelse((Tr_mod5$M5_1 == 8 & Letras(Tr_mod5$M5_1_8X) >= 6 & Letras_num(Tr_mod5$M5_1_8X)) |
(Tr_mod5$M5_1 != 8 & is.na(Tr_mod5$M5_1_8X)) | (is.na(Tr_mod5$M5_1) & is.na(Tr_mod5$M5_1_8X)),"OK","MAL")

Val_M5_1A <- ifelse((Tr_mod5$M1_5A_3 == 1 & (Tr_mod5$M5_1A_1 == 1 | Tr_mod5$M5_1A_2 == 1 | Tr_mod5$M5_1A_3 == 1 |
Tr_mod5$M5_1A_4 == 1)) | (Tr_mod5$M1_5A_3 %in% c(NA,2,9) & is.na(Tr_mod5$M5_1A_1) & is.na(Tr_mod5$M5_1A_2) &
is.na(Tr_mod5$M5_1A_3) & is.na(Tr_mod5$M5_1A_4)),"OK","MAL") 

Val_M5_1A_4X <- ifelse((Tr_mod5$M5_1A_4 == 1 & Letras(Tr_mod5$M5_1A_4X) >= 6 & Letras_num(Tr_mod5$M5_1A_4X)) |
(Tr_mod5$M5_1A_4 != 1 & is.na(Tr_mod5$M5_1A_4X)) | (is.na(Tr_mod5$M5_1A_4) & is.na(Tr_mod5$M5_1A_4X)),"OK","MAL")

Val_M5_2_4X <- ifelse((Tr_mod5$M5_2_4 == 1 & Letras(Tr_mod5$M5_2_4X) >= 6 & Letras_num(Tr_mod5$M5_2_4X)) |
(Tr_mod5$M5_2_4 != 1 & is.na(Tr_mod5$M5_2_4X)) | (is.na(Tr_mod5$M5_2_4) & is.na(Tr_mod5$M5_2_4X)),"OK","MAL")

Val_M5_2 <- ifelse(Tr_mod5$ID_DELITO == "11" & (Tr_mod5$M5_2_1 == 1 | Tr_mod5$M5_2_2 == 1 | Tr_mod5$M5_2_3 == 1 | Tr_mod5$M5_2_4 == 1)
| (Tr_mod5$ID_DELITO != "11" & is.na(Tr_mod5$M5_2_1) & is.na(Tr_mod5$M5_2_2) & is.na(Tr_mod5$M5_2_3) 
& is.na(Tr_mod5$M5_2_4)),"OK","MAL")

Val_M5_2_3 <- ifelse((Tr_mod5$M5_2_3 == 1 & Tr_mod5$M5_2_1 == 2 & Tr_mod5$M5_2_2 == 2 & Tr_mod5$M5_2_4 == 2) | 
((Tr_mod5$M5_2_1 == 1 | Tr_mod5$M5_2_2 == 1 | Tr_mod5$M5_2_4 == 1) & Tr_mod5$M5_2_3 == 2) | 
(is.na(Tr_mod5$M5_2_1) & is.na(Tr_mod5$M5_2_2) & is.na(Tr_mod5$M5_2_3) & is.na(Tr_mod5$M5_2_4)),"OK","MAL")

Val_M5_4X <- ifelse((Tr_mod5$M5_4 == 1 & Letras(Tr_mod5$M5_4_1X) >= 6 & Letras_num(Tr_mod5$M5_4_1X)) | 
(Tr_mod5$M5_4 %in% c(NA,2,9) & is.na(Tr_mod5$M5_4_1X)),"OK","MAL")

### VALIDACIONES MODULO 6 ###
Val_M6 <- ifelse((Tr_mod6$P4_4_12 == 1 & Tr_mod6$M6_1 %in% c(1:6,9) & Tr_mod6$M6_2 %in% c(1:4,9) & 
Tr_mod6$M6_3 %in% c(1:3,9) & Tr_mod6$M6_4 %in% c(NA,1,2,9)),"OK","MAL") 

Val_M6_1 <- ifelse((Tr_mod6$ID_DELITO == "12" & Tr_mod6$P4_4_12 == 1 & Tr_mod6$M6_1 %in% c(1:6,9) & 
Tr_mod6$M6_2 %in% c(1:4,9) & Tr_mod6$M6_3 %in% c(1,2) & !is.na(Tr_mod6$M6_4)) | (Tr_mod6$ID_DELITO == "12" &
Tr_mod6$P4_4_12 == 1 & Tr_mod6$M6_1 %in% c(1:6,9) & Tr_mod6$M6_2 %in% c(1:4,9) & Tr_mod6$M6_3 %in% c(2,9) &
is.na(Tr_mod6$M6_4)) | ((Tr_mod6$ID_DELITO != "12" | Tr_mod6$M1_2 == 33 ) & is.na(Tr_mod6$M6_1) & is.na(Tr_mod6$M6_2) &
is.na(Tr_mod6$M6_3) & is.na(Tr_mod6$M6_4)),"OK","MAL") 

### VALIDACIONES MODULO 7 ###
Val_M7 <- ifelse((Tr_mod7$P6_1 == 1 | Tr_mod7$P6_2 == 1 | Tr_mod7$P6_3 == 1 | Tr_mod7$P6_4 == 1) & 
Tr_mod7$M7_1 %in% c(1:5,9),"OK","MAL")

Val_M7_1_5X <- ifelse((Tr_mod7$M7_1 == 5 & Letras(Tr_mod7$M7_1_5X) >= 6 & Letras_num(Tr_mod7$M7_1_5X)) | 
(Tr_mod7$M7_1 != 5 & is.na(Tr_mod7$M7_1_5X)),"OK","MAL")


################ SALTOS #######################

Sal_P7_1 <- ifelse((Tr_del$ID_DELITO == "04" & Tr_del$M1_1 == "13" & Tr_del$M1_2 == Tr_del$CVE_ENT & is.na(Tr_del$M1_3)
& is.na(Tr_del$M1_4) & is.na(Tr_del$M1_5) & is.na(Tr_del$M1_5_5X) & is.na(Tr_del$M1_5A_1) & is.na(Tr_del$M1_5A_2) & is.na(Tr_del$M1_5A_3) &
is.na(Tr_del$M1_5A_4) & is.na(Tr_del$M1_5A_4X) & is.na(Tr_del$M1_6) & is.na(Tr_del$M1_7) & is.na(Tr_del$M1_8) &
is.na(Tr_del$M1_9A) & is.na(Tr_del$M1_9B) & is.na(Tr_del$M1_10) & is.na(Tr_del$M1_11_1) & is.na(Tr_del$M1_11_2) &
is.na(Tr_del$M1_11_3) & is.na(Tr_del$M1_11_4) & is.na(Tr_del$M1_11_5) & is.na(Tr_del$M1_12) & is.na(Tr_del$M1_13) &
is.na(Tr_del$M1_14_1) & is.na(Tr_del$M1_14_2) & is.na(Tr_del$M1_14_3) & is.na(Tr_del$M1_14_4) & is.na(Tr_del$M1_14_4X) &
is.na(Tr_del$M1_15) &
is.na(Tr_del$M1_15A) & is.na(Tr_del$M1_16) & is.na(Tr_del$M1_17_1) & is.na(Tr_del$M1_17_2) & is.na(Tr_del$M1_17_3)
& is.na(Tr_del$M1_17_4) & is.na(Tr_del$M1_17_5) & is.na(Tr_del$M1_17_6) & is.na(Tr_del$M1_17_7) & is.na(Tr_del$M1_17_8) &
is.na(Tr_del$M1_17_8X) & is.na(Tr_del$M1_17A) & !is.na(Tr_del$M1_18)) 
| (Tr_del$ID_DELITO != "04" & !is.na(Tr_del$M1_1) & !is.na(Tr_del$M1_2) & !is.na(Tr_del$M1_3) & !is.na(Tr_del$M1_4) &
!is.na(Tr_del$M1_5) & is.na(Tr_del$M1_5A_1) & is.na(Tr_del$M1_5A_2) & is.na(Tr_del$M1_5A_3) & is.na(Tr_del$M1_5A_4) &
!is.na(Tr_del$M1_6) & Tr_del$M1_7 %in% c(NA,1,2,9) & Tr_del$M1_8 %in% c(NA,1:6,9) & ((Tr_del$M1_9A > 0 & Tr_del$M1_9A <= 99) | is.na(Tr_del$M1_9A)) & 
((Tr_del$M1_9B > 0 & Tr_del$M1_9B <= 99) | is.na(Tr_del$M1_9B)) & Tr_del$M1_10 %in% c(NA,1:9) & Tr_del$M1_11_1 %in% c(NA,1:2,9) &
Tr_del$M1_11_2 %in% c(NA,1:2,9) & Tr_del$M1_11_3 %in% c(NA,1:2,9) & Tr_del$M1_11_4 %in% c(NA,1:2,9) &
Tr_del$M1_11_5 %in% c(NA,1:2,9) & Tr_del$M1_12 %in% c(NA,1:2) & Tr_del$M1_13 %in% c(NA,1:2,9) & Tr_del$M1_14_1 %in% c(NA,1:2,9) &
Tr_del$M1_14_2 %in% c(NA,1:2,9) & Tr_del$M1_14_3 %in% c(NA,1:2,9) & Tr_del$M1_14_4 %in% c(NA,1:2,9) & Tr_del$M1_15 %in% c(NA,1:2,9) &
Tr_del$M1_15A %in% c(NA,1:2,9) & Tr_del$M1_16 %in% c(NA,1:2,9) & Tr_del$M1_17_1 %in% c(NA,1:2,9) & Tr_del$M1_17_2 %in% c(NA,1:2,9)
& Tr_del$M1_17_3 %in% c(NA,1:2,9) & Tr_del$M1_17_4 %in% c(NA,1:2,9) & Tr_del$M1_17_5 %in% c(NA,1:2,9) & Tr_del$M1_17_6 %in% c(NA,1:2,9)
& Tr_del$M1_17_7 %in% c(NA,1:2,9) & Tr_del$M1_17_8 %in% c(NA,1:2,9) & Tr_del$M1_17A %in% c(NA,1:2,9) & Tr_del$M1_18 %in% c(1:2,9))
,"OK","MAL")


Sal_M1_1 <- ifelse((!is.na(Tr_del$M1_1) & Tr_del$ID_DELITO %in% c("05","10","13") & Tr_del$E23 == "U" & Tr_del$M1_5 == 2 & Tr_del$M1_2 == Tr_del$CVE_ENT) 
| (!is.na(Tr_del$M1_1) & Tr_del$ID_DELITO %in% c("05","10","13") & Tr_del$E23 != "U" & !Tr_del$E17 %in% c("23","48","49") & Tr_del$M1_2 == Tr_del$CVE_ENT & Tr_del$M1_5 == 2) 
| (!is.na(Tr_del$M1_1) & !Tr_del$ID_DELITO %in% c("05","10","13") & 
Tr_del$M1_2 %in% c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20",
"21","22","23","24","25","26","27","28","29","30","31","32","33")),"OK","MAL")


Sal_M1_2.0 <- ifelse((Tr_del$M1_2 == 33 & !is.na(Tr_del$M1_2X) & rowSums(Tr_del[, M1_1_33_numeric], na.rm = TRUE) == 0 & 
rowSums(!is.na(Tr_del[, M1_1_33_character[-1]]) & trimws(Tr_del[, M1_1_33_character[-1]]) != "") == 0) |
(Tr_del$M1_2 != 33 & is.na(Tr_del$M1_2X) & rowSums(Tr_del[, M1_1_33_numeric], na.rm = TRUE) != 0), "OK", "MAL")

Sal_M1_2 <- ifelse((is.na(Sal_M1_2.0)),"MAL",Sal_M1_2.0) 

Sal_M1_5 <- ifelse((Tr_del$M1_5 != 5 & is.na(Tr_del$M1_5X)) | (Tr_del$M1_5 == 5 & Letras(Tr_del$M1_5X) >= 6 & Letras_num(Tr_del$M1_5X)) |
(is.na(Tr_del$M1_5) & is.na(Tr_del$M1_5X)),"OK","MAL")

Sal_M1_5A <- ifelse((Tr_del$M1_5A_4 != 1 & is.na(Tr_del$M1_5A_4X)) | (Tr_del$M1_5A_4 == 1 & Letras(Tr_del$M1_5A_4X) >= 6 & Letras_num(Tr_del$M1_5A_4X)) |
(is.na(Tr_del$M1_5A_4) & is.na(Tr_del$M1_5A_4X)),"OK","MAL")

Sal_M1_5_5X <- ifelse((!Tr_del$ID_DELITO %in% c("07","08","09","11") 
& rowSums(Tr_del[,M1_1_33_numeric[6:13]],na.rm = TRUE) == 0) | (Tr_del$ID_DELITO %in% c("07","08","09","11") &
!is.na(Tr_del$M1_5A_1) & !is.na(Tr_del$M1_5A_2) & !is.na(Tr_del$M1_5A_3) & !is.na(Tr_del$M1_5A_4) & !is.na(Tr_del$M1_6) &
(is.na(Tr_del$M1_7) | !is.na(Tr_del$M1_7)) & (is.na(Tr_del$M1_8) | !is.na(Tr_del$M1_8)) & (is.na(Tr_del$M1_9A) | !is.na(Tr_del$M1_9A)) &
(is.na(Tr_del$M1_9B) | !is.na(Tr_del$M1_9B)) & (is.na(Tr_del$M1_10) | !is.na(Tr_del$M1_10)) & (is.na(Tr_del$M1_11_1) | !is.na(Tr_del$M1_11_1)) &
(is.na(Tr_del$M1_11_2) | !is.na(Tr_del$M1_11_2)) & (is.na(Tr_del$M1_11_3) | !is.na(Tr_del$M1_11_3)) & (is.na(Tr_del$M1_11_4) | !is.na(Tr_del$M1_11_4)) &
(is.na(Tr_del$M1_11_5) | !is.na(Tr_del$M1_11_5)) & (is.na(Tr_del$M1_12) | !is.na(Tr_del$M1_12)) | 
(Tr_del$M1_2 == 33 & rowSums(Tr_del[,M1_1_33_numeric[6:13]],na.rm = TRUE) == 0)),"OK","MAL")

Sal_M1_6 <- ifelse((Tr_del$M1_6 %in% c(2,9) & rowSums(Tr_del[,M1_1_33_numeric[7:28]],na.rm = TRUE) == 0 & 
is.na(Tr_del$M1_14_4X) & is.na(Tr_del$M1_17_8X)) | (Tr_del$M1_6 %in% c(NA,1) &
Tr_del$M1_7 %in% c(NA,1,2,9) & Tr_del$M1_8 %in% c(NA,1:6,9) & (!is.na(Tr_del$M1_9A) | is.na(Tr_del$M1_9A)) &
(!is.na(Tr_del$M1_9B) | is.na(Tr_del$M1_9B)) & Tr_del$M1_10 %in% c(NA,1:9) & Tr_del$M1_11_1 %in% c(NA,1,2,9)  
& Tr_del$M1_11_2 %in% c(NA,1,2,9) & Tr_del$M1_11_3 %in% c(NA,1,2,9) & Tr_del$M1_11_4 %in% c(NA,1,2,9)
& Tr_del$M1_11_5 %in% c(NA,1,2,9) & Tr_del$M1_12 %in% c(NA,1,2) & Tr_del$M1_13 %in% c(NA,1,2,9)
& Tr_del$M1_14_1 %in% c(NA,1,2,9) & Tr_del$M1_14_2 %in% c(NA,1,2,9) & Tr_del$M1_14_3 %in% c(NA,1,2,9)
& Tr_del$M1_14_4 %in% c(NA,1,2,9) & (!is.na(Tr_del$M1_14_4X) | is.na(Tr_del$M1_14_4X)) 
& Tr_del$M1_15 %in% c(NA,1,2,9) & Tr_del$M1_15A %in% c(NA,1,2,9) & Tr_del$M1_16 %in% c(NA,1,2,9) &
Tr_del$M1_17_1 %in% c(NA,1,2,9) & Tr_del$M1_17_2 %in% c(NA,1,2,9) & Tr_del$M1_17_3 %in% c(NA,1,2,9) &
Tr_del$M1_17_4 %in% c(NA,1,2,9) & Tr_del$M1_17_5 %in% c(NA,1,2,9) & Tr_del$M1_17_6 %in% c(NA,1,2,9) &
Tr_del$M1_17_7 %in% c(NA,1,2,9) & Tr_del$M1_17_8 %in% c(NA,1,2,9) & (!is.na(Tr_del$M1_17_8X) | is.na(Tr_del$M1_17_8X)) &
Tr_del$M1_17A %in% c(NA,1,2,9)),"OK","MAL")

Sal_M1_7 <- ifelse((Tr_del$M1_7 %in% c(2,9) & rowSums(Tr_del[,M1_1_33_numeric[8:28]],na.rm = TRUE) == 0 &
is.na(Tr_del$M1_14_4X) & is.na(Tr_del$M1_17_8X)) | 
(Tr_del$M1_7 %in% c(NA,1,2,9) & Tr_del$M1_8 %in% c(NA,1:6,9) & (!is.na(Tr_del$M1_9A) | is.na(Tr_del$M1_9A)) &
(!is.na(Tr_del$M1_9B) | is.na(Tr_del$M1_9B)) & Tr_del$M1_10 %in% c(NA,1:9) & Tr_del$M1_11_1 %in% c(NA,1,2,9)  
& Tr_del$M1_11_2 %in% c(NA,1,2,9) & Tr_del$M1_11_3 %in% c(NA,1,2,9) & Tr_del$M1_11_4 %in% c(NA,1,2,9)
& Tr_del$M1_11_5 %in% c(NA,1,2,9) & Tr_del$M1_12 %in% c(NA,1,2) & Tr_del$M1_13 %in% c(NA,1,2,9)
& Tr_del$M1_14_1 %in% c(NA,1,2,9) & Tr_del$M1_14_2 %in% c(NA,1,2,9) & Tr_del$M1_14_3 %in% c(NA,1,2,9)
& Tr_del$M1_14_4 %in% c(NA,1,2,9) & (!is.na(Tr_del$M1_14_4X) | is.na(Tr_del$M1_14_4X)) 
& Tr_del$M1_15 %in% c(NA,1,2,9) & Tr_del$M1_15A %in% c(NA,1,2,9) & Tr_del$M1_16 %in% c(NA,1,2,9) &
Tr_del$M1_17_1 %in% c(NA,1,2,9) & Tr_del$M1_17_2 %in% c(NA,1,2,9) & Tr_del$M1_17_3 %in% c(NA,1,2,9) &
Tr_del$M1_17_4 %in% c(NA,1,2,9) & Tr_del$M1_17_5 %in% c(NA,1,2,9) & Tr_del$M1_17_6 %in% c(NA,1,2,9) &
Tr_del$M1_17_7 %in% c(NA,1,2,9) & Tr_del$M1_17_8 %in% c(NA,1,2,9) & (!is.na(Tr_del$M1_17_8X) | is.na(Tr_del$M1_17_8X)) &
Tr_del$M1_17A %in% c(NA,1,2,9)),"OK","MAL")

Sal_M1_8 <- ifelse((Tr_del$M1_8 == 9 & rowSums(Tr_del[,M1_1_33_numeric[8:10]],na.rm = TRUE) == 0) | 
(Tr_del$M1_8 %in% c(NA,1:6) & rowSums(Tr_del[,M1_1_33_numeric[8:10]],na.rm = TRUE) > 0),"OK","MAL")

Sal_M1_9A <- ifelse((Tr_del$M1_9A == 99 & is.na(Tr_del$M1_9B)) | 
(Tr_del$M1_9A != 99 & Tr_del$M1_9B >= 0) |
(is.na(Tr_del$M1_9A) & is.na(Tr_del$M1_9B)),"OK","MAL")

Sal_M1_11_5 <- ifelse((Tr_del$M1_11_5 == 1 & is.na(Tr_del$M1_12)) | (Tr_del$M1_11_5 != 1 & Tr_del$M1_12 %in% c(1,2)) |
(is.na(Tr_del$M1_11_5) & is.na(Tr_del$M1_12)),"OK","MAL")

Sal_M1_12 <- ifelse((Tr_del$ID_DELITO %in% c("08","09","10","16") & rowSums(Tr_del[,M1_1_33_numeric[14:28]],na.rm = TRUE) == 0 &
is.na(Tr_del$M1_14_4X) & is.na(Tr_del$M1_17_8X)) | (!Tr_del$ID_DELITO %in% c("04","08","09","10","16") & Tr_del$M1_13 %in% c(NA,1,2,9) &
Tr_del$M1_14_1 %in% c(NA,1,2,9) & Tr_del$M1_14_2 %in% c(NA,1,2,9) & Tr_del$M1_14_3 %in% c(NA,1,2,9) & Tr_del$M1_14_4 %in% c(NA,1,2,9) &
(!is.na(Tr_del$M1_14_4X) | is.na(Tr_del$M1_14_4X)) & Tr_del$M1_15 %in% c(NA,1,2,9) & Tr_del$M1_15A %in% c(NA,1,2,9) &
Tr_del$M1_16 %in% c(NA,1,2,9) & Tr_del$M1_17_1 %in% c(NA,1,2,9) & Tr_del$M1_17_2 %in% c(NA,1,2,9) & Tr_del$M1_17_3 %in% c(NA,1,2,9) &
Tr_del$M1_17_4 %in% c(NA,1,2,9) & Tr_del$M1_17_5 %in% c(NA,1,2,9) & Tr_del$M1_17_6 %in% c(NA,1,2,9) & Tr_del$M1_17_7 %in% c(NA,1,2,9) &
Tr_del$M1_17_8 %in% c(NA,1,2,9) & (!is.na(Tr_del$M1_17_8X) | is.na(Tr_del$M1_17_8X)) & Tr_del$M1_17A %in% c(NA,1,2,9)),"OK","MAL")

Sal_M1_13 <- ifelse((Tr_del$M1_13 != 1 & rowSums(Tr_del[,M1_1_33_numeric[15:19]],na.rm = TRUE) == 0 & 
is.na(Tr_del$M1_14_4X)) | 
(Tr_del$M1_13 == 1 & Tr_del$M1_14_1 %in% c(1,2,9) & Tr_del$M1_14_2 %in% c(1,2,9) &
Tr_del$M1_14_3 %in% c(1,2,9) & Tr_del$M1_14_4 %in% c(1,2,9) & Tr_del$M1_15 %in% c(1,2,9) & Tr_del$M1_15A %in% c(NA,1,2,9)) |
(rowSums(Tr_del[,M1_1_33_numeric[14:19]],na.rm = TRUE) == 0),"OK","MAL")

Sal_M1_14_4 <- ifelse((Tr_del$M1_14_4 == 1 & Letras(Tr_del$M1_14_4X) >= 6 & Letras_num(Tr_del$M1_14_4X)) &
(Tr_del$M1_14_4 != 1 & is.na(Tr_del$M1_14_4X)) | (is.na(Tr_del$M1_14_4) & is.na(Tr_del$M1_14_4X)),"OK","MAL")

Sal_M1_15 <- ifelse((Tr_del$M1_15 == 1 & is.na(Tr_del$M1_15A) & is.na(Tr_del$M1_16)) | (Tr_del$M1_15 != 1 & !is.na(Tr_del$M1_15A) &
!is.na(Tr_del$M1_16)) | (is.na(Tr_del$M1_15) & is.na(Tr_del$M1_15A) & is.na(Tr_del$M1_16)),"OK","MAL")

Sal_M1_16 <- ifelse((Tr_del$M1_16 %in% c(2,9) & rowSums(Tr_del[,M1_1_33_numeric[21:28]],na.rm = TRUE) == 0 &
is.na(Tr_del$M1_17_8X)) | (Tr_del$M1_16 == 1 & !is.na(Tr_del$M1_17_1) & !is.na(Tr_del$M1_17_2)
& !is.na(Tr_del$M1_17_3) & !is.na(Tr_del$M1_17_4) & !is.na(Tr_del$M1_17_5) & !is.na(Tr_del$M1_17_6)
& !is.na(Tr_del$M1_17_7) & !is.na(Tr_del$M1_17_8)) | (is.na(Tr_del$M1_16) & is.na(Tr_del$M1_17_1) & is.na(Tr_del$M1_17_2)
& is.na(Tr_del$M1_17_3) & is.na(Tr_del$M1_17_4) & is.na(Tr_del$M1_17_5) & is.na(Tr_del$M1_17_6) & is.na(Tr_del$M1_17_7)
& is.na(Tr_del$M1_17_8) & is.na(Tr_del$M1_17_8X)),"OK","MAL")

Sal_M1_17_8 <- ifelse((Tr_del$M1_17_8 == 1 & !is.na(Tr_del$M1_17_8X)) | (Tr_del$M1_17_8 != 1 & is.na(Tr_del$M1_17_8X)) |
(is.na(Tr_del$M1_17_8) & is.na(Tr_del$M1_17_8X)),"OK","MAL")

Sal_M1_17_8X <- ifelse((Tr_del$M1_16 == 1 & Tr_del$M1_17_1 %in% c(1,2,9) &Tr_del$M1_17_2 %in% c(1,2,9) & Tr_del$M1_17_3 %in% c(1,2,9) & Tr_del$M1_17_4 %in% c(1,2,9) &
                          Tr_del$M1_17_5 %in% c(1,2,9) & Tr_del$M1_17_6 %in% c(1,2,9) & Tr_del$M1_17_7 %in% c(1,2,9) & Tr_del$M1_17_8 %in% c(1,2,9) & is.na(Tr_del$M1_17A)) |
                         (Tr_del$M1_16 %in% c(2,9) & is.na(Tr_del$M1_17_1) & is.na(Tr_del$M1_17_2) & is.na(Tr_del$M1_17_3) & is.na(Tr_del$M1_17_4) & is.na(Tr_del$M1_17_5)
                       & is.na(Tr_del$M1_17_6) & is.na(Tr_del$M1_17_7) & is.na(Tr_del$M1_17_8) & is.na(Tr_del$M1_17_8X) & !is.na(Tr_del$M1_17A)) | (is.na(Tr_del$M1_16) & is.na(Tr_del$M1_17_1) & 
                        is.na(Tr_del$M1_17_2) & is.na(Tr_del$M1_17_3) & is.na(Tr_del$M1_17_4) & is.na(Tr_del$M1_17_5) & is.na(Tr_del$M1_17_6) & is.na(Tr_del$M1_17_7) & is.na(Tr_del$M1_17_8) & is.na(Tr_del$M1_17_8X) & !is.na(Tr_del$M1_17A))
                       | (Tr_del$M1_15 == 1 & Tr_del$M1_17_1 %in% c(1,2,9) &Tr_del$M1_17_2 %in% c(1,2,9) & Tr_del$M1_17_3 %in% c(1,2,9) & Tr_del$M1_17_4 %in% c(1,2,9) &
                            Tr_del$M1_17_5 %in% c(1,2,9) & Tr_del$M1_17_6 %in% c(1,2,9) & Tr_del$M1_17_7 %in% c(1,2,9) & Tr_del$M1_17_8 %in% c(1,2,9) & is.na(Tr_del$M1_17A)),"OK","MAL")

Sal_M1_18 <- ifelse((Tr_del$M1_18 == 1 & is.na(Tr_del$M1_19) & is.na(Tr_del$M1_19X)) | (Tr_del$M1_18 == 2 &
!is.na(Tr_del$M1_19)) | (is.na(Tr_del$M1_18) & is.na(Tr_del$M1_19)),"OK","MAL")

Sal_M1_19X <- ifelse((Tr_del$M1_19 == 9 & !is.na(Tr_del$M1_19X)) | (Tr_del$M1_19 != 9 & is.na(Tr_del$M1_19X)) |
                       (is.na(Tr_del$M1_19) & is.na(Tr_del$M1_19X)),"OK","MAL")

Sal_M1_19.1 <- ifelse((!is.na(Tr_del$M1_19) & is.na(Tr_del$M1_20) & is.na(Tr_del$M1_21) & is.na(Tr_del$M1_21_7X) & is.na(Tr_del$M1_22) & is.na(Tr_del$M1_22_6X) & is.na(Tr_del$M1_23) & is.na(Tr_del$M1_23_7X) &
                         is.na(Tr_del$M1_24) & is.na(Tr_del$M1_25)) |
                        (is.na(Tr_del$M1_19) & is.na(Tr_del$M1_20) & is.na(Tr_del$M1_21) & is.na(Tr_del$M1_21_7X) & is.na(Tr_del$M1_22) & is.na(Tr_del$M1_22_6X) & is.na(Tr_del$M1_23) & is.na(Tr_del$M1_23_7X) &
                           is.na(Tr_del$M1_24) & is.na(Tr_del$M1_25)) |
                        (is.na(Tr_del$M1_19) & !is.na(Tr_del$M1_20)),"OK","MAL")

Sal_M1_19 <- ifelse((Tr_del$M1_18 %in% c(9) & rowSums(Tr_del[,M1_1_33_numeric[30:36]],na.rm = TRUE) == 0 &
is.na(Tr_del$M1_19X) & is.na(Tr_del$M1_21X) & is.na(Tr_del$M1_23X)) | (Tr_del$M1_18 == 2 & !is.na(Tr_del$M1_19)
& rowSums(Tr_del[,M1_1_33_numeric[31:36]],na.rm = TRUE) == 0) |
(Tr_del$M1_18 == 1 & is.na(Tr_del$M1_19) & is.na(Tr_del$M1_19X)) | (is.na(Tr_del$M1_18) & is.na(Tr_del$M1_19) & is.na(Tr_del$M1_19X)),"OK","MAL")

Sal_M1_20 <- ifelse((Tr_del$M1_20 == 2 & is.na(Tr_del$M1_21)) | (Tr_del$M1_20 == 9 & is.na(Tr_del$M1_21) & is.na(Tr_del$M1_22)) |
(Tr_del$M1_20 == 1 & !is.na(Tr_del$M1_21) & is.na(Tr_del$M1_22) & is.na(Tr_del$M1_22X)) | 
(is.na(Tr_del$M1_20) & is.na(Tr_del$M1_21) & is.na(Tr_del$M1_21X) & is.na(Tr_del$M1_22) & is.na(Tr_del$M1_22X)),"OK","MAL")

Sal_M1_21 <- ifelse((Tr_del$M1_21 == 7 & Letras_num(Tr_del$M1_21_7X) & Letras(Tr_del$M1_27) >= 6) |
                      (Tr_del$M1_21 != 7 & is.na(Tr_del$M1_21_7X)) | (is.na(Tr_del$M1_21) & is.na(Tr_del$M1_21_7X)),"OK","MAL")


Sal_M1_22 <- ifelse((Tr_del$M1_22 == 6 & !is.na(Tr_del$M1_22X)) | (Tr_del$M1_22 != 6 & is.na(Tr_del$M1_22X)) | 
(is.na(Tr_del$M1_22) & is.na(Tr_del$M1_22X)),"OK","MAL")

Sal_M1_23 <- ifelse((Tr_del$M1_23 == 7 & !is.na(Tr_del$M1_23X)) | (Tr_del$M1_23 != 7 & is.na(Tr_del$M1_23X)) | 
(is.na(Tr_del$M1_23) & is.na(Tr_del$M1_23X)),"OK","MAL")

Sal_M1_26 <- ifelse((Tr_del$M1_26 %in% c(2,9) & rowSums(Tr_del[,M1_1_33_numeric[38:47]],na.rm = TRUE) == 0 & 
is.na(Tr_del$M1_27_11X)) | (Tr_del$M1_26 == 1 & Tr_del$M1_27_1 %in% c(1,2) & Tr_del$M1_27_2 %in% c(1,2) &
Tr_del$M1_27_3 %in% c(1,2) & Tr_del$M1_27_4 %in% c(1,2) & Tr_del$M1_27_5 %in% c(1,2) & 
Tr_del$M1_27_6 %in% c(1,2) & Tr_del$M1_27_7 %in% c(1,2) & Tr_del$M1_27_8 %in% c(1,2) &
Tr_del$M1_27_9 %in% c(1,2) & Tr_del$M1_27_11 %in% c(1,2)) | (rowSums(Tr_del[,M1_1_33_numeric[37:47]],na.rm = TRUE) == 0 &
is.na(Tr_del$M1_27_11X)),"OK","MAL")

Sal_M1_27_9 <- ifelse((Tr_del$M1_27_9 == 1 & !is.na(Tr_del$M1_27_9X)) | (Tr_del$M1_27_9 != 1 & is.na(Tr_del$M1_27_9X)) | 
(is.na(Tr_del$M1_27_9) & is.na(Tr_del$M1_27_9X)),"OK","MAL")

Sal_M1_27 <- ifelse((Tr_del$M1_27_11 == 1 & Tr_del$M1_27_1 == 2 & Tr_del$M1_27_2 == 2 & Tr_del$M1_27_3 == 2 
& Tr_del$M1_27_4 == 2 & Tr_del$M1_27_5 == 2 & Tr_del$M1_27_6 == 2 & Tr_del$M1_27_7 == 2 & Tr_del$M1_27_8 == 2
& Tr_del$M1_27_9 == 2) | 
((Tr_del$M1_27_1 == 1 | Tr_del$M1_27_2 == 1 | Tr_del$M1_27_3 == 1 | Tr_del$M1_27_4 == 1 | Tr_del$M1_27_5 == 1 
| Tr_del$M1_27_6 == 1 | Tr_del$M1_27_7 == 1 | Tr_del$M1_27_8 == 1 | Tr_del$M1_27_9 == 1) & Tr_del$M1_27_11 == 2) |
(is.na(Tr_del$M1_27_1) & is.na(Tr_del$M1_27_2) & is.na(Tr_del$M1_27_3) & is.na(Tr_del$M1_27_4)
& is.na(Tr_del$M1_27_5) & is.na(Tr_del$M1_27_6) & is.na(Tr_del$M1_27_7) & is.na(Tr_del$M1_27_8)
& is.na(Tr_del$M1_27_9) & is.na(Tr_del$M1_27_11)),"OK","MAL")

Sal_M1_27_11 <- ifelse(((Tr_del$M1_18 == 1 | Tr_del$M1_26 == 1) & rowSums(Tr_del[,M1_1_33_numeric[48:53]],na.rm = TRUE) == 0 &
is.na(Tr_del$M1_28_6X) & !is.na(Tr_del$M1_29_1)) | ((Tr_del$M1_18 != 1 | Tr_del$M1_26 != 1) & !is.na(Tr_del$M1_28_1) & !is.na(Tr_del$M1_28_2) &
!is.na(Tr_del$M1_28_3) & !is.na(Tr_del$M1_28_4) & !is.na(Tr_del$M1_28_5) & !is.na(Tr_del$M1_28_6) &
!is.na(Tr_del$M1_28_6X)) | (rowSums(Tr_del[,M1_1_33_numeric[47:53]],na.rm = TRUE) == 0 & is.na(Tr_del$M1_28_6X)),"OK","MAL")

Sal_M1_28_5 <- ifelse((Tr_del$M1_28_5 == 1 & !is.na(Tr_del$M1_28_5X)) | (Tr_del$M1_28_5 != 1 & is.na(Tr_del$M1_28_5X)) |
(is.na(Tr_del$M1_28_5) & is.na(Tr_del$M1_28_5X)),"OK","MAL")

Sal_M1_27.2 <- ifelse((Tr_del$M1_28_6 == 1 & Tr_del$M1_28_1 == 2 & Tr_del$M1_28_2 == 2 & Tr_del$M1_28_3 == 2 
& Tr_del$M1_28_4 == 2 & Tr_del$M1_28_5 == 2) | ((Tr_del$M1_28_1 == 1 | Tr_del$M1_28_2 == 1 | Tr_del$M1_28_3 == 1 
| Tr_del$M1_28_4 == 1 | Tr_del$M1_28_5 == 1) &  Tr_del$M1_28_6 == 2) | (is.na(Tr_del$M1_28_1) & is.na(Tr_del$M1_28_2) &
is.na(Tr_del$M1_28_3) & is.na(Tr_del$M1_28_4) & is.na(Tr_del$M1_28_5) & is.na(Tr_del$M1_28_6) & is.na(Tr_del$M1_28_6X)),"OK","MAL")

Sal_M1_29_5 <- ifelse((Tr_del$M1_29_5 == 1 & !is.na(Tr_del$M1_29_5X)) | (Tr_del$M1_29_5 != 1 & is.na(Tr_del$M1_29_5X)) |
(is.na(Tr_del$M1_29_5) & is.na(Tr_del$M1_29_5X)),"OK","MAL")

Sal_M1_29_5.1 <- ifelse((Tr_del$M1_29_1 == 2 & Tr_del$M1_29_2 == 2 & Tr_del$M1_29_3 == 2 & Tr_del$M1_29_4 == 2 &
                           Tr_del$M1_29_5 == 2 & Tr_del$M1_29_6 == 1 & is.na(Tr_del$M1_30) & is.na(Tr_del$M1_31) & is.na(Tr_del$M1_31X)) | 
                          (Tr_del$M1_29_1 == 9 & Tr_del$M1_29_2 == 9 & Tr_del$M1_29_3 == 9 & Tr_del$M1_29_4 == 9 & 
                           Tr_del$M1_29_5 == 2 & is.na(Tr_del$M1_30) & is.na(Tr_del$M1_31) & is.na(Tr_del$M1_31X)) |
                          ((Tr_del$M1_29_1 == 1 | Tr_del$M1_29_2 == 1 | Tr_del$M1_29_3 == 1 | Tr_del$M1_29_4 == 1 |
                             Tr_del$M1_29_5 == 1) & Tr_del$M1_29_6 == 2 & Tr_del$M1_30 %in% c(1:6) & !is.na(Tr_del$M1_31) & !is.na(Tr_del$M1_31X)) |
                          ((Tr_del$M1_29_1 == 1 | Tr_del$M1_29_2 == 1 | Tr_del$M1_29_3 == 1 | Tr_del$M1_29_4 == 1 |
                              Tr_del$M1_29_5 == 1) & Tr_del$M1_29_6 == 2 & Tr_del$M1_30 %in% c(7,9) & is.na(Tr_del$M1_31) & is.na(Tr_del$M1_31X)) |
                          (Tr_del$M1_2 == 33 & is.na(Tr_del$M1_29_1) & is.na(Tr_del$M1_29_2) & is.na(Tr_del$M1_29_3) & is.na(Tr_del$M1_29_4) & is.na(Tr_del$M1_29_5) &
                             is.na(Tr_del$M1_29_6) & is.na(Tr_del$M1_29_5X) & is.na(Tr_del$M1_30) & is.na(Tr_del$M1_31) & is.na(Tr_del$M1_31X)) |
                          (Tr_del$ID_DELITO == "16" & !is.na(Tr_del$M1_29_1) & !is.na(Tr_del$M1_29_2) & !is.na(Tr_del$M1_29_3) & !is.na(Tr_del$M1_29_4) & !is.na(Tr_del$M1_29_5) &
                             !is.na(Tr_del$M1_29_6) & is.na(Tr_del$M1_30) & is.na(Tr_del$M1_31) & is.na(Tr_del$M1_31X)),"OK","MAL")

Sal_M1_29_6 <- ifelse(((Tr_del$M1_29_6 == 1 | Tr_del$ID_DELITO == "16") & is.na(Tr_del$M1_30) & is.na(Tr_del$M1_31) &
is.na(Tr_del$M1_31X)) | ((Tr_del$M1_29_6 != 1 | Tr_del$ID_DELITO != 16) & !is.na(Tr_del$M1_30) & !is.na(Tr_del$M1_31) &
!is.na(Tr_del$M1_31X)) | (is.na(Tr_del$M1_29_6) & is.na(Tr_del$M1_30) & is.na(Tr_del$M1_31) & is.na(Tr_del$M1_31X)),"OK","MAL")  


Sal_M1_29_4 <- ifelse((Tr_del$ID_DELITO == "16" & is.na(Tr_del$M1_30) & is.na(Tr_del$M1_31) & is.na(Tr_del$M1_31X)) |
(Tr_del$ID_DELITO != "16" & Tr_del$M1_30 %in% c(1:6) & !is.na(Tr_del$M1_31) & !is.na(Tr_del$M1_31X)) | 
(Tr_del$ID_DELITO != "16" & Tr_del$M1_30 %in% c(7,9) & is.na(Tr_del$M1_31) & is.na(Tr_del$M1_31X)) | 
(Tr_del$M1_2 == 33 & is.na(Tr_del$M1_30) & is.na(Tr_del$M1_31) & is.na(Tr_del$M1_31X)),"OK","MAL")


Sal_M1_30 <- ifelse((Tr_del$M1_30 %in% c(7,9) & is.na(Tr_del$M1_31) & is.na(Tr_del$M1_31X)) | 
(!Tr_del$M1_30 %in% c(7,9) & !is.na(Tr_del$M1_31) & !is.na(Tr_del$M1_31X)) |
(is.na(Tr_del$M1_30) & is.na(Tr_del$M1_31) & is.na(Tr_del$M1_31X)),"OK","MAL")

Sal_M1_31 <- ifelse((Tr_del$M1_31 == 9 & Tr_del$M1_31X == "NO SABE / NO RESPONDE") | (Tr_del$M1_31 != 9 & Tr_del$M1_31X != "NO SABE / NO RESPONDE") |
(is.na(Tr_del$M1_31) & is.na(Tr_del$M1_31X)),"OK","MAL")

Sal_M1_31X <- ifelse((is.na(Tr_del$M1_31) & is.na(Tr_del$M1_31X)) | (!is.na(Tr_del$M1_31) & !is.na(Tr_del$M1_31X)),"OK","MAL")

Sal_M1_32_10 <- ifelse((Tr_del$M1_32_10 != 1 & is.na(Tr_del$M1_32_10X)) | (Tr_del$M1_32_10 == 1 & !is.na(Tr_del$M1_32_10X)) |
(is.na(Tr_del$M1_32_10) & is.na(Tr_del$M1_32_10X)),"OK","MAL")

Sal_M1_32_10X <- ifelse(((Tr_del$M1_32_1 == 1 | Tr_del$M1_32_2 == 1 | Tr_del$M1_32_3 == 1 | Tr_del$M1_32_4 == 1 | 
Tr_del$M1_32_5 == 1 | Tr_del$M1_32_6 == 1 | Tr_del$M1_32_7 == 1 | Tr_del$M1_32_8 == 1 | Tr_del$M1_32_9 == 1 |
Tr_del$M1_32_10 == 1) & Tr_del$M1_32_11 == 2) | (Tr_del$M1_32_11 == 1 & Tr_del$M1_32_1 == 2 & Tr_del$M1_32_2 == 2
& Tr_del$M1_32_3 == 2 & Tr_del$M1_32_4 == 2 & Tr_del$M1_32_5 == 2 & Tr_del$M1_32_6 == 2 & Tr_del$M1_32_7 == 2
& Tr_del$M1_32_8 == 2 & Tr_del$M1_32_9 == 2 & Tr_del$M1_32_10 == 2) | 
(rowSums(Tr_del[,M1_1_33_numeric[57:67]],na.rm = TRUE) == 0),"OK","MAL")


Sal_M1_31X.1 <- ifelse((Tr_del$CONSEC == 1 & !is.na(Tr_del$M1_32_1) & !is.na(Tr_del$M1_32_2) & !is.na(Tr_del$M1_32_3)
                      & !is.na(Tr_del$M1_32_4) & !is.na(Tr_del$M1_32_5) & !is.na(Tr_del$M1_32_6) & !is.na(Tr_del$M1_32_7)
                      & !is.na(Tr_del$M1_32_8) & !is.na(Tr_del$M1_32_9) & !is.na(Tr_del$M1_32_10) & !is.na(Tr_del$M1_32_11)) |
                       (Tr_del$CONSEC != 1 & is.na(Tr_del$M1_32_1) & is.na(Tr_del$M1_32_2) & is.na(Tr_del$M1_32_3)
                        & is.na(Tr_del$M1_32_4) & is.na(Tr_del$M1_32_5) & is.na(Tr_del$M1_32_6) & is.na(Tr_del$M1_32_7)
                        & is.na(Tr_del$M1_32_8) & is.na(Tr_del$M1_32_9) & is.na(Tr_del$M1_32_10) & is.na(Tr_del$M1_32_11)),"OK","MAL")

Sal_M1_32_11 <- ifelse(((Tr_del$ID_DELITO != "01" | Tr_del$P4_1B == 0) & is.na(Tr_del$M2_1) & is.na(Tr_del$M2_2)) | 
((Tr_del$ID_DELITO == "01" & Tr_del$P4_1B > 0) & !is.na(Tr_del$M2_1) & !is.na(Tr_del$M2_2))
| ((Tr_del$ID_DELITO == "01" & Tr_del$P4_1B > 0) & Tr_del$M2_1 %in% c(0,99) & is.na(Tr_del$M2_2)) |
((Tr_del$ID_DELITO == "01" & Tr_del$P4_1B > 0) & !Tr_del$M2_1 %in% c(0,99) & !is.na(Tr_del$M2_2)),"OK","MAL")

Sal_M2_1 <- ifelse((Tr_del$M2_1 %in% c(0,99) & is.na(Tr_del$M2_2)) | (!Tr_del$M2_1 %in% c(0,99) & !is.na(Tr_del$M2_2)) |
                     (Tr_del$ID_DELITO != "01" & is.na(Tr_del$M2_1) & is.na(Tr_del$M2_2)) | (Tr_del$M1_2 == 33 & Tr_del$ID_DELITO == "33" &
                                                                                               is.na(Tr_del$M2_1) & is.na(Tr_del$M2_2)),"OK","MAL")

Sal_M2_2 <- ifelse((!Tr_del$ID_DELITO %in% c("05","06") & rowSums(Tr_del[,M1_1_33_numeric[70:77]],na.rm = TRUE) == 0 & is.na(Tr_del$M3_1_8X)) |
(Tr_del$ID_DELITO %in% c("05","06") & !is.na(Tr_del$M3_1_1) & !is.na(Tr_del$M3_1_2) & !is.na(Tr_del$M3_1_3) & 
!is.na(Tr_del$M3_1_3) & !is.na(Tr_del$M3_1_4) & !is.na(Tr_del$M3_1_5) & !is.na(Tr_del$M3_1_6) & !is.na(Tr_del$M3_1_7) & !is.na(Tr_del$M3_1_8)) | 
(Tr_del$ID_DELITO %in% c("05","06") & Tr_del$M1_2 == 33 & rowSums(Tr_del[,M1_1_33_numeric[70:77]],na.rm = TRUE) == 0 & is.na(Tr_del$M3_1_8X)),"OK","MAL")

Sal_M3_1_8 <- ifelse((Tr_del$M3_1_8 != 1 & is.na(Tr_del$M3_1_8X)) | (Tr_del$M3_1_8 == 1 & !is.na(Tr_del$M3_1_8X)) |
(is.na(Tr_del$M3_1_8) & is.na(Tr_del$M3_1_8X)),"OK","MAL")

Sal_M3_1_8X <- ifelse((!Tr_del$ID_DELITO %in% c("07","08","09") & is.na(Tr_del$M4_1)) | (Tr_del$ID_DELITO %in% c("07","08","09") & !is.na(Tr_del$M4_1)) |
(Tr_del$ID_DELITO %in% c("07","08","09") & Tr_del$M1_2 == 33 &is.na(Tr_del$M4_1)),"OK","MAL")

Sal_M4_1 <- ifelse((Tr_del$M4_1 == 8 & !is.na(Tr_del$M4_1X)) | (Tr_del$M4_1 != 8 & !is.na(Tr_del$M4_1X)) | 
(is.na(Tr_del$M4_1) & is.na(Tr_del$M4_1X)),"OK","MAL")

Sal_M4_1_8X <- ifelse(
(Tr_del$ID_DELITO != "11" & is.na(Tr_del$M5_1) & is.na(Tr_del$M5_1_8X) & is.na(Tr_del$M5_1A_1) &
is.na(Tr_del$M5_1A_2) & is.na(Tr_del$M5_1A_3) & is.na(Tr_del$M5_1A_4) & is.na(Tr_del$M5_1A_4X) 
& is.na(Tr_del$M5_2_1) & is.na(Tr_del$M5_2_2) & is.na(Tr_del$M5_2_3) &
is.na(Tr_del$M5_2_4) & is.na(Tr_del$M5_2_4X) & is.na(Tr_del$M5_3) & is.na(Tr_del$M5_4)) 

| 
(Tr_del$ID_DELITO == "11" & !is.na(Tr_del$M5_1) & Tr_del$M1_5A_3 == 1 & !is.na(Tr_del$M5_1A_1) & 
!is.na(Tr_del$M5_1A_2) & !is.na(Tr_del$M5_1A_3) & !is.na(Tr_del$M5_1A_4) & !is.na(Tr_del$M5_2_1) 
& !is.na(Tr_del$M5_2_2) & Tr_del$M5_2_3 != 1 & !is.na(Tr_del$M5_2_4) & Tr_del$M5_3 == 2 & !is.na(Tr_del$M5_4)) 
| 
(Tr_del$ID_DELITO == "11" & !is.na(Tr_del$M5_1) & Tr_del$M1_5A_3 == 1 & !is.na(Tr_del$M5_1A_1) & 
!is.na(Tr_del$M5_1A_2) & !is.na(Tr_del$M5_1A_3) & !is.na(Tr_del$M5_1A_4) & !is.na(Tr_del$M5_2_1) 
& !is.na(Tr_del$M5_2_2) & Tr_del$M5_2_3 != 1 & !is.na(Tr_del$M5_2_4) & Tr_del$M5_3 != 2 & is.na(Tr_del$M5_4))
|
(Tr_del$ID_DELITO == "11" & !is.na(Tr_del$M5_1) & Tr_del$M1_5A_3 == 1 & !is.na(Tr_del$M5_1A_1) & 
!is.na(Tr_del$M5_1A_2) & !is.na(Tr_del$M5_1A_3) & !is.na(Tr_del$M5_1A_4) & !is.na(Tr_del$M5_2_1) 
& !is.na(Tr_del$M5_2_2) & Tr_del$M5_2_3 == 1 & !is.na(Tr_del$M5_2_4) & is.na(Tr_del$M5_3) & is.na(Tr_del$M5_4))
|
(Tr_del$ID_DELITO == "11" & !is.na(Tr_del$M5_1) & Tr_del$M1_5A_3 != 1 & is.na(Tr_del$M5_1A_1) &
is.na(Tr_del$M5_1A_2) & is.na(Tr_del$M5_1A_3) & is.na(Tr_del$M5_1A_4) & !is.na(Tr_del$M5_2_1) &
!is.na(Tr_del$M5_2_2) & Tr_del$M5_2_3 != 1 & !is.na(Tr_del$M5_2_4) & Tr_del$M5_3 == 2 & !is.na(Tr_del$M5_4))
|
(Tr_del$ID_DELITO == "11" & !is.na(Tr_del$M5_1) & Tr_del$M1_5A_3 != 1 & is.na(Tr_del$M5_1A_1) &
is.na(Tr_del$M5_1A_2) & is.na(Tr_del$M5_1A_3) & is.na(Tr_del$M5_1A_4) & !is.na(Tr_del$M5_2_1) &
!is.na(Tr_del$M5_2_2) & Tr_del$M5_2_3 != 1 & !is.na(Tr_del$M5_2_4) & Tr_del$M5_3 != 2 & is.na(Tr_del$M5_4))
|
(Tr_del$ID_DELITO == "11" & !is.na(Tr_del$M5_1) & Tr_del$M1_5A_3 != 1 & is.na(Tr_del$M5_1A_1) &
is.na(Tr_del$M5_1A_2) & is.na(Tr_del$M5_1A_3) & is.na(Tr_del$M5_1A_4) & !is.na(Tr_del$M5_2_1) &
!is.na(Tr_del$M5_2_2) & Tr_del$M5_2_3 == 1 & !is.na(Tr_del$M5_2_4) & is.na(Tr_del$M5_3) & is.na(Tr_del$M5_4))
|
(Tr_del$M1_2 == 33 & is.na(Tr_del$M5_1) & is.na(Tr_del$M5_1A_1) & is.na(Tr_del$M5_1A_2) & is.na(Tr_del$M5_1A_3) &
is.na(Tr_del$M5_1A_4) & is.na(Tr_del$M5_1A_4X) & is.na(Tr_del$M5_2_1) & is.na(Tr_del$M5_2_2) & is.na(Tr_del$M5_2_3) &
is.na(Tr_del$M5_2_4) & is.na(Tr_del$M5_2_4X) & is.na(Tr_del$M5_3) & is.na(Tr_del$M5_4)),"OK","MAL")

Sal_M5_1 <- ifelse((Tr_del$M1_5A_3 == 1 & !is.na(Tr_del$M5_1A_1) & !is.na(Tr_del$M5_1A_2) & !is.na(Tr_del$M5_1A_3)
                    & !is.na(Tr_del$M5_1A_4)) | (Tr_del$M1_5A_3 != 1 & is.na(Tr_del$M5_1A_1) & is.na(Tr_del$M5_1A_2) & is.na(Tr_del$M5_1A_3) &
                                                 is.na(Tr_del$M5_1A_4) & is.na(Tr_del$M5_1A_4X)) |
                     (is.na(Tr_del$M1_5A_3) & is.na(Tr_del$M5_1A_1) & is.na(Tr_del$M5_1A_2) & is.na(Tr_del$M5_1A_3) &
                      is.na(Tr_del$M5_1A_4) & is.na(Tr_del$M5_1A_4X)),"OK","MAL")

Sal_M5_1_8X <- ifelse((Tr_del$M5_1 != 8 & is.na(Tr_del$M5_1_8X)) | (Tr_del$M5_1 == 8 & !is.na(Tr_del$M5_1_8X)) | 
(is.na(Tr_del$M5_1) & is.na(Tr_del$M5_1_8X)),"OK","MAL")

Sal_M5_1A_4 <- ifelse((Tr_del$M5_1A_4 == 1 & !is.na(Tr_del$M5_1A_4X)) | (Tr_del$M5_1A_4 != 1 & is.na(Tr_del$M5_1A_4X)) |
                      (is.na(Tr_del$M5_1A_4) & is.na(Tr_del$M5_1A_4X)),"OK","MAL")

Sal_M5_2_4 <- ifelse((Tr_del$M5_2_4 != 1 & is.na(Tr_del$M5_2_4X)) | (Tr_del$M5_2_4 == 1 & !is.na(Tr_del$M5_2_4X)) |
(is.na(Tr_del$M5_2_4) & is.na(Tr_del$M5_2_4X)),"OK","MAL")

Sal_M5_2_4.1 <- ifelse((Tr_del$M5_2_3 == 1 & is.na(Tr_del$M5_2_4X) & is.na(Tr_del$M5_3) & is.na(Tr_del$M5_4) & is.na(Tr_del$M5_4_1X)) |
                         (Tr_del$M5_2_3 %in% c(2,9) & Tr_del$M5_3 == 2 & !is.na(Tr_del$M5_4)) | 
                         (Tr_del$M5_2_3 %in% c(2,9) & Tr_del$M5_3 %in% c(1,9) & is.na(Tr_del$M5_4)) |
                         (is.na(Tr_del$M5_2_3) & is.na(Tr_del$M5_3)),"OK","MAL")

Sal_M5_4 <- ifelse((Tr_del$M5_4 == 1 & !is.na(Tr_del$M5_4X)) | (Tr_del$M5_4 != 1 & is.na(Tr_del$M5_4X)) |
(is.na(Tr_del$M5_4) & is.na(Tr_del$M5_4X)),"OK","MAL")

Sal_M5_4_1X <- ifelse((Tr_del$ID_DELITO != "12" & is.na(Tr_del$M6_1) & is.na(Tr_del$M6_2) &
is.na(Tr_del$M6_3) & is.na(Tr_del$M6_4)) | 
(Tr_del$ID_DELITO == "12" & !is.na(Tr_del$M6_1) &
!is.na(Tr_del$M6_2) & Tr_del$M6_3 %in% c(1,2) & !is.na(Tr_del$M6_4)) |
(Tr_del$ID_DELITO == "12" & !is.na(Tr_del$M6_1) & !is.na(Tr_del$M6_2) & Tr_del$M6_3 %in% c(3,9) & is.na(Tr_del$M6_4)) |
(Tr_del$ID_DELITO == "12" & Tr_del$M1_2 == 33 & is.na(Tr_del$M6_1) & is.na(Tr_del$M6_2) & is.na(Tr_del$M6_3) & is.na(Tr_del$M6_4)),"OK","MAL")

Sal_M6_4 <- ifelse((Tr_del$ID_DELITO != "16" & is.na(Tr_del$M7_1) & is.na(Tr_del$M7_1X)) |
(Tr_del$ID_DELITO == "16" & Tr_del$M7_1 %in% c(1:4,9) & is.na(Tr_del$M7_1X)) | 
(Tr_del$ID_DELITO == "16" & Tr_del$M7_1 == 5 & !is.na(Tr_del$M7_1X)) |
(Tr_del$M1_2 == 33 & is.na(Tr_del$M7_1) & is.na(Tr_del$M7_1X)),"OK","MAL")

# REQUERIMIENTOS FUNCIONALES #

Reqfun_M1_1 <- ifelse((Tr_del$ID_DELITO == "04" & Tr_del$M1_1 == "13") & 
(Tr_del$ID_DELITO != "04" & Tr_del$M1_1 != "13"),"OK","MAL")

Reqfun_M1_2 <- ifelse((Tr_del$ID_DELITO == "04" & Tr_del$M1_2 == Tr_del$CVE_ENT) | 
(Tr_del$ID_DELITO %in% c("05","10","13") & Tr_del$E23 == "U"& Tr_del$M1_2 == Tr_del$CVE_ENT) |
(Tr_del$ID_DELITO %in% c("05","10","13") & Tr_del$E23 != "U" & !Tr_del$E17 %in% c("23","48","49")) | 
(Tr_del$ID_DELITO != "04" & Tr_del$M1_2 %in% c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16",
"17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32")),"OK","MAL")

Reqfun_M1_5 <- ifelse((Tr_del$ID_DELITO %in% c("05","10","13") & Tr_del$E23 == "U" & Tr_del$M1_5 == 2) |
(Tr_del$ID_DELITO %in% c("05","10","13") & Tr_del$E23 != "U" & !Tr_del$E17 %in% c("23","48","49") & Tr_del$M1_5 == 2) |
(!Tr_del$ID_DELITO %in% c("05","10","13") & Tr_del$E23 != "U" & Tr_del$E17 %in% c("23","48","49") & Tr_del$M1_5 != 2) |
(!Tr_del$ID_DELITO %in% c("05","10","13") & Tr_del$E23 != "U" & Tr_del$M1_5 != 2),"OK","MAL")

Reqfun_M1_27_11 <- ifelse(((Tr_del$M1_27_1 == 1 | Tr_del$M1_27_2 == 1 | Tr_del$M1_27_3 == 1 | Tr_del$M1_27_4 == 1 |
Tr_del$M1_27_5 == 1 | Tr_del$M1_27_6 == 1 | Tr_del$M1_27_7 == 1 | Tr_del$M1_27_8 == 1 | Tr_del$M1_27_9 == 1) & Tr_del$M1_27_11 == 2) |
(Tr_del$M1_27_11 == 1 & Tr_del$M1_27_1 == 2 & Tr_del$M1_27_2 == 2 & Tr_del$M1_27_3 == 2 & Tr_del$M1_27_4 == 2 & 
Tr_del$M1_27_5 == 2 & Tr_del$M1_27_6 == 2 & Tr_del$M1_27_7 == 2 & Tr_del$M1_27_8 == 2 & Tr_del$M1_27_9 == 2) | 
(is.na(Tr_del$M1_27_1) & is.na(Tr_del$M1_27_2) & is.na(Tr_del$M1_27_3) & is.na(Tr_del$M1_27_4) & is.na(Tr_del$M1_27_5) &
is.na(Tr_del$M1_27_6) & is.na(Tr_del$M1_27_7) & is.na(Tr_del$M1_27_8) & is.na(Tr_del$M1_27_9)),"OK","MAL")

Reqfun_M1_28_6 <- ifelse(((Tr_del$M1_28_1 == 1 | Tr_del$M1_28_2 == 1 | Tr_del$M1_28_3 == 1 | Tr_del$M1_28_4 == 1 |
Tr_del$M1_28_5 == 1) & Tr_del$M1_28_6 == 2) | (Tr_del$M1_28_6 == 1 & Tr_del$M1_28_1 == 2 & Tr_del$M1_28_2 == 2 &
Tr_del$M1_28_3 == 2 & Tr_del$M1_28_4 == 2 & Tr_del$M1_28_5 == 2) | (is.na(Tr_del$M1_28_1) & is.na(Tr_del$M1_28_2) & is.na(Tr_del$M1_28_3) & is.na(Tr_del$M1_28_4) &
                                                                      is.na(Tr_del$M1_28_5) & is.na(Tr_del$M1_28_6)),"OK","MAL")

 

Reqfun_M1_31X <- ifelse((Tr_del$M1_31 == 9 & Tr_del$M1_31X == "NO SABE / NO RESPONDE") | (Tr_del$M1_31 != 9 & Letras(Tr_del$M1_31X) >= 3) |
(is.na(Tr_del$M1_31) & is.na(Tr_del$M1_31X)),"OK","MAL")

Reqfun_M1_32 <- ifelse((Tr_del$CONSEC == 1 & Tr_del$M1_2 != 33 & !is.na(Tr_del$M1_32_1) & !is.na(Tr_del$M1_32_2) & 
!is.na(Tr_del$M1_32_3) & !is.na(Tr_del$M1_32_4) & !is.na(Tr_del$M1_32_5) &
!is.na(Tr_del$M1_32_6) & !is.na(Tr_del$M1_32_7) & !is.na(Tr_del$M1_32_8) & !is.na(Tr_del$M1_32_9) &
!is.na(Tr_del$M1_32_10) & !is.na(Tr_del$M1_32_11)) | 
((Tr_del$CONSEC != 1 | Tr_del$M1_2 == 33) & is.na(Tr_del$M1_32_1) & is.na(Tr_del$M1_32_2) & 
is.na(Tr_del$M1_32_3) & is.na(Tr_del$M1_32_4) & is.na(Tr_del$M1_32_5) &
is.na(Tr_del$M1_32_6) & is.na(Tr_del$M1_32_7) & is.na(Tr_del$M1_32_8) & is.na(Tr_del$M1_32_9) &
is.na(Tr_del$M1_32_10) & is.na(Tr_del$M1_32_11)) | (Tr_del$M1_2 == 33 & is.na(Tr_del$M1_32_1) & is.na(Tr_del$M1_32_2) & 
is.na(Tr_del$M1_32_3) & is.na(Tr_del$M1_32_4) & is.na(Tr_del$M1_32_5) &
is.na(Tr_del$M1_32_6) & is.na(Tr_del$M1_32_7) & is.na(Tr_del$M1_32_8) & is.na(Tr_del$M1_32_9) &
is.na(Tr_del$M1_32_10) & is.na(Tr_del$M1_32_11)),"OK","MAL")

Reqfun_M2_2 <- ifelse((Tr_del$M2_1 == 99 & Tr_del$M2_2 == 99) | (Tr_del$M2_1 != 99 & Tr_del$M2_2 != 99) |
                        (Tr_del$M2_1 == 0 & Tr_del$M2_2 == 0) | (Tr_del$M2_1 %in% c(1:98) & !is.na(Tr_del$M2_2)) | 
                        (is.na(Tr_del$M2_1) & is.na(Tr_del$M2_2)),"OK","MAL")

Reqfun_M1_27_1 <- ifelse(((Tr_del$M1_27_1 == 1 | Tr_del$M1_27_2 == 1 | Tr_del$M1_27_3 == 1 | Tr_del$M1_27_4 == 1
| Tr_del$M1_27_5 == 1 | Tr_del$M1_27_6 == 1 | Tr_del$M1_27_7 == 1 | Tr_del$M1_27_8 == 1 | Tr_del$M1_27_9 == 1) & 
Tr_del$M1_27_11 == 2) | 
(Tr_del$M1_27_11 == 1 & Tr_del$M1_27_1 == 2 & Tr_del$M1_27_2 == 2 & Tr_del$M1_27_3 == 2
& Tr_del$M1_27_4 == 2 & Tr_del$M1_27_5 == 2 & Tr_del$M1_27_6 == 2 & Tr_del$M1_27_7 == 2 & Tr_del$M1_27_8 == 2
& Tr_del$M1_27_9 == 2) | (is.na(Tr_del$M1_27_1) & is.na(Tr_del$M1_27_2) & is.na(Tr_del$M1_27_3) & is.na(Tr_del$M1_27_4) & is.na(Tr_del$M1_27_5) &
                            is.na(Tr_del$M1_27_6) & is.na(Tr_del$M1_27_7) & is.na(Tr_del$M1_27_8) & is.na(Tr_del$M1_27_9) & is.na(Tr_del$M1_27_11)),"OK","MAL")


Reqfun_M1_11_5 <- ifelse((Tr_del$M1_11_5 == 2 & (Tr_del$M1_11_1 == 1 | Tr_del$M1_11_2 == 1 | Tr_del$M1_11_3 == 1 |
Tr_del$M1_11_4 == 1)) | (Tr_del$M1_11_1 == 2 & Tr_del$M1_11_2 == 2 & Tr_del$M1_11_3 == 2 & Tr_del$M1_11_4 == 2 &
Tr_del$M1_11_5 == 1),"OK","MAL")

Reqfun_M1_29_6 <- ifelse(((Tr_del$M1_29_1 == 1 | Tr_del$M1_29_2 == 1 | Tr_del$M1_29_3 == 1 | Tr_del$M1_29_4 == 1 |
Tr_del$M1_29_5 == 1) & Tr_del$M1_29_6 == 2) | (Tr_del$M1_29_6 == 1 & Tr_del$M1_29_1 == 2 & Tr_del$M1_29_2 == 2 & Tr_del$M1_29_3 == 2 &
Tr_del$M1_29_4 == 2 & Tr_del$M1_29_5 == 2) | (Tr_del$M1_29_1 == 9 & Tr_del$M1_29_2 == 9 & Tr_del$M1_29_3 == 9 &
Tr_del$M1_29_4 == 9 & Tr_del$M1_29_5 == 2 & Tr_del$M1_29_6 == 2) | 
(is.na(Tr_del$M1_29_1) & is.na(Tr_del$M1_29_2) & is.na(Tr_del$M1_29_3)
& is.na(Tr_del$M1_29_4) & is.na(Tr_del$M1_29_5)),"OK","MAL")

Reqfun_M1_32_11 <- ifelse(((Tr_del$M1_32_1 == 1 | Tr_del$M1_32_2 == 1 | Tr_del$M1_32_3 == 1 | Tr_del$M1_32_4 == 1 |
                             Tr_del$M1_32_5 == 1 | Tr_del$M1_32_6 == 1 | Tr_del$M1_32_7 == 1 | Tr_del$M1_32_8 == 1 |
                             Tr_del$M1_32_9 == 1 | Tr_del$M1_32_10 == 1) & Tr_del$M1_32_11 == 2) | 
                            (Tr_del$M1_32_1 == 2 & Tr_del$M1_32_2 == 2 & Tr_del$M1_32_3 == 2 & Tr_del$M1_32_4 == 2 & 
                            Tr_del$M1_32_5 == 2 & Tr_del$M1_32_6 == 2 & Tr_del$M1_32_7 == 2 & Tr_del$M1_32_8 == 2 & 
                            Tr_del$M1_32_9 == 2 & Tr_del$M1_32_10 == 2 & Tr_del$M1_32_11 == 1) |
                            (is.na(Tr_del$M1_32_1) & is.na(Tr_del$M1_32_2) & is.na(Tr_del$M1_32_3) & is.na(Tr_del$M1_32_4)
                             & is.na(Tr_del$M1_32_5) & is.na(Tr_del$M1_32_6) & is.na(Tr_del$M1_32_7) & is.na(Tr_del$M1_32_8)
                             & is.na(Tr_del$M1_32_9) & is.na(Tr_del$M1_32_10) & is.na(Tr_del$M1_32_11)),"OK","MAL")

Reqfun_M4_1 <- ifelse((Tr_del$ID_DELITO == "07" & Tr_del$M4_1 %in% c(1,2,8)) | 
(Tr_del$ID_DELITO == "08" & Tr_del$M4_1 %in% c(2,4,5,6,7,8)) |
(Tr_del$ID_DELITO == "09" & Tr_del$M4_1 %in% c(2,3,8)) | (is.na(Tr_del$M4_1)),"OK","MAL")

# DATA FRAMES 

# 1.1 Data Frame de Rangos (Ran)
df_ran_del <- data.frame(
  Ran_M1_1, Ran_M1_2, Ran_M1_3, Ran_M1_4, Ran_M1_5, Ran_M1_5A_1, Ran_M1_5A_2, 
  Ran_M1_5A_3, Ran_M1_5A_4, Ran_M1_6, Ran_M1_7, Ran_M1_8, Ran_M1_9A, Ran_M1_9B, 
  Ran_M1_10, Ran_M1_11_1, Ran_M1_11_2, Ran_M1_11_3, Ran_M1_11_4, Ran_M1_11_5, 
  Ran_M1_12, Ran_M1_13, Ran_M1_14_1, Ran_M1_14_2, Ran_M1_14_3, Ran_M1_14_4, 
  Ran_M1_15, Ran_M1_15A, Ran_M1_16, Ran_M1_17_1, Ran_M1_17_2, Ran_M1_17_3, 
  Ran_M1_17_4, Ran_M1_17_5, Ran_M1_17_6, Ran_M1_17_7, Ran_M1_17_8, Ran_M1_17A, 
  Ran_M1_18, Ran_M1_19, Ran_M1_20, Ran_M1_21, Ran_M1_22, Ran_M1_23, Ran_M1_24, 
  Ran_M1_25, Ran_M1_26, Ran_M1_27_1, Ran_M1_27_2, Ran_M1_27_3, Ran_M1_27_4, 
  Ran_M1_27_5, Ran_M1_27_6, Ran_M1_27_7, Ran_M1_27_8, Ran_M1_27_9, Ran_M1_27_11, 
  Ran_M1_28_1, Ran_M1_28_2, Ran_M1_28_3, Ran_M1_28_4, Ran_M1_28_5, Ran_M1_28_6, 
  Ran_M1_29_1, Ran_M1_29_2, Ran_M1_29_3, Ran_M1_29_4, Ran_M1_29_5, Ran_M1_29_6, 
  Ran_M1_30, Ran_M1_31, Ran_M1_32_1, Ran_M1_32_2, Ran_M1_32_3, Ran_M1_32_4, 
  Ran_M1_32_5, Ran_M1_32_6, Ran_M1_32_7, Ran_M1_32_8, Ran_M1_32_9, Ran_M1_32_10, 
  Ran_M1_32_11, Ran_M2_1, Ran_M2_2, Ran_M3_1_1, Ran_M3_1_2, Ran_M3_1_3, Ran_M3_1_4, 
  Ran_M3_1_5, Ran_M3_1_6, Ran_M3_1_7, Ran_M3_1_8, Ran_M4_1, Ran_M5_1, Ran_M5_1A_1, 
  Ran_M5_1A_2, Ran_M5_1A_3, Ran_M5_1A_4, Ran_M5_2_1, Ran_M5_2_2, Ran_M5_2_3, 
  Ran_M5_2_4, Ran_M5_3, Ran_M5_4, Ran_M6_1, Ran_M6_2, Ran_M6_3, Ran_M6_4, Ran_M7_1
)

# 1.2 Data Frame de Validación (Val) - Exclusivo de Tr_del
df_val_del <- data.frame(
  Val_M1_1, Val_M1_2X, Val_M1_5_5X, Val_M1_5A_4X, Val_M1_5A_1, Val_M1_9_1, 
  Val_M1_11, Val_M1_11_5, Val_M1_14_4X, Val_M1_14, Val_M1_17_7_1, Val_M1_17_7_2, 
  Val_M1_17_8X, Val_M1_17_1, Val_M1_19_9X, Val_M1_21_7X, Val_M1_22_6X, 
  Val_M1_23_7X, Val_M1_27_9X, Val_M1_27, Val_M1_28_5X, Val_M1_29_5X, Val_M1_29, 
  Val_M1_30, Val_M1_31, Val_M1_31X, Val_M1_32_10X
)

# 1.3 Data Frame de Salto (Sal)
df_sal_del <- data.frame(
  Sal_P7_1, Sal_M1_1, Sal_M1_2.0, Sal_M1_2, Sal_M1_5, Sal_M1_5A, Sal_M1_5_5X, 
  Sal_M1_6, Sal_M1_7, Sal_M1_8, Sal_M1_9A, Sal_M1_11_5, Sal_M1_12, Sal_M1_13, 
  Sal_M1_14_4, Sal_M1_15, Sal_M1_16, Sal_M1_17_8, Sal_M1_17_8X, Sal_M1_18, 
  Sal_M1_19X, Sal_M1_19.1, Sal_M1_19, Sal_M1_20, Sal_M1_21, Sal_M1_22, Sal_M1_23, 
  Sal_M1_26, Sal_M1_27_9, Sal_M1_27, Sal_M1_27_11, Sal_M1_28_5, Sal_M1_27.2, 
  Sal_M1_29_5, Sal_M1_29_5.1, Sal_M1_29_6, Sal_M1_29_4, Sal_M1_30, Sal_M1_31, 
  Sal_M1_31X, Sal_M1_32_10, Sal_M1_32_10X, Sal_M1_31X.1, Sal_M1_32_11, Sal_M2_1, 
  Sal_M2_2, Sal_M3_1_8, Sal_M3_1_8X, Sal_M4_1, Sal_M4_1_8X, Sal_M5_1, Sal_M5_1_8X, 
  Sal_M5_1A_4, Sal_M5_2_4, Sal_M5_2_4.1, Sal_M5_4, Sal_M5_4_1X, Sal_M6_4
)

# 1.4 Data Frame de Requisitos Funcionales (Reqfun)
df_reqfun_del <- data.frame(
  Reqfun_M1_1, Reqfun_M1_2, Reqfun_M1_5, Reqfun_M1_27_11, Reqfun_M1_28_6, 
  Reqfun_M1_31X, Reqfun_M1_32, Reqfun_M2_2, Reqfun_M1_27_1, Reqfun_M1_11_5, 
  Reqfun_M1_29_6, Reqfun_M1_32_11, Reqfun_M4_1
)

# --- RESÚMENES PARA LA BASE PRINCIPAL --- #
Resumen_Ran_Del    <- ifelse(rowSums(df_ran_del == "MAL", na.rm = TRUE) > 0, "MAL", "OK")
Resumen_Val_Del    <- ifelse(rowSums(df_val_del == "MAL", na.rm = TRUE) > 0, "MAL", "OK")
Resumen_Sal_Del    <- ifelse(rowSums(df_sal_del == "MAL", na.rm = TRUE) > 0, "MAL", "OK")
Resumen_Reqfun_Del <- ifelse(rowSums(df_reqfun_del == "MAL", na.rm = TRUE) > 0, "MAL", "OK")

# Resumen Total Tr_del
Resumen_Total_Del <- ifelse(Resumen_Ran_Del == "OK" & Resumen_Val_Del == "OK" & 
                              Resumen_Sal_Del == "OK" & Resumen_Reqfun_Del == "OK", 
                            "OK", "MAL")



# DATA FRAME GRANDE 
df <-data.frame(
  Ran_M1_1, Ran_M1_2, Ran_M1_3, Ran_M1_4, Ran_M1_5, Ran_M1_5A_1, Ran_M1_5A_2, 
  Ran_M1_5A_3, Ran_M1_5A_4, Ran_M1_6, Ran_M1_7, Ran_M1_8, Ran_M1_9A, Ran_M1_9B, 
  Ran_M1_10, Ran_M1_11_1, Ran_M1_11_2, Ran_M1_11_3, Ran_M1_11_4, Ran_M1_11_5, 
  Ran_M1_12, Ran_M1_13, Ran_M1_14_1, Ran_M1_14_2, Ran_M1_14_3, Ran_M1_14_4, 
  Ran_M1_15, Ran_M1_15A, Ran_M1_16, Ran_M1_17_1, Ran_M1_17_2, Ran_M1_17_3, 
  Ran_M1_17_4, Ran_M1_17_5, Ran_M1_17_6, Ran_M1_17_7, Ran_M1_17_8, Ran_M1_17A, 
  Ran_M1_18, Ran_M1_19, Ran_M1_20, Ran_M1_21, Ran_M1_22, Ran_M1_23, Ran_M1_24, 
  Ran_M1_25, Ran_M1_26, Ran_M1_27_1, Ran_M1_27_2, Ran_M1_27_3, Ran_M1_27_4, 
  Ran_M1_27_5, Ran_M1_27_6, Ran_M1_27_7, Ran_M1_27_8, Ran_M1_27_9, Ran_M1_27_11, 
  Ran_M1_28_1, Ran_M1_28_2, Ran_M1_28_3, Ran_M1_28_4, Ran_M1_28_5, Ran_M1_28_6, 
  Ran_M1_29_1, Ran_M1_29_2, Ran_M1_29_3, Ran_M1_29_4, Ran_M1_29_5, Ran_M1_29_6, 
  Ran_M1_30, Ran_M1_31, Ran_M1_32_1, Ran_M1_32_2, Ran_M1_32_3, Ran_M1_32_4, 
  Ran_M1_32_5, Ran_M1_32_6, Ran_M1_32_7, Ran_M1_32_8, Ran_M1_32_9, Ran_M1_32_10, 
  Ran_M1_32_11, Ran_M2_1, Ran_M2_2, Ran_M3_1_1, Ran_M3_1_2, Ran_M3_1_3, Ran_M3_1_4, 
  Ran_M3_1_5, Ran_M3_1_6, Ran_M3_1_7, Ran_M3_1_8, Ran_M4_1, Ran_M5_1, Ran_M5_1A_1, 
  Ran_M5_1A_2, Ran_M5_1A_3, Ran_M5_1A_4, Ran_M5_2_1, Ran_M5_2_2, Ran_M5_2_3, 
  Ran_M5_2_4, Ran_M5_3, Ran_M5_4, Ran_M6_1, Ran_M6_2, Ran_M6_3, Ran_M6_4, Ran_M7_1,
  Val_M1_1, Val_M1_2X, Val_M1_5_5X, Val_M1_5A_4X, Val_M1_5A_1, Val_M1_9_1, 
  Val_M1_11, Val_M1_11_5, Val_M1_14_4X, Val_M1_14, Val_M1_17_7_1, Val_M1_17_7_2, 
  Val_M1_17_8X, Val_M1_17_1, Val_M1_19_9X, Val_M1_21_7X, Val_M1_22_6X, 
  Val_M1_23_7X, Val_M1_27_9X, Val_M1_27, Val_M1_28_5X, Val_M1_29_5X, Val_M1_29, 
  Val_M1_30, Val_M1_31, Val_M1_31X, Val_M1_32_10X,
  Sal_P7_1, Sal_M1_1, Sal_M1_2.0, Sal_M1_2, Sal_M1_5, Sal_M1_5A, Sal_M1_5_5X, 
  Sal_M1_6, Sal_M1_7, Sal_M1_8, Sal_M1_9A, Sal_M1_11_5, Sal_M1_12, Sal_M1_13, 
  Sal_M1_14_4, Sal_M1_15, Sal_M1_16, Sal_M1_17_8, Sal_M1_17_8X, Sal_M1_18, 
  Sal_M1_19X, Sal_M1_19.1, Sal_M1_19, Sal_M1_20, Sal_M1_21, Sal_M1_22, Sal_M1_23, 
  Sal_M1_26, Sal_M1_27_9, Sal_M1_27, Sal_M1_27_11, Sal_M1_28_5, Sal_M1_27.2, 
  Sal_M1_29_5, Sal_M1_29_5.1, Sal_M1_29_6, Sal_M1_29_4, Sal_M1_30, Sal_M1_31, 
  Sal_M1_31X, Sal_M1_32_10, Sal_M1_32_10X, Sal_M1_31X.1, Sal_M1_32_11, Sal_M2_1, 
  Sal_M2_2, Sal_M3_1_8, Sal_M3_1_8X, Sal_M4_1, Sal_M4_1_8X, Sal_M5_1, Sal_M5_1_8X, 
  Sal_M5_1A_4, Sal_M5_2_4, Sal_M5_2_4.1, Sal_M5_4, Sal_M5_4_1X, Sal_M6_4,
  Reqfun_M1_1, Reqfun_M1_2, Reqfun_M1_5, Reqfun_M1_27_11, Reqfun_M1_28_6, 
  Reqfun_M1_31X, Reqfun_M1_32, Reqfun_M2_2, Reqfun_M1_27_1, Reqfun_M1_11_5, 
  Reqfun_M1_29_6, Reqfun_M1_32_11, Reqfun_M4_1,Resumen_Ran_Del,Resumen_Val_Del,Resumen_Sal_Del,
  Resumen_Reqfun_Del,Resumen_Total_Del
)
 

# Módulo 2 (Tr_mod2)
df_val_mod2 <- data.frame(Val_M2, Val_M2_1, Val_M2_1.1, Val_M2_2)
Resumen_Val_Mod2 <- ifelse(rowSums(df_val_mod2 == "MAL", na.rm = TRUE) > 0, "MAL", "OK")
df_mod2 = data.frame(Val_M2, Val_M2_1, Val_M2_1.1, Val_M2_2,Resumen_Val_Mod2)

# Módulo 3 (Tr_mod3)
df_val_mod3 <- data.frame(Val_M3, Val_M3_1_8X, Val_M3_1_1)
Resumen_Val_Mod3 <- ifelse(rowSums(df_val_mod3 == "MAL", na.rm = TRUE) > 0, "MAL", "OK")
df_mod3 = data.frame(Val_M3, Val_M3_1_8X, Val_M3_1_1,Resumen_Val_Mod3)

# Módulo 4 (Tr_mod4)
df_val_mod4 <- data.frame(Val_M4, Val_M4_1_8X, Val_M4_1.1)
Resumen_Val_Mod4 <- ifelse(rowSums(df_val_mod4 == "MAL", na.rm = TRUE) > 0, "MAL", "OK")
df_mod4 = data.frame(Val_M4, Val_M4_1_8X, Val_M4_1.1,Resumen_Val_Mod4)

# Módulo 5 (Tr_mod5)
df_val_mod5 <- data.frame(
  Val_M5, Val_M5.1, Val_M5_1_8X, Val_M5_1A, Val_M5_1A_4X, 
  Val_M5_2_4X, Val_M5_2, Val_M5_2_3, Val_M5_4X
)
Resumen_Val_Mod5 <- ifelse(rowSums(df_val_mod5 == "MAL", na.rm = TRUE) > 0, "MAL", "OK")
df_mod5 <- data.frame(
  Val_M5, Val_M5.1, Val_M5_1_8X, Val_M5_1A, Val_M5_1A_4X, 
  Val_M5_2_4X, Val_M5_2, Val_M5_2_3, Val_M5_4X,Resumen_Val_Mod5
)

# Módulo 6 (Tr_mod6)
df_val_mod6 <- data.frame(Val_M6, Val_M6_1)
Resumen_Val_Mod6 <- ifelse(rowSums(df_val_mod6 == "MAL", na.rm = TRUE) > 0, "MAL", "OK")
df_mod6 <- data.frame(Val_M6, Val_M6_1,Resumen_Val_Mod6)

# Módulo 7 (Tr_mod7)
df_val_mod7 <- data.frame(Val_M7, Val_M7_1_5X)
Resumen_Val_Mod7 <- ifelse(rowSums(df_val_mod7 == "MAL", na.rm = TRUE) > 0, "MAL", "OK")
df_mod7 <- data.frame(Val_M7, Val_M7_1_5X,Resumen_Val_Mod7)

lista_modulos <- list(
  "Validaciones Modulo 2" = df_mod2,
  "Validaciones Modulo 3" = df_mod3,
  "Validaciones Modulo 4" = df_mod4,
  "Validaciones Modulo 5" = df_mod5,
  "Validaciones Modulo 6" = df_mod6,
  "Validaciones Modulo 7" = df_mod7
)

 
write.xlsx(x = df, file = "df_modulos", sheetName = "Reporte", overwrite = TRUE)
write.xlsx(lista_modulos, file = "Resultados_Modulos_ENVE.xlsx", overwrite = TRUE)

































