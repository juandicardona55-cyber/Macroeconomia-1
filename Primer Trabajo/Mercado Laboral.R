################################################################################################################################################################
#########################                                                                                                      #################################
#                                                           UNIVERSIDAD DEL QUINDIO                                                                            #
#                                                                  ECONOMIA                                                                                    #
#                                                               MACROECONOMIA I                                                                                #
#########################                                                                                                      #################################
################################################################################################################################################################

# By:Juan Diego Cardona Betancourt
# Juand.cardonab@uqvirtual.edu.co
# +57 3113553717


print("¿Cuantas AREAS hay?")
unique(GEIH$AREA)
table(unique(GEIH$AREA))

#Para comprobar que no hayan duplicados

table(duplicated(GEIH[, c("DIRECTORIO", "SECUENCIA_P", "ORDEN")]))

#Limpieza en bucle-Pasar variables cuantitativas discretas a continuas

for (N in names(GEIH)) {
  if(class(GEIH[[N]])=="integer"){GEIH[[N]]=as.numeric(GEIH[[N]])}
}

#Poblacion Total

xtabs( FEX_C18 ~ MES , data= GEIH , addNA = TRUE)

#PET - Poblacion en edad de trabajar

xtabs( FEX_C18 ~ PET + MES, data= GEIH , addNA = TRUE)

#FT - Fuerza de trabajo

GEIH = GEIH |> dplyr::mutate(FT_GEN = case_when( OCI == 1 | DSI == 1 ~ 1,TRUE ~ NA))

xtabs( FEX_C18 ~ FT_GEN + MES , data= GEIH , addNA = TRUE)

#FFT - Poblacion fuera de la fuerza de trabajo

xtabs( FEX_C18 ~ FFT + MES , data= GEIH , addNA = TRUE)

#OCI - Ocupados

xtabs( FEX_C18 ~ OCI + MES , data= GEIH , addNA = TRUE)

#DSI - Desocupados

xtabs( FEX_C18 ~ DSI + MES , data= GEIH , addNA = TRUE)


#Indicadores de mercado laboral


####enero-marzo


###Datos para hallar indicadores

#Ocupados trimestre 1

OC1=(GEIH %>%
  filter(MES %in% c("01", "02", "03"), OCI==1) %>%
  group_by(MES) %>%
  summarise(
    OCI = sum(FEX_C18, na.rm = TRUE)
  ) %>%
  summarise(
    OCUPADOSI = sum(OCI) / 3
  ))
OC1

#Desocupados trimestre 1

DSI1=(GEIH %>%
  filter(MES %in% c("01", "02", "03"), DSI==1) %>%
  group_by(MES) %>%
  summarise(
    DSI = sum(FEX_C18, na.rm = TRUE)
  ) %>%
  summarise(
    DESOCUPADOSI = sum(DSI) / 3
  ))
DSI1

#Poblacion total trimestre 1

PT1=(GEIH %>%
  filter(MES %in% c("01", "02", "03")) %>%
  group_by(MES) %>%
  summarise(
    PT = sum(FEX_C18, na.rm = TRUE)
  ) %>%
  summarise(
    PTOTALI = sum(PT) / 3
  ))
PT1

#Fuerza de trabajo trimestre 1

FT1=(GEIH %>%
       filter(MES %in% c("01", "02", "03"), FT_GEN==1) %>%
       group_by(MES) %>%
       summarise(
         FT = sum(FEX_C18, na.rm = TRUE)
       ) %>%
       summarise(
         FTRABAJOI = sum(FT) / 3
       ))
FT1

#PET trimestre 1

PET1=(GEIH %>%
        filter(MES %in% c("01", "02", "03"), PET==1) %>%
        group_by(MES) %>%
        summarise(
          PET1 = sum(FEX_C18, na.rm = TRUE)
        ) %>%
        summarise(
          POBETRABAJARI = sum(PET1) / 3
        ))
PET1

###Solucion indicadores

#Tasa bruta de participacion

TBP1=((OC1+DSI1)/PT1)*100
TBP1

#Tasa global de participacion TGP

TGP1=((OC1+DSI1)/PET1)*100
TGP1

#Tasa de paro TP

TP1=(DSI1/(OC1+DSI1))*100
TP1

#Tasa de ocupados TO

TO1=(OC1/(PET1))*100
TO1


####Julio-Septiembre


###Datos para hallar indicadores

#Ocupados trimestre 3

OC3=(GEIH %>%
       filter(MES %in% c("07", "08", "09"), OCI==1) %>%
       group_by(MES) %>%
       summarise(
         OCI = sum(FEX_C18, na.rm = TRUE)
       ) %>%
       summarise(
         OCUPADOSIII = sum(OCI) / 3
       ))
OC3

#Desocupados trimestre 3

DSI3=(GEIH %>%
        filter(MES %in% c("07", "08", "09"), DSI==1) %>%
        group_by(MES) %>%
        summarise(
          DSI = sum(FEX_C18, na.rm = TRUE)
        ) %>%
        summarise(
          DESOCUPADOSIII = sum(DSI) / 3
        ))
DSI3

#Poblacion total trimestre 3

PT3=(GEIH %>%
       filter(MES %in% c("07", "08", "09")) %>%
       group_by(MES) %>%
       summarise(
         PT = sum(FEX_C18, na.rm = TRUE)
       ) %>%
       summarise(
         PTOTALIII = sum(PT) / 3
       ))
PT3

#Fuerza de trabajo trimestre 3

FT3=(GEIH %>%
       filter(MES %in% c("07", "08", "09"), FT_GEN==1) %>%
       group_by(MES) %>%
       summarise(
         FT = sum(FEX_C18, na.rm = TRUE)
       ) %>%
       summarise(
         FTRABAJOIII = sum(FT) / 3
       ))
FT3

#PET trimestre 3

PET3=(GEIH %>%
        filter(MES %in% c("07", "08", "09"), PET==1) %>%
        group_by(MES) %>%
        summarise(
          PET1 = sum(FEX_C18, na.rm = TRUE)
        ) %>%
        summarise(
          POBETRABAJARIII = sum(PET1) / 3
        ))
PET3

###Solucion indicadores

#Tasa bruta de participacion

TBP3=((OC3+DSI3)/PT3)*100
TBP3

#Tasa global de participacion TGP

TGP3=((OC3+DSI3)/PET3)*100
TGP3

#Tasa de paro TP

TP3=(DSI3/(OC3+DSI3))*100
TP3

#Tasa de ocupados TO

TO3=(OC3/(PET3))*100
TO3

#Tabla de datos indicadores
 
#Para pasar meses a trimestres 
GEIH <- GEIH %>% 
  mutate(
    TRIMESTRE = case_when(
      MES %in% c("01", "02", "03") ~ 1,
      MES %in% c("04", "05", "06") ~ 2,
      MES %in% c("07", "08", "09") ~ 3,
      MES %in% c("10", "11", "12") ~ 4
    ))
table(GEIH$TRIMESTRE)

#Tabla de indicadores

TASAS<-data.frame(
  TRIMESTRE= c(1,3),
  TBP=c(TBP1,TBP3),
  TGP=c(TGP1,TGP3)
)
TASAS

#terminar y organizar
data.frame(
  TRIMESTRE=c(1,3),
  DSI1
  )
