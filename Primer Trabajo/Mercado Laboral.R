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


#INDICADORES DE MERCADO LABORAL


####ENERO-MARZO


###DATOS PARA HALLAR INDICADORES

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

###SOLUCION INDICADORES 1

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


####JULIO-SEPTIEMBRE


###DATOS PARA HALLAR INDICADORES

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

###SOLUCION INDICADORES

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

#TABLAS DE DATOS INDICADORES
 
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

#ENERO-MARZO

TABIND1<-data.frame(
  TRIMESTRE=1,
  TBP1=TBP1,
  TGP1=TGP1,
  TP1=TP1,
  TO1=TO1
)

TABIND1<- TABIND1 %>% rename(TBP= OCUPADOSI)
TABIND1<- TABIND1 %>% rename(TGP= OCUPADOSI.1)
TABIND1<- TABIND1 %>% rename(TP= DESOCUPADOSI)
TABIND1<- TABIND1 %>% rename(TO= OCUPADOSI.2)

TABIND1

#JULIO-SEPTIEMBRE

TABIND3<-data.frame(
  TRIMESTRE=3,
  TBP3=TBP3,
  TGP3=TGP3,
  TP3=TP3,
  TO3=TO3
)

TABIND3<- TABIND3 %>% rename(TBP= OCUPADOSIII)
TABIND3<- TABIND3 %>% rename(TGP= OCUPADOSIII.1)
TABIND3<- TABIND3 %>% rename(TP= DESOCUPADOSIII)
TABIND3<- TABIND3 %>% rename(TO= OCUPADOSIII.2)

TABIND3
