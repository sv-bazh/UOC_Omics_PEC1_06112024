knitr::purl(input = "act 1-1.Rmd", output = "Bazhenova-Svetlana-PEC1.R",documentation = 0)

sessionInfo()

#if (!require("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install(version = "3.19")

#BiocManager::install("SummarizedExperiment")

#install.packages('data.table')
#install.packages('tidyr')
#install.packages('stringr')
#install.packages("plotly")
#install.packages("ggpubr")
#install.packages("webshot")

library(data.table)    
library(tidyr)
library(stringr)
v = read.csv('DataValues_S013.csv')
val <- subset(v, select=-c(X,X.1,SUBJECTS,SURGERY, AGE, GENDER,Group))
pats <- subset(v, select= c(SUBJECTS,SURGERY, AGE, GENDER,Group))
valt <- transpose(val)

colnames(valt) <- rownames(val)
rownames(valt) <- colnames(val)

# duplico el VarName para despues usarlo como Index
valt$VarName <- rownames(valt)

# separo el Varname para tener el punto temporal como una columna separada
valt2 <- separate(valt, col = VarName, into = c("varname", "timepoint"), sep = "_")

pats$SURGERY<-as.factor(pats$SURGERY)
pats$GENDER<-as.factor(pats$GENDER)
pats$Group<-as.factor(pats$Group)

melt_data <- melt(valt2, id = c("varname", "timepoint"))

colnames(melt_data)[colnames(melt_data) == 'variable'] <- 'SUBJECTS'

reshaped_values <- melt_data %>%
  pivot_wider(names_from = varname, values_from = value)

all_values <- merge(x=reshaped_values,y=pats,all.x=TRUE)

all_values$SUBJECTS<-as.factor(all_values$SUBJECTS)
all_values$timepoint<-as.factor(all_values$timepoint)
all_values$SURGERY<-as.factor(all_values$SURGERY)
all_values$GENDER<-as.factor(all_values$GENDER)
all_values$Group<-as.factor(all_values$Group)
all_values$na_count <- apply(is.na(all_values), 1, sum)

summary(pats$GENDER)
summary(pats$SURGERY)
summary(pats$AGE)
summary(pats$Group)

library(ggplot2)
library(dplyr)
ggplot(pats, aes(x = GENDER, y = AGE, color = SURGERY,shape=Group)) +
  geom_jitter(width = .15)


library(ggplot2)
library(dplyr)


ggplot(all_values, aes(x = AGE, y = na_count,color = GENDER,shape=as.factor(SURGERY))) +
  geom_point() +
  facet_wrap(~ timepoint,nrow =1)


library(plotly)

plot_ly(x = colSums(is.na(all_values)), y = colnames(all_values), type = 'bar', orientation = 'h')%>%
layout(title = "Cantidad de filas con valores faltantes por columna")


namedCounts <- sapply(all_values, function(x) sum(is.na(x)))
namedCounts <- namedCounts[namedCounts>20]
print(paste0(names(namedCounts)," :",unname(namedCounts)))

library(ggplot2)
library(dplyr)

ggplot(all_values, aes(x = AGE, y = bmi,color = GENDER,shape=as.factor(SURGERY))) +
  geom_point() +
  labs(title="Variacion del bmi en el tiempo, por edad, sexo y tipo de cirugía") +
  facet_wrap(~ timepoint,nrow =1)

library(ggplot2)
library(dplyr)
ggplot(all_values, aes(x = AGE, y = GLU,color = GENDER,shape=as.factor(SURGERY))) +
  geom_point() +
  labs(title="Variacion del GLU en el tiempo, por edad, sexo y tipo de cirugía") +
  facet_wrap(~ timepoint,nrow =1)

library(ggplot2)
library(dplyr)
ggplot(all_values, aes(x = AGE, y = Trp,color = GENDER,shape=as.factor(SURGERY))) +
  geom_point() +
  labs(title="Variacion del Trp en el tiempo, por edad, sexo y tipo de cirugía") +
  facet_wrap(~ timepoint,nrow =1)

all_values$Serotonin[all_values$Serotonin == -99.000] <- NA
all_values$Serotonin[all_values$Serotonin == -99.000] <- NA
library(ggplot2)
library(dplyr)

ggplot(all_values, aes(x = AGE, y = Serotonin,color = GENDER,shape=as.factor(SURGERY))) +
  geom_point() +
  labs(title="Variacion de la Serotonina en el tiempo, por edad, sexo y tipo de cirugía") +
  facet_wrap(~ timepoint,nrow =1)

ser_trp_t0_t5 <- subset(v, select= c(SUBJECTS,SURGERY, AGE, GENDER,Group,GLU_T0,GLU_T5,bmi_T0,bmi_T5,Trp_T0,Trp_T5,Serotonin_T0,Serotonin_T5))
ser_trp_t0_t5$Serotonin_T0[ser_trp_t0_t5$Serotonin_T0 == -99.000] <- NA
ser_trp_t0_t5$Serotonin_T5[ser_trp_t0_t5$Serotonin_T5 == -99.000] <- NA

ser_trp_t0_t5$SUBJECTS<-as.factor(ser_trp_t0_t5$SUBJECTS)
ser_trp_t0_t5$SURGERY<-as.factor(ser_trp_t0_t5$SURGERY)
ser_trp_t0_t5$GENDER<-as.factor(ser_trp_t0_t5$GENDER)
ser_trp_t0_t5$Group<-as.factor(ser_trp_t0_t5$Group)

ser_trp_t0_t5$Serotonin_diff <- (ser_trp_t0_t5$Serotonin_T5 - ser_trp_t0_t5$Serotonin_T0)/ser_trp_t0_t5$Serotonin_T0
ser_trp_t0_t5$bmi_diff <- (ser_trp_t0_t5$bmi_T5 - ser_trp_t0_t5$bmi_T0)/ser_trp_t0_t5$bmi_T0
ser_trp_t0_t5$Trp_diff <- (ser_trp_t0_t5$Trp_T5 - ser_trp_t0_t5$Trp_T0)/ser_trp_t0_t5$Trp_T0
ser_trp_t0_t5$GLU_diff <- (ser_trp_t0_t5$GLU_T5 - ser_trp_t0_t5$GLU_T0)/ser_trp_t0_t5$GLU_T0

ser_trp <- subset(ser_trp_t0_t5, select= c(SUBJECTS,SURGERY, AGE, GENDER,Group,GLU_diff,bmi_diff,Trp_diff,Serotonin_diff))
summary(ser_trp)
head(ser_trp)

library(ggplot2)
library(dplyr)

ggplot(ser_trp, aes(x = GENDER, y = bmi_diff, color = SURGERY)) +
  labs(title="Diferencias del BMI entre T5 y T0, por edad, sexo y tipo de cirugía") +
  geom_jitter(width = .2)

library(ggplot2)
library(dplyr)
ggplot(ser_trp, aes(x = GENDER, y = GLU_diff, color = SURGERY)) +
  labs(title="Diferencias del nivel de Glucosa entre T5 y T0, por edad, sexo y tipo de cirugía") +
  geom_jitter(width = .2)

library(ggplot2)
library(dplyr)
ggplot(ser_trp, aes(x = GENDER, y = Serotonin_diff, color = SURGERY)) +
  labs(title="Diferencias del nivel de Serotonina entre T5 y T0, por edad, sexo y tipo de cirugía") +
  geom_jitter(width = .2)

library(ggplot2)
library(dplyr)
ggplot(ser_trp, aes(x = GENDER, y = Trp_diff, color = SURGERY)) +
  labs(title="Diferencias del nivel de Tryptophan entre T5 y T0, por edad, sexo y tipo de cirugía") +
  geom_jitter(width = .2)

library("ggpubr")

ggscatter(ser_trp, x = "bmi_diff", y = "GLU_diff", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Relative difference of bmi between T5 and T0", ylab = "Relative difference of GLU between T5 and T0")

cor.test(ser_trp$GLU_diff, ser_trp$bmi_diff,  method = "pearson", use = "complete.obs")

library("ggpubr")

ggscatter(ser_trp, x = "bmi_diff", y = "Serotonin_diff", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Relative difference of bmi between T5 and T0", ylab = "Relative difference of Serotonin between T5 and T0")

cor.test(ser_trp$Serotonin_diff, ser_trp$bmi_diff,  method = "pearson", use = "complete.obs")

library("ggpubr")

ggscatter(ser_trp, x = "Trp_diff", y = "Serotonin_diff", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Relative difference of tryptophan between T5 and T0", ylab = "Relative difference of Serotonin between T5 and T0")

cor.test(ser_trp$Serotonin_diff, ser_trp$bmi_diff,  method = "pearson", use = "complete.obs")


#Podemos extraer los metadatos de los pacientes de las primeras lineas del dataset DataValues. Necesitamos que el index de la tabla de metadatos sean los mismos numeros de los pacientes (la columna SUBJECTS). 

library(data.table)    
library(tidyr)
library(stringr)
library(SummarizedExperiment)

dv = read.csv('DataValues_S013.csv')
dv <- subset(dv, select=-c(X,X.1,SURGERY, AGE, GENDER,Group))

col_metadata <- fread('DataValues_S013.csv', select = c("SURGERY", "AGE", "GENDER","Group"))
rownames(col_metadata) <- dv$SUBJECTS

rownames(dv) <- dv$SUBJECTS

dvt <- transpose(dv)

colnames(dvt) <- rownames(dv)
rownames(dvt) <- colnames(dv)
dvt = dvt[-1,]

dvt <- dvt[order(row.names(dvt)), ]

#En orden de tener la estructura correcta para crear los assays de un SummarizedExperiment tendremos que hacer una transposicion de los datos en el dataset de los valores para tener los samples (aqui la columna SUBJECTS) como columnas. 

#Necesitamos crear la tabla de los metadatos de la filas (nombres de metabolitos y datos que cambian en el tiempo). Para ello vamos a unificar las tablas DataInfo y AAInformation.

#El index de la tabla va a tener que ser el mismo que el index de la tabla de los valores nueva (dvt). 

#Verificamos los valores de la columna Description del dataset DataInfo y vemos que son reduntandes asi que podemos quitar la columna ya que no aporta ninguna informacion util.
#Las columnas X y VarName contienen los mismos valores asi que podemos quitar también la columna X

di = read.csv('DataInfo_S013.csv',row.names = 1)

# quito las filas que son parte del dataset de datos clinicos de los pacientes
di <- di[!(row.names(di) %in% c('SUBJECTS','SURGERY','AGE','GENDER','Group','X')),]

# quito la columna Description ya que el unico valor es dataDesc
di2 <- di[,-3] 

# duplico el VarName para despues usarlo como Index
di2$VarName_dup <- di2$VarName

# separo el Varname para tener el punto temporal como una columna separada
di4 <- separate(di2, col = VarName, into = c("varname", "timepoint"), sep = "_")

# quito todos los caracteres non alphanumericos a la columna varname
di4$var_alphanum <- str_replace_all(di4$varname, "[^[:alnum:]]", "")


da = read.csv('AAInformation_S006.csv')
da <- subset(da, select=-c(X,X.1))

da$var_alphanum <- str_replace_all(da$Metabolite.abbreviation, "[^[:alnum:]]", "")

# construimos los metadatos de las features unificando los dataset AAInformation_S006.csv y DataInfo_S013.csv modificados previamente 

row_metadata <- merge(x=di4,y=da,all.x=TRUE)
row.names(row_metadata) <- row_metadata$VarName_dup
row_metadata <- subset(row_metadata, select=-c(var_alphanum,VarName_dup,varname))
row_metadata <- row_metadata[order(row.names(row_metadata)), ]


se <- SummarizedExperiment(
  assays = list(counts = dvt),
  colData = col_metadata,
  rowData = row_metadata,
  metadata = "Palau-Rodriguez M, Tulipani S, Marco-Ramell A, Miñarro A, Jáuregui O, Sanchez-Pla A, Ramos-Molina B, Tinahones FJ, Andres-Lacueva C. Metabotypes of response to bariatric surgery independent of the magnitude of weight loss. PLoS One. 2018 Jun 1;13(6):e0198214. doi: 10.1371/journal.pone.0198214. PMID: 29856816; PMCID: PMC5983508."
)
se

head(assay(se))

head(colData(se))

head(rowData(se))

metadata(se)

save(se, file = "contenedor.rda")

head(all_values)

dimnames(se)
