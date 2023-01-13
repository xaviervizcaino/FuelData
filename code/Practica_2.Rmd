---
title: "PRACTICA - 2"
subtitle: "FuelData"
author: 
  - Martí Antentas Parés
  - Xavier Vizcaino Gascon
date: '`r format(Sys.Date(),"%e de %B, %Y")`'
output:
  pdf_document: 
    toc: yes
    toc_depth: 2
---

```{r setup_1, include=FALSE}
#Setup R-code chunks
knitr::opts_chunk$set(echo = TRUE)

#Definició vspace entre text i codi
hook_source_def = knitr::knit_hooks$get('source')
knitr::knit_hooks$set(source = function(x, options) {
  if (!is.null(options$vspaceecho)) {
    begin <- paste0("\\vspace{", options$vspaceecho, "}")
    stringr::str_c(begin, hook_source_def(x, options))
  } else {
    hook_source_def(x, options)
  }
})
```

```{r installations, include=FALSE}
#Comprovació de paquets i instal·lacions
if(!require(tidyverse)){
  install.packages("tidyverse")
}
if(!require(data.table)){
  install.packages("data.table")
}
if(!require(rstatix)){
  install.packages("rstatix")
}
if(!require(stats)){
  install.packages("stats")
}
if(!require(nortest)){
  install.packages("nortest")
}
if(!require(car)){
  install.packages("car")
}
if(!require(DescTools)){
  install.packages("DescTools")
}
```

```{r load_libraries, include=FALSE}
#Càrrega de llibreries
library(tidyverse)
library(data.table)
library(rstatix)
library(stats)
library(nortest)
library(car)
library(DescTools)
library(knitr)
```

```{r general, include=FALSE}
#Paleta de colors pròpia per a gràfics
mypalette<-c("#F8766D","#00BFC4","#7CAE00","#C77CFF","#D89000" )
```

## 1. Descripció del dataset

Per a la realització d'aquesta segona pràctica s'utilitza el *dataset* generat a la primera pràctica usant tècniques de *web scraping*. Aquest es combina amb un altre *dataset* amb l'objectiu de realitzar un anàlisi més profund i enriquidor.

Com a recordatori; el conjunt de dades extret en la PRACTICA_1 conté la informació (general i de preu) de totes les estacions de servei d’Espanya obtinguda en cinc dies consecutius, del 14/11/2022 al 18/11/2022 ambdós inclosos. Per a cada execució de *web scraping* (diària), es van extreure les dades de totes les estacions de servei a totes les províncies d’Espanya i per a tots els tipus de carburants disponibles en la pàgina.

> NOTA : Per a la correcta execució del *script* és imprescindible definir la ruta on es troba l'arxiu **Practica_2.Rmd** com a *working directory*.

Les operacions d'aquesta pràctica han de permetre donar resposta a les següents preguntes:

* Les dades d'estudi es poden aplicar a tota la geografia espanyola?
* Hi ha zones amb preus marcadament diferents de la resta?
* La mitjana dels preus dels combustibles són diferents entre Barcelona i Madrid?
* La mitjana dels preus dels combustibles és diferent en les ciutats petites, mitjanes i grans?
* Existeix correlació entre els preus del combustible i el nombre d'habitants d'un municipi?
* El nombre de benzineres en un municipi influeix en els preus del combustible?

```{r working_dir, include=FALSE}
#Obtenció de variables d'entorn
wd<-getwd()
datadir<-file.path(dirname(wd), "data")
```

El procés s'inicia amb la lectura de l'arxiu de dades; que es realitza amb les següents opcions: escollint el separador i el tipus de codificació. A continuació es fa un primer sumari de les dades.

```{r file_import, vspaceecho='10pt'}
#Importar arxiu
fueldata <- read.csv(file.path(datadir, "FuelScraper", "dataset.csv"), 
                     encoding="UTF-8", sep=";")
                     
#Visualitzar summary de dades
summary(fueldata)
```

Aquest primer anàlisi indica que pot ser interessant canviar algunes variables a tipus factor, així com canviar el format de les variables temporals a tipus *date* i *lubridate*.
  
```{r factors, vspaceecho='10pt'}
#Vector de variables a modificar
t_vector<-c("Province","Road_side","Sale_1", "Sale_2", "Fuel_type")

#Loop
for (i in t_vector){
  #Canvi de tipus a factor
  fueldata[,i]<-as.factor(fueldata[,i])
}

#Canvis en variables temporals
fueldata$Capture_date<-as.Date(fueldata$Capture_date, format = "%Y/%m/%d")
fueldata$Update_date<-as.Date(fueldata$Update_date, format = "%d/%m/%Y")
fueldata$Capture_time<-lubridate::hms(fueldata$Capture_time)
```

Com a darrer pas en el procés de càrrega del *dataset* original es generen copies de *backup* per a les variables *Province* i *City*. Aquestes posteriorment s'hauran de modificar a través de processos de normalització de noms per tal de maximitzar la compatibilitat de les dades amb les dels altres *datasets* a integrar.
  
```{r backup_vars, vspaceecho='10pt'}
#Creació de variables de backup
fueldata$bckup.Province<-fueldata$Province
fueldata$bckup.City<-fueldata$City
```

## 2. Integració i selecció

Amb l'objectiu d'obtenir un *dataset* amb més informació que permeti generar més valor a través de l'anàlisi, es llegeix un arxiu addicional que conté el cens de població per municipis. Aquesta informació s'extreu del web de l'Institut Nacional d'Estadística (INE). En aquest cas, la lectura es realitza amb les mateixes opcions que el *dataset* original.

```{r file_import_2, vspaceecho='10pt'}
#Importar arxiu
pobdata <- read.csv(file.path(datadir, "pobmun", "pobmun22.csv"), 
                    encoding="UTF-8", sep=";")
```

A partir d'aquest moment s'executen un seguit d'operacions d'adaptació, principalment en les variables *Province* i *City* per tal de maximitzar la validesa del resultat de la integració. Així doncs, es canvien els noms de les variables i es transformen les dades a majúscules per habilitar posteriors comparacions entre els dos *datasets*.

```{r var_name_change, vspaceecho='10pt'}
#Canvis de noms
names(pobdata)[names(pobdata) == "PROVINCIA"] <- "Province"
names(pobdata)[names(pobdata) == "NOMBRE"] <- "City"
names(pobdata)[names(pobdata) == "CPRO"] <- "P_code"
names(pobdata)[names(pobdata) == "CMUN"] <- "C_code"
names(pobdata)[names(pobdata) == "POB22"] <- "Population"
names(pobdata)[names(pobdata) == "HOMBRES"] <- "P_Male"
names(pobdata)[names(pobdata) == "MUJERES"] <- "P_Female"

#Transformació a majúscules
pobdata$Province<-toupper(pobdata$Province)
pobdata$City<-toupper(pobdata$City)
```

També, es normalitzen els valors en les variables *Province* i *City* dels dos *datasets*, aquesta operació té com a objectiu eliminar accents i caràcters especials com la *ñ*. Per fer-ho es canvia el tipus de dades d'aquestes variables de UTF-8 a ASCII.

```{r char_var_code_change, vspaceecho='10pt'}
#Conversió de codificació per a normalització de caracters
fueldata$Province<-iconv(fueldata$Province, from = 'UTF-8', to = 'ASCII//TRANSLIT')
fueldata$City<-iconv(fueldata$City, from = 'UTF-8', to = 'ASCII//TRANSLIT')
pobdata$Province<-iconv(pobdata$Province, from = 'UTF-8', to = 'ASCII//TRANSLIT')
pobdata$City<-iconv(pobdata$City, from = 'UTF-8', to = 'ASCII//TRANSLIT')
```

Es canvia la denominació de 3 províncies per tal de fer la informació compatible entre els *datasets* **fueldata** i **pobdata**.

```{r province_id, vspaceecho='10pt'}
#Canvis específics en variable província
fueldata[fueldata$Province=="ALICANTE","Province"]<-"ALICANTE/ALACANT"
fueldata[fueldata$Province=="VALENCIA / VALENCIA","Province"]<-"VALENCIA/VALENCIA"
fueldata[fueldata$Province=="CASTELLON / CASTELLO","Province"]<-"CASTELLON/CASTELLO"
```

A continuació es modifica l'ús d'articles en els camps *Province* i *City* utilitzant RegEx. Inicialment, en el dataset **fueldata** les provincies o municipis amb articles tenen una estructura del tipus: "nom_municipi (article)", mentre que en el dataset **pobdata** l'estructura d'aquests es del tipus "nom_municipi, article". Així doncs es realitzen canvis en el primer per a fer-lo compatible amb el segon.

```{r city_regex, vspaceecho='10pt'}
#Canvis en l'ús d'articles a través de RegEx
fueldata$Province<-sub("(\\w+) \\((\\w+)\\)","\\1, \\2",fueldata$Province, fixed=FALSE)
fueldata$City<-sub("((\\w| )+) \\(((\\w|')+)\\)","\\1, \\3",fueldata$City, fixed=FALSE)
```

També es realitzen tot un seguit de canvis individuals (que no es detallen en la memòria, però si en el codi). Aquests canvis individuals tenen com a objectius maximitzar la informació disponible en el *dataset* resultant.

```{r individuals, include=FALSE}
#Canvis individuals
fueldata[fueldata$Province=="ALBACETE" & fueldata$City=="CHINCHILLA DE MONTE-ARAGO","City"]<-"CHINCHILLA DE MONTE-ARAGON"
fueldata[fueldata$Province=="ALICANTE/ALACANT" & fueldata$City=="BENITACHELL","City"]<-"BENITACHELL/POBLE NOU DE BENITATXELL, EL"
fueldata[fueldata$Province=="ALICANTE/ALACANT" & fueldata$City=="ALTEA LA VELLA","City"]<-"ALTEA"
fueldata[fueldata$Province=="ALICANTE/ALACANT" & fueldata$City=="CALPE","City"]<-"CALP"
fueldata[fueldata$Province=="ALICANTE/ALACANT" & fueldata$City=="HONDON DE LAS NIEVES","City"]<-"FONDO DE LES NEUS, EL/HONDON DE LAS NIEVES"
fueldata[fueldata$Province=="ALICANTE/ALACANT" & fueldata$City=="MONOVAR","City"]<-"MONOVAR/MONOVER"
fueldata[fueldata$Province=="ALICANTE/ALACANT" & fueldata$City=="MURO DEL ALCOY","City"]<-"MURO DE ALCOY"
fueldata[fueldata$Province=="ALICANTE/ALACANT" & fueldata$City=="ORIHUELA - COSTA, NUCLEO","City"]<-"ORIHUELA"
fueldata[fueldata$Province=="ALICANTE/ALACANT" & fueldata$City=="PINOSO","City"]<-"PINOS, EL/PINOSO"
fueldata[fueldata$Province=="ALICANTE/ALACANT" & fueldata$City=="VILLAJOYOSA / VILA JOIOSA, LA","City"]<-"VILLAJOYOSA/VILA JOIOSA, LA"
fueldata[fueldata$Province=="ALICANTE/ALACANT" & fueldata$City=="XIXONA","City"]<-"JIJONA/XIXONA"
fueldata[fueldata$Province=="AVILA" & fueldata$City=="MADRIGAL ALTAS TORRES","City"]<-"MADRIGAL DE LAS ALTAS TORRES"
fueldata[fueldata$Province=="AVILA" & fueldata$City=="SANTA M DE LOS CABALLEROS","City"]<-"SANTA MARIA DE LOS CABALLEROS"
fueldata[fueldata$Province=="BADAJOZ" & fueldata$City=="VALENCIA MOMBUEY","City"]<-"VALENCIA DEL MOMBUEY"
fueldata[fueldata$Province=="BARCELONA" & fueldata$City=="MONTCADA CENTRE","City"]<-"MONTCADA I REIXAC"
fueldata[fueldata$Province=="BARCELONA" & fueldata$City=="MONTORNES CENTRE","City"]<-"MONTORNES DEL VALLES"
fueldata[fueldata$Province=="BARCELONA" & fueldata$City=="MONTORNES NORD","City"]<-"MONTORNES DEL VALLES"
fueldata[fueldata$Province=="BARCELONA" & fueldata$City=="PALMA DE CERVELLO","City"]<-"PALMA DE CERVELLO, LA"
fueldata[fueldata$Province=="BARCELONA" & fueldata$City=="POBLE SEC","City"]<-"BARCELONA"
fueldata[fueldata$Province=="BARCELONA" & fueldata$City=="PONT VILOMARA I ROCAFORT","City"]<-"PONT DE VILOMARA I ROCAFORT, EL"
fueldata[fueldata$Province=="BARCELONA" & fueldata$City=="SANT FOST CAMPSENTELLES","City"]<-"SANT FOST DE CAMPSENTELLES"
fueldata[fueldata$Province=="BARCELONA" & fueldata$City=="SANT SALVADOR GUARDIOLA","City"]<-"SANT SALVADOR DE GUARDIOLA"
fueldata[fueldata$Province=="BARCELONA" & fueldata$City=="SANTA MARGARIDA MONTBUI","City"]<-"SANTA MARGARIDA DE MONTBUI"
fueldata[fueldata$Province=="BARCELONA" & fueldata$City=="TEIA CENTRE","City"]<-"TEIA"
fueldata[fueldata$Province=="BIZKAIA" & fueldata$City=="ABADINO-ZELAIETA","City"]<-"ABADINO"
fueldata[fueldata$Province=="BIZKAIA" & fueldata$City=="ABANTO","City"]<-"ABANTO Y CIERVANA-ABANTO ZIERBENAA"
fueldata[fueldata$Province=="BIZKAIA" & fueldata$City=="AMOREBIETA","City"]<-"AMOREBIETA-ETXANO"
fueldata[fueldata$Province=="BIZKAIA" & fueldata$City=="BERRIZ-OLAKUETA","City"]<-"BERRIZ"
fueldata[fueldata$Province=="BIZKAIA" & fueldata$City=="ORDUNA","City"]<-"URDUNA/ORDUNA"
fueldata[fueldata$Province=="BIZKAIA" & fueldata$City=="IZURZA","City"]<-"IZURTZA"
fueldata[fueldata$Province=="BURGOS" & fueldata$City=="HUERTA DEL REY","City"]<-"HUERTA DE REY"
fueldata[fueldata$Province=="CADIZ" & fueldata$City=="LINEA LA CONCEPCION, LA","City"]<-"LINEA DE LA CONCEPCION, LA"
fueldata[fueldata$Province=="CADIZ" & fueldata$City=="MEDINA-SIDONIA", "City"]<-"MEDINA SIDONIA"
fueldata[fueldata$Province=="CANTABRIA" & fueldata$City=="CASTRO URDIALES","City"]<-"CASTRO-URDIALES"
fueldata[fueldata$Province=="CANTABRIA" & fueldata$City=="CORRALES, LOS","City"]<-"CORRALES DE BUELNA, LOS"
fueldata[fueldata$Province=="CANTABRIA" & fueldata$City=="LOS CORRALES DE BUELNA","City"]<-"CORRALES DE BUELNA, LOS"
fueldata[fueldata$Province=="CASTELLON/CASTELLO" & fueldata$City=="ALCORA","City"]<-"ALCORA, L'"
fueldata[fueldata$Province=="CASTELLON/CASTELLO" & fueldata$City=="ALMAZORA","City"]<-"ALMASSORA"
fueldata[fueldata$Province=="CASTELLON/CASTELLO" & fueldata$City=="ALQUERIAS DEL NINO PERDIDO","City"]<-"ALQUERIES, LES/ALQUERIAS DEL NINO PERDIDO"
fueldata[fueldata$Province=="CASTELLON/CASTELLO" & fueldata$City=="BENASAL","City"]<-"BENASSAL"
fueldata[fueldata$Province=="CASTELLON/CASTELLO" & fueldata$City=="BENICASIM","City"]<-"BENICASIM/BENICASSIM"
fueldata[fueldata$Province=="CASTELLON/CASTELLO" & fueldata$City=="BENLLOCH","City"]<-"BENLLOC"
fueldata[fueldata$Province=="CASTELLON/CASTELLO" & fueldata$City=="BURRIANA","City"]<-"BORRIANA/BURRIANA"
fueldata[fueldata$Province=="CASTELLON/CASTELLO" & fueldata$City=="CASTELLON DE LA PLANA","City"]<-"CASTELLO DE LA PLANA"
fueldata[fueldata$Province=="CASTELLON/CASTELLO" & fueldata$City=="CHILCHES","City"]<-"CHILCHES/XILXES"
fueldata[fueldata$Province=="CASTELLON/CASTELLO" & fueldata$City=="LUCENA DEL CID","City"]<-"LLUCENA/LUCENA DEL CID"
fueldata[fueldata$Province=="CASTELLON/CASTELLO" & fueldata$City=="OROPESA","City"]<-"OROPESA DEL MAR/ORPESA"
fueldata[fueldata$Province=="CASTELLON/CASTELLO" & fueldata$City=="PENISCOLA","City"]<-"PENISCOLA/PENISCOLA"
fueldata[fueldata$Province=="CASTELLON/CASTELLO" & fueldata$City=="VILLAFRANCA DEL CID","City"]<-"VILAFRANCA/VILLAFRANCA DEL CID"
fueldata[fueldata$Province=="CASTELLON/CASTELLO" & fueldata$City=="VILLARREAL","City"]<-"VILA-REAL"
fueldata[fueldata$Province=="CASTELLON/CASTELLO" & fueldata$City=="VISTABELLA DEL MAESTRAZGO","City"]<-"VISTABELLA DEL MAESTRAT"
fueldata[fueldata$Province=="CIUDAD REAL" & fueldata$City=="VILLANUEVA LOS INFANTES","City"]<-"VILLANUEVA DE LOS INFANTES"
fueldata[fueldata$Province=="CORDOBA" & fueldata$City=="FERNAN NUNEZ","City"]<-"FERNAN-NUNEZ"
fueldata[fueldata$Province=="CORDOBA" & fueldata$City=="VALENZUELA Y LLANADAS","City"]<-"VALENZUELA"
fueldata[fueldata$Province=="CORUNA, A" & fueldata$City=="BETANZOS-O-VELLO","City"]<-"BETANZOS"
fueldata[fueldata$Province=="CORUNA, A" & fueldata$City=="LARACHA","City"]<-"LARACHA, A"
fueldata[fueldata$Province=="CORUNA, A" & fueldata$City=="OROSO PEQUENO","City"]<-"OROSO"
fueldata[fueldata$Province=="CORUNA, A" & fueldata$City=="POBRA DO CARAMINAL","City"]<-"POBRA DO CARAMINAL, A"
fueldata[fueldata$Province=="CORUNA, A" & fueldata$City=="PONTES DE GARCIA RODRIGUEZ, AS (SANTA MARIA)","City"]<-"PONTES DE GARCIA RODRIGUEZ, AS"
fueldata[fueldata$Province=="GIPUZKOA" & fueldata$City=="DONOSTIA-SAN SEBASTIAN","City"]<-"DONOSTIA/SAN SEBASTIAN"
fueldata[fueldata$Province=="GIPUZKOA" & fueldata$City=="LASARTE","City"]<-"LASARTE-ORIA"
fueldata[fueldata$Province=="GIRONA" & fueldata$City=="BISBAL D'EMPORDA(LA)","City"]<-"BISBAL D'EMPORDA, LA"
fueldata[fueldata$Province=="GIRONA" & fueldata$City=="BRUNYOLA","City"]<-"BRUNYOLA I SANT MARTI SAPRESA"
fueldata[fueldata$Province=="GIRONA" & fueldata$City=="CALONGE","City"]<-"CALONGE I SANT ANTONI"
fueldata[fueldata$Province=="GIRONA" & fueldata$City=="CASTELL D'ARO","City"]<-"CASTELL-PLATJA D'ARO"
fueldata[fueldata$Province=="GIRONA" & fueldata$City=="LA JONQUERA","City"]<-"JONQUERA, LA"
fueldata[fueldata$Province=="GIRONA" & fueldata$City=="LES PLANES D'HOSTOLES","City"]<-"PLANES D'HOSTOLES, LES"
fueldata[fueldata$Province=="GIRONA" & fueldata$City=="PEDRET","City"]<-"PEDRET I MARZA"
fueldata[fueldata$Province=="GIRONA" & fueldata$City=="PLATJA D'ARO","City"]<-"CASTELL-PLATJA D'ARO"
fueldata[fueldata$Province=="GIRONA" & fueldata$City=="ST JOAN DE LES ABADESSES","City"]<-"SANT JOAN DE LES ABADESSES"
fueldata[fueldata$Province=="GIRONA" & fueldata$City=="TOSSA","City"]<-"TOSSA DE MAR"
fueldata[fueldata$Province=="GRANADA" & fueldata$City=="GUEJAR-SIERRA","City"]<-"GUEJAR SIERRA"
fueldata[fueldata$Province=="GRANADA" & fueldata$City=="HUETOR-SANTILLAN","City"]<-"HUETOR DE SANTILLAN"
fueldata[fueldata$Province=="GRANADA" & fueldata$City=="HUETOR-TAJAR","City"]<-"HUETOR TAJAR"
fueldata[fueldata$Province=="GRANADA" & fueldata$City=="HUETOR-VEGA","City"]<-"HUETOR VEGA"
fueldata[fueldata$Province=="GRANADA" & fueldata$City=="PINOS-PUENTE","City"]<-"PINOS PUENTE"
fueldata[fueldata$Province=="GUADALAJARA" & fueldata$City=="MOLINA","City"]<-"MOLINA DE ARAGON"
fueldata[fueldata$Province=="HUELVA" & fueldata$City=="ISLA-CRISTINA","City"]<-"ISLA CRISTINA"
fueldata[fueldata$Province=="HUELVA" & fueldata$City=="VILLANUEVA CASTILLEJOS","City"]<-"VILLANUEVA DE LOS CASTILLEJOS"
fueldata[fueldata$Province=="HUESCA" & fueldata$City=="AINSA","City"]<-"AINSA-SOBRARBE"
fueldata[fueldata$Province=="HUESCA" & fueldata$City=="CANFRANC-ESTACION","City"]<-"CANFRANC"
fueldata[fueldata$Province=="HUESCA" & fueldata$City=="ESTOPINAN","City"]<-"ESTOPINAN DEL CASTILLO"
fueldata[fueldata$Province=="JAEN" & fueldata$City=="BEDMAR","City"]<-"BEDMAR Y GARCIEZ"
fueldata[fueldata$Province=="LLEIDA" & fueldata$City=="GIMENELLS","City"]<-"GIMENELLS I EL PLA DE LA FONT"
fueldata[fueldata$Province=="LLEIDA" & fueldata$City=="MONTFERRER","City"]<-"MONTFERRER I CASTELLBO"
fueldata[fueldata$Province=="LLEIDA" & fueldata$City=="PRATS","City"]<-"PRATS I SANSOR"
fueldata[fueldata$Province=="LLEIDA" & fueldata$City=="VIELHA","City"]<-"VIELHA E MIJARAN"
fueldata[fueldata$Province=="LUGO" & fueldata$City=="BARREIROS (CASCO URBANO)","City"]<-"BARREIROS"
fueldata[fueldata$Province=="LUGO" & fueldata$City=="LOURENZA (CASCO URBANO)","City"]<-"LOURENZA"
fueldata[fueldata$Province=="LUGO" & fueldata$City=="PASTORIZA","City"]<-"PASTORIZA, A"
fueldata[fueldata$Province=="MADRID" & fueldata$City=="ARGANDA","City"]<-"ARGANDA DEL REY"
fueldata[fueldata$Province=="MADRID" & fueldata$City=="HORCAJO DE LA SIERRA","City"]<-"HORCAJO DE LA SIERRA-AOSLOS"
fueldata[fueldata$Province=="MADRID" & fueldata$City=="LOZOYUELA","City"]<-"LOZOYUELA-NAVAS-SIETEIGLESIAS"
fueldata[fueldata$Province=="MADRID" & fueldata$City=="ORUSCO","City"]<-"ORUSCO DE TAJUNA"
fueldata[fueldata$Province=="MALAGA" & fueldata$City=="ARROYO DE LA MIEL-BENALMADENA COSTA","City"]<-"BENALMADENA"
fueldata[fueldata$Province=="MALAGA" & fueldata$City=="VILLANUEVA CONCEPCION","City"]<-"VILLANUEVA DE LA CONCEPCION"
fueldata[fueldata$Province=="MURCIA" & fueldata$City=="FUENTE ALAMO","City"]<-"FUENTE ALAMO DE MURCIA"
fueldata[fueldata$Province=="NAVARRA" & fueldata$City=="ANSOAIN","City"]<-"ANSOAIN/ANTSOAIN"
fueldata[fueldata$Province=="NAVARRA" & fueldata$City=="BARANAIN","City"]<-"BARANAIN/BARANAIN"
fueldata[fueldata$Province=="NAVARRA" & fueldata$City=="BERA / VERA DE BIDASOA","City"]<-"BERA"
fueldata[fueldata$Province=="NAVARRA" & fueldata$City=="ESTELLA O LIZARRA","City"]<-"ESTELLA-LIZARRA"
fueldata[fueldata$Province=="NAVARRA" & fueldata$City=="ETXARRI-ARANATZ","City"]<-"ETXARRI ARANATZ"
fueldata[fueldata$Province=="NAVARRA" & fueldata$City=="HUARTE","City"]<-"HUARTE/UHARTE"
fueldata[fueldata$Province=="NAVARRA" & fueldata$City=="NOAIN","City"]<-"NOAIN (VALLE DE ELORZ)/NOAIN (ELORTZIBAR)"
fueldata[fueldata$Province=="NAVARRA" & fueldata$City=="ORCOYEN","City"]<-"ORKOIEN"
fueldata[fueldata$Province=="NAVARRA" & fueldata$City=="ORONZ","City"]<-"ORONZ/ORONTZE"
fueldata[fueldata$Province=="NAVARRA" & fueldata$City=="PERALTA","City"]<-"PERALTA/AZKOIEN"
fueldata[fueldata$Province=="NAVARRA" & fueldata$City=="SANGUESA","City"]<-"SANGUESA/ZANGOZA"
fueldata[fueldata$Province=="NAVARRA" & fueldata$City=="TIEBAS","City"]<-"TIEBAS-MURUARTE DE RETA"
fueldata[fueldata$Province=="NAVARRA" & fueldata$City=="URDAZUBI / URDAX","City"]<-"URDAZUBI/URDAX"
fueldata[fueldata$Province=="NAVARRA" & fueldata$City=="URZAINQUI","City"]<-"URZAINQUI/URZAINKI"
fueldata[fueldata$Province=="NAVARRA" & fueldata$City=="VILLAVA O ATARRABIA","City"]<-"VILLAVA/ATARRABIA"
fueldata[fueldata$Province=="OURENSE" & fueldata$City=="A MEZQUITA","City"]<-"MEZQUITA, A"
fueldata[fueldata$Province=="OURENSE" & fueldata$City=="AVION, OURENSE","City"]<-"AVION"
fueldata[fueldata$Province=="OURENSE" & fueldata$City=="BARCO, O","City"]<-"BARCO DE VALDEORRAS, O"
fueldata[fueldata$Province=="OURENSE" & fueldata$City=="CARBALLINO","City"]<-"CARBALLINO, O"
fueldata[fueldata$Province=="OURENSE" & fueldata$City=="CASTRO DE CALDELAS, O","City"]<-"CASTRO CALDELAS"
fueldata[fueldata$Province=="OURENSE" & fueldata$City=="ENTRIMO, CAPITAL","City"]<-"ENTRIMO"
fueldata[fueldata$Province=="OURENSE" & fueldata$City=="NOGUEIRA","City"]<-"NOGUEIRA DE RAMUIN"
fueldata[fueldata$Province=="OURENSE" & fueldata$City=="POBRA DE TRIVES","City"]<-"POBRA DE TRIVES, A"
fueldata[fueldata$Province=="PALENCIA" & fueldata$City=="OSORNO","City"]<-"OSORNO LA MAYOR"
fueldata[fueldata$Province=="PALMAS, LAS" & fueldata$City=="EL CALERO-TELDE","City"]<-"TELDE"
fueldata[fueldata$Province=="PALMAS, LAS" & fueldata$City=="SAN BARTOLOME TIRAJANA","City"]<-"SAN BARTOLOME DE TIRAJANA"
fueldata[fueldata$Province=="PALMAS, LAS" & fueldata$City=="SANTA LUCIA","City"]<-"SANTA LUCIA DE TIRAJANA"
fueldata[fueldata$Province=="PALMAS, LAS" & fueldata$City=="COSTA TEGUISE","City"]<-"TEGUISE"
fueldata[fueldata$Province=="PALMAS, LAS" & fueldata$City=="VILLA DE TEGUISE","City"]<-"TEGUISE"
fueldata[fueldata$Province=="PONTEVEDRA" & fueldata$City=="CERDEDO","City"]<-"CERDEDO-COTOBADE"
fueldata[fueldata$Province=="PONTEVEDRA" & fueldata$City=="PAZOS","City"]<-"PAZOS DE BORBEN"
fueldata[fueldata$Province=="PONTEVEDRA" & fueldata$City=="PONTECALDELAS","City"]<-"PONTE CALDELAS"
fueldata[fueldata$Province=="RIOJA, LA" & fueldata$City=="CUZCURRITA-RIO TIRON","City"]<-"CUZCURRITA DE RIO TIRON"
fueldata[fueldata$Province=="SALAMANCA" & fueldata$City=="FRESNO-ALHANDIGA","City"]<-"FRESNO ALHANDIGA"
fueldata[fueldata$Province=="SANTA CRUZ DE TENERIFE" & fueldata$City=="ARICO NUEVO","City"]<-"ARICO"
fueldata[fueldata$Province=="SANTA CRUZ DE TENERIFE" & fueldata$City=="BRENA","City"]<-"BRENA BAJA"
fueldata[fueldata$Province=="SANTA CRUZ DE TENERIFE" & fueldata$City=="FUENCALIENTE","City"]<-"FUENCALIENTE DE LA PALMA"
fueldata[fueldata$Province=="SANTA CRUZ DE TENERIFE" & fueldata$City=="PINAR, EL","City"]<-"PINAR DE EL HIERRO, EL"
fueldata[fueldata$Province=="SANTA CRUZ DE TENERIFE" & fueldata$City=="SAN MIGUEL","City"]<-"SAN MIGUEL DE ABONA"
fueldata[fueldata$Province=="SANTA CRUZ DE TENERIFE" & fueldata$City=="TEGUESTE CENTRO","City"]<-"TEGUESTE"
fueldata[fueldata$Province=="SANTA CRUZ DE TENERIFE" & fueldata$City=="VALVERDE","City"]<-"VALVERDE DEL HIERRO"
fueldata[fueldata$Province=="SANTA CRUZ DE TENERIFE" & fueldata$City=="VILAFLOR","City"]<-"VILAFLOR DE CHASNA"
fueldata[fueldata$Province=="SEGOVIA" & fueldata$City=="FUENTESAUCO FUENTIDUENA","City"]<-"FUENTESAUCO DE FUENTIDUENA"
fueldata[fueldata$Province=="SEVILLA" & fueldata$City=="CASTILBLANCO ARROYOS","City"]<-"CASTILBLANCO DE LOS ARROYOS"
fueldata[fueldata$Province=="SEVILLA" & fueldata$City=="CASTILLO DE LAS GUARDAS","City"]<-"CASTILLO DE LAS GUARDAS, EL"
fueldata[fueldata$Province=="SEVILLA" & fueldata$City=="HUEVAR","City"]<-"HUEVAR DEL ALJARAFE"
fueldata[fueldata$Province=="SEVILLA" & fueldata$City=="LANTEJUELA, LA","City"]<-"LANTEJUELA"
fueldata[fueldata$Province=="SEVILLA" & fueldata$City=="NAVAS DE LA CONCEPCION","City"]<-"NAVAS DE LA CONCEPCION, LAS"
fueldata[fueldata$Province=="SEVILLA" & fueldata$City=="PALACIOS Y VILLAFRANCA","City"]<-"PALACIOS Y VILLAFRANCA, LOS"
fueldata[fueldata$Province=="SEVILLA" & fueldata$City=="PUEBLA DE LOS INFANTES","City"]<-"PUEBLA DE LOS INFANTES, LA"
fueldata[fueldata$Province=="SEVILLA" & fueldata$City=="VALENCINA CONCEPCION","City"]<-"VALENCINA DE LA CONCEPCION"
fueldata[fueldata$Province=="SEVILLA" & fueldata$City=="VILLANUEVA RIO Y MINAS","City"]<-"VILLANUEVA DEL RIO Y MINAS"
fueldata[fueldata$Province=="SORIA" & fueldata$City=="BURGO DE OSMA, EL","City"]<-"BURGO DE OSMA-CIUDAD DE OSMA"
fueldata[fueldata$Province=="TARRAGONA" & fueldata$City=="HOSPITALET DE L'INFANT","City"]<-"VANDELLOS I L'HOSPITALET DE L'INFANT"
fueldata[fueldata$Province=="TARRAGONA" & fueldata$City=="LA RAPITA","City"]<-"SANT CARLES DE LA RAPITA"
fueldata[fueldata$Province=="TARRAGONA" & fueldata$City=="RODA DE BARA","City"]<-"RODA DE BERA"
fueldata[fueldata$Province=="TARRAGONA" & fueldata$City=="SANT PERE I SANT PAU","City"]<-"TARRAGONA"
fueldata[fueldata$Province=="TARRAGONA" & fueldata$City=="VANDELLOS","City"]<-"VANDELLOS I L'HOSPITALET DE L'INFANT"
fueldata[fueldata$Province=="TOLEDO" & fueldata$City=="CALZADA DE OROPESA, LA","City"]<-"CALZADA DE OROPESA"
fueldata[fueldata$Province=="TOLEDO" & fueldata$City=="SESENA NUEVO","City"]<-"SESENA"
fueldata[fueldata$Province=="TOLEDO" & fueldata$City=="TORRE ESTEBAN HAMBRAN","City"]<-"TORRE DE ESTEBAN HAMBRAN, LA"
fueldata[fueldata$Province=="VALENCIA/VALENCIA" & fueldata$City=="ALBORAYA","City"]<-"ALBORAIA/ALBORAYA"
fueldata[fueldata$Province=="VALENCIA/VALENCIA" & fueldata$City=="ALGIMIA DE ALFARA","City"]<-"ALGIMIA D'ALFARA"
fueldata[fueldata$Province=="VALENCIA/VALENCIA" & fueldata$City=="BELLREGUARD POBLE","City"]<-"BELLREGUARD"
fueldata[fueldata$Province=="VALENCIA/VALENCIA" & fueldata$City=="BENISANO","City"]<-"BENISSANO"
fueldata[fueldata$Province=="VALENCIA/VALENCIA" & fueldata$City=="GENOVES","City"]<-"GENOVES, EL"
fueldata[fueldata$Province=="VALENCIA/VALENCIA" & fueldata$City=="LLOSA DE RANES","City"]<-"LLOSA DE RANES, LA"
fueldata[fueldata$Province=="VALENCIA/VALENCIA" & fueldata$City=="MASALAVES","City"]<-"MASSALAVES"
fueldata[fueldata$Province=="VALENCIA/VALENCIA" & fueldata$City=="MONSERRAT","City"]<-"MONTSERRAT"
fueldata[fueldata$Province=="VALENCIA/VALENCIA" & fueldata$City=="MONTROY","City"]<-"MONTROI/MONTROY"
fueldata[fueldata$Province=="VALENCIA/VALENCIA" & fueldata$City=="NAQUERA","City"]<-"NAQUERA/NAQUERA"
fueldata[fueldata$Province=="VALENCIA/VALENCIA" & fueldata$City=="PUIG","City"]<-"PUIG DE SANTA MARIA, EL"
fueldata[fueldata$Province=="VALENCIA/VALENCIA" & fueldata$City=="RAFELBUNOL","City"]<-"RAFELBUNYOL"
fueldata[fueldata$Province=="VALENCIA/VALENCIA" & fueldata$City=="REAL DE GANDIA","City"]<-"REAL DE GANDIA, EL"
fueldata[fueldata$Province=="VALENCIA/VALENCIA" & fueldata$City=="SAGUNTO","City"]<-"SAGUNTO/SAGUNT"
fueldata[fueldata$Province=="VALENCIA/VALENCIA" & fueldata$City=="VILLALONGA","City"]<-"VILALLONGA/VILLALONGA"
fueldata[fueldata$Province=="ZAMORA" & fueldata$City=="CORRALES","City"]<-"CORRALES DEL VINO"
fueldata[fueldata$Province=="ZARAGOZA" & fueldata$City=="VILLAMAYOR","City"]<-"VILLAMAYOR DE GALLEGO"
```

Després de tots els canvis; s'integren els dos *datasets* amb l'objectiu d'obtenir un únic *dataset* resultant que contingui tota la informació combinada. Aquesta integració es realitza de manera completa (all = TRUE), així les dades que no tenen una parella en l'altre *dataset* es mantenen en el *dataset* resultant afegint *NA* en la informació no disponible.

```{r dataset_merge, vspaceecho='10pt'}
#Combinació de datasets
total<-merge(fueldata, pobdata, by=c("Province", "City"), all = TRUE)
```

De manera adicional a les tasques d'integració s'analitza la variable *Brand* específicament, convertint-la en factor i reduint els seus nivells possibles a les 10 marques amb més representació.

```{r top_brands, vspaceecho='10pt'}
#Obtenció de marques mes rellevants
total$Brand.factor<-as.factor(total$Brand)
Brands<-as.data.frame(head(summary(total$Brand.factor),10))
names(Brands)[1]<-"Stations"
kable(Brands)
```

S'observa que en alguns registres, la marca apareix com a *substring* en la variable *Brand*, per tant, un cop obtingudes les 10 marques amb més representació s'itera sobre tots els registres per tal de normalitzar el camp marca. També, es defineix com a *"OTROS"* la variable *Brand* de tots aquells registres on la seva marca no és una de les 10 més representatives.

```{r brand_normalization, vspaceecho='10pt'}
#Noms de les 10 marques més representatives
Brand.names<-row.names(Brands)
total$Brand.factor<-as.character(total$Brand.factor)

#Iteració en les 10 marques, normalitzant nom de la marca si el conté en el string
for (brand in Brand.names){
  total$Brand.factor<-if_else(grepl(brand, total$Brand.factor),
                              brand, total$Brand.factor)
}

#Assignació de camp "OTROS"
total$Brand.factor<-if_else(total$Brand.factor %in% Brand.names,
                            total$Brand.factor, "OTROS")
total$Brand.factor<-as.factor(total$Brand.factor)
```

\newpage
Finalment, i per acabar amb les tasques d'aquest apartat, es realitza una selecció de dades limitada a les següents característiques:

* dades del dia 16 de Novembre
* dades dels combustibles
  + *Gasóleo A habitual*
  + *Gasolina 95 E5*

```{r data_selection, vspaceecho='10pt'}
#Selecció de dades
data<-total[total$Capture_date == as.Date("2022/11/16", format ="%Y/%m/%d"),]
data<-data[data$Fuel_type == "Gasóleo A habitual" | data$Fuel_type == "Gasolina 95 E5",]
```

## 3. Neteja de dades

### Zeros i elements buits

Com a primer pas en la neteja de dades, es procedeix a eliminar tots aquells registres del *dataset* integrat on el camp *Capture_date* sigui *NA*. Aquests seran registres que no han estat capturats en la fase de *web scraping* i per tant seran municipis que apareixen en el cens de població, però no tenen estació de servei. Han aparegut en el *dataset* quan s'ha realitzat l'operació de combinació plena o *FULL JOINT* en el pas anterior.

```{r clean_na, vspaceecho='10pt'}
#Neteja de NAs
data<-data[!is.na(data$Capture_date),]
```

> NOTA: Aquest pas seria prescindible si en el moment de realitzar l'operació *merge* anterior haguessin realitzat una *LEFT JOIN* amb les opcions **all.x = TRUE**, **all.y = FALSE**, enlloc del **all = TRUE** utilitzat.
 
Actualment el *dataset* conté `r nrow(data)` registres, dels quals `r nrow(data[!is.na(data$Capture_date) & is.na(data$P_code),])` són registres dels quals no se'n coneix el cens. Això representa un `r round((nrow(data[!is.na(data$Capture_date) & is.na(data$P_code),]))/(nrow(data)),2)*100`% del total de registres.

Amb l'objectiu d'identificar les provincies amb un percentatge de registres on no es coneix el cens, es procedeix a realitzar aquest mateix estudi per a cada provincia. El resultat d'aquest estudi s'observa en el següent gràfic on s'identifiquen:

* en verd, aquelles províncies amb un percentatge de NA inferior a 5%
* en blau les províncies amb un percentatge entre 5 i 10%
* en vermell aquelles províncies amb un percentatge de NA superior al 10%

\vspace{10pt}
```{r na_percent, echo=FALSE, fig.height=2.75, warning=FALSE}
DT<-data.table(data)
DT<-DT[, lapply(.SD, function(x) 100*(sum(is.na(x))/((sum(is.na(x)))+(sum(!is.na(x)))))),
       by = Province]
DT<-DT[,c("Province", "Population")]
p<-ggplot(data=DT, aes(x=Province, y=Population,
                       fill=ifelse(Population<5,mypalette[3],
                                   ifelse(Population>10, mypalette[2],
                                          mypalette[1])))) +
  geom_bar(stat="identity") +
  labs(y="NA percent [%]") +
  theme(axis.text.x = element_text(angle = 90, size = 6.5, vjust = 0.5, hjust=1,
                                   color=ifelse(DT$Population<5,mypalette[3],
                                   ifelse(DT$Population>10, mypalette[1],
                                          mypalette[2]))),
        legend.position = 'none')
p
```

En vista dels resultats, per a estudis que no considerin dades de cens per municipis, es pot utilitzar el *dataset* complert ja que aquest prové de l'operació de *scraping* i no conté zeros o valors nuls. Tanmateix, quan l'objectiu de l'estudi requereixi considerar informació del cens, es recomana limitar l'estudi a les províncies anteriorment identificades en verd i en blau, per raons de representativitat.

```{r provinces_for_population, vspaceecho='10pt'}
#Agrupament de províncies segons NAs percent
green_province<-DT[DT$Population<5,1]
blue_province<-DT[DT$Population>=5 & DT$Population<10,1]
```

### Valors extrems

Per tal d'identificar visualment els valors extrems de la variable *price* es genera un *violin plot* amb un *boxplot* al interior per cada tipus de combustible. Aquesta combinació de gràfics permet; per una banda, analitzar els *outliers* a través de la visualització *boxplot*, i per altra, conèixer la distribució de la població a través de la visualització *violin*. Així doncs permet, d'un cop de vista, veure si la informació que aporta el *boxplot* es consistent amb la distribució de les dades.

\vspace{10pt}
```{r fuel_price, echo=FALSE, fig.height=3.5, warning=FALSE}
g1<-ggplot(data=data, aes(x=Fuel_type, y=Price, color=Fuel_type)) +
  geom_violin() +
  labs(title = "Fuel price", y="Price [€/l]", x="") +
  theme(axis.text.x = element_blank(), legend.direction = "vertical", legend.position = "right")+
  geom_boxplot(width=0.15, color="black")
g1
```

Observant l'existència de valors extrems s'aprofundeix en l'anàlisi; inicialment, obtenint els valors característics del boxplot per a cada un dels combustibles seleccionats i graficats.

```{r fuel_boxplot_stats, vspaceecho='10pt', echo=FALSE}
st_D<-boxplot.stats(data[data$Fuel_type=="Gasóleo A habitual","Price"])
st_G<-boxplot.stats(data[data$Fuel_type=="Gasolina 95 E5","Price"])
stt<-rbind(st_D$stats, st_G$stats)
rownames(stt)<-c("Gasóleo A habitual","Gasolina 95 E5")
colnames(stt)<-c("Min", "Q1", "Med", "Q3", "Max")
kable(stt)
```

S'observa que la diferència de preus entre les medianes de la població *Gasóleo A habitual* i *Gasolina 95 E5* és de **`r stt[1,3]-stt[2,3]`€/l**, sent el combustible *Gasóleo A habitual* el més car.

\vspace{10pt}
**Valors extrems superiors** 

S'obtenen els registres que son valors extrems superiors tant pel cas del combustible *Gasóleo A habitual* com pel *Gasolina 95 E5*.

```{r extreme_up, vspaceecho='10pt'}
#Valors extrems superiors Gasóleo A habitual
st<-boxplot.stats(data[data$Fuel_type=="Gasóleo A habitual","Price"])
DiesUP<-data[data$Fuel_type =="Gasóleo A habitual" & data$Price>st$stats[5],] %>%
  group_by(Province, Brand) %>%
  as.data.frame()

#Valors extrems superiors Gasolina 95 E5
st<-boxplot.stats(data[data$Fuel_type=="Gasolina 95 E5","Price"])
GasUP<-data[data$Fuel_type =="Gasolina 95 E5" & data$Price>st$stats[5],] %>%
  group_by(Province, Brand) %>%
  as.data.frame()
```

Combinant la informació anterior, s'obté les dades de les estacions de servei on els dos combustibles es consideren *outliers*. Fet que indica que la estació de servei en general té uns preus més cars que la mitjana, pels dos productes. 

```{r UPs_merge, vspaceecho='10pt'}
#Combinació de valors extrems
UPs<-merge(DiesUP, GasUP, by=c("Province", "City", "Capture_date", "Address", "Brand"),
           all = FALSE)

#Ordenar valors
UPs<-UPs[c("Province", "City", "Address", "Brand", "Fuel_type.x", "Price.x",
           "Fuel_type.y", "Price.y")] %>%
  arrange(Province, City, Brand, Address)
```

Finalment, es comprova quines estacions de servei (de les marcades com a cares) no mantenen la diferència de medianes de preus de combustibles obtinguda anteriorment. Per tal de flexibilitzar la condició, es considerarà com a límit el 80% de la diferència de medianes. Per tant, es seleccionen els registres on la diferència de preu entre els dos combustibles sigui inferior a **`r round(0.8*(stt[1,3]-stt[2,3]),2)`€/l** per considerar que els seus valors no són prou consistents i que per tant poden contenir errades.

Els registres seleccionats anteriorment s'eliminen de l'estudi per a ser *outliers* i no mostrar prou consistència.

```{r inconsistent_values_remove, vspaceecho='10pt'}
#Comprovació incosistència
UPs<-UPs[UPs$Price.x-UPs$Price.y < round(0.8*(stt[1,3]-stt[2,3]),2),]

#Extreure dades inconsistents
data<-anti_join(data,UPs, by=c("Province", "City", "Address", "Brand"))
```

> NOTA: L'estudi d'*outliers* en l'extrem superior es podria allargar força més considerant: la consistència de registres en l'horitzó temporal (diferents dates), considerant la consistència de registres d'una mateixa marca en una zona pròxima, ampliar l'estudi de consistencia a tots els productes de l'estació de servei...

\vspace{10pt}
**Valors extrems inferiors**

Observant el *violin plot* anterior per al combustible *Gasóleo A habitual*, crida l'atenció la concentració de mostres al voltant del preu 1.5€/l, import que es considera "extrem". Així doncs, es seleccionen els valors d'aquest combustible i en aquest rang per estudiar-los amb més profunditat, obtenint el nombre de registres d'aquestes característiques per província.

\newpage
```{r Low_analysis, vspaceecho='10pt'}
#Selecció de registres en la zona objectiu i summarització
DiesConc<-data[data$Fuel_type =="Gasóleo A habitual" & 
                 1.55>data$Price & 
                 data$Price>1.45,] %>%
  group_by(Province) %>%
  summarise(n=n()) %>%
  as.data.frame()
kable(DiesConc)
```

La taula anterior mostra que els registres obtinguts pertanyen tots a províncies amb tipus impositius especials. Això indica, que per tal de fer un estudi coherent del preu dels combustibles s'hauran de considerar els registres de les estacions de servei peninsulars de manera separada dels registres insulars i de ciutats autònomes, doncs la diferencia d'impostos, comporta variacions significatives en el preu dels combustibles.

```{r peninsula_selection, vspaceecho='10pt'}
#Selecció dades peninsulars
non_peninsula=c("CEUTA", "MELILLA", "PALMAS, LAS", "SANTA CRUZ DE TENERIFE")
data.peninsula<-data[!(data$Province %in% non_peninsula), ]
```

Un cop extretes les dades de les ciutats autònomes de Ceuta i Melilla, així com les dades de les provinicies de les illes canàries, es grafiquen les dades peninsulars on s'observa una clara reducció en el nombre de valors extrems. 

\vspace{10pt}
```{r fuel_price_peninsula, echo=FALSE, fig.height=3.5, warning=FALSE}
g2<-ggplot(data=data.peninsula, aes(x=Fuel_type, y=Price, color=Fuel_type)) +
  geom_violin() +
  labs(title = "Fuel price (peninsula)", y="Price [€/l]", x="") +
  theme(axis.text.x = element_blank(), legend.direction = "vertical", legend.position = "right")+
  geom_boxplot(width=0.15, color="black")

g2
```

Per tal de seleccionar els valors extrems inferiors, es procedeix d'acord al mètode utilitzat prèviament en els valors extrems superiors. Així doncs, s'obtenen els registres que són valors extrems inferiors tant pel cas del combustible *Gasóleo A habitual* com pel *Gasolina 95 E5*.

```{r extreme_values_low_selection, vspaceecho='10pt'}
#Valors extrems superiors Gasóleo A habitual
st<-boxplot.stats(data.peninsula[data.peninsula$Fuel_type=="Gasóleo A habitual","Price"])
DiesLow<-data.peninsula[data.peninsula$Fuel_type =="Gasóleo A habitual" &
                          data.peninsula$Price<st$stats[1],] %>%
  group_by(Province, Brand) %>%
  as.data.frame()

#Valors extrems superiors Gasolina 95 E5
st<-boxplot.stats(data.peninsula[data.peninsula$Fuel_type=="Gasolina 95 E5","Price"])
GasLow<-data.peninsula[data.peninsula$Fuel_type =="Gasolina 95 E5" &
                         data.peninsula$Price<st$stats[1],] %>%
  group_by(Province, Brand) %>%
  as.data.frame()
```

L'existència d'estacions de servei *low cost* pot explicar l'aparició de valors extrems inferiors, coherents, en la variable *price*. Així doncs, es interessant saber si els registres seleccionats provenen d'estacions de servei d'una marca entre les 10 més representatives o formen part del grup "OTROS" que inclou aquest tipus d'estacions de servei de baix cost.

```{r low_extreme_deep_analysis_1, vspaceecho='10pt'}
#Analisis de les marques dels valors extrems inferiors
st_D<-summary(DiesLow$Brand.factor)
st_G<-summary(GasLow$Brand.factor)
stt<-rbind(st_D,st_G)
rownames(stt)<-c("Gasóleo A habitual","Gasolina 95 E5")
kable(t(stt))
```

En vista dels resultats, interessa analitzar amb més detall els registres de valors extrems inferiors obtinguts per estacions de servei **no** *lowcost*, per tractar-se de registres candidats a erronis.

```{r , vspaceecho='10pt'}
#Anàlisis pel combustible Gasóleo A habitual
D_1<-DiesLow[DiesLow$Brand.factor=="BP",]
D_2<-DiesLow[DiesLow$Brand.factor=="SHELL",]
Diesel<-data.peninsula[data.peninsula$Fuel_type=="Gasóleo A habitual" &
                     (data.peninsula$Brand.factor=="BP" |
                        data.peninsula$Brand.factor=="SHELL"),]
```

Graficant la distribució de la variable *price*, es considera el valor extrem de la marca **BP** (marcat amb un triangle negre en el gràfic) poc consistent, per altra banda, es considera que no hi ha prou evidència per decidir si el valor extrem de la marca **SHELL** és erroni i per tant es manté (marcat amb un triangle del mateix color que el gràfic).

```{r, echo=FALSE, fig.height=2.7, warning=FALSE}
g3<-ggplot(data=Diesel, aes(x=Brand.factor, y=Price, color=Brand.factor)) +
  geom_violin() +
  geom_point(data = D_1, size=5, color="black", shape = 2) +
  geom_point(data = D_2, size=5, shape = 2) +
  labs(title = "Gasóleo A price", y="Price [€/l]", x="") +
  theme(axis.text.x = element_blank(), legend.direction = "vertical",
        legend.position = "right")+
  geom_boxplot(width=0.15, color="black")

g3
```

El registre anteriorment esmentat es guarda per a una posterior extracció del *dataset*.

```{r, vspaceecho='10pt'}
#Element poc consistent a extreure
to_remove_1<-DiesLow[DiesLow$Brand.factor=="BP",]
```

Finalitzat l'anàlisi per al combustible *Gasóleo A habitual*, es continua amb l'estudi pel combustible *Gasolina 95 E5*.

```{r, vspaceecho='10pt'}
#Anàlisis pel combustible Gasolina 95 E5
G_1<-GasLow[GasLow$Brand.factor=="CAMPSA",]
G_2<-GasLow[GasLow$Brand.factor=="CEPSA",]
G_3<-GasLow[GasLow$Brand.factor=="GALP",]
G_4<-GasLow[GasLow$Brand.factor=="SHELL",]
G_4_min<-G_4[which.min(G_4$Price),]
Gasolina<-data.peninsula[data.peninsula$Fuel_type=="Gasolina 95 E5" &
                     (data.peninsula$Brand.factor=="CAMPSA" |
                        data.peninsula$Brand.factor=="CEPSA" |
                        data.peninsula$Brand.factor=="GALP" |
                        data.peninsula$Brand.factor=="SHELL"),]
```
\vspace{10pt}
```{r, echo=FALSE, fig.height=2.7, warning=FALSE}
g4<-ggplot(data=Gasolina, aes(x=Brand.factor, y=Price, color=Brand.factor)) +
  geom_violin() +
  geom_point(data = G_1, size=5, shape = 2) +
  geom_point(data = G_2, size=5, color="black", shape = 2) +
  geom_point(data = G_3, size=5, shape = 2) +
  geom_point(data = G_4_min, size=5, color="black", shape = 2) +
  labs(title = "Gasolina 95 E5 price", y="Price [€/l]", x="") +
  theme(axis.text.x = element_blank(), legend.direction = "vertical",
        legend.position = "right")+
  geom_boxplot(width=0.15, color="black")

g4
```

\newpage
Analitzant els resultats, visualment es consideren valors poc consistents:

* El valor extrem inferior per la marca **CEPSA** (marcat amb un triangle negre).
* El valor extrem mínim per la marca **SHELL** (marcat amb un triangle negre).

Conseqüentment es seleccionen els valors poc consistents i es procedeix a la seva eliminació.

```{r, vspaceecho='10pt'}
#Elements poc consistents
to_remove_2<-GasLow[GasLow$Brand.factor=="CEPSA",]
to_remove_3<-GasLow[GasLow$Brand.factor=="SHELL",]
to_remove_3<-to_remove_3[which.min(to_remove_3$Price),]

#Combinació d'elements
to_remove<-rbind(to_remove_1, to_remove_2, to_remove_3)

#Extracció
data.peninsula<-anti_join(data.peninsula,to_remove,
                          by=c("Province", "City", "Address", "Brand", "Fuel_type"))
```

Finalment es genera l'arxiu *clean_dataset.csv* que conté les dades finals, després dels processos d'integració, selecció i neteja de dades.

```{r, vspaceecho='10pt'}
#Exportar a csv
exp_folder<-file.path(datadir, "FuelData")
if (!dir.exists(exp_folder)){
  dir.create(exp_folder)
}
exp_file<-file.path(exp_folder, "clean_dataset.csv")
write.csv2(data.peninsula, file = exp_file, row.names = TRUE)
```

## 4. Anàlisi de dades

### Selecció dels grups de dades

L'anàlisi que durem a terme es divideix en 4 punts. En primer lloc estudiarem si hi ha diferències en els preus dels carburants entre Barcelona i Madrid. En segon lloc analitzarem si el preu es veu influenciat pel nombre d'habitants del municipi. A continuació veurem si el nombre d'estacions de servei per municipi influeixen en el preu i, per últim, analitzarem si la marca influeix en el preu.

```{r basic_price_analysis, vspaceecho='10pt'}
total<-data.peninsula
summary(total$Price)
summary(total[total$Fuel_type=="Gasóleo A habitual", "Price"])
summary(total[total$Fuel_type=="Gasolina 95 E5", "Price"])
```
A continuació es presenta un histograma amb l'objectiu de conèixer la distribució de la variable *Price*.

\vspace{10pt}
```{r echo=FALSE, fig.height=3.5, message=FALSE, warning=FALSE}
mean_1<-mean(total[total$Fuel_type=="Gasóleo A habitual",]$Price)
mean_2<-mean(total[total$Fuel_type=="Gasolina 95 E5",]$Price)

ggplot(data = total, aes(x=Price, color=Fuel_type)) +
  geom_histogram(fill="white", bins = 60) +
  geom_vline(aes(xintercept=mean_1), color=mypalette[1]) + 
  geom_vline(aes(xintercept=mean_2), color=mypalette[2]) +
  labs(title = "Distribució de preus") +
  xlab("Preu") +
  ylab("Nombre d'estacions de servei")
```

### Comprovació de la normalitat i homoscedasticitat

Tot i que el gràfic anterior ja ens introdueix la **no normalitat**, s'analitza a través de tests de normalitat si la variable *Price* segueix una distribució normal.

```{r, vspaceecho='10pt'}
#Anderson-Darling tests
ad.test(total$Price)
ad.test(total[total$Fuel_type=="Gasóleo A habitual", "Price"])
ad.test(total[total$Fuel_type=="Gasolina 95 E5", "Price"])
```

Valorant el p-valor obtingut, aquest és molt inferior al nivell de significació (0.05) i per tant es concloure que les dades **no** segueixen una distribució normal. Per tant, es crea una nova variable amb els nom *pnorm* amb les dades modificades segons la transformació de BoxCox i s'executa novament el test de normalitat.

```{r, vspaceecho='10pt'}
#Transformacions de BoxCox
total$pnorm<-BoxCox(total$Price, lambda = BoxCoxLambda(total$Price))
ad.test(total$pnorm)
```
Donat que les dades de la variable *Price* no segueixen una distribució normal i que la transformació Box-Cox, tampoc en millora la normalitat serà necessari considerar les versions no-paramètriques dels tests a realitzar.

```{r, fig.height=4, vspaceecho='10pt', echo=FALSE}
#QQplots
par(mfrow=c(1,2))
qqnorm(total$Price, main="Price")
qqline(total$Price)
qqnorm(total$pnorm, main="pnorm")
qqline(total$pnorm)
```



### Aplicació de proves estadístiques per comparar els grups de dades

En primer lloc, es realitza un test per comprovar la diferència de mitjanes entre les ciutats de Barcelona i Madrid. Filtrant les dades será possible analitzar si hi ha diferències entre les ciutats per cada tipus de carburant.

```{r price_BCN_vs_MAD, vspaceecho='10pt'}
#Anàlisis pel carburant Gasóleo A habitual
capital_filter <- total %>%
  filter(Fuel_type == "Gasóleo A habitual" & (City == "BARCELONA" | City == "MADRID"))
wilcox.test(Price ~ City, data = capital_filter, na.rm=TRUE, paired=FALSE,
            exact=FALSE, conf.int=TRUE)

#Análisi pel carburant Gasolina 95 E5
capital_filter2 <- total %>%
  filter(Fuel_type == "Gasolina 95 E5" & (City == "BARCELONA" | City == "MADRID"))
wilcox.test(Price ~ City, data = capital_filter2, na.rm=TRUE, paired=FALSE,
            exact=FALSE, conf.int=TRUE)
```

En ambdos tests el p-value obtingut és inferior al nivell de significació, així doncs es pot concloure que les diferències de preu entre les dues capitals són significatives pels dos combustibles.

A continuació s'estudia si hi ha diferències significatives entre la mitjana de preus dels dos combustibles en funció del nombre d'habitants del municipi. En aquest cas com que es consideren les dades del cens per a definir la dimensió del municipi, limitarem l'estudi a les províncies amb un baix percentatge de NAs.

```{r price_S_vs_M_vs_L, vspaceecho='10pt'}
#Preparació de les dades
pop_filter <- total %>%
  filter(Province %in% green_province$Province | Province %in% blue_province$Province) %>%
  filter(Fuel_type == "Gasóleo A habitual") %>%
  mutate(Pop_size = ifelse(Population < 10000, "S",
                           ifelse(Population > 100000, "L", "M")))

#Test
kruskal.test(Price ~ Pop_size, data = pop_filter)
```
El p-valor obtingut en el test estadístic és més petit que el nivell de significació (0,05), per tant es pot concloure que hi ha diferències significatives en el preu del combustible *Gasóleo A habitual* en funció del tractament (ciutat S, M o L)

A continuació es mostra el BoxPlot pel preu de *Gasóleo A habitual* en funció de la dimensió del municipi.

```{r, vspaceecho='10pt', echo=FALSE}
boxplot(Price ~ Pop_size, data = pop_filter, main = "BoxPlot Gasóleo A habitual",
        xlab = "Mida del municipi", ylab = "Preu")
```

Es repeteix el mateix procediment, però pel cas de *Gasolina 95 E5*.

```{r, vspaceecho='10pt'}
#Preparació de les dades
pop_filter_fuel <- total %>%
  filter(Province %in% green_province$Province | Province %in% blue_province$Province) %>%
  filter(Fuel_type == "Gasolina 95 E5") %>%
  mutate(Pop_size = ifelse(Population < 10000, "S",
                           ifelse(Population > 100000, "L", "M")))

#Test
kruskal.test(pnorm ~ Pop_size, data = pop_filter_fuel)
```

Novament, s'obté un p-valor inferior a 0,05 que permet concloure que la dimensió dels municipis té una influència significativa en el preu de la *Gasolina 95 E5*. Tanmateix, es mostra a continuació un Box-Plot que permet visualitzar gràficament aquestes diferències.

```{r, vspaceecho='10pt', echo=FALSE}
boxplot(Price ~ Pop_size, data = pop_filter_fuel, main = "BoxPlot Gasóleo A habitual",
        xlab = "Mida del municipi", ylab = "Preu")

```

Els resultats d'aquests anàlisi mostren que:

* De mitjana, el *Gasóleo A habitual* és més car que la *Gasolina 95 E5*.
* El *Gasóleo A habitual* és més barat a Barcelona que a Madrid, en canvi la *Gasolina 95 E5* es més barata a Madrid que a Barcelona.
* La dimensió del municipi (petit, mitjà o gran) té una influencia estadísticament significativa en el preu dels dos combustibles en estudi.

A continuació es vol estudiar si els preus del combustible estan correlacionats linealment amb el nombre d'estacions de servei per municipi.

```{r, vspaceecho='10pt'}
#Data subset
gasoil <- subset(total, Fuel_type == "Gasóleo A habitual")
gasolina <- subset(total, Fuel_type == "Gasolina 95 E5")

#Aggregats i freqüència
mean_gasoil_city <- aggregate(gasoil$Price, by=list(gasoil$City), mean)
gasoil_frequency <- table(gasoil$City)
gasoil_frequency <- as.data.frame(gasoil_frequency)

mean_gasolina_city <- aggregate(gasolina$Price, by=list(gasolina$City), mean)
gasolina_frequency <- table(gasolina$City)
gasolina_frequency <- as.data.frame(gasolina_frequency)
```

A continuació s'analitza si existeix relació entre el preu mitja del combustible de cada municipi i el nombre d'estacions de servei en aquell mateix municipi a través de la creació de models lineals.

```{r, vspaceecho='10pt'}
#Data management
result <- merge(mean_gasoil_city, gasoil_frequency, by.x = "Group.1", by.y = "Var1")
result <- result %>% rename(Municipio = Group.1, Meanp = x)

#Linear model pel combustible Gasóleo A habitual
rg <- lm(Meanp ~ Freq, data = result)
summary(rg)
```

```{r, vspaceecho='10pt'}
#Data management
resultg <- merge(mean_gasolina_city, gasolina_frequency, by.x = "Group.1", by.y = "Var1")
resultg <- resultg %>% rename(Municipio = Group.1, Meanp = x)

#Linear model pel combustible Gasolina 95 E5
lmg <- lm(Meanp ~ Freq, data = resultg)
summary(lmg)
```

```{r, fig.height=3.5, vspaceecho='10pt', echo=FALSE}
#Plots
par(mfrow=c(1,2))
plot(result$Meanp, result$Freq, main = "Gasóleo A habitual", xlab = "Price", ylab = "Frequency")
plot(resultg$Meanp, resultg$Freq, main = "Gasolina 95 E5", xlab = "Price", ylab = "Frequency")
```

En ambdos casos s'observa que no existeix una relació lineal entre el preu del combustible i el nombre d'estacions de servei en el municipi ja que els coeficients *Adjusted R-squared* són respectivament `r summary(rg)$adj.r.squared` i `r summary(lmg)$adj.r.squared`

Per finalitzar, es vol estudiar si hi ha diferencies estadísticament significatives en el preu dels combustibles en funció de la marca de l'estació de servei.

```{r, vspaceecho='10pt'}
#Gasóleo A habitual
gasoilb <- total[total$Fuel_type == "Gasóleo A habitual",]
kruskal.test(Price ~ Brand.factor, data = gasoilb)

#Gasolina 95 E5
gasolinab <- total[total$Fuel_type == "Gasolina 95 E5",]
kruskal.test(Price ~ Brand.factor, data = gasolinab)
```

Com que en ambdòs casos obtenim p-valors menors que el nivell de significació (0.05), es pot concloure que hi ha, com a mínim, un grup (marca d'estació de servei) estadísticament diferent dels altres grups en per cada combustible.

## 5. Resolució del problema

A través de l'obtenció de dades, la integració amb altres *datasets* i les operacions de selecció i de neteja; s'ha pogut donar resposta a les preguntes introduïdes a l'inici del document. Concretament es pot concloure que:

* Les dades d'estudi es poden aplicar a tota la geografia espanyola, però s'ha de tenir en compte que hi ha zones amb característiques impositives especials; que generen valors extrems. Tanmateix es important destacar que la integració dels *datasets* realitzada comporta algunes limitacions de compatibilitat. Aquestes limitacions fan que les dades en algunes províncies concretes continguin un nombre elevat (especialment en percentatge sobre el total) de NAs.
* Hi ha zones amb preus marcadament diferents de la resta, aquestes son les ciutats autònomes de Ceuta i Melilla així com les províncies de les illes canaries.
* La mitjana dels preus dels combustibles són estadísticament diferents entre Barcelona i Madrid. El *Gasóleo A habitual* és més barat a Barcelona que a Madrid, en canvi la *Gasolina 95 E5* es més barata a Madrid que a Barcelona 
* La mitjana dels preus dels combustibles és estadísticament diferent en les ciutats petites, mitjanes i grans.
* No hi ha evidència d'una relació lineal que vinculi el nombre de benzineres en un municipi (*freq*) i el preu mintja (*meanp*) del combustible en el municipi.
* La mitjana dels preus dels combustibles és estadísticament diferent en diferents marques d'estacions de servei.

## 6. Llicència

El projecte es distribueix sota llicència CC BY-NC 4.0 (Creative Commons Reconocimiento-No Comercial). Aquesta llicència permet alterar i difondre l’obra original a condició que es faci referència a l’autor, i sempre amb finalitats no comercials.

## 7. Codi

El codi es pot trobar en el següent repositori de GitHub: 

* [FuelData](https://github.com/xaviervizcaino/FuelData)

## 8. Vídeo

El vídeo amb l'explicació del desenvolupament es pot trobar a través del següent enllaç

* [vídeo](https://drive.google.com/file/d/1VvNjRVNgVLQ3uDTQa3vFjyRvemvWdPL5)

\newpage
## 9. Contribucions

```{r, vspaceecho='10pt', out.width="550px", echo=FALSE}
folder<-dirname(dirname(wd))
file<-file.path(folder, "contribucions.PNG")
knitr::include_graphics(file)
```