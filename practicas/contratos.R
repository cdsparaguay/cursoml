## set working directory
## setwd("~/MEGAsync/CDS/cursoml/practicas/data")

## libs
library(stringr)
library(ggplot2)
library(dplyr)

## stock prices
contratos = read.csv(file = 'contratos_17_18.csv', header = T, stringsAsFactors = F, sep = ";")

## convertir tipos de atributos a utilizar
contratos$nro_licitacion = contratos$nro_licitacion*1000
contratos$convocante = as.factor(contratos$convocante)
contratos$categoria = as.factor(contratos$categoria)
contratos$tipo_procedimiento = as.factor(contratos$tipo_procedimiento)
contratos$etapa_licitacion = as.factor(contratos$etapa_licitacion)
contratos$estado = as.factor(contratos$estado)
contratos$ruc = as.factor(contratos$ruc)
contratos$monto_adjudicado = as.numeric(contratos$monto_adjudicado)
contratos$moneda = as.factor(contratos$moneda)
contratos$fecha_publicacion_adj = as.Date(contratos$fecha_publicacion_adj)
contratos$subasta = as.logical(contratos$subasta)
contratos$fecha_apertura = as.Date(contratos$fecha_apertura)
contratos$fecha_firma_contrato = as.Date(contratos$fecha_firma_contrato)
contratos$fecha_publicacion_lla = as.Date(contratos$fecha_publicacion_lla)
contratos$con_proceso = as.logical(contratos$con_proceso)

## fecha de adenda, convertir nulos a NA y luego aplicar a Date la columna
contratos$fecha_publicacion_amp[which(contratos$fecha_publicacion_amp == "null")] = NA
contratos$fecha_publicacion_amp = as.Date(contratos$fecha_publicacion_amp)

## split marcas en columnas logicas
contratos = mutate(contratos, plurianual=(grepl('plurianual',contratos$marcas)))
contratos = mutate(contratos, contrato_abierto=(grepl('contrato_abierto',contratos$marcas)))
contratos = mutate(contratos, fonacide=(grepl('fonacide',contratos$marcas)))
contratos = mutate(contratos, abastecimiento_simultaneo=(grepl('abastecimiento_simultaneo',contratos$marcas)))
contratos = mutate(contratos, adreferendum=(grepl('adreferendum',contratos$marcas)))
contratos = mutate(contratos, licitacion_sin_difusion=(grepl('licitacion_sin_difusion',contratos$marcas)))
contratos = mutate(contratos, urgencia_impostergable=(grepl('urgencia_impostergable',contratos$marcas)))
contratos = mutate(contratos, seguridad_nacional=(grepl('seguridad_nacional',contratos$marcas)))
contratos = mutate(contratos, precalificacion=(grepl('precalificacion',contratos$marcas)))

## split columnas de fechas
contratos = mutate(contratos, anho_fecha_publicacion_adj=as.numeric(format(contratos$fecha_publicacion_adj, "%Y")))
contratos = mutate(contratos, mesnro_fecha_publicacion_adj=as.numeric(format(contratos$fecha_publicacion_adj, "%m")))
contratos = mutate(contratos, mes_fecha_publicacion_adj=months(contratos$fecha_publicacion_adj))
contratos$mes_fecha_publicacion_adj = factor(contratos$mes_fecha_publicacion_adj,
                                            levels = c('enero','febrero','marzo','abril',
                                                       'mayo','junio','julio','agosto',
                                                       'septiembre','octubre','noviembre','diciembre'),
                                            ordered = TRUE)

contratos = mutate(contratos, anho_fecha_apertura=as.numeric(format(contratos$fecha_apertura, "%Y")))
contratos = mutate(contratos, mesnro_fecha_apertura=as.numeric(format(contratos$fecha_apertura, "%m")))
contratos = mutate(contratos, mes_fecha_apertura=months(contratos$fecha_apertura))
contratos$mes_fecha_apertura = factor(contratos$mes_fecha_apertura,
                                             levels = c('enero','febrero','marzo','abril',
                                                        'mayo','junio','julio','agosto',
                                                        'septiembre','octubre','noviembre','diciembre'),
                                             ordered = TRUE)

contratos = mutate(contratos, anho_fecha_firma_contrato=as.numeric(format(contratos$fecha_firma_contrato, "%Y")))
contratos = mutate(contratos, mesnro_fecha_firma_contrato=as.numeric(format(contratos$fecha_firma_contrato, "%m")))
contratos = mutate(contratos, mes_fecha_firma_contrato=months(contratos$fecha_firma_contrato))
contratos$mes_fecha_firma_contrato = factor(contratos$mes_fecha_firma_contrato,
                                             levels = c('enero','febrero','marzo','abril',
                                                        'mayo','junio','julio','agosto',
                                                        'septiembre','octubre','noviembre','diciembre'),
                                             ordered = TRUE)

contratos = mutate(contratos, anho_fecha_publicacion_amp=as.numeric(format(contratos$fecha_publicacion_amp, "%Y")))
contratos = mutate(contratos, mesnro_fecha_publicacion_amp=as.numeric(format(contratos$fecha_publicacion_amp, "%m")))
contratos = mutate(contratos, mes_fecha_publicacion_amp=months(contratos$fecha_publicacion_amp))
contratos$mes_fecha_publicacion_amp = factor(contratos$mes_fecha_publicacion_amp,
                                             levels = c('enero','febrero','marzo','abril',
                                                        'mayo','junio','julio','agosto',
                                                        'septiembre','octubre','noviembre','diciembre'),
                                             ordered = TRUE)

contratos = mutate(contratos, anho_fecha_publicacion_lla=as.numeric(format(contratos$fecha_publicacion_lla, "%Y")))
contratos = mutate(contratos, mesnro_fecha_publicacion_lla=as.numeric(format(contratos$fecha_publicacion_lla, "%m")))
contratos = mutate(contratos, mes_fecha_publicacion_lla=months(contratos$fecha_publicacion_lla))
contratos$mes_fecha_publicacion_lla = factor(contratos$mes_fecha_publicacion_lla,
                                             levels = c('enero','febrero','marzo','abril',
                                                        'mayo','junio','julio','agosto',
                                                        'septiembre','octubre','noviembre','diciembre'),
                                             ordered = TRUE)

## agregar diferencia en dias
contratos = mutate(contratos, diasPublicacionApertura=contratos$fecha_apertura - contratos$fecha_publicacion_lla)
contratos = mutate(contratos, diasAperturaAdjudicacion=contratos$fecha_publicacion_adj - contratos$fecha_apertura)
contratos = mutate(contratos, diasAdjudicacionContrato=contratos$fecha_firma_contrato - contratos$fecha_publicacion_adj)
contratos = mutate(contratos, diasPublicacionContrato=contratos$fecha_firma_contrato - contratos$fecha_publicacion_lla)
contratos = mutate(contratos, diasAmpliacionContrato=contratos$fecha_publicacion_amp - contratos$fecha_firma_contrato)

## conversion de montos de contratos a Gs
contratos = mutate(contratos, monto_adjudicado_gs=contratos$monto_adjudicado)
contratos[which(contratos$X_moneda=="USD"), "monto_adjudicado_gs"] = contratos[which(contratos$X_moneda=="USD"), "monto_adjudicado_gs"] * 6000

contratosFiltrados = contratos[,c("nro_licitacion",
                                   "convocante",
                                   "categoria",
                                   "tipo_procedimiento",
                                   "etapa_licitacion",
                                   "estado",
                                   "ruc",
                                   "moneda",
                                   "monto_adjudicado_gs",
                                   "con_proceso",
                                   "subasta", 
                                   "plurianual",
                                   "contrato_abierto",
                                   "fonacide",
                                   "abastecimiento_simultaneo",
                                   "adreferendum",
                                   "licitacion_sin_difusion",
                                   "urgencia_impostergable",
                                   "seguridad_nacional",
                                   "precalificacion",
                                   "con_proceso",
                                   "fecha_publicacion_lla", 
                                   "anho_fecha_publicacion_lla",
                                   "mesnro_fecha_publicacion_lla",
                                   "mes_fecha_publicacion_lla",
                                   "fecha_apertura",
                                   "anho_fecha_apertura",
                                   "mesnro_fecha_apertura",
                                   "mes_fecha_apertura",
                                   "fecha_publicacion_adj",
                                   "anho_fecha_publicacion_adj",
                                   "mesnro_fecha_publicacion_adj",
                                   "mes_fecha_publicacion_adj",
                                   "fecha_firma_contrato",
                                   "anho_fecha_firma_contrato",
                                   "mesnro_fecha_firma_contrato",
                                   "mes_fecha_firma_contrato",
                                   "fecha_publicacion_amp",
                                   "anho_fecha_publicacion_amp",
                                   "mesnro_fecha_publicacion_amp",
                                   "mes_fecha_publicacion_amp",
                                   "diasPublicacionApertura",
                                   "diasAperturaAdjudicacion",
                                   "diasAdjudicacionContrato",
                                   "diasPublicacionContrato",
                                   "diasAmpliacionContrato")]

write.csv(contratosFiltrados,'contratos_17_18_procesados.csv',row.names = T)

