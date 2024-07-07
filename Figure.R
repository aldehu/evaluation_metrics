# rm
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data
library("readxl")
m.data <- read_excel("output_simulation_M100L3N5000.xlsx", sheet = "sim_metric")[,-1]
colnames(m.data)
cnames = c("AUC_f1pc", "ACC_f1pc", "TPR_f1pc", "TNR_f1pc", "CSI_f1pc", "GS_f1pc", "SSI_f1pc", "FAITH_f1pc",
"PDIF_f1pc", "MCC_f1pc", "G_M_f1pc", "F1_f1pc", "KAPPA_f1pc",
"AUC_f1l", "ACC_f1l", "TPR_f1l", "TNR_f1l", "CSI_f1l", "GS_f1l", "SSI_f1l", "FAITH_f1l","PDIF_f1l", "MCC_f1l", "G_M_f1l",
"F1_f1l", "KAPPA_f1l",
"AUC_kpc", "ACC_kpc", "TPR_kpc", "TNR_kpc", "CSI_kpc", "GS_kpc", "SSI_kpc",
"FAITH_kpc", "PDIF_kpc", "MCC_kpc", "G_M_kpc", "F1_kpc", "KAPPA_kpc",
"AUC_kl", "ACC_kl", "TPR_kl", "TNR_kl", "CSI_kl", "GS_kl", "SSI_kl", "FAITH_kl",
"PDIF_kl", "MCC_kl", "G_M_kl", "F1_kl", "KAPPA_kl")
colnames(m.data) = cnames
m.data = as.data.frame(m.data)

# estandarized metrics
m.data$PDIF_f1l = 1-m.data$PDIF_f1l
m.data$PDIF_f1pc = 1 - m.data$PDIF_f1pc
m.data$PDIF_kl = 1 - m.data$PDIF_kl
m.data$PDIF_kpc = 1 - m.data$PDIF_kpc

m.data$GS_f1l = (3*m.data$GS_f1l+1)/4
m.data$GS_f1pc = (3*m.data$GS_f1pc+1)/4
m.data$GS_kl = (3*m.data$GS_kl+1)/4
m.data$GS_kpc = (3*m.data$GS_kpc+1)/4

## ANALISE FOR KAPPA
# base kappa
AUC_k = select(m.data, AUC_kpc, AUC_kl)
ACC_k = select(m.data, ACC_kpc, ACC_kl)
TPR_k = select(m.data, TPR_kpc, TPR_kl)
TNR_k = select(m.data, TNR_kpc, TNR_kl)
CSI_k = select(m.data, CSI_kpc, CSI_kl)
GS_k = select(m.data, GS_kpc, GS_kl)
SSI_k = select(m.data, SSI_kpc, SSI_kl)
FAITH_k = select(m.data, FAITH_kpc, FAITH_kl)
PDIF_k = select(m.data, PDIF_kpc, PDIF_kl)
MCC_k = select(m.data, MCC_kpc, MCC_kl)
G_M_k = select(m.data, G_M_kpc, G_M_kl)
F1_k = select(m.data, F1_kpc, F1_kl)
KAPPA_k = select(m.data, KAPPA_kpc, KAPPA_kl)

# models
R = nrow(m.data)
model = as.factor(c(rep("PC", R), rep("Logistic", R)))

# ALL METRICS BY MODEL FOR K
K = 13
model = as.factor(rep(c(rep("PC", R), rep("Logistic", R)),K))
metrics = as.factor(c(rep("AUC", 2*R), rep("ACC", 2*R),
                      rep("TPR", 2*R), rep("TNR", 2*R),
                      rep("CSI", 2*R), rep("SGS", 2*R),
                      rep("SSI", 2*R), rep("FAITH", 2*R),
                      rep("SPDIF", 2*R), rep("MCC", 2*R),
                      rep("GM", 2*R), rep("F1", 2*R),
                      rep("KAPPA", 2*R)))
dataf1 = data.frame(value = c(AUC_k[,1], AUC_k[,2],
                              ACC_k[,1], ACC_k[,2], 
                              TPR_k[,1], TPR_k[,2], 
                              TNR_k[,1], TNR_k[,2], 
                              CSI_k[,1], CSI_k[,2], 
                              GS_k[,1], GS_k[,2],
                              SSI_k[,1], SSI_k[,2],
                              FAITH_k[,1], FAITH_k[,2], 
                              PDIF_k[,1], PDIF_k[,2], 
                              MCC_k[,1], MCC_k[,2],
                              G_M_k[,1], G_M_k[,2],
                              F1_k[,1], F1_k[,2],
                              KAPPA_k[,1], KAPPA_k[,2]),
                    metric = metrics,
                    model = model)
dataf1$metric = ordered(dataf1$metric,
                        levels = c("AUC", "ACC", "TPR", "TNR", "CSI", "SGS", "SSI",
                                   "FAITH", "SPDIF", "MCC", "GM", "F1", "KAPPA"))

# ECDF plots by metrics
require(dplyr)
library(ggplot2)
dataf1 %>%  
  ggplot(aes(x=value, linetype=model))+ 
  stat_ecdf(size=1.05)+
  ylab("ECDF")+
  facet_wrap(~ metric, nrow=4, scales = "free") +
  theme_bw()

gecdf025 = ggplot(dataf1, aes(x=value, linetype=model))+ 
  stat_ecdf(size=1.05)+
  ylab("ECDF")+
  facet_wrap(~ metric, nrow=4, scales = "free") +
  theme_bw()


cairo_ps(filename = "ECDFkappaN5000L3.eps",
         width = 12, height = 7, pointsize = 12,
         fallback_resolution = 300)
print(gecdf025)
dev.off()

# K.S test
#auc_pc = data.m %>% filter(metric == "auc" & model == "PC")
ks.auc = ks.test(AUC_k[,1], AUC_k[,2])
m1 = broom::tidy(ks.auc)

ks.acc = ks.test(ACC_k[,1], ACC_k[,2])
m2 = broom::tidy(ks.acc)

ks.tpr = ks.test(TPR_k[,1], TPR_k[,2])
m3 = broom::tidy(ks.tpr)

ks.tnr = ks.test(TNR_k[,1], TNR_k[,2])
m4 = broom::tidy(ks.tnr)

ks.csi = ks.test(CSI_k[,1], CSI_k[,2])
m5 = broom::tidy(ks.csi)

ks.gs = ks.test(GS_k[,1], GS_k[,2])
m6 = broom::tidy(ks.gs)

ks.ssi = ks.test(SSI_k[,1], SSI_k[,2])
m7 = broom::tidy(ks.ssi)

ks.faith =  ks.test(FAITH_k[,1], FAITH_k[,2])
m8 = broom::tidy(ks.faith)

ks.pdif = ks.test(PDIF_k[,1], PDIF_k[,2])
m9 = broom::tidy(ks.pdif)

ks.mcc = ks.test(MCC_k[,1], MCC_k[,2])
m10 = broom::tidy(ks.mcc)

ks.gm = ks.test(G_M_k[,1], G_M_k[,2])
m11 = broom::tidy(ks.gm)

ks.f1 = ks.test(F1_k[,1], F1_k[,2])
m12 = broom::tidy(ks.f1)

ks.kappa = ks.test(KAPPA_k[,1], KAPPA_k[,2])
m13 = broom::tidy(ks.kappa)

options(scipen=999)
aux2 = rbind.data.frame(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, m11, m12, m13)
aux2 = as.data.frame(aux2)
row.names(aux2) = c("AUC", "ACC", "TPR", "TNR", "CSI", "SGS", "SSI",
                    "FAITH", "SPDIF", "MCC", "GM", "F1", "KAPPA")

colnames(aux2) = c("statistic_k", "p.value_k")

library(openxlsx)
write.xlsx(aux2, file = "ks_test_L3N5000.xlsx", rowNames=T)




