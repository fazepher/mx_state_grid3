library(tidyverse)
library(geofacet)
library(magrittr)

# Geofacet de México
mx_edos_nk <- data.frame(
  row = c(1, 1, 1, 1, 1, 1, 
          2, 2, 2, 2, 2, 
          3, 3, 3, 3, 
          4, 4, 4, 4, 4, 4, 4, 
          5, 5, 5, 5, 5, 5, 5, 
          6, 6, 6),
  col = c(1, 2, 3, 4, 5, 6, 
          1, 3, 4, 5, 6, 
          4, 5, 6, 7, 
          4, 5, 6, 7, 8, 11, 12,
          5, 6, 7, 8, 9, 10, 11,
          7, 8,9),
  code = c("BC", "SON", "CHIH", "COAH", "NL", "TAM", 
           "BCS", "SIN", "DGO", "ZAC", "SLP", 
           "NAY", "AGS", "GTO", "HGO",  
           "COL", "JAL", "QRO","CDMX","PUE", "YUC", "QROO", 
           "MICH", "MEX", "MOR", "TLAX", "VER", "TAB", "CAMP", 
           "GRO", "OAX","CHPS"),
  name = c("Baja California", "Sonora", "Chihuahua", "Coahuila", "Nuevo León", "Tamaulipas", 
           "Baja California Sur", "Sinaloa", "Durango", "Zacatecas", "San Luis Potosí", 
           "Nayarit", "Aguascalientes", "Guanajuato", "Hidalgo",  
           "Colima", "Jalisco", "Querétaro", "Ciudad de México", "Puebla", "Yucatan", "Quintana Roo", 
           "Michoacán", "Estado de México", "Morelos", "Tlaxcala", "Veracruz","Tabasco","Campeche",  
           "Guerrero", "Oaxaca","Chiapas"),
  code_edo = c("02","26","08","05","19","28",
               "03","25","10","32","24",
               "18","01","11","13",
               "06","14","22","09","21","31","23",
               "16", "15","17","29","30","27","04",
               "12","20","07"),
  stringsAsFactors = FALSE) 

colores_partidos <- c("PAN" = "dodgerblue3",
                      "PRI" = "red",
                      "PRD" = "gold",
                      "PT" = "saddlebrown",
                      "PVEM" = "springgreen4",
                      "MC" = "sienna1",
                      "PANAL" = "turquoise3",
                      "MORENA" = "maroon",
                      "PES" = "plum1",
                      "INDEP" = "magenta4",
                      "FPM" = "dodgerblue3",
                      "JPM" = "red", 
                      "JHH" = "maroon")


Coal_Senado <- read_csv("Datos_Senado.csv",
                        locale = locale(encoding = "latin1"))

Senado_CD <- read_csv("20180708_2100_CW_senadurias/senadurias_fazh.csv",
                      col_types = paste(rep("c",42),collapse = "")) %>% 
  select(ID_ESTADO,NOMBRE_ESTADO,PAN:CAND_IND_02) %>% 
  gather(Partido,Votos,-ID_ESTADO,-NOMBRE_ESTADO) %>% 
  transmute(edo=as.integer(ID_ESTADO),NOMBRE_ESTADO,Partido,Votos) %>% 
  mutate(Partido = case_when(Partido == "MOVIMIENTO CIUDADANO" ~ "MC",
                             Partido == "NUEVA ALIANZA" ~ "PANAL",
                             Partido == "ENCUENTRO SOCIAL" ~ "PES",
                             Partido %in% c("CAND_IND_01","CAND_IND_02") ~ "INDEP",
                             T ~ Partido)) %>% 
  group_by(edo,NOMBRE_ESTADO,Partido) %>% 
  summarise_at("Votos",funs(sum(as.integer(.),na.rm = .))) %>% 
  ungroup %>% 
  left_join(Coal_Senado,by = "edo") %>% 
  spread(Partido,Votos) %>% 
  mutate(FPM = if_else(!is.na(FPM1),
                       PAN + PRD + MC + PAN_PRD_MC + PAN_PRD + PAN_MC + PRD_MC, 
                       0L),
         JPM = if_else(!is.na(JPM1),
                       PRI + PVEM + PANAL + PRI_PVEM_NA + PRI_PVEM + PRI_NA + PVEM_NA, 
                       0L),
         JHH = if_else(!is.na(JHH1),
                       MORENA + PT + PES + PT_MORENA_PES + PT_MORENA + PT_PES + MORENA_PES, 
                       0L)) %>% 
  gather(Senador,Votos,one_of(names(colores_partidos))) %>% 
  filter(!{JPM1 %in% c("PRI","PVEM","PANAL") & Senador %in% c("PRI","PVEM","PANAL")}) %>% 
  filter(!{FPM1 %in% c("PAN","PRD","MC") & Senador %in% c("PAN","PRD","MC")}) %>% 
  filter(!{JHH1 %in% c("MORENA","PT","PES") & Senador %in% c("MORENA","PT","PES")}) %>% 
  split(.$edo) %>% 
  map_df(function(estado) estado %>% 
           mutate(Rank=rank(Votos,ties.method="random")) %>% 
           top_n(2,Rank) %>% 
           arrange(desc(Rank)) %>% 
           extract2("Senador") %>% 
           {mutate(estado,Ganador=.[1],Primera_Min = .[2])}) %>% 
  mutate(edo = str_pad(edo,2,"left","0"),
         Senador = factor(Senador,
                          levels=rev(c("FPM","JPM","JHH",
                                       "PAN","PRI","PRD","PT","PVEM",
                                       "MC","PANAL","MORENA","PES","INDEP")),
                          ordered = T),
         MR1 = case_when(Ganador == "FPM" ~ FPM1,
                         Ganador == "JPM" ~ JPM1,
                         Ganador == "JHH" ~ JHH1,
                         T ~ Ganador),
         MR2 = case_when(Ganador == "FPM" ~ FPM2,
                         Ganador == "JPM" ~ JPM2,
                         Ganador == "JHH" ~ JHH2,
                         T ~ Ganador),
         PM = case_when(Primera_Min == "FPM" ~ FPM1,
                        Primera_Min == "JPM" ~ JPM1,
                        Primera_Min == "JHH" ~ JHH1,
                        T ~ Primera_Min))


Datos_graf_senado <- Senado_CD  %>% 
  select(edo,ESTADO,Ganador,Primera_Min,MR1,MR2,PM,Senador,Votos) %>% 
  group_by(edo) %>%
  mutate(Pct = Votos/sum(Votos),Senador = as.character(Senador)) %>% 
  ungroup %>% 
  rename(code_edo = edo) %>% 
  left_join(mx_edos_nk,by="code_edo") %>% 
  gather(Tipo_Senador,Partido,MR1:PM) %>% 
  mutate(Partido = factor(Partido,levels=names(colores_partidos),ordered = TRUE)) %>% 
  arrange(code_edo) %>% 
  filter({Senador == Ganador & Tipo_Senador %in% c("MR1","MR2")} | {Senador == Primera_Min & Tipo_Senador == "PM"}) 

graf_ppal <- Datos_graf_senado %>% 
  ggplot(aes(x=Tipo_Senador,y=Pct, label = round(100*Pct), fill = Partido)) + 
  geom_col() + 
  geom_text(color = "gray25", nudge_y = 0.2) + 
  facet_geo(~code_edo,grid = mx_edos_nk,label = "name") + 
  labs(title = "Senadores electos por entidad federativa", 
       subtitle = "Proceso electoral, México 2018.", 
       x = "Tipo de Senadores", 
       y = "% efectivo de la fórmula", 
       caption = "Fuente: @fazepher con datos de los cómputos distritales del INE. 
\n MR1/MR2 significan las posiciones de mayoría relativa y PM significa Primera Minoría.", 
       fill = "Partido por orden de registro") + 
  guides(fill = guide_legend(nrow = 3)) + 
  ylim(c(0,1)) +
  theme_minimal() + 
  scale_fill_manual(values = colores_partidos) + 
  theme(legend.position = c(4.5/6,8.5/12), 
        plot.background = element_rect(fill="gray96"), 
        plot.margin = unit(c(30,75,30,75),units = "points"),
        panel.background = element_rect(fill="gray98"),
        axis.title.x = element_text(margin = margin(t=15)),
        axis.title.y = element_text(margin = margin(r=30)), 
        panel.grid = element_blank()) 


# Pluris_CD <- read_csv("20180708_2100_CW_senadurias/senadurias_fazh.csv",
#                       col_types = paste(rep("c",42),collapse = "")) %>% 
#   select(ID_ESTADO,NOMBRE_ESTADO,ID_DISTRITO,PAN:CAND_IND_02) %>% 
#   gather(Partido,Votos,-ID_ESTADO,-NOMBRE_ESTADO,-ID_DISTRITO) %>% 
#   transmute(edo=as.integer(ID_ESTADO),NOMBRE_ESTADO,ID_DISTRITO,Partido,Votos) %>% 
#   mutate(Partido = case_when(Partido == "MOVIMIENTO CIUDADANO" ~ "MC",
#                              Partido == "NUEVA ALIANZA" ~ "PANAL",
#                              Partido == "ENCUENTRO SOCIAL" ~ "PES",
#                              Partido %in% c("CAND_IND_01","CAND_IND_02") ~ "INDEP",
#                              T ~ Partido)) %>% 
#   group_by(edo,NOMBRE_ESTADO,ID_DISTRITO,Partido) %>% 
#   summarise_at("Votos",funs(sum(as.integer(.),na.rm = .))) %>% 
#   spread(Partido,Votos) %>% 
#   mutate(PAN = PAN + floor(PAN_PRD_MC/3) + floor(PAN_MC/2) + floor(PAN_PRD/2),
#          PRI = PRI + floor(PRI_PVEM_NA/3) + floor(PRI_PVEM/2) + floor(PRI_NA/2),
#          PRD = PRD + floor(PAN_PRD_MC/3) + floor(PRD_MC/2) + floor(PAN_PRD/2),
#          PVEM = PVEM + floor(PRI_PVEM_NA/3) + floor(PRI_PVEM/2) + floor(PVEM_NA/2),
#          PT = PT + floor(PT_MORENA_PES/3) + floor(PT_MORENA/2) + floor(PT_PES/2),
#          MC = MC + floor(PAN_PRD_MC/3) + floor(PAN_MC/2) + floor(PRD_MC/2),
#          PANAL = PANAL + floor(PRI_PVEM_NA/3) + floor(PRI_NA/2) + floor(PVEM_NA/2),
#          MORENA = MORENA + floor(PT_MORENA_PES/3) + floor(PT_MORENA/2) + floor(MORENA_PES/2),
#          PES = PES + floor(PT_MORENA_PES/3) + floor(PT_PES/2) + floor(MORENA_PES/2)) %>% 
#   mutate_at(c("PAN_PRD_MC","PRI_PVEM_NA","PT_MORENA_PES"),funs(.%%3)) %>% 
#   mutate_at(c("PAN_PRD","PAN_MC","PRD_MC",
#               "PRI_PVEM","PRI_NA","PVEM_NA",
#               "PT_MORENA","PT_PES","MORENA_PES"),funs(.%%2)) %>% 
#   mutate_at(c("PAN","PRD","MC"),funs(if_else(. == max(PAN,PRD,MC), . + PAN_PRD_MC, .))) %>% 
#   mutate_at(c("PAN","PRD"),funs(if_else(. == max(PAN,PRD), . + PAN_PRD, .))) %>% 
#   mutate_at(c("PAN","MC"),funs(if_else(. == max(PAN,MC), . + PAN_MC, .))) %>% 
#   mutate_at(c("PRD","MC"),funs(if_else(. == max(PRD,MC), . + PRD_MC, .))) %>% 
#   mutate_at(c("PRI","PVEM","PANAL"),funs(if_else(. == max(PRI,PVEM,PANAL), . + PRI_PVEM_NA, .))) %>% 
#   mutate_at(c("PRI","PVEM"),funs(if_else(. == max(PRI,PVEM), . + PRI_PVEM, .))) %>% 
#   mutate_at(c("PRI","PANAL"),funs(if_else(. == max(PRI,PANAL), . + PRI_NA, .))) %>% 
#   mutate_at(c("PVEM","PANAL"),funs(if_else(. == max(PVEM,PANAL), . + PVEM_NA, .))) %>% 
#   mutate_at(c("PT","MORENA","PES"),funs(if_else(. == max(PT,MORENA,PES), . + PT_MORENA_PES, .))) %>% 
#   mutate_at(c("PT","MORENA"),funs(if_else(. == max(PT,MORENA), . + PT_MORENA, .))) %>% 
#   mutate_at(c("PT","PES"),funs(if_else(. == max(PT,PES), . + PT_PES, .))) %>% 
#   mutate_at(c("MORENA","PES"),funs(if_else(. == max(MORENA,PES), . + MORENA_PES, .))) %>% 
#   select(-one_of(c(c("PAN_PRD_MC","PRI_PVEM_NA","PT_MORENA_PES"),
#                    c("PAN_PRD","PAN_MC","PRD_MC",
#                      "PRI_PVEM","PRI_NA","PVEM_NA",
#                      "PT_MORENA","PT_PES","MORENA_PES")))) %>% 
#   gather(Partido,Votos,-edo,-NOMBRE_ESTADO,-ID_DISTRITO) 

# graf_pluris <- Pluris_CD %>% 
#   arrange(edo,NOMBRE_ESTADO,ID_DISTRITO) %>% 
#   ungroup() %>% 
#   group_by(Partido) %>% 
#   summarise_at("Votos",sum) %>% 
#   mutate(Pct = Votos/sum(Votos),
#          DERECHO_RP = {Pct >= 0.03 & Partido != "INDEP"} %>% as.numeric,
#          CN = sum(DERECHO_RP*Votos,na.rm = TRUE)/32,
#          RP_CN = DERECHO_RP*floor(Votos/CN),
#          Resto = DERECHO_RP*Votos-CN*RP_CN,
#          Sen_Rest = 32-sum(RP_CN),
#          RP_RM = {n() - rank(Resto,ties.method = "random") + 1 <= Sen_Rest} %>% as.numeric,
#          TOT_RP = RP_CN + RP_RM) %>% 
#   filter(Partido != "INDEP") %>% 
#   ggplot(aes(x=factor(Partido,levels = names(colores_partidos), ordered = TRUE),
#              y=1,fill=Partido,label=TOT_RP)) + 
#   geom_col() + 
#   geom_text(position = position_stack(vjust=0.5), size = rel(10),color="gray25") +
#   scale_fill_manual(values = colores_partidos) + 
#   labs(x = "Senadores plurinominales", y = "") + 
#   theme_void() + 
#   theme(legend.position = "none",
#         plot.margin = unit(c(8,0,8,0),units = "cm"),
#         axis.text.x = element_text(size = rel(20),color="gray45"),
#         axis.title = element_text(size = rel(2), margin = margin(t = 20), color = "gray45"))
  
  
  
  
  
  
  
  
  
  
  