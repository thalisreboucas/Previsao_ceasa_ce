# Analisando os graficos
ABACATE[[5]] #P 
ABACAXI[[4]] #P
ABOBORA[[4]] #P
ABOBRINHA[[4]] #N
ALFACE[[4]] #P
ALHO[[4]] # N
BANANA_NANICA[[4]] # P
BANANA_PRATA[[4]] # P
BATATA[[4]] # P OU N
BATATA_DOCE[[4]] # P
BERINJELA[[4]] # P
BETERRABA[[4]]
BROCOLO[[4]] #P*
CARA[[4]] # P*
CEBOLA[[4]] #N
CENOURA[[4]] #N
CHUCHU[[4]] # 0 
COCO_VERDE[[4]] # P
COUVE[[4]] # P
COUVE_FLOR[[4]] #N*
GOIABA[[4]] # P = N 
INHAME[[4]] # N = P
JILO[[4]] # P
LARANJA_PERA[[4]]  #P = N
LIMAO_TAHITI[[4]] #N
MACA[[4]] #N
MAMAO_FORMOSA[[4]] # 0
MAMAO_HAWAY[[4]] #0
MANDIOCA[[4]] #P*
MANDIOQUINHA[[4]] #P*
MANGA[[4]] # N*=P*
MARACUJA_AZEDO[[4]] # P*
MELANCIA[[4]]
MELAO_AMARELO[[4]]
MILHO_VERDE[[4]]
MORANGO[[4]]
OVOS[[4]]
PEPINO[[4]]
PERA_IMPORTADA[[4]]
PIMENTAO_VERDE[[4]]
QUIABO[[4]]
REPOLHO[[4]]
TANGERINA[[4]]
TOMATE[[4]]
UVA_ITALIA[[4]]
UVA_NIAGARA[[4]]
VAGEM[[4]]

# vendo as previsões

ABACATE_Prev[[3]]

ABACAXI_Prev[[3]]
ABOBORA_Prev[[3]]
ABOBRINHA_Prev[[3]]
ALFACE_Prev[[3]]
ALHO_Prev[[3]]
BANANA_NANICA_Prev[[3]]
BANANA_PRATA_Prev[[3]]
BATATA_Prev[[3]]
BATATA_DOCE_Prev[[3]]
BERINJELA_Prev[[3]]
BETERRABA_Prev[[3]]
BROCOLO_Prev[[3]]
CARA_Prev[[3]]
CEBOLA_Prev[[3]]
CENOURA_Prev[[3]]
CHUCHU_Prev[[3]]
COCO_VERDE_Prev[[3]]
COUVE_Prev[[3]]
COUVE_FLOR_Prev[[3]]
GOIABA_Prev[[3]]
INHAME_Prev[[3]]
JILO_Prev[[3]]
LARANJA_PERA_Prev[[3]]
LIMAO_TAHITI_Prev[[3]]
MACA_Prev[[3]]
MAMAO_FORMOSA_Prev[[3]]
MAMAO_HAWAY_Prev[[3]]
MANDIOCA_Prev[[3]]
MANDIOQUINHA_Prev[[3]]
MANGA_Prev[[3]]
MARACUJA_AZEDO_Prev[[3]]
MELANCIA_Prev[[3]]
MELAO_AMARELO_Prev[[3]]
MILHO_VERDE_Prev[[3]]
MORANGO_Prev[[3]]
OVOS_Prev[[3]]
PEPINO_Prev[[3]]
PERA_IMPORTADA_Prev[[3]]
PIMENTAO_VERDE_Prev[[3]]
QUIABO_Prev[[3]]
REPOLHO_Prev[[3]]
TANGERINA_Prev[[3]]
TOMATE_Prev[[3]]
UVA_ITALIA_Prev[[3]]
UVA_NIAGARA_Prev[[3]]
VAGEM_Prev[[3]]

ABACATE_tb<-ABACATE_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 1,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
ABACAXI_tb<-ABACAXI_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 2,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
ABOBORA_tb<-ABOBORA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 3,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
ABOBRINHA_tb<-ABOBRINHA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 4,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
ALFACE_tb<-ALFACE_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 5,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
ALHO_tb<-ALHO_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 6,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
BANANA_NANICA_tb<-BANANA_NANICA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 7,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
BANANA_PRATA_tb<-BANANA_PRATA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 8,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
BATATA_tb<-BATATA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 9,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
BATATA_DOCE_tb<-BATATA_DOCE_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 10,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
BERINJELA_tb<-BERINJELA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 11,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
BETERRABA_tb<-BETERRABA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 12,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
BROCOLO_tb<-BROCOLO_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 13,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
CARA_tb<-CARA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 14,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
CEBOLA_tb<-CEBOLA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 15,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
CENOURA_tb<-CENOURA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 16,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
CHUCHU_tb<-CHUCHU_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 17,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
COCO_VERDE_tb<-COCO_VERDE_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 18,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
COUVE_tb<-COUVE_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 19,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
COUVE_FLOR_tb<-COUVE_FLOR_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 20,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
GOIABA_tb<-GOIABA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 21,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
INHAME_tb<-INHAME_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 22,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
JILO_tb<-JILO_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 23,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
LARANJA_PERA_tb<-LARANJA_PERA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 24,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
LIMAO_TAHITI_tb<-LIMAO_TAHITI_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 25,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MACA_tb<-MACA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 26,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MAMAO_FORMOSA_tb<-MAMAO_FORMOSA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 27,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MAMAO_HAWAY_tb<-MAMAO_HAWAY_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 28,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MANDIOCA_tb<-MANDIOCA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 29,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MANDIOQUINHA_tb<-MANDIOQUINHA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 30,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MANGA_tb<-MANGA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 31,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MARACUJA_AZEDO_tb<-MARACUJA_AZEDO_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 32,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MELANCIA_tb<-MELANCIA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 33,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MELAO_AMARELO_tb<-MELAO_AMARELO_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 34,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MILHO_VERDE_tb<-MILHO_VERDE_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 35,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MORANGO_tb<-MORANGO_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 36,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
OVOS_tb<-OVOS_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 37,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
PEPINO_tb<-PEPINO_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 38,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
PERA_IMPORTADA_tb<-PERA_IMPORTADA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 39,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
PIMENTAO_VERDE_tb<-PIMENTAO_VERDE_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 40,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
QUIABO_tb<-QUIABO_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 41,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
REPOLHO_tb<-REPOLHO_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 42,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
TANGERINA_tb<-TANGERINA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 43,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
TOMATE_tb<-TOMATE_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 44,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
UVA_ITALIA_tb<-UVA_ITALIA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 45,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
UVA_NIAGARA_tb<-UVA_NIAGARA_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 46,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
VAGEM_tb<-VAGEM_Prev[[2]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 47,.value = round(.value,2),.conf_hi = round(.conf_hi,2))



setwd("E:\\edime\\Thalis\\MEU\\Ceasa")
Tabela <- bind_rows(ABACATE_tb,ABACAXI_tb,ABOBORA_tb,ABOBRINHA_tb,ALFACE_tb,ALHO_tb,BANANA_NANICA_tb,BANANA_PRATA_tb,BATATA_tb,BATATA_DOCE_tb,BERINJELA_tb,BETERRABA_tb,BROCOLO_tb,CARA_tb,CEBOLA_tb,CENOURA_tb,CHUCHU_tb,COCO_VERDE_tb,COUVE_tb,COUVE_FLOR_tb,GOIABA_tb,INHAME_tb,JILO_tb,LARANJA_PERA_tb,LIMAO_TAHITI_tb,MACA_tb,MAMAO_FORMOSA_tb,MAMAO_HAWAY_tb,MANDIOCA_tb,MANDIOQUINHA_tb,MANGA_tb,MARACUJA_AZEDO_tb,MELANCIA_tb,MELAO_AMARELO_tb,MILHO_VERDE_tb,MORANGO_tb,OVOS_tb,PEPINO_tb,PERA_IMPORTADA_tb,PIMENTAO_VERDE_tb,QUIABO_tb,REPOLHO_tb,TANGERINA_tb,TOMATE_tb,UVA_ITALIA_tb,UVA_NIAGARA_tb,VAGEM_tb)
write.csv2(Tabela,file="Prev_ceasa.csv")


ABACATE_tb<-ABACATE[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 1,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
ABACAXI_tb<-ABACAXI[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 2,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
ABOBORA_tb<-ABOBORA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 3,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
ABOBRINHA_tb<-ABOBRINHA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 4,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
ALFACE_tb<-ALFACE[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 5,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
ALHO_tb<-ALHO[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 6,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
BANANA_NANICA_tb<-BANANA_NANICA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 7,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
BANANA_PRATA_tb<-BANANA_PRATA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 8,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
BATATA_tb<-BATATA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 9,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
BATATA_DOCE_tb<-BATATA_DOCE[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 10,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
BERINJELA_tb<-BERINJELA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 11,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
BETERRABA_tb<-BETERRABA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 12,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
BROCOLO_tb<-BROCOLO[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 13,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
CARA_tb<-CARA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 14,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
CEBOLA_tb<-CEBOLA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 15,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
CENOURA_tb<-CENOURA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 16,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
CHUCHU_tb<-CHUCHU[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 17,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
COCO_VERDE_tb<-COCO_VERDE[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 18,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
COUVE_tb<-COUVE[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 19,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
COUVE_FLOR_tb<-COUVE_FLOR[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 20,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
GOIABA_tb<-GOIABA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 21,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
INHAME_tb<-INHAME[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 22,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
JILO_tb<-JILO[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 23,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
LARANJA_PERA_tb<-LARANJA_PERA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 24,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
LIMAO_TAHITI_tb<-LIMAO_TAHITI[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 25,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MACA_tb<-MACA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 26,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MAMAO_FORMOSA_tb<-MAMAO_FORMOSA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 27,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MAMAO_HAWAY_tb<-MAMAO_HAWAY[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 28,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MANDIOCA_tb<-MANDIOCA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 29,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MANDIOQUINHA_tb<-MANDIOQUINHA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 30,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MANGA_tb<-MANGA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 31,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MARACUJA_AZEDO_tb<-MARACUJA_AZEDO[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 32,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MELANCIA_tb<-MELANCIA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 33,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MELAO_AMARELO_tb<-MELAO_AMARELO[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 34,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MILHO_VERDE_tb<-MILHO_VERDE[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 35,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
MORANGO_tb<-MORANGO[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 36,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
OVOS_tb<-OVOS[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 37,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
PEPINO_tb<-PEPINO[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 38,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
PERA_IMPORTADA_tb<-PERA_IMPORTADA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 39,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
PIMENTAO_VERDE_tb<-PIMENTAO_VERDE[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 40,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
QUIABO_tb<-QUIABO[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 41,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
REPOLHO_tb<-REPOLHO[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 42,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
TANGERINA_tb<-TANGERINA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 43,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
TOMATE_tb<-TOMATE[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 44,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
UVA_ITALIA_tb<-UVA_ITALIA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 45,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
UVA_NIAGARA_tb<-UVA_NIAGARA[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 46,.value = round(.value,2),.conf_hi = round(.conf_hi,2))
VAGEM_tb<-VAGEM[[3]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.value,.conf_hi) %>%  mutate(.model_id = 47,.value = round(.value,2),.conf_hi = round(.conf_hi,2))


Tabela <- bind_rows(ABACATE_tb,ABACAXI_tb,ABOBORA_tb,ABOBRINHA_tb,ALFACE_tb,ALHO_tb,BANANA_NANICA_tb,BANANA_PRATA_tb,BATATA_tb,BATATA_DOCE_tb,BERINJELA_tb,BETERRABA_tb,BROCOLO_tb,CARA_tb,CEBOLA_tb,CENOURA_tb,CHUCHU_tb,COCO_VERDE_tb,COUVE_tb,COUVE_FLOR_tb,GOIABA_tb,INHAME_tb,JILO_tb,LARANJA_PERA_tb,LIMAO_TAHITI_tb,MACA_tb,MAMAO_FORMOSA_tb,MAMAO_HAWAY_tb,MANDIOCA_tb,MANDIOQUINHA_tb,MANGA_tb,MARACUJA_AZEDO_tb,MELANCIA_tb,MELAO_AMARELO_tb,MILHO_VERDE_tb,MORANGO_tb,OVOS_tb,PEPINO_tb,PERA_IMPORTADA_tb,PIMENTAO_VERDE_tb,QUIABO_tb,REPOLHO_tb,TANGERINA_tb,TOMATE_tb,UVA_ITALIA_tb,UVA_NIAGARA_tb,VAGEM_tb)
write.csv2(Tabela,file="treino_ceasa.csv")#3

ABACATE_tb<-ABACATE[[6]] %>%   select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 1,.actual = round(.actual,2))
ABACAXI_tb<-ABACAXI[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 2,.actual = round(.actual,2))
ABOBORA_tb<-ABOBORA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 3,.actual = round(.actual,2))
ABOBRINHA_tb<-ABOBRINHA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 4,.actual = round(.actual,2))
ALFACE_tb<-ALFACE[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 5,.actual = round(.actual,2))
ALHO_tb<-ALHO[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 6,.actual = round(.actual,2))
BANANA_NANICA_tb<-BANANA_NANICA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 7,.actual = round(.actual,2))
BANANA_PRATA_tb<-BANANA_PRATA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 8,.actual = round(.actual,2))
BATATA_tb<-BATATA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 9,.actual = round(.actual,2))
BATATA_DOCE_tb<-BATATA_DOCE[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 10,.actual = round(.actual,2))
BERINJELA_tb<-BERINJELA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 11,.actual = round(.actual,2))
BETERRABA_tb<-BETERRABA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 12,.actual = round(.actual,2))
BROCOLO_tb<-BROCOLO[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 13,.actual = round(.actual,2))
CARA_tb<-CARA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 14,.actual = round(.actual,2))
CEBOLA_tb<-CEBOLA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 15,.actual = round(.actual,2))
CENOURA_tb<-CENOURA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 16,.actual = round(.actual,2))
CHUCHU_tb<-CHUCHU[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 17,.actual = round(.actual,2))
COCO_VERDE_tb<-COCO_VERDE[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 18,.actual = round(.actual,2))
COUVE_tb<-COUVE[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 19,.actual = round(.actual,2))
COUVE_FLOR_tb<-COUVE_FLOR[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 20,.actual = round(.actual,2))
GOIABA_tb<-GOIABA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 21,.actual = round(.actual,2))
INHAME_tb<-INHAME[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 22,.actual = round(.actual,2))
JILO_tb<-JILO[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 23,.actual = round(.actual,2))
LARANJA_PERA_tb<-LARANJA_PERA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 24,.actual = round(.actual,2))
LIMAO_TAHITI_tb<-LIMAO_TAHITI[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 25,.actual = round(.actual,2))
MACA_tb<-MACA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 26,.actual = round(.actual,2))
MAMAO_FORMOSA_tb<-MAMAO_FORMOSA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 27,.actual = round(.actual,2))
MAMAO_HAWAY_tb<-MAMAO_HAWAY[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 28,.actual = round(.actual,2))
MANDIOCA_tb<-MANDIOCA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 29,.actual = round(.actual,2))
MANDIOQUINHA_tb<-MANDIOQUINHA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 30,.actual = round(.actual,2))
MANGA_tb<-MANGA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 31,.actual = round(.actual,2))
MARACUJA_AZEDO_tb<-MARACUJA_AZEDO[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 32,.actual = round(.actual,2))
MELANCIA_tb<-MELANCIA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 33,.actual = round(.actual,2))
MELAO_AMARELO_tb<-MELAO_AMARELO[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 34,.actual = round(.actual,2))
MILHO_VERDE_tb<-MILHO_VERDE[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 35,.actual = round(.actual,2))
MORANGO_tb<-MORANGO[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 36,.actual = round(.actual,2))
OVOS_tb<-OVOS[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 37,.actual = round(.actual,2))
PEPINO_tb<-PEPINO[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 38,.actual = round(.actual,2))
PERA_IMPORTADA_tb<-PERA_IMPORTADA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 39,.actual = round(.actual,2))
PIMENTAO_VERDE_tb<-PIMENTAO_VERDE[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 40,.actual = round(.actual,2))
QUIABO_tb<-QUIABO[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 41,.actual = round(.actual,2))
REPOLHO_tb<-REPOLHO[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 42,.actual = round(.actual,2))
TANGERINA_tb<-TANGERINA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 43,.actual = round(.actual,2))
TOMATE_tb<-TOMATE[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 44,.actual = round(.actual,2))
UVA_ITALIA_tb<-UVA_ITALIA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 45,.actual = round(.actual,2))
UVA_NIAGARA_tb<-UVA_NIAGARA[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 46,.actual = round(.actual,2))
VAGEM_tb<-VAGEM[[6]] %>% filter(.model_desc != 'ACTUAL') %>%  select(.model_id,.model_desc,.index,.actual,.prediction,.residuals) %>%  mutate(.model_id = 47,.actual = round(.actual,2))


Tabela <- bind_rows(ABACATE_tb,ABACAXI_tb,ABOBORA_tb,ABOBRINHA_tb,ALFACE_tb,ALHO_tb,BANANA_NANICA_tb,BANANA_PRATA_tb,BATATA_tb,BATATA_DOCE_tb,BERINJELA_tb,BETERRABA_tb,BROCOLO_tb,CARA_tb,CEBOLA_tb,CENOURA_tb,CHUCHU_tb,COCO_VERDE_tb,COUVE_tb,COUVE_FLOR_tb,GOIABA_tb,INHAME_tb,JILO_tb,LARANJA_PERA_tb,LIMAO_TAHITI_tb,MACA_tb,MAMAO_FORMOSA_tb,MAMAO_HAWAY_tb,MANDIOCA_tb,MANDIOQUINHA_tb,MANGA_tb,MARACUJA_AZEDO_tb,MELANCIA_tb,MELAO_AMARELO_tb,MILHO_VERDE_tb,MORANGO_tb,OVOS_tb,PEPINO_tb,PERA_IMPORTADA_tb,PIMENTAO_VERDE_tb,QUIABO_tb,REPOLHO_tb,TANGERINA_tb,TOMATE_tb,UVA_ITALIA_tb,UVA_NIAGARA_tb,VAGEM_tb)
write.csv2(Tabela,file="residuos_ceasa.csv")#6
