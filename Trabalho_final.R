#Pacotes necessários e diretório de trabalho

pacman::p_load(tidyverse, readr, readxl, lubridate, dplyr, foreign, janitor, magrittr)

setwd("~/metodos_quantitativos_listas/Trabalho-Final-")

#Preparação dos dados 

processos_concentrado <- read_excel("processos_concentrado.xlsx") %>% 
  janitor::clean_names() %>% 
  select(processo, relator_atual, data_autuacao) %>% rename(relator = relator_atual,
                                                            data_ajuizamento = data_autuacao)
                                                      

View(processos_concentrado)

requerentes <- read_excel("requerentes.xlsx") %>% 
  janitor::clean_names() %>% select(-legitimado_polo_passivo) %>%
  rename(requerente = legitimado_polo_ativo)   

View(requerentes)

processos_concentrado <- processos_concentrado %>% left_join(requerentes) %>% 
  distinct()   

View(processos_concentrado)

processos_concentrado <- processos_concentrado %>% 
  mutate (requerente_perfil = case_when(str_detect(requerente, 
                                                   regex("PARTIDO|DEMOCRATA|REDE|PODEMOS|SOLIDARIEDADE|UNIÃO_BRASIL|CIDADANIA")) 
                                        ~ "Partido político", str_detect(requerente, regex("ESTADO|DISTRITO")) 
                                        ~ "Governador de Estado ou do Distrito Federal", str_detect(requerente, regex("ORDEM"))
                                        ~"OAB", str_detect(requerente, regex("PROCURADOR|PROCURADORA|PROCURADORIA")) 
                                        ~ "Procurador-Geral da República", str_detect(requerente, regex("ASSOCIAÇÃO|ASSOCIACAO|ESCRITORIO|ESCRITÓRIO|ALIANCA|ARTICULACAO|ARTICULAÇÃO|INSTITUTO"))
                                        ~"Confederação sindical ou entidade de classe", str_detect(requerente, 
                                                                                                   regex("CONFEDERAÇÃO|CONFEDERACAO|FEDERAÇÃO|FEDERACAO|UNIDAS|CENTRAL|SINDICATO"))
                                        ~ "Confederação sindical ou entidade de classe", str_detect(requerente, regex("CÂMARA|CAMARA|SENADO|CONGRESSO NACIONAL"))
                                        ~ "Poder Legislativo", str_detect(requerente, regex("PRESIDENTE DA REPÚBLICA"))
                                        ~ "Presidente da República")) 

partidos <- processos_concentrado %>%
  filter(requerente_perfil %in% c("Partido político", "Presidente da República"))

View(partidos)

presidentes <- partidos %>% filter(requerente_perfil == "Presidente da República") %>% 
  mutate(ano_ajuizamento = year(data_ajuizamento)) %>% 
  select(processo, relator, ano_ajuizamento, requerente) %>% 
  mutate(Presidente_requerente = case_when(str_detect(ano_ajuizamento,"1995|1996|1997|1998") ~ "FHC 1", 
                                           str_detect(ano_ajuizamento,"1999|2000|2001|2002") ~ "FHC 2",
                                           str_detect(ano_ajuizamento,"2003|2004|2005|2006") ~ "Lula 1",  
                                           str_detect(ano_ajuizamento,"2007|2008|2009|2010") ~ "Lula 2",
                                           str_detect(ano_ajuizamento,"2011|2012|2013|2014") ~ "Dilma 1", 
                                           str_detect(ano_ajuizamento,"2015|2016") ~ "Dilma 2",
                                           str_detect(ano_ajuizamento,"2017|2018") ~ "Temer", 
                                           str_detect(ano_ajuizamento,"2019|2020|2021|2022") ~ "Bolsonaro",
                                           str_detect(ano_ajuizamento,"2023|2024") ~ "Lula 3"))

presidentes <- presidentes %>% select(-requerente) %>% rename(requerente = Presidente_requerente)

View(presidentes)

partidos <- partidos %>% filter(requerente_perfil == "Partido político") %>% 
  select(-requerente_perfil) %>% 
  mutate(ano_ajuizamento = year(data_ajuizamento)) %>% 
  select(processo, relator, ano_ajuizamento, requerente)         

partidos_presidentes <- rbind(presidentes, partidos)

View(partidos_presidentes)

partidos_presidentes <- partidos_presidentes %>%  
  mutate(Presidente_ajuizamento = case_when(str_detect(ano_ajuizamento,"1988|1989") ~ "Sarney",
                                            str_detect(ano_ajuizamento,"1990|1991|1992") ~ "Collor",
                                            str_detect(ano_ajuizamento,"1993|1994") ~ "Itamar", 
                                            str_detect(ano_ajuizamento,"1995|1996|1997|1998") ~ "FHC 1", 
                                            str_detect(ano_ajuizamento,"1999|2000|2001|2002") ~ "FHC 2",
                                            str_detect(ano_ajuizamento,"2003|2004|2005|2006") ~ "Lula 1",  
                                            str_detect(ano_ajuizamento,"2007|2008|2009|2010") ~ "Lula 2",
                                            str_detect(ano_ajuizamento,"2011|2012|2013|2014") ~ "Dilma 1", 
                                            str_detect(ano_ajuizamento,"2015|2016") ~ "Dilma 2",
                                            str_detect(ano_ajuizamento,"2017|2018") ~ "Temer", 
                                            str_detect(ano_ajuizamento,"2019|2020|2021|2022") ~ "Bolsonaro",
                                            str_detect(ano_ajuizamento,"2023|2024") ~ "Lula 3"))

coalizoes <- read.csv2("coalizoes.csv")

View(coalizoes)

coalizoes <- coalizoes %>% select(Presidente, Partidos_coalizao) 

partidos_presidentes <- partidos_presidentes %>%
  mutate(governo_oposicao = 
           case_when(str_detect(requerente, 
                                regex("FHC|Lula|Dilma|Bolsonaro|Temer", 
                                      ignore.case =TRUE)) ~"Governo", 
                     (str_detect(requerente, 
                                regex("PMDB|PFL", 
                                      ignore.case =TRUE)) & 
                        str_detect(Presidente_ajuizamento, regex("Sarney", ignore.case = TRUE))) ~"Governo",
                     (str_detect(requerente, 
                                 regex("PRN|PFL|PDS|PTB|PL", 
                                       ignore.case =TRUE)) & 
                        str_detect(Presidente_ajuizamento, regex("Collor", ignore.case = TRUE))) ~"Governo",
                     (str_detect(requerente, 
                                 regex("PFL|PTB|PMDB|PSDB|PSB|PP", 
                                       ignore.case =TRUE)) & 
                        str_detect(Presidente_ajuizamento, regex("Itamar", ignore.case = TRUE))) ~"Governo",
                     (str_detect(requerente, 
                                 regex("PSDB|PFL|PMDB|PTB|PPB", 
                                       ignore.case =TRUE)) & 
                        str_detect(Presidente_ajuizamento, regex("FHC 1", ignore.case = TRUE))) ~"Governo",
                     (str_detect(requerente, 
                                 regex("PSDB|PFL|PMDB|PPB", 
                                       ignore.case =TRUE)) & 
                        str_detect(Presidente_ajuizamento, regex("FHC 2", ignore.case = TRUE))) ~"Governo",
                     (str_detect(requerente, 
                                 regex("PT|PL|PCdoB|PC DO B|PSB|PTB|PDT|PPS|PV|PMDB|PARTIDO TRABALHISTA BRASILEIRO", 
                                       ignore.case =TRUE)) & 
                        str_detect(Presidente_ajuizamento, regex("Lula 1", ignore.case = TRUE))) ~"Governo",
                     (str_detect(requerente, 
                                 regex("PT|PR|PCdoB|PC DO B|PSB|PTB|PMDB|PP|PDT|PRB|PARTIDO DO MOVIMENTO DEMOCRÁTICO BRASILEIRO", 
                                       ignore.case =TRUE)) & 
                        str_detect(Presidente_ajuizamento, regex("Lula 2", ignore.case = TRUE))) ~"Governo",
                     (str_detect(requerente, 
                                 regex("PT|PR|PCdoB|PSB|PMDB|PP|PDT|PRB|PARTIDO DOS TRABALHADORES", 
                                       ignore.case =TRUE)) & 
                        str_detect(Presidente_ajuizamento, regex("Dilma 1", ignore.case = TRUE))) ~"Governo",
                     (str_detect(requerente, 
                                 regex("PT|PMDB|PDT|PCdoB|PC DO B|PR|PP|PSD|PTB|PARTIDO DOS TRABALHADORES", 
                                       ignore.case =TRUE)) & 
                        str_detect(Presidente_ajuizamento, regex("Dilma 2", ignore.case = TRUE))) ~"Governo",
                     (str_detect(requerente, 
                                 regex("PMDB|PSDB|PR|PRB|PSD|PTB|DEM|Democratas|PPS|PP", 
                                       ignore.case =TRUE)) & 
                        str_detect(Presidente_ajuizamento, regex("Temer", ignore.case = TRUE))) ~"Governo",
                     (str_detect(requerente, 
                                 regex("PSL", 
                                       ignore.case =TRUE)) & 
                        str_detect(Presidente_ajuizamento, regex("Bolsonaro", ignore.case = TRUE))) ~ "Governo",
                     (str_detect(requerente, 
                                 regex("PT|União Brasil|PSD|PSB|MDB|Rede|PSol|P-Sol|PDT|PCdoB|PC DO B|PARTIDO COMUNISTA DO BRASIL|PARTIDO DOS TRABALHADORES|PARTIDO SOCIALISMO E LIBERDADE|REDE SUSTENTABILIDADE|PARTIDO DEMOCRATICO TRABALHISTA", 
                                       ignore.case =TRUE)) & 
                        str_detect(Presidente_ajuizamento, regex("Lula 3", ignore.case = TRUE))) ~"Governo"))


partidos_presidentes$governo_oposicao <- partidos_presidentes$governo_oposicao %>% replace_na("Oposição")

View(partidos_presidentes)

Sumario_ajuizamento <- partidos_presidentes %>% group_by(governo_oposicao) %>% 
  summarise(n = n())

View(Sumario_ajuizamento)  

Sumario_ajuizamento <- Sumario_ajuizamento %>% mutate(Prop = n/2131*100)

Sumario_ajuizamento <- Sumario_ajuizamento %>% 
  rename(Posicionamento = governo_oposicao, Número = n, Proporção = Prop)

Sumario_ajuizamento2 <- partidos_presidentes %>% group_by(governo_oposicao) %>% 
  summarise(n = n())

View(Sumario_ajuizamento2)

Sumario_presidente <- partidos_presidentes %>% group_by(Presidente_ajuizamento, governo_oposicao) %>%
  summarise(n=n())

View(Sumario_presidente)

Sumario_presidente <- Sumario_presidente %>% 
  pivot_wider(names_from = governo_oposicao, values_from = n) %>%
  rename(Presidente = Presidente_ajuizamento) %>% mutate_all(replace_na,0)

(Plot_ajuizamento <- partidos_presidentes %>%  
  ggplot(aes(y = Presidente_ajuizamento, fill = governo_oposicao, stat = "identity")) + 
           geom_bar() + scale_x_continuous(breaks = seq(0,700, by=100)) + theme_minimal() +
    labs(title=paste("Ações ajuizadas pela Coalização de Governo e pela Oposição"), 
         x="", y="", fill = ""))

decisoes <- read_excel("decisoes_concentrado.xlsx", 
                                   col_types = c("text", "date", "text", "text", "text", "text")) %>% 
  janitor::clean_names() %>% select(processo, data, andamento_agrupado) %>%
  rename(data_decisao = data, decisao = andamento_agrupado) %>% 
  filter(str_detect(decisao, regex("Procedente|Improcedente|Liminar deferida|Liminar indeferida"))) %>%
  mutate(ano_decisao = year(data_decisao)) %>% select(- data_decisao)


decisoes1 <- partidos_presidentes %>% left_join(decisoes)

View(decisoes1)

decisoes1 <- decisoes1 %>% na.omit() %>% select(-relator,-Presidente_ajuizamento,-governo_oposicao)

decisoes1 <- decisoes1 %>% mutate(Presidente_decisao = case_when(str_detect(ano_decisao,"1988|1989") ~ "Sarney",
                                                                 str_detect(ano_decisao,"1990|1991|1992") ~ "Collor",
                                                                 str_detect(ano_decisao,"1993|1994") ~ "Itamar", 
                                                                 str_detect(ano_decisao,"1995|1996|1997|1998") ~ "FHC 1", 
                                                                 str_detect(ano_decisao,"1999|2000|2001|2002") ~ "FHC 2",
                                                                 str_detect(ano_decisao,"2003|2004|2005|2006") ~ "Lula 1",  
                                                                 str_detect(ano_decisao,"2007|2008|2009|2010") ~ "Lula 2",
                                                                 str_detect(ano_decisao,"2011|2012|2013|2014") ~ "Dilma 1", 
                                                                 str_detect(ano_decisao,"2015|2016") ~ "Dilma 2",
                                                                 str_detect(ano_decisao,"2017|2018") ~ "Temer", 
                                                                 str_detect(ano_decisao,"2019|2020|2021|2022") ~ "Bolsonaro",
                                                                 str_detect(ano_decisao,"2023|2024") ~ "Lula 3"))

  
 decisoes1 <- decisoes1 %>% 
   mutate(decisao_agrupada = case_when(str_detect(decisao, regex("indeferida|Improcedente")) ~ "Desfavorável",
                                       str_detect(decisao, regex("deferida|Procedente")) ~ "Favorável"))

 decisoes_favoraveis <- decisoes1 %>% filter(decisao_agrupada == "Favorável")  

 View(decisoes_favoraveis)  
 
 decisoes_favoraveis <- decisoes_favoraveis %>% select(-ano_ajuizamento, -decisao, - decisao_agrupada)
 
 decisoes_favoraveis <- decisoes_favoraveis %>% 
   mutate(fav_coalizao = case_when((str_detect(requerente,regex("PMDB|PFL", ignore.case =TRUE)) & 
                                      str_detect(Presidente_decisao, regex("Sarney", ignore.case = TRUE))) ~ "Sim",
                                   (str_detect(requerente, regex("PRN|PFL|PDS|PTB|PL", ignore.case =TRUE)) & 
                                      str_detect(Presidente_decisao, regex("Collor", ignore.case = TRUE))) ~ "Sim",
                                   (str_detect(requerente, regex("PFL|PTB|PMDB|PSDB|PSB|PP", ignore.case =TRUE)) & 
                                      str_detect(Presidente_decisao, regex("Itamar", ignore.case = TRUE))) ~ "Sim",
                                   (str_detect(requerente, regex("FHC 1|FHC 2|PSDB|PFL|PMDB|PTB|PPB", ignore.case =TRUE)) & 
                                      str_detect(Presidente_decisao, regex("FHC 1", ignore.case = TRUE))) ~"Sim",
                                   (str_detect(requerente, regex("FHC 1|FHC2|PSDB|PFL|PMDB|PPB", ignore.case =TRUE)) & 
                                      str_detect(Presidente_decisao, regex("FHC 2", ignore.case = TRUE))) ~ "Sim",
                                   (str_detect(requerente, regex("Lula 1|PT|PL|PCdoB|PC DO B|PSB|PTB|PDT|PPS|PV|PMDB|PARTIDO TRABALHISTA BRASILEIRO", ignore.case =TRUE)) & 
                                      str_detect(Presidente_decisao, regex("Lula 1", ignore.case = TRUE))) ~ "Sim",
                                   (str_detect(requerente, regex("Lula 1|LUla 2|PT|PR|PCdoB|PC DO B|PSB|PTB|PMDB|PP|PDT|PRB|PARTIDO DO MOVIMENTO DEMOCRÁTICO BRASILEIRO", ignore.case =TRUE)) & 
                                      str_detect(Presidente_decisao, regex("Lula 2", ignore.case = TRUE))) ~"Sim",
                                   (str_detect(requerente, regex("Dilma 1|PT|PR|PCdoB|PSB|PMDB|PP|PDT|PRB|PARTIDO DOS TRABALHADORES", ignore.case =TRUE)) & 
                                      str_detect(Presidente_decisao, regex("Dilma 1", ignore.case = TRUE))) ~"Sim",
                                   (str_detect(requerente, regex("Dilma1|Dilma 2|PT|PMDB|PDT|PCdoB|PC DO B|PR|PP|PSD|PTB|PARTIDO DOS TRABALHADORES", ignore.case =TRUE)) & 
                                      str_detect(Presidente_decisao, regex("Dilma 2", ignore.case = TRUE))) ~"Sim",
                                   (str_detect(requerente, regex("PMDB|PSDB|PR|PRB|PSD|PTB|DEM|Democratas|PPS|PP", ignore.case =TRUE)) & 
                                      str_detect(Presidente_decisao, regex("Temer", ignore.case = TRUE))) ~"Sim",
                                   (str_detect(requerente, regex("PSL|Bolsonaro", ignore.case =TRUE)) & 
                                      str_detect(Presidente_decisao, regex("Bolsonaro", ignore.case = TRUE))) ~ "Sim",
                                   (str_detect(requerente, regex("Lula 1|Lula 2|lula 3|PT|União Brasil|PSD|PSB|MDB|Rede|PSol|P-Sol|PDT|PCdoB|PC DO B|PARTIDO COMUNISTA DO BRASIL|PARTIDO DOS TRABALHADORES|PARTIDO SOCIALISMO E LIBERDADE|REDE SUSTENTABILIDADE|PARTIDO DEMOCRATICO TRABALHISTA", ignore.case =TRUE)) & 
                                      str_detect(Presidente_decisao, regex("Lula 3", ignore.case = TRUE))) ~ "Sim"))
 
 
decisoes_favoraveis$fav_coalizao <- decisoes_favoraveis$fav_coalizao %>% replace_na("Não")
 
Sumario_decisoes_favoraveis <- decisoes_favoraveis %>% group_by(fav_coalizao) %>%
  summarise(n= n())

View(Sumario_decisoes_favoraveis) 

Sumario_decisoes_favoraveis <- Sumario_decisoes_favoraveis %>% 
  pivot_wider(names_from = fav_coalizao, values_from = n) 
 
Sumario_decisoes_favoraveis

Sumario_decisoes_favoraveis <- Sumario_decisoes_favoraveis %>%
  rename(Governo = Sim, Oposição = Não) 

Sumario_decisoes_favoraveis <- Sumario_decisoes_favoraveis %>% 
  t()

Sumario_decisoes_favoraveis <- Sumario_decisoes_favoraveis %>% as.data.frame() %>%
  rename(Dec_fav = V1)

Sumario_decisoes_favoraveis <- Sumario_decisoes_favoraveis %>% 
  mutate(governo_oposicao = 
           case_when(str_detect(Dec_fav,regex ("175")) ~ "Governo", str_detect(Dec_fav,regex("518")) ~ "Oposição"))

Sumario_decisoes_favoraveis <- Sumario_decisoes_favoraveis %>% 
  select(governo_oposicao, Dec_fav)

Sumario_decisoes_favoraveis_prop <- Sumario_decisoes_favoraveis %>% 
  left_join(Sumario_ajuizamento)
  
View(Sumario_decisoes_favoraveis_prop)  

Sumario_decisoes_favoraveis_prop <- Sumario_decisoes_favoraveis_prop %>%
  rename(Ações = n, Posicionamento = governo_oposicao) %>% select(-Prop) %>%
  mutate(Êxito = Dec_fav/Ações*100)

#Política Judicial

##Uma proposta de mensuração dos posicionamentos ideológicos dos Ministros do Supremo Tribunal Federal 
##com base nas decisões proferidas em ações de controle concentrado de constitucionalidade 
##ajuizadas por partidos políticos e pelos Presidentes da República desde a promulgação da 
##Constituição de 1988

##Hipótese nula (H0): a média dos "posicionamentos ideológicos" dos Ministros do STF
##indicados por Presidentes da República classificados ideologicamente como de 
##Direita não diverge da média dos "posicionamentos ideológicos" dos Ministros do STF 
##indicados por Presidentes da República ideologicamente classificados como de Esquerda. 

##Hipótese alternativa (HA): a média dos "posicionamentos ideológicos" dos Ministros do STF
##indicados por Presidentes da República classificados ideologicamente como de 
##Direita diverge da média dos "posicionamentos ideológicos" dos Ministros do STF 
##indicados por Presidentes da República ideologicamente classificados como de Esquerda. 

##Para a mensuração do denominado "posicionamento ideológico" dos Ministros do 
##Supremo Tribunal Federal foi criado um "proxy" consistente na diferença entre 
##o número de decisões proferidas em processos de controle concentrado de 
##constitucionalidade (ADC/ADI/ADPF) favoráveis a partidos políticos de esquerda 
##decisões favoráveis a partidos de direita. 

##Tendo em vista que o número de processos ajuizados pelos partidos de direita 
##é menor do que o número de processos ajuizados pelos partidos de esquerda (786 e 1341), 
##foi atribuído o peso de 1,7 às decisões proferidas nos processos ajuizados pelos 
##partidos de direita. A fim de evitar valores negativos, a cada um dos valores foi 
##adicionado 50 unidades. Desse modo, valores acima de 50 revelam posicionamento 
##ideológico à esquerda e valores abaixo de 50 revelam posicionamento ideológico à direita. 

##Os Ministros da Suprema Corte foram classificados com base na ideologia do Presidente da República 
##que os indicou. Além dos processos ajuizados pelos partidos políticos, foram incluídos 
##os ajuizados pelos Presidentes da República eleitos a partir de 1988. Foram consideradas 
##as decisões proferidas após a promulgação da Constituição de 1988. 

#Carregando os pacotes 

pacman::p_load(tidyverse, readr, readxl, lubridate, dplyr, foreign, janitor)

#Definição do diretório de trabalho 

setwd("~/metodos_quantitativos_listas/Judicial-Politics")

#Arquivos extraídos do sítio eletrônico do Programa "Corte Aberta" - 
#(https://portal.stf.jus.br/hotsites/corteaberta/): processos_concentrado.xlsx;
#requerentes.xlsx; decisoes_concentrado.xlsx

#Preparação dos dados 

processos_concentrado <- read_excel("processos_concentrado.xlsx") %>% 
  janitor::clean_names()

processos_concentrado <- processos_concentrado %>% 
  select(processo, relator_atual, data_autuacao)

View(processos_concentrado)

requerentes <- read_excel("requerentes.xlsx") %>% 
  janitor::clean_names() 

requerentes <- requerentes %>% select(-legitimado_polo_passivo) %>%
  rename(requerente = legitimado_polo_ativo)   

View(requerentes)

processos_concentrado <- processos_concentrado %>% left_join(requerentes) %>% 
  distinct()   

View(processos_concentrado)  

processos_concentrado <- processos_concentrado %>% 
  mutate (requerente_perfil = case_when(str_detect(requerente, 
                                                   regex("PARTIDO|DEMOCRATA|REDE|PODEMOS|SOLIDARIEDADE|UNIÃO_BRASIL|CIDADANIA")) 
                                        ~ "Partido político", str_detect(requerente, regex("ESTADO|DISTRITO")) 
                                        ~ "Governador de Estado ou do Distrito Federal", str_detect(requerente, regex("ORDEM"))
                                        ~"OAB", str_detect(requerente, regex("PROCURADOR|PROCURADORA|PROCURADORIA")) 
                                        ~ "Procurador-Geral da República", str_detect(requerente, regex("ASSOCIAÇÃO|ASSOCIACAO|ESCRITORIO|ESCRITÓRIO|ALIANCA|ARTICULACAO|ARTICULAÇÃO|INSTITUTO"))
                                        ~"Confederação sindical ou entidade de classe", str_detect(requerente, 
                                                                                                   regex("CONFEDERAÇÃO|CONFEDERACAO|FEDERAÇÃO|FEDERACAO|UNIDAS|CENTRAL|SINDICATO"))
                                        ~ "Confederação sindical ou entidade de classe", str_detect(requerente, regex("CÂMARA|CAMARA|SENADO|CONGRESSO NACIONAL"))
                                        ~ "Poder Legislativo", str_detect(requerente, regex("PRESIDENTE DA REPÚBLICA"))
                                        ~ "Presidente da República")) 

partidos <- processos_concentrado %>%
  filter(requerente_perfil %in% c("Partido político", "Presidente da República"))

View(partidos)  

presidente <- partidos %>% filter(requerente_perfil == "Presidente da República") %>% 
  mutate(ano_ajuizamento = year(data_autuacao)) %>% 
  select(processo, relator_atual, ano_ajuizamento, requerente) %>%
  mutate(Presidente_requerente = case_when(str_detect(ano_ajuizamento,"1995|1996|1997|1998|1999|2000|2001|2002") 
                                           ~ "FHC", str_detect(ano_ajuizamento,"2003|2004|2005|2006|2007|2008|2009|2010|2023|2024")
                                           ~ "Lula", str_detect(ano_ajuizamento,"2011|2012|2013|2014|2015|2016") ~ "Dilma",
                                           str_detect(ano_ajuizamento,"2017|2018") ~ "Temer",
                                           str_detect(ano_ajuizamento,"2019|2020|2021|2022") ~ "Bolsonaro")) %>% 
  select(-requerente) %>% rename(requerente = Presidente_requerente) %>% 
  mutate(requerente_ideologia = case_when(str_detect(requerente, regex("Lula|Dilma")) ~ "Esquerda",
                                          str_detect(requerente, regex("FHC|Bolsonaro|Temer")) ~ "Direita")) 

presidente <- presidente %>% rename(relator = relator_atual)

View(presidente)

partidos <- partidos %>% filter(requerente_perfil == "Partido político") %>% 
  select(-requerente_perfil) %>% 
  mutate(ano_ajuizamento = year(data_autuacao)) %>% 
  select(processo, relator_atual, ano_ajuizamento, requerente) %>% 
  mutate(partido_ideologia = case_when(str_detect(requerente, regex("PSTU|PCO|PCB|PSOL|PC DO B|PT|PDT|PSB|PARTIDO DOS TRABALHADORES|PARTIDO DEMOCRÁTICO TRABALHISTA|PARTIDO SOCIALISTA BRASILEIRO|PARTIDO TRABALHISTA BRASILEIRO|PST|P-SOL|PARTIDO COMUNISTA DO BRASIL|PARTIDO DEMOCRATICO TRABALHISTA")) ~ "Esquerda", 
                                       str_detect(requerente, regex("REDE|PPS|PV|PARTIDO POPULAR SOCIALISTA|PARTIDO VERDE|PARTIDO PÓPULAR SOCIAL")) ~ "Esquerda", 
                                       str_detect(requerente, regex("PTB|Avante|Solidariedade|SDD|PMN|PMB|PHS|PMDB|PSD|PSDB|Podemos|PPL|PL|PRTB|PROS|PR|PRP|PRB|PTC|PSL|Novo|PSC|Progressistas|Patriota|Democratas|União|PDC|PFL|PARTIDO DA REPÚBLICA|PARTIDO LIBERAL|DEMOCRATAS|PARTIDO TRABALHISTA CRISTÃO|SOLIDARIEDADE|PARTIDO DA SOCIAL DEMOCRACIA BRASILEIRA|PODEMOS|PARTIDO DO MOVIMENTO|CIDADANIA|PATRIOTA|NOVO|PARTIDO DA MULHER BRASILEIRA|PARTIDO SOCIAL|PARTIDO DA MOBILIZACAO NACIONAL|PARTIDO TRABLHISTA NACIONAL|PARTIDO REPUBLICANO|PARTIDO RENOVADOR|PEN")) ~ "Direita")) %>% 
  rename(relator = relator_atual, requerente_ideologia = partido_ideologia)  


partidos <- rbind(presidente, partidos)

Sumario_ajuizamento_ideologia <- partidos %>% group_by(requerente_ideologia) %>%
  summarise(acoes_ajuizadas = n()) %>% na.omit() %>% 
  mutate(prop = acoes_ajuizadas/2127*100)

Sumario_ajuizamento_ideologia <- Sumario_ajuizamento_ideologia %>%
  rename(Requerente = requerente_ideologia, Número = acoes_ajuizadas, Proporção = prop)

View(Sumario_ajuizamento_ideologia)

decisoes_concentrado <- read_excel("decisoes_concentrado.xlsx", 
                                   col_types = c("text", "date", "text", "text", "text", "text")) %>% 
  janitor::clean_names() %>% select(processo, data, andamento_agrupado) %>%
  rename(data_decisao = data, decisao = andamento_agrupado) %>% 
  filter(str_detect(decisao, regex("Procedente|Improcedente|Liminar deferida|Liminar indeferida"))) %>% 
  select(processo, decisao)

View(decisoes_concentrado)

decisoes_partidos <- partidos %>% left_join(decisoes_concentrado)

View(decisoes_partidos)

decisoes_partidos <- decisoes_partidos %>% na.omit() %>%
  mutate(presidente_indicacao = case_when(str_detect(relator, regex("DJACI")) ~"Castelo Branco",
                                          str_detect(relator, regex("MAYER|MOREIRA")) ~"Geisel", 
                                          str_detect(relator, regex("NÉRI|NERI|ALDIR|OSCAR|SYDNEY")) ~ "Figueiredo",
                                          str_detect(relator, regex("BORJA|CELSO|MADEIRA|BROSSARD|PERTENCE")) ~ "Sarney", 
                                          str_detect(relator, regex("REZEK|MARCO|ILMAR|VELLOSO")) ~ "Collor", 
                                          str_detect(relator, regex("CORRÊA|CORREA")) ~ "Itamar",
                                          str_detect(relator, regex("JOBIM|MENDES|GRACIE")) ~ "FHC",
                                          str_detect(relator, regex("CÁRMEN|CARMEN|GRAU|BRITTO|LEWANDOWSKI|ZANIN|DIREITO|TOFFOLI|BARBOSA|PELUSO|DINO")) ~ "Lula", 
                                          str_detect(relator, regex("FUX|BARROSO|FACHIN|TEORI|WEBER")) ~ "Dilma", 
                                          str_detect(relator, regex("MORAES")) ~ "Temer",
                                          str_detect(relator, regex("NUNES|MENDONÇA|MENDONCA")) ~ "Bolsonaro")) %>% 
  mutate(decisao_agrupada = case_when(str_detect(decisao, regex("indeferida|Improcedente")) ~ "Desfavorável",
                                      str_detect(decisao, regex("deferida|Procedente")) ~ "Favorável")) %>%
  select(processo, relator, presidente_indicacao, requerente, requerente_ideologia, decisao, decisao_agrupada) %>%
  mutate(Presidente_ideologia = case_when(str_detect(presidente_indicacao, regex("Sarney|Collor|Temer|Bolsonaro|Itamar|FHC|Geisel|Castelo|Figueiredo")) ~ "Direita",
                                          str_detect(presidente_indicacao, regex("Dilma|Lula")) ~"Esquerda"))%>%
  select(processo, relator, presidente_indicacao, Presidente_ideologia, requerente, requerente_ideologia, decisao, decisao_agrupada)

partidos_favoravel <- decisoes_partidos %>% filter(decisao_agrupada == "Favorável")

View(partidos_favoravel)

partidos_favoravel_resumo <- partidos_favoravel %>% 
  select(relator, Presidente_ideologia, requerente_ideologia)

View(partidos_favoravel_resumo)

sumario1 <- partidos_favoravel_resumo %>% group_by(relator, requerente_ideologia) %>% 
  summarise(decisoes_favoraveis = n())

View(sumario1)

sumario1 <- sumario1 %>% 
  pivot_wider(names_from = requerente_ideologia, values_from = decisoes_favoraveis) %>% 
  mutate_all(replace_na, 0) %>% mutate(posicao_ideologia = Esquerda - 1.7*Direita) %>% mutate(presidente_indicacao = case_when(str_detect(relator, regex("DJACI")) ~"Castelo Branco",
                                                                                                                               str_detect(relator, regex("MAYER|MOREIRA")) ~"Geisel", 
                                                                                                                               str_detect(relator, regex("NÉRI|NERI|ALDIR|OSCAR|SYDNEY")) ~ "Figueiredo",
                                                                                                                               str_detect(relator, regex("BORJA|CELSO|MADEIRA|BROSSARD|PERTENCE")) ~ "Sarney", 
                                                                                                                               str_detect(relator, regex("REZEK|MARCO|ILMAR|VELLOSO")) ~ "Collor", 
                                                                                                                               str_detect(relator, regex("CORRÊA|CORREA")) ~ "Itamar",
                                                                                                                               str_detect(relator, regex("JOBIM|MENDES|GRACIE")) ~ "FHC",
                                                                                                                               str_detect(relator, regex("CÁRMEN|CARMEN|GRAU|BRITTO|LEWANDOWSKI|ZANIN|DIREITO|TOFFOLI|BARBOSA|PELUSO|DINO")) ~ "Lula", 
                                                                                                                               str_detect(relator, regex("FUX|BARROSO|FACHIN|TEORI|WEBER")) ~ "Dilma", 
                                                                                                                               str_detect(relator, regex("MORAES")) ~ "Temer",
                                                                                                                               str_detect(relator, regex("NUNES|MENDONÇA|MENDONCA")) ~ "Bolsonaro")) %>% 
  mutate(Presidente_ideologia = case_when(str_detect(presidente_indicacao, regex("Sarney|Collor|Temer|Bolsonaro|Itamar|FHC|Geisel|Castelo|Figueiredo")) ~ "Direita",
                                          str_detect(presidente_indicacao, regex("Dilma|Lula")) ~"Esquerda"))

sumario2 <- sumario1 %>% select(relator, Presidente_ideologia, posicao_ideologia) %>% 
  rename(Ministro = relator, Presidente_ideologia_indicacao = Presidente_ideologia) %>% na.omit() %>% 
  mutate(posicao_ideologia_ajustada = posicao_ideologia + 50) 

sumario2 <- sumario2 %>% select(-posicao_ideologia) %>%
  rename(Presidente = Presidente_ideologia_indicacao,Posicionamento_ideológico = posicao_ideologia_ajustada)

sumario2 <- sumario2[-c(16),] ##Observação extrema excluída a fim de garantir a normalidade
##da distribuição. 

View(sumario2)

distribuição <- sumario2 %>% 
  ggplot(aes(fill=Presidente_ideologia_indicacao,x=posicao_ideologia_ajustada, 
             color=Presidente_ideologia_indicacao,group=Presidente_ideologia_indicacao)) + 
  geom_density(color=NA,alpha=.65) + 
  geom_vline(data=. %>% group_by(Presidente_ideologia_indicacao) %>% 
               summarise(media=mean(posicao_ideologia_ajustada,na.rm = T)),
             linewidth=1, aes(xintercept=media,color=Presidente_ideologia_indicacao)) + 
  guides(color="none") + theme_minimal() + scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  labs(title = "Distribuição dos posicionamentos ideológicos dos Ministros do STF", x = "Posicionamento ideológico",
       y = "Densidade", fill = "Ideologia do Presidente",
       color = "Ideologia do Presidente responsável pela indicação")
distribuição

#Teste da normalidade da distribuição da variável dependente 
#Teste de Shapiro-Wilk 

install.packages("rstatix")
library(rstatix)

sumario2 %>% group_by(Presidente_ideologia_indicacao) %>% 
  shapiro_test(posicao_ideologia_ajustada)

# A tibble: 2 × 4
#Presidente_ideologia_indicacao variable                   statistic     p
#<chr>                          <chr>                          <dbl> <dbl>
#1 Direita                      posicao_ideologia_ajustada     0.967 0.809
#2 Esquerda                     posicao_ideologia_ajustada     0.966 0.796

## A hipótese nula não foi rejeitada (H0 = os dados seguem a distribuição normal). 


#Teste de variância - Teste de Levene 

install.packages("car")
library(car)

leveneTest(posicao_ideologia_ajustada ~ Presidente_ideologia_indicacao, 
           data = sumario2, center = mean)

##Levene's Test for Homogeneity of Variance (center = mean)
##Df  F value    Pr(>F)    
##group  1  19.222 0.0001488 ***
##  28                      
##---
##Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#P-valor menor que 0,05. Não se rejeita a hipótese nula. Os grupos 
##("Direita" e "Esquerda") apresentam variâncias homogêneas. 

#Teste de hipótese - Teste-t para amostras independentes com variâncias iguais

##Tendo em vista a existência de duas amostras independentes ("decisões proferidas 
##por Ministros do STF indicados por Presidentes classificados ideologicamente como 
##de esquerda"  e decisões proferidas por Ministros do STF indicados por Presidentes 
##classificados ideologicamente como de direita") e considerando que essas amostras 
##apresentam variâncias homogêneas, optou-se pelo "Teste-t para amostras independentes 
##com variâncias iguais".

## É pressuposto do referido teste a distribuição normal dos dados. Em um primeiro
##momento, aferiu-se, por intermédio do Teste de Shapiro-Wilk, que a distribuição das
##observações relativas ao grupo dos Ministros indicados por Presidentes de Direita não 
##era normal. 
##Após análise pormenorizada dos dados, verificou-se que a anormalidade da distribuição 
##decorria da existência de uma observação extrema ("outlier") - Ministro Ilmar Galvão.
##Optou-se por excluí-la, a fim de permitir a utilização do Teste-t. 

library(infer)

t_test_sumario2 <- sumario2 %>% 
  t_test(formula = posicao_ideologia_ajustada ~ Presidente_ideologia_indicacao, 
         order = c("Esquerda", "Direita"), var.equal = TRUE)
t_test_sumario2

##A tibble: 1 × 7
##statistic  t_df p_value alternative estimate lower_ci upper_ci
##<dbl> <dbl>   <dbl> <chr>          <dbl>    <dbl>    <dbl>
##  1      2.38    29  0.0240 two.sided       9.34     1.32     17.4

##Hipótese nula (H0) rejeitada. Existência, portanto, de divergência estatisticamente 
#signficante entre os posicionamento ideológicos adotados pelos Ministros do STF 
##indicados por Presidentes de Direita e de Esquerda. 


boxplot <- boxplot(posicao_ideologia_ajustada ~ Presidente_ideologia_indicacao, data = sumario2, 
                   ylab = "Posicionamento ideológico", 
                   xlab = "Ideologia do Presidente responsável pela indicação")
boxplot

sumario3 <- sumario2 %>%
  group_by(Presidente_ideologia_indicacao) %>%
  summarize(media = mean(posicao_ideologia_ajustada),
            ci_lower = media - qt(0.975, df=n()-1) * sd(posicao_ideologia_ajustada)/sqrt(n()),
            ci_upper = media + qt(0.975, df=n()-1) * sd(posicao_ideologia_ajustada)/sqrt(n()))

View(sumario3)

intervalo <- ggplot(sumario3, aes(y = factor(Presidente_ideologia_indicacao, 
                                             labels = c("Direita", "Esquerda")), 
                                  x = media, color = factor(Presidente_ideologia_indicacao))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
  scale_color_manual(values = c("#1F77B4", "#FF7F0E")) + 
  labs(title = "Posicionamento ideológico dos Ministros do STF",
       x = "Posicionamento ideológico",
       y = "Ideologia do Presidente", color = "Grupo") +
  theme_minimal() + theme(legend.position = "none") 
intervalo

intervalo2 <- sumario2 %>% ggplot(aes(y = posicao_ideologia_ajustada, x = factor(Presidente_ideologia_indicacao, labels = c("Direita", "Esquerda")), 
                                      color = factor(Presidente_ideologia_indicacao))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("#1F77B4", "#FF7F0E")) + 
  labs(title = "Posicionamento ideológico dos Ministros do STF",
       y = "Posicionamento ideológico",
       x = "Ideologia do Presidente responsável pela indicação",
       color = "Grupo") + theme_minimal() + theme(legend.position = "none")
intervalo2

teste_t <- sumario2 %>% 
  specify(posicao_ideologia_ajustada ~ Presidente_ideologia_indicacao) %>%
  calculate(stat = "t", order = c("Direita","Esquerda"))
teste_t

#Distribuição hipótese nula 

distribuicao_teorica <- sumario2 %>% 
  specify(posicao_ideologia_ajustada ~ Presidente_ideologia_indicacao) %>%
  hypothesize(null = "independence") %>%
  calculate(stat = "t", order = c("Direita","Esquerda"))
distribuicao_teorica

distribuicao_teorica %>%
  visualize(method = "theoretical") +
  shade_p_value(distribuicao_teorica, direction = "two-sided") +
  labs(title = "Distribuição teórica",x="Posicionamento ideológico",
       y="Densidade")


