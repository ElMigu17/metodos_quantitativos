
### Funções de inicio/uteis (função que são uteis pra diversos propositos)

comeco <- function(){
  library(readr)
  library("dplyr") 
  library(stringr)
  library(car)
  library(lmtest)
  library(tseries)
  library(mfx)
  table <- read_csv("Documents/oitavo_periodo/testeteste/archive/sao-paulo-properties-april-2019.csv",
  col_types = cols(New = col_logical(),
  Negotiation_Type = col_character()))
  
  table = limpeza(table)
  return(table)
}

limpeza <- function(table){
  table <- table[table$Latitude >= -24, ]
  table <- table[table$Latitude <= -22, ]
  table <- table[table$Longitude >= -48, ]
  table <- table[table$Negotiation_Type == 'rent', ]
  attach(table)
  return(table)
}

var_nome <- function (some_variable, name=deparse(substitute(some_variable))) {
  name
}


### Funções de tabela

info_tabela_discreto <- function(coluna, valores_possiveis){
  
  for(val in valores_possiveis){
    aux <- length(coluna[coluna == val])
    total <- length(coluna)
    true_percentage <- (aux/total)*100
    print(paste("Quantidade com o valor", val, true_percentage))
  }
}

info_tabela_continuo <- function(coluna, nome){
  print(paste("Media, Desvio padrão, minimo e máximo do", nome))
  print(mean(coluna))
  print(sd(coluna))
  print(min(coluna))
  print(max(coluna))
}

### Funções Regressão

district_to_zone <- function(tabela){
  zonas <- list( "Leste" = list("Aricanduva", "Carrão", "Vila Formosa", "Cidade Tiradentes", "Ermelino Matarazzo", "Ponte Rasa", "Guaianazes", "Lajeado", "Itaim Paulista", "Vila Curuçá", "Itaquera", "Cidade Líder", "José Bonifácio", "Parque do Carmo", "Mooca", "Água Rasa", "Belém", "Brás", "Moóca", "Pari", "Tatuapé", "Penha", "Artur Alvim", "Cangaíba", "Penha", "Vila Matilde", "São Mateus", "São Rafael", "São Miguel", "Jardim Helena", "Vila Jacuí", "Sapopemba", "Vila Prudente", "São Lucas", "Iguatemi"),
              "Norte" = list("Medeiros", "Casa Verde", "Cachoeirinha", "Limão", "Brasilândia", "Freguesia do Ó", "Jaçanã", "Tremembé", "Perus", "Anhanguera", "Pirituba", "Jaraguá", "São Domingos", "Santana", "Tucuruvi", "Mandaqui", "Vila Maria", "Vila Guilherme", "Vila Medeiros"),
              "Oeste" = list("Vila Olimpia", "Vila Madalena", "Butantã", "Morumbi", "Raposo Tavares", "Rio Pequeno", "Vila Sônia", "Lapa", "Barra Funda", "Jaguara", "Jaguaré", "Perdizes", "Vila Leopoldina", "Pinheiros", "Alto de Pinheiros", "Itaim Bibi", "Jardim Paulista", "Pinheiros"),
              "Sul" = list("Brooklin", "Cursino", "Campo Limpo", "Capão Redondo", "Vila Andrade", "Capela do Socorro", "Cidade Dutra", "Grajaú", "Socorro", "Cidade Ademar", "Pedreira", "Ipiranga", "Sacomã", "Jabaquara", "M'Boi Mirim", "Jardim Ângela", "Jardim São Luis", "Parelheiros", "Marsilac", "Santo Amaro", "Campo Belo", "Campo Grande", "Santo Amaro", "Moema", "Saúde", "Vila Mariana"))
  

  tabela$District <- str_replace(District, '/São Paulo', '')
  Districts_names <- District[!duplicated(District)]
  
  for(i in seq(1,length(zonas))){
    zona_em_analise <- zonas[i]
    tabela[names(zonas)[i]] <- (District %in% zona_em_analise[[1]])*1
  }
  return(tabela)
  
}

criacao_boleanos_zonas <- function(tabela, District){
  Districts_names <- District[!duplicated(District)]
  Districts_names <- Districts_names[Districts_names != 'Vila Olimpia/São Paulo' ]
  for (d in Districts_names){
    nome_distrito_limpo <- str_replace(d, '/São Paulo', '')
    nome_distrito_limpo <- str_replace(nome_distrito_limpo, ' ', '_')
    nome_distrito_limpo <- str_replace(nome_distrito_limpo, ' ', '_')
    nome_distrito_limpo <- str_replace(nome_distrito_limpo, ' ', '_')
    tabela[nome_distrito_limpo] <- (District == d) *1
  }
  return(tabela)
}
faz_regressao <- function(tabela){

  outras_variaveis <- list("Size","Rooms","Toilets","Parking","Latitude","Longitude","Elevator","Furnished","Swimming_Pool","New")
  nome_zonas <- list("Leste", "Oeste", "Sul", "Norte")
  
  todas_variaveis<-append(nome_zonas, outras_variaveis)
  form <- as.formula(paste("(Price+Condo)~", paste(todas_variaveis, collapse="+")))
  print(form)
  print(tabela)
  reg <- lm(form)
  return(reg)
}


### Funções de Teste

## Heterocedasticidade

teste_heterocedasticidade <- function(){
  res <- resid(reg)
  y_hat <- fitted(reg)
  reg_white <- lm(res^2~y_hat+I(y_hat^2))
  summary(reg_white)
}

## Teste normalidade 

teste_normalidade <- function(reg){
  residuo1 <- resid(reg)
  jarque.bera.test(residuo1)
}

### Fim das funções ####

# array_continuo <- list(Price, Condo, Size, Toilets, Parking)
# array_binario <- list(Elevator, Furnished, Swimming_Pool, New)

# Binario: Price, Condo, Size, Toilets, Parking,
# Continuo: Elevator, Furnished, Swimming_Pool, New,
# Não sei: District, Latitude, Longitude, Negotiation_Type, Suites, Rooms
# Property_Type


tabela <- comeco()
attach(tabela)
### Tabelas
info_tabela_continuo(Price, var_nome(Price))
info_tabela_continuo(Condo, var_nome(Condo))
info_tabela_continuo(Size, var_nome(Size))
info_tabela_continuo(Rooms, var_nome(Rooms))
info_tabela_continuo(Toilets, var_nome(Toilets))
info_tabela_continuo(Parking, var_nome(Parking))
info_tabela_continuo(Latitude, var_nome(Latitude))
info_tabela_continuo(Longitude, var_nome(Longitude))

info_tabela_discreto(Elevator, list(0,1))
info_tabela_discreto(Furnished, list(0,1))
info_tabela_discreto(Swimming_Pool, list(0,1))
info_tabela_discreto(New, list(0,1))

### Regressão

#reg1 <- lm((Price+Condo)~Size+Rooms+Toilets+Parking+Latitude+Longitude+Elevator+Furnished+Swimming_Pool+New, data=tabela)
#summary(reg1)

tabela <- district_to_zone(tabela)
attach(tabela)
reg<-faz_regressao(tabela)
summary(reg)


### Testes
# Heterocedasticidade
teste_heterocedasticidade()
teste_normalidade(reg)
resettest(reg)






      