
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

faz_regressao <- function(tabela){
  Districts_names <- District[!duplicated(District)]
  Districts_names <- Districts_names[Districts_names != 'Vila Olimpia/São Paulo']
  Districts_names_limpos <- str_replace(Districts_names, '/São Paulo', '')
  Districts_names_limpos <- str_replace(Districts_names_limpos, ' ', '_')
  Districts_names_limpos <- str_replace(Districts_names_limpos, ' ', '_')

  outras_variaveis <- list("Size","Rooms","Toilets","Parking","Latitude","Longitude","Elevator","Furnished","Swimming_Pool","New")
  
  todas_variaveis<-append(Districts_names_limpos, outras_variaveis)
  form <- as.formula(paste("(Price+Condo)~", paste(todas_variaveis, collapse="+")))
  reg <- lm(form)
  return(reg)
}

criacao_boleanos_distritos <- function(tabela, District){
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

tabela <- criacao_boleanos_distritos(tabela, District)
attach(tabela)
reg<-faz_regressao(tabela)
summary(reg)


### Testes
# Heterocedasticidade
teste_heterocedasticidade()
teste_normalidade(reg)
resettest(reg)


