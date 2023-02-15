
comeco <- function(){
  library(readr)
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
  attach(table)
  return(table)
}

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

var_nome <- function (some_variable, name=deparse(substitute(some_variable))) {
  name
}
tabela <- comeco()
attach(tabela)
info_tabela_discreto(Elevator)

array_continuo <- list(Price, Condo, Size, Rooms, Toilets, Parking)
array_binario <- list(Elevator, Furnished, Swimming_Pool, New)

info_tabela_continuo(Price, var_nome(Price))
info_tabela_continuo(Condo, var_nome(Condo))
info_tabela_continuo(Size, var_nome(Size))
info_tabela_continuo(Rooms, var_nome(Rooms))
info_tabela_continuo(Toilets, var_nome(Toilets))
info_tabela_continuo(Parking, var_nome(Parking))

info_tabela_discreto
