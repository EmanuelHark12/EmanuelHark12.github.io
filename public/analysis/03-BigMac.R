#' Author: Emanuel Hark Maciel
#' Subject: Prevision of Big-Mac's price

setwd('./Projeto Data Science/Big-Mac')
# library(tidyverse)
library(magrittr)
library(tidyverse)
library(tidymodels)
library(rlang)
# Import -----------------------------------------------------------------------

mac_today <- readr::read_csv('big-mac-source-data.csv') %>% as_tibble()
# Tidy -------------------------------------------------------------------------
mac_sum <- summary(mac_today)

# Visualize --------------------------------------------------------------------

## FASE 1 --------------------------------------------------------------------


### 1) Faça uma análise descritiva de cada variável no dataset (Boxplots, histogramas, medidas resumo) --------------------------------------------------------------------

#### Funções --------------------------------------------------------------------


var_summary <- function(df,var,name){
  df |> 
    summarise(Média = mean({{var}}),
              Mediana = median({{var}}),
              Desvio_Padrão = sd({{var}}),
              Desvio_Absoluto = mad({{var}}),
              Mínino = min({{var}}),
              Máximo = max({{var}})) |> 
    bind_cols(Nome = name)
}
outlier_cities_top <- function(df,var){
  df %>%
    select(X1,{{var}}) %>%
    filter({{var}} > quantile({{var}})[4] + 1.5 *
             (quantile({{var}})[4]-quantile({{var}})[2]))
}
outlier_cities_bottom <- function(df,var){
  df %>%
    select(X1,{{var}}) %>%
    filter({{var}} < quantile({{var}})[2] - 1.5 *
             (quantile({{var}})[4]-quantile({{var}})[2]))
}

#### Bigmac --------------------------------------------------------------------

ggplot(mac_today) +
  geom_histogram(aes(x = BigMac),binwidth = 15) +
  ggtitle('Tempo de trabalho para a compra de 1 bigmac') +
  labs(x = 'Minutos',y='Cidades') +
  scale_x_continuous(breaks = seq(min(mac_today$BigMac)-3,max(mac_today$BigMac),by=15))

var_summary(mac_today,BigMac,'BigMac')

#### Bread --------------------------------------------------------------------
ggplot(mac_today) +
  geom_boxplot(aes(y=Bread)) +
  ggtitle('Tempo de trabalho para a compra de 1 kg de pão') +
  labs(y = 'Minutos')

var_summary(mac_today,Bread,'Bread')

outlier_cities_top(mac_today,Bread)

#### Rice --------------------------------------------------------------------

var_summary(mac_today,Rice,'Rice')

ggplot(mac_today) +
  geom_boxplot(aes(y=Rice)) +
  ggtitle('Tempo de trabalho para a compra de 1 kg de Arroz') +
  labs(y = 'Minutos')

outlier_cities_top(mac_today,Rice)

####  Food Index --------------------------------------------------------------------

var_summary(mac_today,FoodIndex,'FoodIndex')

ggplot(mac_today) +
  geom_boxplot(aes(y=FoodIndex)) +
  ggtitle('FoodIndex') +
  labs(y = 'Minutos')

outlier_cities_top(mac_today,FoodIndex)

#### Bus --------------------------------------------------------------------

ggplot(mac_today) +
  geom_histogram(aes(x=Bus)) +
  ggtitle('Preço da Passagem de ônibus') +
  labs(y = 'Cidades')

#### Apt --------------------------------------------------------------------

ggplot(mac_today) +
  geom_histogram(aes(x=Apt),binwidth = 100) +
  ggtitle('Preço dos Apartamentos') +
  labs(y='Cidades',x='Preço')

ggplot(mac_today) +
  geom_boxplot(aes(y=Apt)) +
  ggtitle('Preço dos Apartamentos') +
  labs(y='Preço')

var_summary(mac_today,Apt,'Apt')

outlier_cities_top(mac_today,Apt)

#### TeachGI --------------------------------------------------------------------

var_summary(mac_today,TeachGI,'TeachGI')

ggplot(mac_today) +
  geom_histogram(aes(x=TeachGI)) +
  ggtitle('Salário bruto anual de professores') +
  labs(x='Preço',y='Cidades')

ggplot(mac_today) +
  geom_boxplot(aes(y=TeachGI)) +
  ggtitle('Salário bruto anual de professores') +
  labs(y='Preço')

outlier_cities_top(mac_today,TeachGI)

#### TeachNI --------------------------------------------------------------------

var_summary(mac_today,TeachNI,'TeachNI')

ggplot(mac_today) +
  geom_boxplot(aes(y=TeachNI)) +
  ggtitle('Salário líquido anual de professores') +
  labs(y='Preço')

outlier_cities_top(mac_today,TeachNI)

#### TaxRate --------------------------------------------------------------------


var_summary(mac_today,TaxRate,'TaxRate')

ggplot(mac_today) +
  geom_boxplot(aes(y=TaxRate)) +
  ggtitle('Imposto pago por professores') +
  labs(y='Imposto')

ggplot(mac_today) +
  geom_histogram(aes(x=TaxRate),binwidth = 10) +
  ggtitle('Imposto pago por professores') +
  labs(x='Imposto',y='Preço')
outlier_cities_bottom(mac_today,TaxRate)

#### Teach Hours --------------------------------------------------------------------

var_summary(mac_today,TeachHours,'TeachHours')

ggplot(mac_today) +
  geom_boxplot(aes(y=TeachHours)) +
  ggtitle('Carga horária dos professores') +
  labs(y = 'Horas')

outlier_cities_top(mac_today,TeachHours)
outlier_cities_bottom(mac_today,TeachHours)

### 2) Durante a análise, você encontrou alguma observação que talvez seja inconsistente? Se sim, qual? --------------------------------------------------------------------

texto = 'Taxa de Imposto negativa para professores do ensino fundamental da cidade de Lima'

outlier_cities_bottom(mac_today,TaxRate)

### 3) Análise, de forma descritiva, a relação entre a variável BigMac e as demais variáveis. (Use gráficos de dispersão com curvas de tendência e calcule a correlação) ------------------------------------------------------------------------

#### Matriz de Correlação ------------------------------------------------------------------------
all_vectors <- function(x) select(x,-X1)
mac_today |>
  all_vectors() |>
  cor()

#### Rice ------------------------------------------------------------------------

ggplot(mac_today) +
  geom_point(aes(x=BigMac,y=Rice)) +
  ggtitle('Relação BigMac x Rice') +
  labs(x='Minutos para compra do Big Mac',y='Minutos para compra de 1kg de arroz')

#### Bread ------------------------------------------------------------------------
ggplot(mac_today) +
  geom_point(aes(x=BigMac,y=Bread)) +
  ggtitle('Relação BigMac x Bread') +
  labs(x='Minutos para compra do Big Mac',y='Minutos para compra de 1kg de Pão')

##### Relação de 1/x  ----------------------------------------------------------


#### FoodIndex ------------------------------------------------------------------------
ggplot(mac_today) +
  geom_point(aes(x=BigMac,y=FoodIndex)) +
  ggtitle('Relação BigMac x FoodIndex') +
  labs(x='Minutos para compra do Big Mac',y='Ìndices alimentício')

##### Relação de 1/x  ----------------------------------------------------------

#### Bus ------------------------------------------------------------------------
ggplot(mac_today) +
  geom_point(aes(x=BigMac,y=Bus)) +
  ggtitle('Relação BigMac x Bus') +
  labs(x='Minutos para compra do Big Mac',y='Preço de passagem')

##### Relação de 1/x  ------------------------------------------------------------------------

#### Apt ------------------------------------------------------------------------
ggplot(mac_today) +
  geom_point(aes(x=BigMac,y=Apt)) +
  ggtitle('Relação BigMac x Apt') +
  labs(x='Minutos para compra do Big Mac',y='Preço do apartamento')

##### Relação de 1/x  ----------------------------------------------------------

#### TeachGI -----------------------------
ggplot(mac_today) +
  geom_point(aes(x=BigMac,y=TeachGI)) +
  ggtitle('Relação BigMac x TeacGI') +
  labs(x='Minutos para compra do Big Mac',y='Salário bruto dos professores')

##### Relação de 1/x --------

#### TeachNI ----------------
ggplot(mac_today) +
  geom_point(aes(x=BigMac,y=TeachNI)) +
  ggtitle('Relação BigMac x TeacNI') +
  labs(x='Minutos para compra do Big Mac',y='Salário líquido dos professores')

##### Relação de 1/x -------

#### TaxRate --------
ggplot(mac_today) +
  geom_point(aes(x=BigMac,y=TaxRate)) +
  ggtitle('Relação BigMac x TaxRate') +
  labs(x='Minutos para compra do Big Mac',y='Preço dos impostos')

#### Teach Hours -----
ggplot(mac_today) +
  geom_point(aes(x=BigMac,y=TeachHours)) +
  ggtitle('Relação BigMac x TeachHours') +
  labs(x='Minutos para compra do Big Mac',y='Horas de aulas')
# Model ------------------------------------------------------------------------

# FASE 2 -----

## 4) Seleção de modelo pelo método stepwise -----
table_model <- mac_today %>% select(BigMac,Rice,Bread,FoodIndex,Bus,Apt,TeachGI,TeachNI,TaxRate)
### 1) Seleção das variáveis que foram observadas relações
colunas <- colnames(table_model)[-1]
variavel_resposta = colnames(table_model)[1]
n_var_exp = 0
variavel_explicativa <- c()
k = 1
while(n_var_exp != 0 || k > 0){
  p_values = c()
  p_var = c()
  resume_list = list()
  for (i in seq(1,length(colunas))){
    if (is.element(colunas[i],variavel_explicativa) == FALSE){
      f_str <- paste(variavel_resposta, "~", paste(c(variavel_explicativa,colunas[i]), collapse = " + "))
      print(f_str)
      lm_preco <- linear_reg() %>% set_engine('lm') %>%
        set_mode('regression') %>% fit(as.formula(f_str),data=mac_today)
      resume = summary(lm_preco$fit)
      p = resume$coefficients[4*(n_var_exp + 2)]
      resume_list = append(resume_list,list(resume$coefficients))
      p_values = c(p_values,p)
      p_var = c(p_var,colunas[i])
    }
  }
  c = min(p_values)
  x = match(c,p_values)
  list_values = resume_list[[x]]
  var = p_var[x]
  variavel_explicativa = c(variavel_explicativa,var)
  variaveis = variavel_explicativa
  n_var_exp = n_var_exp + 1
  if(c < 0.15){
    var_retired = c()
    v = length(list_values)
    for(j in seq(1,length(variavel_explicativa))){
      if(list_values[v-j+1] < 0.15){
      }
      else{
        var_retired = c(var_retired,variavel_explicativa[j])
        n_var_exp = n_var_exp - 1
      }
    }
    variavel_explicativa <- setdiff(variaveis,var_retired)
    k = k + 1
  }
  else{
    n_var_exp = 0
    k = 0
    variavel_explicativa = variavel_explicativa[-length(variavel_explicativa)]
  }
}
variavel_explicativa
f_str <- paste(variavel_resposta, "~", paste(c(variavel_explicativa), collapse = " + "))
lm_preco <- linear_reg() %>% set_engine('lm') %>% set_mode('regression') %>% fit(as.formula(f_str),data=mac_today)
resume = summary(lm_preco$fit)
resume
# Export -----------------------------------------------------------------------

# readr::write_rds(d, "")

F