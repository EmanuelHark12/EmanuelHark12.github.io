# Data ----

dados_final <- read.csv('/home/emanuelhark12/Documentos/Site/EmanuelHark12.github.io/public/analysis/data/02-dados_final.csv') |> 
  dplyr::rename(id = X,Name = Titulo,Type = New_Tipo,Time = New_Tempo,Price = Preco,Rating = New_Nota,User = Dono) |> 
  dplyr::mutate(User = dplyr::recode(User,'Emanuel'='E','Alexandre'='A','Emerson'='M','Gustavo'='G'))
sumario = summary(dados_final) # Verificação das variáveis
sumario
dados_eng <- dados_final |> dplyr::mutate(Type = dplyr::recode(Type,'Lanches' = 'Snacks','Italiana' = 'Italian','Doces & Bolos' = 'Sweets & Cakes',
                                                                'Saudável' = 'Fit','Brasileira' = 'Brazilian','Japonesa' = 'Japonese','Coreana' = 'Korean',
                                                                'Árabe' = 'Arabian','Mercado' = 'Supermarket','Salgados'='Savory','Pastel' = 'Pastry',
                                                                'Carnes' = 'Meats','Chinesa' = 'Chinese', 'Cafeteria' = 'Coffee Shop','Padaria' = 'Bakery',
                                                               'Hambúrger'='Burguer','Portuguesa'='Portuguese','Bebidas'='Drinks')) 

# User data ---- 

loja_distinto <- dados_final |> 
  dplyr::group_by(User) |> 
  dplyr::summarise(Number = dplyr::n_distinct(Type))

distinct_shop <- dados_eng |> 
  dplyr::group_by(User) |> 
  dplyr::summarise(Number = dplyr::n_distinct(Type))

n_loja <- dados_final |> 
  dplyr::group_by(Type)|> 
  dplyr::summarise(Num=dplyr::n())

n_shop <- dados_eng |> 
  dplyr::group_by(Type)|> 
  dplyr::summarise(Num=dplyr::n())

intervalo_tempo <- dados_final |>
  dplyr::mutate(Interval = cut(Time,c(0,15,30,45,60,75,90,105))) |>  
  dplyr::group_by(Interval) |> 
  dplyr::summarise(Number = dplyr::n())

time_interval <- dados_eng |>
  dplyr::mutate(Interval = cut(Time,c(0,15,30,45,60,75,90,105))) |> 
  dplyr::group_by(Interval) |> 
  dplyr::summarise(Number = dplyr::n())

intervalo_preco <- dados_final |>
  dplyr::mutate(Interval = cut(Price,c(-1,0,5,10,15,20,25))) |>  
  dplyr::group_by(Interval) |> 
  dplyr::summarise(Number = dplyr::n())

price_interval <- dados_eng |>
  dplyr::mutate(Interval = cut(Price,c(-1,0,5,10,15,20,25))) |>
  dplyr::group_by(Interval) |> 
  dplyr::summarise(Number = dplyr::n())

# Graphs ----

## Type -----

ggplot2::ggplot(n_shop,ggplot2::aes(y=forcats::fct_reorder(Type,Num),x=Num,fill=Num<=10)) +
  ggplot2::geom_bar(stat='identity',show.legend = FALSE) +
  ggplot2::labs(x='Number of restaurants',y='Specialty') +
  ggplot2::ggtitle("Number of restaurants by type around the members of IME Jr") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))  +
  ggplot2::coord_cartesian(xlim=c(0,50),expand = FALSE) 

ggplot2::ggplot(n_loja,ggplot2::aes(y=forcats::fct_reorder(Type,Num),x=Num,fill=Num<=10)) +
  ggplot2::geom_bar(stat='identity',show.legend = FALSE) +
  ggplot2::labs(x='Número de restaurantes',y='Especialidade') +
  ggplot2::ggtitle("Número de restaurantes por tipo nos arredorres dos membros da IME Jr") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))  +
  ggplot2::coord_cartesian(xlim=c(0,50),expand = FALSE) 

## Time ----

ggplot2::ggplot(time_interval,ggplot2::aes(y=Interval,x=Number)) +
  ggplot2::geom_bar(stat='identity',show.legend = FALSE) +
  ggplot2::labs(x='Number of restaurants',y='Interval') +
  ggplot2::ggtitle("Number of restaurants by delivery time around the members of IME Jr") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))  +
  ggplot2::coord_cartesian(xlim=c(0,70),expand = FALSE) 

ggplot2::ggplot(time_interval,ggplot2::aes(y=Interval,x=Number)) +
  ggplot2::geom_bar(stat='identity',show.legend = FALSE) +
  ggplot2::labs(x='Número de restaurantes',y='Time Interval') +
  ggplot2::ggtitle("Número de restaurantes pelo tempo de entrega nos arredorres dos membros da IME Jr") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))  +
  ggplot2::coord_cartesian(xlim=c(0,50),expand = FALSE) 

## Price ----

ggplot2::ggplot(intervalo_preco,ggplot2::aes(y=Interval,x=Number)) +
  ggplot2::geom_bar(stat='identity',show.legend = FALSE) +
  ggplot2::labs(x='Number of restaurants',y='Price Interval') +
  ggplot2::ggtitle("Number of restaurants by price interval around the members of IME Jr") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))  +
  ggplot2::coord_cartesian(xlim=c(0,70),expand = FALSE) 

ggplot2::ggplot(intervalo_preco,ggplot2::aes(y=Interval,x=Number)) +
  ggplot2::geom_bar(stat='identity',show.legend = FALSE) +
  ggplot2::labs(x='Número de restaurantes',y='Intervalo de preço') +
  ggplot2::ggtitle("Número de restaurantes pelo intervalo de preço nos arredorres dos membros da IME Jr") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))  +
  ggplot2::coord_cartesian(xlim=c(0,70),expand = FALSE) 

## Rating ----

ggplot2::ggplot(rate_interval,ggplot2::aes(y=Interval,x=Number)) +
  ggplot2::geom_bar(stat='identity',show.legend = FALSE) +
  ggplot2::labs(x='Number of restaurants',y='Rating') +
  ggplot2::ggtitle("Number of restaurants by rating around the members of IME Jr") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))  +
  ggplot2::coord_cartesian(xlim=c(0,70),expand = FALSE) 

ggplot2::ggplot(intervalo_nota,ggplot2::aes(y=Interval,x=Number)) +
  ggplot2::geom_bar(stat='identity',show.legend = FALSE) +
  ggplot2::labs(x='Número de restaurantes',y='Nota') +
  ggplot2::ggtitle("Número de restaurantes pela nota nos arredorres dos membros da IME Jr") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))  +
  ggplot2::coord_cartesian(xlim=c(0,70),expand = FALSE) 

## User x Type ----

ggplot2::ggplot(loja_distinto,ggplot2::aes(x=Number,y=forcats::fct_reorder(User,Number))) +
  ggplot2::geom_bar(stat='identity',show.legend = FALSE) +
  ggplot2::labs(x='Number of specialty',y='IME Jr member') +
  ggplot2::ggtitle("Number of types of restaurants by IME Jr member") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

ggplot2::ggplot(loja_distinto,ggplot2::aes(x=Number,y=forcats::fct_reorder(User,Number))) +
  ggplot2::geom_bar(stat='identity',show.legend = FALSE) +
  ggplot2::labs(x='Número de especialidades',y='Membro da IME Jr') +
  ggplot2::ggtitle("Número de tipos de restaurantes por membro da IME Jr") + 
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

## User x Time -----

ggplot2::ggplot(dados_final,ggplot2::aes(y=Time,x = User,fill=User)) + 
  ggplot2::geom_boxplot()+
  ggplot2::labs(x="IME Jr's member",y='Delivery Time') +
  ggplot2::ggtitle("Delivery time of restaurants by the members of IME Jr") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

ggplot2::ggplot(dados_eng,ggplot2::aes(y=Time,x=User,fill = User)) + 
  ggplot2::geom_boxplot()+
  ggplot2::labs(x='Membro da IME Jr',y='Tempo de Entrega') +
  ggplot2::ggtitle("Tempo de entrega dos restaurantes por membros da IME Jr") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

## User x Price ----

ggplot2::ggplot(dados_final,ggplot2::aes(y=Price,x = User,fill=User)) + 
  ggplot2::geom_boxplot()+
  ggplot2::labs(x="IME Jr's member",y='Price') +
  ggplot2::ggtitle("Price of restaurants by IME Jr member") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

ggplot2::ggplot(dados_eng,ggplot2::aes(y=Price,x=User,fill = User)) + 
  ggplot2::geom_boxplot()+
  ggplot2::labs(x='Membro da IME Jr',y='Preço') +
  ggplot2::ggtitle("Preço dos restaurantes por membros da IME Jr") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

## User x Rating ----

ggplot2::ggplot(dados_final,ggplot2::aes(y=Rating,x = User,fill=User)) + 
  ggplot2::geom_boxplot()+
  ggplot2::labs(x="IME Jr's member",y='Rate') +
  ggplot2::ggtitle("Ratings of restaurants by the members of IME Jr") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

ggplot2::ggplot(dados_eng,ggplot2::aes(y=Rating,x=User,fill = User)) + 
  ggplot2::geom_boxplot()+
  ggplot2::labs(x='Membro da IME Jr',y='Nota') +
  ggplot2::ggtitle("Nota dos restaurants por membro da IME Jr ") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

