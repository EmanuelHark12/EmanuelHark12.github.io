# Data -----

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
g = grid::rasterGrob(jpeg::readJPEG("/home/emanuelhark12/Documentos/Site/EmanuelHark12.github.io/img/nba_court.jpg"),width = grid::unit(1,'npc'),height = grid::unit(1,'npc'),interpolate = FALSE)
g2 = grid::rasterGrob(jpeg::readJPEG("/home/emanuelhark12/Documentos/Site/EmanuelHark12.github.io/img/nba_court2.jpg"),width = grid::unit(1,'npc'),height = grid::unit(1,'npc'),interpolate = FALSE)
data2022 = nbastatR::teams_shots(teams = "Portland Trail Blazers",
                                 seasons = 2022)
data2021 = nbastatR::teams_shots(teams = "Portland Trail Blazers",
                                 seasons = 2021)
data2020 = nbastatR::teams_shots(teams = "Portland Trail Blazers",
                                 seasons = 2020)
data2019 = nbastatR::teams_shots(teams = "Portland Trail Blazers",
                                 seasons = 2019)
data2018 = nbastatR::teams_shots(teams = "Portland Trail Blazers",
                                 seasons = 2018)
data2017 = nbastatR::teams_shots(teams = "Portland Trail Blazers",
                                 seasons = 2017)
ids2021 <- unique(data2021$idGame)
ids2022 <- unique(data2022$idGame)
ids2020 <- unique(data2020$idGame)
ids2019 <- unique(data2019$idGame)
ids2018 <- unique(data2018$idGame)
ids2017 <- unique(data2017$idGame)
ww2021 = nbastatR::box_scores(game_ids = c(ids2021[1:12],ids2020[1:12],ids2019[1:12],ids2018[1:12],ids2017[1:12]),box_score_types = c("Traditional"), result_types = c("player"), join_data = TRUE, assign_to_environment = TRUE, return_message = TRUE) 
game1 <- ww2021[[2]][[1]]

ww2022 = nbastatR::box_scores(game_ids = ids2022[1:12], box_score_types = c("Traditional"), result_types = c("player"), join_data = TRUE, assign_to_environment = TRUE, return_message = TRUE)
game2 <- ww2022[[2]][[1]]

data = rbind(data2022,data2021,data2020,data2019,data2018,data2017) |> 
  dplyr::filter(namePlayer == 'Damian Lillard' & idGame %in% c(ids2021[1:12],ids2020[1:12],ids2019[1:12],ids2018[1:12],ids2017[1:12],ids2022[1:12]))
games2021 <- game1 |> 
  dplyr::filter(namePlayer == 'Damian Lillard') |> 
  dplyr::mutate(Ano = c(rep('2020-21',12),rep('2019-20',12),rep('2018-19',12),rep('2017-18',12),rep('2016-17',12))) |> 
  dplyr::group_by(namePlayer,Ano) |> 
  dplyr::summarise(assist = round(mean(ast),3),
                   steal  = round(mean(stl),3),
                   reb    = round(mean(treb),3),
                   turnover = round(mean(tov),3),
                   fouls = round(mean(pf),3),
                   block = round(mean(blk),3),
                   fg = round(100 * sum(fgm)/sum(fga),3),
                   s_fg = round(100 * sum(fg2m)/sum(fg2a),3),
                   t_fg = round(100 * sum(fg3m)/sum(fg3a),3),
                   f_fg = round(100 * sum(ftm)/sum(fta),3),
                   points = round(mean(pts),3))
games2022 <- game2 |> 
  dplyr::filter(namePlayer == 'Damian Lillard') |> 
  dplyr::group_by(namePlayer) |> 
  dplyr::summarise(assist = round(mean(ast),3),
                   steal  = round(mean(stl),3),
                   reb    = round(mean(treb),3),
                   turnover = round(mean(tov),3),
                   fouls = round(mean(pf),3),
                   block = round( mean(blk),3),
                   fg = round(100 * sum(fgm)/sum(fga),3),
                   s_fg = round(100 * sum(fg2m)/sum(fg2a),3),
                   t_fg = round(100 * sum(fg3m)/sum(fg3a),3),
                   f_fg = round(100 * sum(ftm)/sum(fta),3),
                   points = round(mean(pts),3)) |> 
  dplyr::mutate(Ano = c('2021-22'))
games_stats <- rbind(games2021,games2022)

gamess <- rbind(game1,game2)
games_list <- gamess |> dplyr::filter(namePlayer == 'Damian Lillard') 
game_list_2 <- games_list |> 
  dplyr::mutate(Ano = c(rep('2020-21',12),rep('2019-20',12),rep('2018-19',12),rep('2017-18',12),rep('2016-17',12),rep('2021-22',12))) |> 
  dplyr::arrange(desc(pts)) |> 
  dplyr::group_by(Ano) |> 
  dplyr::slice(tail(dplyr::row_number(),5)) |> 
  dplyr::select(Ano,pts,idGame)
game_list_3 <- gamess |> 
  dplyr::filter(idGame %in% game_list_2$idGame & slugTeam != 'POR') |> 
  dplyr::group_by(idGame) |> 
  dplyr::slice(tail(dplyr::row_number(),1)) |> 
  dplyr::select(slugTeam)
game_list_4 <- game_list_2 |> 
  dplyr::full_join(game_list_3,by='idGame')
game_list_5 <- game_list_4 |> 
  dplyr::ungroup() |> 
  dplyr::mutate(Time_20_21 = c(slugTeam[6:10],rep(NA,25)),
                Pontos_20_21 = c(pts[6:10],rep(NA,25)),
                Time_19_20 = c(slugTeam[11:15],rep(NA,25)),
                Pontos_19_20 = c(pts[11:15],rep(NA,25)),
                Time_18_19 = c(slugTeam[16:20],rep(NA,25)),
                Pontos_18_19 = c(pts[16:20],rep(NA,25)),
                Time_17_18 = c(slugTeam[21:25],rep(NA,25)),
                Pontos_17_18 = c(pts[21:25],rep(NA,25)),
                Time_16_17 = c(slugTeam[26:30],rep(NA,25)),
                Pontos_16_17 = c(pts[26:30],rep(NA,25))) |> 
  dplyr::select(4,2,5,6,7,8,9,10,11,12,13,14) |> 
  head(5)

new_season <- data |> 
  tidyr::complete(zoneRange,slugSeason) |> 
  dplyr::group_by(zoneRange,slugSeason) |> 
  dplyr::summarise(Quantidade = dplyr::n(),
                   Acerto = sum(isShotMade),
                   Media = round(100 * mean(isShotMade),3)) |> 
  tidyr::replace_na(list(Acerto =0,Media=0))
season_2022_1 <- new_season |> dplyr::filter(slugSeason == '2021-22') |> 
  dplyr::select(zoneRange,Quantidade,Acerto,Media)
season_2022 <- season_2022_1  |> 
  dplyr::ungroup() |> 
  rbind(c('Total',sum(season_2022_1$Quantidade),sum(season_2022_1$Acerto),round(sum(season_2022_1$Acerto)/sum(season_2022_1$Quantidade),3)))

season_2021_1 <- new_season |> dplyr::filter(slugSeason == '2020-21') |> 
  dplyr::select(zoneRange,Quantidade,Acerto,Media)
season_2021 <- season_2021_1  |> 
  dplyr::ungroup() |> 
  rbind(c('Total',sum(season_2021_1$Quantidade),sum(season_2021_1$Acerto),round(sum(season_2021_1$Acerto)/sum(season_2021_1$Quantidade),3)))

season_2020_1 <- new_season |> dplyr::filter(slugSeason == '2019-20') |> 
  dplyr::select(zoneRange,Quantidade,Acerto,Media)
season_2020 <- season_2020_1  |> 
  dplyr::ungroup() |> 
  rbind(c('Total',sum(season_2020_1$Quantidade),sum(season_2020_1$Acerto),round(sum(season_2020_1$Acerto)/sum(season_2020_1$Quantidade),3)))

season_2019_1 <- new_season |> dplyr::filter(slugSeason == '2018-19') |> 
  dplyr::select(zoneRange,Quantidade,Acerto,Media)
season_2019 <- season_2019_1  |> 
  dplyr::ungroup() |> 
  rbind(c('Total',sum(season_2019_1$Quantidade),sum(season_2019_1$Acerto),round(sum(season_2019_1$Acerto)/sum(season_2019_1$Quantidade),3)))

season_2018_1 <- new_season |> dplyr::filter(slugSeason == '2017-18') |> 
  dplyr::select(zoneRange,Quantidade,Acerto,Media)
season_2018 <- season_2018_1  |> 
  dplyr::ungroup() |> 
  rbind(c('Total',sum(season_2018_1$Quantidade),sum(season_2018_1$Acerto),round(sum(season_2018_1$Acerto)/sum(season_2018_1$Quantidade),3)))

season_2017_1 <- new_season |> dplyr::filter(slugSeason == '2016-17') |> 
  dplyr::select(zoneRange,Quantidade,Acerto,Media)
season_2017 <- season_2017_1  |> 
  dplyr::ungroup() |> 
  rbind(c('Total',sum(season_2017_1$Quantidade),sum(season_2017_1$Acerto),round(sum(season_2017_1$Acerto)/sum(season_2017_1$Quantidade),3)))
season_shots <- season_2022 |> 
  dplyr::full_join(season_2021,by='zoneRange') |> 
  dplyr::full_join(season_2020,by='zoneRange') |> 
  dplyr::full_join(season_2019,by='zoneRange') |> 
  dplyr::full_join(season_2018,by='zoneRange') |>
  dplyr::full_join(season_2017,by='zoneRange') |> 
  dplyr::mutate(Tipo_de_Arremesso = dplyr::recode(zoneRange,'Less Than 8 ft.'='Paint','8-16 ft.'='Free-Throw Area','16-24 ft.' = 'Mid-Range','24+ ft.' = '3-points Area','Total'='Total'))
# Court Graph -----

## 21-22 ----
data |> 
  ggplot2::ggplot(ggplot2::aes(x=locationX,y=locationY)) +
  ggplot2::annotation_custom(g, -250, 250, -50, 420) +
  ggplot2::geom_point(ggplot2::aes(color= isShotMade == TRUE)) +
  ggplot2::scale_color_manual(name='Shots Attemped',values=c("blue","red"),
                              labels = c('Failed','Made')) +
  ggplot2::ggtitle('Shots attemps by Damian Lillard(2021-22)') + 
  ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 axis.line = ggplot2::element_blank(),
                 plot.background = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 panel.border = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(hjust = 0.5)) +
  ggplot2::xlim(-250, 250) +
  ggplot2::ylim(-50, 420) +
  ggplot2::facet_wrap(~slugSeason)
data |>  
  ggplot2::ggplot(ggplot2::aes(x=locationX,y=locationY)) +
  ggplot2::annotation_custom(g, -250, 250, -50, 420) +
  ggplot2::geom_point(ggplot2::aes(color= isShotMade == TRUE)) +
  ggplot2::scale_color_manual(name='Tentativas de Cestas',values=c("blue","red"),labels = c('Erro','Acerto')) +
  ggplot2::ggtitle('Tentativas de Arremessos de Damian Lillard(2021-22)') + 
  ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 axis.line = ggplot2::element_blank(),
                 plot.background = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 panel.border = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(hjust = 0.5)
  ) +
  ggplot2::xlim(-250, 250) +
  ggplot2::ylim(-50, 420) +
  ggplot2::facet_wrap(~slugSeason)

# Worst-Point Table ----
game_list_5 |> 
  knitr::kable(caption = "<center>Games with less points by Damian Lillard</center>",col.names = c("OPP","PTS","OPP","PTS","OPP","PTS","OPP","PTS","OPP","PTS","OPP","PTS")) |> 
  kableExtra::kable_classic(full_width = F, html_font = "Roboto",bootstrap_options = "striped") |> 
  kableExtra::add_header_above(c('2016-17'=2,'2017-18'=2,'2018-19'=2,'2019-20'=2,'2020-21'=2,'2021-22'=2)) |> 
  kableExtra::footnote("OPP: slug of Opponent Team;PTS: Points",
                       number = c('LAC = Los Angeles Clippers, PHX = Phoenix Suns, MEM = Memphis Grizzlies',
                                  'CHI = Chicago Bulls, BKN = Brooklyn Nets, IND = Indiana Pacers',
                                  'NOP = New Orleans Pelicans, HOU = Houston Rockets, MIL = Milwaukee Bucks',
                                  'SAS = San Antonio Spurs, SAC = Sacramento Kings, OKC = Oklahoma City Thunder',
                                  'TOR = Toronto Raptors, UTA = Utah Jazz, CHA = Charlotte Hornets')) 

game_list_5 |> 
  knitr::kable(caption = "<center>Jogos com menos pontos feitos por Damian Lillard</center>",col.names = c("OPP","PTS","OPP","PTS","OPP","PTS","OPP","PTS","OPP","PTS","OPP","PTS")) |> 
  kableExtra::kable_classic(full_width = F, html_font = "Roboto") |> 
  kableExtra::add_header_above(c('2016-17'=2,'2017-18'=2,'2018-19'=2,'2019-20'=2,'2020-21'=2,'2021-22'=2)) |> 
  kableExtra::footnote("OPP: sigla do Time Adversário;PTS: Pontos",
                       number = c('LAC = Los Angeles Clippers, PHX = Phoenix Suns, MEM = Memphis Grizzlies',
                                  'CHI = Chicago Bulls, BKN = Brooklyn Nets, IND = Indiana Pacers',
                                  'NOP = New Orleans Pelicans, HOU = Houston Rockets, MIL = Milwaukee Bucks',
                                  'SAS = San Antonio Spurs, SAC = Sacramento Kings, OKC = Oklahoma City Thunder',
                                  'TOR = Toronto Raptors, UTA = Utah Jazz, CHA = Charlotte Hornets')) 
# Field-Goal Table ----

season_shots[2:20] |>
  dplyr::select(19,c(1:18)) |> 
  knitr::kable(caption = "<center>Number of shots by Damian Lillard</center>",
               col.names = c("Type of Throw","FGA","FG","FG%","FGA","FG","FG%","FGA","FG","FG%","FGA","FG","FG%","FGA","FG","FG%","FGA","FG","FG%")) |> 
  kableExtra::kable_classic(full_width = F, html_font = "Roboto") |> 
  kableExtra::add_header_above(c(' '=1,'2021-22'=3,'2020-21'=3,'2019-20'=3,
                                 '2018-19'=3,'2017-18'=3,'2016-17'=3)) |> 
  kableExtra::footnote("FGA: Field Goal Attempts;FG: Field Goal; FG%: Field Goal Percentage") 

season_shots[2:20] |>
  dplyr::select(19,c(1:18))  |>
  knitr::kable(caption = "<center>Número de arremessos feitos por Damian Lillard</center>",col.names = c("Tipo de Arremesso","FGA","FG","FG%","FGA","FG","FG%","FGA","FG","FG%","FGA","FG","FG%","FGA","FG","FG%","FGA","FG","FG%")) |> 
  kableExtra::kable_classic(full_width = F, html_font = "Roboto") |> 
  kableExtra::add_header_above(c(' '=1,'2021-22'=3,'2020-21'=3,'2019-20'=3,'2018-19'=3,'2017-18'=3,'2016-17'=3)) |> 
  kableExtra::footnote("FGA: Tentativas de Arremessos de Quadra;FG: Arremessos de Quadra Certos ; FG%: Porcentagem de Arremessos de Quadra Certos") 

# Overall table  -----

games_stats[2:13]  |> 
  knitr::kable(caption = "<center>Other statics by Damian Lillard (%)</center>",
               col.names = c('Year','AST','STL','REB',
                             "TRN",'PF','BLK',"FG%","2FG%","3FG%","FT%","PTS")) |> 
  kableExtra::kable_classic(full_width = F, html_font = "Roboto") |> 
  kableExtra::footnote("AST: Assists per game; STL: Steals per game;REB: Rebounds per game;
                       TRN: Turnovers per game; PF: Personal Fouls per game; BLK: Blocks per game;
                       FG%: Field Goal Percentage;2FG%: 2-Point Field Goal Percentage;3FG%: 3-Point Field Goal Percentage;
                       FT%: Free Throw Percentage;PTS: Points per game")

games_stats[2:13]  |>
  knitr::kable(caption = "<center>Demais estatísticas de Damian Lillard</center>",
               col.names = c('Year','AST','STL','REB',
                             "TRN",'PF','BLK',"FG%","2FG%","3FG%","FT%","PTS")) |> 
  kableExtra::kable_classic(full_width = F, html_font = "Roboto") |> 
  kableExtra::footnote("AST: Assistências por jogo; STL: Roubos por jogo;REB: Rebotes por jogo;
                       TRN: Perda de posse por jogo; PF: Faltas por jogo; BLK: Tocos por jogo;
                       FG%: Porcentagem de Arremessos Certos;2FG%:  Porcentagem de Arremessos Certos de 2 pontos;3FG%: Porcentagem de Arremessos Certos de 3 pontos;FT%: Porcentagem de Arremesso de Lance Livre Certos;PTS: Pontos por jogo")