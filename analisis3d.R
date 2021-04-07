library(tidyverse)
library(ggtext)
library(nbastatR)
library(scales)
library(gt)


# Get data -------

df_statsP <- read_csv('stats_tutorial_player_data_2020-21.csv') %>% 
  janitor::clean_names()  
df_statsP <- df_statsP %>% select(player_id, player_name,
                         team_id, gp, team_abbreviation, fg3m, fg3a, fg3_pct) %>% 
                          filter(gp >= 20 & fg3a >= 70) %>% 
                          mutate(fg3m = round(fg3m/gp,1),
                                 fg3a = round(fg3a/gp,1),
                                 fg3mZ = scale(fg3m),
                                 fg3aZ = scale(fg3a),
                                 fg3_pctZ = scale(fg3_pct),
                                 fg3tz = round(fg3mZ + fg3aZ + fg3_pctZ/3,2)) %>% 
  arrange(desc(fg3tz)) %>%  select(player_id, player_name,
                                       team_id, gp, team = team_abbreviation, fg3m, fg3a, fg3_pct, totalZ = fg3tz) 

oposite <- read_csv('stats_nba_oposite_data_2020-21.csv')%>% 
  janitor::clean_names() 
oposite <- oposite %>% set_names(paste0(c("player_id","player_name","team_id","team_abbreviation","age","fgm_ra","fga_ra",
                                             "fg_pct_ra","fgm_Non-RA","fga_Non-RA","fg_pct_Non-RA",
                                             "fgm_mr","fga_mr","fg_pct_mr","fgm_lc","fga_lc","fg_pct_lc","fgm_rc",
                                             "fga_rc","fg_pct_rc","fgm_ab","fga_ab","fg_pct_ab","fgm_bc","fga_bc","fg_pct_bc"))) %>% 
                       janitor::clean_names()




df_statsP <- df_statsP %>% left_join(oposite, by = c ("player_id"="player_id", "player_name"= "player_name", "team_id"= "team_id", "team"="team_abbreviation")) %>% 
                       janitor::clean_names()


# Lo guardo para no tener que cargar las dos tablas si tengo un error --------


write.csv(df_statsP, "df_statsP.csv", row.names = FALSE)

df_statsP <- read.csv("df_statsP.csv")


# Calculo del promedio del oponente ---------------------------------------



df_stats <- df_statsP %>% group_by(player_name) %>% filter( total_z >= 2 ) %>% 
                         mutate(
                                fgmTot = round(sum(fgm_ra, fgm_non_ra, fgm_mr, fgm_lc, fgm_rc, fgm_ab)/6,2),
                                fgaTot = round(sum(fga_ra, fgm_non_ra, fga_mr, fga_lc, fga_rc, fga_ab)/6,2),
                                fgTot = round(sum(fg_pct_ra, fg_pct_non_ra, fg_pct_mr, fg_pct_lc, fg_pct_rc, fg_pct_ab)/6,2),
                                fgtz = round(fgmTot + fgaTot + fgTot/3)) %>% 
                        arrange(fgmTot) %>% select(player_id, player_name, team_id, team, fgmTot, fgaTot, fgTot) %>% 
  ungroup()


# Join the tabla ----------------------------------------------------------



# Primera tabla -----------------------------------------------------------


df_stats <- df_stats  %>%  left_join(df_statsP, by = c("player_id"="player_id", "player_name"= "player_name", "team_id"= "team_id", "team"="team")) %>% 
                                  janitor::clean_names() 
df_stats <- df_stats %>% select(player_id, player_name,
           team_id, gp, team , fg3m, fg3a, fg3_pct, fgm_tot, fga_tot, fg_tot) %>% 
  arrange(fgm_tot) %>%
  slice(1:32)
# Cabezas y logos ---------------------------------------------------------------
players <- nba_players() %>%filter(isActive == TRUE) %>%  select(player_id = idPlayer, team_id = idTeam, head = urlPlayerHeadshot)

teams <- nba_teams() %>% select(team_id = idTeam, logos = urlThumbnailTeam)

# Union de cabezas y logos ------------------------------------------------


dfCabezasLogos <-  left_join(players, teams, by = c("team_id" ))
df_stats <- df_stats %>%  left_join(dfCabezasLogos, by = c("player_id", "team_id" ))

hoy <- lubridate::today()

# Tabla -------------------------------------------------------------------



df_stats_Gt <- df_stats  %>% select(head, player_name, logos,
                                    team, fg3m, fg3a, fg3_pct,
                                    fgm_tot, fga_tot, fg_tot) %>% 

  mutate(fg3_pct = percent(fg3_pct, .1), 
         fg_tot = percent(fg_tot, .1)) %>% 
  gt() %>%
  tab_header(
    title = md(" **The Best '3&D'?<br>en la**<br><img src='https://seeklogo.com/images/N/nba-logo-41668C66DB-seeklogo.com.png' style='height:30px;'> "),
    subtitle = md(glue::glue("Tiros permitidos por jugadores con mas de 70 intentos de 3 | Mas 20 partidos jugados<br>Hasta {hoy}"))) %>%
  cols_label(head = (" "), 
             player_name = md ("Player"), 
             logos = ("Team"),
             team = (" "),
             fg3m = ("3PM"),
             fg3a = ("3PA"),
             fg3_pct = ("3P%"),
             fgm_tot = ("FGM"),
             fga_tot = ("FGA"),
             fg_tot = ("FG%") )%>% 
  tab_spanner(
    label =  gt::html("<span style='font-weight:bold;font-size:12px'>REALIZA</span>"),
    columns = vars(fg3m, fg3a, fg3_pct)
  ) %>% 
  tab_spanner(
    label =  gt::html("<span style='font-weight:bold;font-size:12px'>PERMITE</span>"),
    columns = vars(fgm_tot, fga_tot, fg_tot)
  ) %>% 
  cols_label(head = (" "), 
             player_name = md ("Player"), 
             logos = (""),
             team = ("Team "),
             fg3m =("3PM"),
             fg3a =("3PA"),
             fg3_pct = ("3P%"),
             fgm_tot = md("FGM<sup>*</sup>"),
             fga_tot = md("FGA<sup>*</sup>"),
             fg_tot = md("FG%<sup>*</sup>") )%>%
  data_color(
    columns = vars(fg3m, fg3a),
    colors = scales::col_numeric(
      palette = c( "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    ) )%>% 
  data_color(
    columns = vars(fg3_pct),
    colors = scales::col_factor(
      palette = c( "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    ) )%>% 
  data_color(
    columns = vars(fgm_tot, fga_tot),
    colors = scales::col_numeric(
      palette = c("#FFD700","#FFA500", "#FF8C00","#FF4500"),
      domain = NULL
    ) )%>% 
  data_color(
    columns = vars(fg_tot),
    colors = scales::col_factor(
      palette = c("#FFD700","#FFA500", "#FF8C00","#FF4500"),
      domain = NULL
      ) )%>% 
  cols_align(
    align = "left",
    columns = 2
  )  %>%
  cols_align(
    align = "center",
    columns = 3
  )  %>%
  
   cols_align(
     align = "left",
     columns = 4
   )  %>%
   cols_align(
     align = "center",
     columns = 5:10
   )  %>%
  text_transform(
    locations = cells_body(vars(head, logos)),
    fn = function(x) {
      web_image(url = x) 
    }
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(2)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  cols_width(2~ px(60)) %>%
  opt_row_striping() %>%

  tab_style(
    style = cell_borders(#esto añade una linea
      sides = "left",
      color = "black",
      weight = px(2)
    ),
    locations = 
      list(
        cells_body(
          columns = 8))

  ) %>% 
  tab_options(
    table.background.color = "#f4f4f4",
    column_labels.font.size = 11.5,
    column_labels.font.weight = "bold",
    row_group.font.weight = NULL,
    row_group.background.color = "#f4f4f4",
    table.font.size = 9,
    heading.title.font.size = 18,
    heading.subtitle.font.size = 10.5,
    table.font.names = "Chivo", 
    table_body.border.bottom.width = px(2),
    table_body.border.bottom.color = "black",
    data_row.padding = px(2)) %>% 
  tab_source_note(
    source_note = md("<sup>*</sup><i>Promedio de FGM, FGA, FG%, en el area restringida, en la pintura,<br>
                      media distancia, esquina izquierda, esquina derecha y detras de la linea de tres<i><br> 
                      **Datos por**: <i>https://www.nba.com/stats/players/opponent-shooting/<i><br> 
                      **Grafica**: <i>Ivo Villanueva<i>"))
df_stats_Gt %>% gtsave("df_stats_Gt.html")

