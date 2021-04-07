library(tidyverse)
library(ggtext)
library(scales)
library(ggchicklet)
library(paletteer)
library(scales)
library(prismatic)
library(ggimage)

theme_ivo <- function () { 
  theme_minimal(base_size=12, base_family="Chivo") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = '#f4f4f4', color = "#f4f4f4"),
      plot.caption = element_markdown(size = 6.5, hjust = .5)
    )
}


# Load the datos ----------------------------------------------------------



df_statsBar <- read.csv("df_statsP.csv")


# Selección de datos ---------------------------------------


df_statsBar <- df_statsBar %>% group_by(player_name) %>% filter( total_z >= 2 ) %>% 
  mutate(
    fgmTot = round(sum(fgm_ra, fgm_non_ra, fgm_mr, fgm_lc, fgm_rc, fgm_ab)/6,2),
    fgaTot = round(sum(fga_ra, fgm_non_ra, fga_mr, fga_lc, fga_rc, fga_ab)/6,2),
    fgTot = round(sum(fg_pct_ra, fg_pct_non_ra, fg_pct_mr, fg_pct_lc, fg_pct_rc, fg_pct_ab)/6,2),
    fgtz = round(fgmTot + fgaTot + fgTot/3)) %>% 
  arrange(fgmTot) %>% 

 
  select(
    player_name, fg_pct_ra, fg_pct_non_ra, fg_pct_mr, fg_pct_lc, fg_pct_rc, fg_pct_ab, fg_pct_bc) %>% 

    mutate(
      fg_pct_ra1 = fg_pct_ra/sum(fg_pct_ra, fg_pct_non_ra, fg_pct_mr, fg_pct_lc, fg_pct_rc, fg_pct_ab,fg_pct_bc),
      fg_pct_non_ra1 = fg_pct_non_ra/sum(fg_pct_ra, fg_pct_non_ra, fg_pct_mr, fg_pct_lc, fg_pct_rc, fg_pct_ab,fg_pct_bc),
      fg_pct_mr1 = fg_pct_mr/sum(fg_pct_ra, fg_pct_non_ra, fg_pct_mr, fg_pct_lc, fg_pct_rc, fg_pct_ab, fg_pct_bc),
      fg_pct_lc1 = fg_pct_lc/sum(fg_pct_ra, fg_pct_non_ra, fg_pct_mr, fg_pct_lc, fg_pct_rc, fg_pct_ab, fg_pct_bc),
      fg_pct_rc1 = fg_pct_rc/sum(fg_pct_ra, fg_pct_non_ra, fg_pct_mr, fg_pct_lc, fg_pct_rc, fg_pct_ab, fg_pct_bc),
      fg_pct_ab1 = fg_pct_ab/sum(fg_pct_ra, fg_pct_non_ra, fg_pct_mr, fg_pct_lc, fg_pct_rc, fg_pct_ab, fg_pct_bc),
      fg_pct_bc1 = fg_pct_bc/sum(fg_pct_ra, fg_pct_non_ra, fg_pct_mr, fg_pct_lc, fg_pct_rc, fg_pct_ab, fg_pct_bc)) %>% 
  ungroup() %>% 
          # mutate(fg_pct_ra1 = percent(fg_pct_ra1, .1),
          #      fg_pct_non_ra1 = percent(fg_pct_non_ra1, .1),
          #      fg_pct_mr1 = percent(fg_pct_mr1,.1),
          #      fg_pct_lc1 = percent(fg_pct_lc1,.1),
          #      fg_pct_rc1 = percent(fg_pct_rc1,.1),
          #      fg_pct_ab1 = percent(fg_pct_ab1,.1),
          #      fg_pct_bc1 = percent(fg_pct_bc1,.1)
          #      ) %>%
  slice(1:32) 
    

# Convertir en pivot longer -----------------------------------------------

 

df_statsBar <- df_statsBar %>% select(player_name, "Restricted Area" = fg_pct_ra1, "Non R. Area" =fg_pct_non_ra1, "Mid Range" = fg_pct_mr1, "Left Corner 3"= fg_pct_lc1,
                                      "Right Corner 3" = fg_pct_rc1, "Above the Break 3" = fg_pct_ab1, Backcourt = fg_pct_bc1) %>% 
  pivot_longer(c("R. Area", "Non R. Area", "Mid Range", "Left Corner 3", "Right Corner 3", "Above the Break 3", Backcourt), names_to = "pct_oposite", values_to = "pct")
  
df_statsChart <- df_statsBar %>% ggplot(
  aes(player_name, pct)) +
    geom_chicklet(aes(fill = fct_inorder(pct_oposite), color = after_scale(clr_darken(fill, 0.5))), alpha = .90) +
  scale_y_continuous(position = "left", labels = c("0%", "25%", "50%", "75%", "100%"), limits = c(0, 1.001)) +
  coord_flip() +
  theme_ivo() +
  scale_fill_fish(discrete = TRUE, option = "Trimma_lantana")+ 
  guides(fill=guide_legend(
    keywidth= .5,
    keyheight= .2,
    default.unit="inch", 
    title.position = 'top',
    label.position = 'bottom', 
    nrow = 1, 
   hjust = 0.5) 
  ) +
  theme(legend.position = 'top', 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.text = element_text(size = 7, vjust = 4),
        legend.title = element_text(size = 8, hjust = .5, vjust = -2),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,-15,0),
        plot.title = element_text(face = 'bold', size = 14.5, hjust = 0.5), 
        plot.subtitle = element_text(size = 8, hjust = 0.5),
        plot.margin = unit(c(.5, 1.5, 1, .5), "lines"), 
        axis.text.y = element_text(margin=margin(0,-27,0,0), size = 7)) + 
  labs(title = "Proporción del porcentaje recibido", 
       subtitle = paste0("Entre el Top 32 de triplistas con buena defensa  (2020-21) | Updated ", format(Sys.Date(), "%B %d, %Y")),
       caption ="<br>**Datos**: *nba.com/stats*  **Gráfico**: *Ivo Villanueva*",
       fill = "Zonas de tiro recibidas")

ggsave(
  "df_statsChart.png", df_statsChart
  ,
 
  height = 10, width = 10, dpi = "retina"
)
