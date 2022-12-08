# Modelando

library(tidyverse)
library(spotifyr)

library(GGally)

theme_set(theme_bw())
pairs_plot <- full_df |>
      #filter(playlist_name %in% c("Peru", "Brazil")) |>
      select(track.duration_ms, playlist_name,
            loudness, #instrumentalness,
            danceability, energy, liveness,
            tempo,
            acousticness, speechiness,
            valence) |>
      GGally::ggpairs(mapping = aes(color = playlist_name),
                      columns = c(
                            "track.duration_ms",
                            "loudness", #"instrumentalness",
                            "danceability", "energy", "liveness",
                            "tempo",
                            "acousticness", "speechiness",
                            "valence"
                      ),
                      upper = list(continuous = wrap("cor", size = 2.5))
      )

ggsave(plot = pairs_plot, filename = "fig_pairs.png", width = 18*.75, height = 15*.75)



full_df |>
      #filter(playlist_name %in% c("Peru", "Brazil")) |>
      select(track.duration_ms, playlist_name,
             loudness, #instrumentalness,
             danceability, energy, liveness,
             tempo,
             acousticness, speechiness,
             valence) |>
      pivot_longer(-playlist_name) |>
      ggplot(aes(x = playlist_name, y = value)) +
      geom_boxplot() +
      ggplot2::facet_wrap(~name, ncol = 3, scales = "free_y") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      labs(x = NULL, y = NULL)

ggsave(filename = "aed_boxplot.png", width = 17*.75, height = 12*.75)


library(FactoMineR)
library(factoextra)


#la_matriz <- full_df |>
#      select(#track.duration_ms,
#             loudness, #instrumentalness,
#             danceability, energy, #liveness,
#             tempo,
#             acousticness, #speechiness,
#             valence)


la_matriz <- full_df |>
      select(track.duration_ms, playlist_name,
             loudness, #instrumentalness,
             danceability, energy, liveness,
             tempo,
             acousticness, speechiness,
             valence) |>
      group_by(playlist_name) |>
      summarise_if(is.numeric, mean)


pca <- PCA(X = la_matriz |> select(-playlist_name), scale.unit = TRUE, graph = FALSE)

summary(pca)

fviz_pca_biplot( title = NULL,
      pca,
      geom.ind = "transparent", repel = TRUE,
      ) +
      geom_point() +
      ggrepel::geom_text_repel(aes(label = la_matriz$playlist_name),
                       point.padding = 0.5)

ggplot2::ggsave(filename = "PCA_output.png", width = 12*.75, 8*.75, dpi=300)

fviz_screeplot(pca, title = "", y = "% de variância explicada", x = "CP")

ggsave(filename = "screeplot.png", width = 12*.75, 7*.75)


leob <- factanal(la_matriz |> select(track.duration_ms,
      loudness, #instrumentalness,
      danceability, energy, liveness,
      tempo,
      acousticness, speechiness,
      valence),
      factors = 3)

# Método das Componentes Principais (Fatores principais)
R <- la_matriz |> select(track.duration_ms,
                         loudness, #instrumentalness,
                         danceability, energy, liveness,
                         tempo,
                         acousticness, speechiness,
                         valence) |> scale() |> cor()

Z <- la_matriz |> select(track.duration_ms,
                         loudness, #instrumentalness,
                         danceability, energy, liveness,
                         tempo,
                         acousticness, speechiness,
                         valence) |> scale()
n <- nrow(R)
p <- ncol(R)

# --- Estimação via componentes principais --- #
# --- Critério: Autovalores maiores do que um --- #
sum(princomp(R, cor=TRUE)[[1]]>1)

# --- Cargas fatoriais --- #
L = cbind(sqrt(eigen(cov(R))[[1]][1]) * eigen(cov(R))[[2]][,1],
          sqrt(eigen(cov(R))[[1]][2]) * eigen(cov(R))[[2]][,2])

round(L, 2)


escores <- t(L) %*% solve(R) %*% t(Z)
escores <- t(escores)

cbind(la_matriz |>  select(track.duration_ms, playlist_name,
                           loudness, #instrumentalness,
                           danceability, energy, liveness,
                           tempo,
                           acousticness, speechiness,
                           valence,  playlist_name
                           ), escores ) |>
      arrange(desc(1)) |> View("!")

