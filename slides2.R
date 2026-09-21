## ----include = FALSE--------------------------------------------------------
#| label: libraries-for-participants
library(tidyverse)
library(colorspace)
library(patchwork)
library(palmerpenguins)
library(GGally)
library(mulgar)
library(conflicted)
library(mclust)
library(MASS)
library(classifly)
library(randomForest)
library(geozoo)
library(tourr)
library(countdown)


## ----include = FALSE--------------------------------------------------------
#| label: code-for-nice-slides
options(width = 60, digits=2)
knitr::opts_chunk$set(
  fig.width = 6,
  fig.height = 6,
  fig.align = "center",
  dev.args = list(bg = 'transparent'),
  out.width = "100%",
  fig.retina = 3,
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  cache = FALSE
)
theme_set(theme_bw(base_size = 14) +
   theme(
     aspect.ratio = 1,
     plot.background = element_rect(fill = 'transparent', colour = NA),
     plot.title.position = "plot",
     plot.title = element_text(size = 14),
     panel.background = element_rect(fill = 'transparent', colour = NA),
     legend.background = element_rect(fill = 'transparent', colour = NA),
     legend.key = element_rect(fill = 'transparent', colour = NA)
   )
)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::slice)
conflicts_prefer(palmerpenguins::penguins)
conflicts_prefer(tourr::flea)

p_tidy <- penguins |>
  select(species, bill_length_mm:body_mass_g) |>
  rename(bl=bill_length_mm,
         bd=bill_depth_mm,
         fl=flipper_length_mm,
         bm=body_mass_g) |>
  na.omit()
p_tidy_std <- p_tidy |>
  mutate_if(is.numeric, function(x) (x-mean(x))/sd(x))



## ---------------------------------------------------------------------------
plan <- tribble(~time, ~topic,
                "30", "Understanding clusters in data using visualisation",
                "30", "Building better classification models with visual input")
knitr::kable(plan)


## ---------------------------------------------------------------------------
#| echo: true
#| eval: false
# library(detourr)
# set.seed(645)
# detour(p_tidy_std[,2:5],
#        tour_aes(projection = bl:bm)) |>
#        tour_path(grand_tour(2), fps = 60,
#                  max_bases=40) |>
#        show_scatter(alpha = 0.7,
#                     axes = FALSE)


## ---------------------------------------------------------------------------
#| message: false
#| warning: false
#| fig-width: 7
#| fig-height: 4
#| out-width: 100%
# Nuisance observations
set.seed(20190514)
x <- (runif(20)-0.5)*4
y <- x
d1 <- data.frame(x1 = c(rnorm(50, -3), 
                            rnorm(50, 3), x),
                 x2 = c(rnorm(50, -3), 
                            rnorm(50, 3), y),
                 cl = factor(c(rep("A", 50), 
                             rep("B", 70))))
d1 <- d1 |> 
  mutate_if(is.numeric, function(x) (x-mean(x))/sd(x))

# Nuisance variables
set.seed(20190512)
d2 <- data.frame(x1=c(rnorm(50, -4), 
                            rnorm(50, 4)),
                 x2=c(rnorm(100)),
                 cl = factor(c(rep("A", 50), 
                             rep("B", 50))))
d2 <- d2 |> 
  mutate_if(is.numeric, function(x) (x-mean(x))/sd(x))

d1_hs <- hclust(dist(d1[,1:2]),
                method="single")
d1 <- d1 |>
  mutate(cls = factor(cutree(d1_hs, 2)))
pc_d1s <- ggplot(d1) +
  geom_point(aes(x=x1, y=x2, colour=cls), 
             size=2, alpha=0.8) +
  scale_colour_discrete_divergingx(palette = "Zissou 1",
                                   nmax=4, rev=TRUE) +
  ggtitle("Result 1") +
  theme(legend.position="none")

d1_hw <- hclust(dist(d1[,1:2]),
                method="ward.D2")
d1 <- d1 |>
  mutate(clw = factor(cutree(d1_hw, 2)))
pc_d1w <- ggplot(d1) +
  geom_point(aes(x=x1, y=x2, colour=clw), 
             size=2, alpha=0.8) +
  scale_colour_discrete_divergingx(palette = "Zissou 1",
                                   nmax=4, rev=TRUE) +
  ggtitle("Result 2") +
  theme(legend.position="none")

pc_d1s + pc_d1w +
  plot_layout(ncol=2)


## ---------------------------------------------------------------------------
#| message: false
#| warning: false
#| fig-width: 7
#| fig-height: 4
#| out-width: 100%
d2_hc <- hclust(dist(d2[,1:2]),
                method="complete")
d2 <- d2 |>
  mutate(clc = factor(cutree(d2_hc, 2)))
pc_d2c <- ggplot(d2) +
  geom_point(aes(x=x1, y=x2, colour=clc), 
             size=2, alpha=0.8) +
  scale_colour_discrete_divergingx(palette = "Zissou 1",
                                   nmax=4, rev=TRUE) +
  ggtitle("Result 1") + 
  theme(legend.position="none")

d2_hw <- hclust(dist(d2[,1:2]),
                method="ward.D2")
d2 <- d2 |>
  mutate(clw = factor(cutree(d2_hw, 2)))
pc_d2w <- ggplot(d2) +
  geom_point(aes(x=x1, y=x2, colour=clw), 
             size=2, alpha=0.8) +
  scale_colour_discrete_divergingx(palette = "Zissou 1",
                                   nmax=4, rev=TRUE) +
  ggtitle("Result 2") + 
  theme(legend.position="none")
  
pc_d2c + pc_d2w +
  plot_layout(ncol=2)


## ---------------------------------------------------------------------------
#| fig-width: 5
#| fig-height: 5
#| out-width: 100%
#| fig-align: "center"
p_BIC <- mclustBIC(p_tidy[,2:5])
ggmc <- ggmcbic(p_BIC, cl=2:9, top=7) + 
  scale_color_discrete_divergingx(palette = "Roma") 
ggmc


## ---------------------------------------------------------------------------
#| eval: FALSE
# p_mc <- Mclust(p_tidy[,2:5],
#                       G=4,
#                       modelNames = "VEE")
# p_mce <- mc_ellipse(ps_mc)
# p_cl <- p_tidy
# p_cl$cl <- factor(p_mc$classification)
# 
# p_mc_data <- p_cl |>
#   select(bl:bm, cl) |>
#   mutate(type = "data") |>
#   bind_rows(bind_cols(p_mce$ell,
#                       type=rep("ellipse",
#                                nrow(p_mce$ell)))) |>
#   mutate(type = factor(type))


## ---------------------------------------------------------------------------
#| eval: FALSE
# animate_xy(p_mc_data[,1:4],
#            col=p_mc_data$cl,
#            pch=c(4, 20 )[as.numeric(p_mc_data$type)],
#            axes="off")
# 
# load("data/penguins_tour_path.rda")
# render_gif(p_mc_data[,1:4],
#            planned_tour(pt1),
#            display_xy(col=p_mc_data$cl,
#                pch=c(4, 20)[
#                  as.numeric(p_mc_data$type)],
#                       axes="off",
#                half_range = 0.7),
#            gif_file="gifs/penguins_best_mc.gif",
#            frames=500,
#            loop=FALSE)
# 
# p_mc <- Mclust(p_tidy[,2:5],
#                       G=3,
#                       modelNames = "EEE")
# p_mce <- mc_ellipse(p_mc)
# p_cl <- p_tidy
# p_cl$cl <- factor(p_mc$classification)
# 
# p_mc_data <- p_cl |>
#   select(bl:bm, cl) |>
#   mutate(type = "data") |>
#   bind_rows(bind_cols(p_mce$ell,
#                       type=rep("ellipse",
#                                nrow(p_mce$ell)))) |>
#   mutate(type = factor(type))
# 
# animate_xy(p_mc_data[,1:4],
#            col=p_mc_data$cl,
#            pch=c(4, 20)[as.numeric(p_mc_data$type)],
#            axes="off")
# 
# # Save the animated gif
# load("data/penguins_tour_path.rda")
# render_gif(p_mc_data[,1:4],
#            planned_tour(pt1),
#            display_xy(col=p_mc_data$cl,
#                pch=c(4, 20)[
#                  as.numeric(p_mc_data$type)],
#                       axes="off",
#                half_range = 0.7),
#            gif_file="gifs/penguins_simpler_mc.gif",
#            frames=500,
#            loop=FALSE)


## ---------------------------------------------------------------------------
#| echo: true
p_mc <- Mclust(
  p_tidy[,2:5], 
  G=3, 
  modelNames = "EEE")
p_mc$parameters$mean
p_mc$parameters$variance$sigma[,,1]


## ---------------------------------------------------------------------------
#| echo: true
p_mce <- mc_ellipse(p_mc)


## ---------------------------------------------------------------------------
#| echo: false
#| fig-width: 6
#| fig-height: 3
s <- geozoo::sphere.hollow(p=2)$points |>
  as.data.frame()
s_p <- ggplot(s, aes(x=V1, y=V2)) + 
  geom_point(colour="#EC5C00") +
  theme(axis.title = element_blank(),
        axis.text = element_blank())
ell <- mulgar::gen_vc_ellipse(matrix(c(1, 0.7, 0.7, 1), ncol=2, byrow=T)) |> as.data.frame()
e_p <- ggplot(ell, aes(x=V1, y=V2)) + 
  geom_point(colour="#EC5C00") +
  theme(axis.title = element_blank(),
        axis.text = element_blank())
s_p + e_p + plot_layout(ncol=2)


## ---------------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| out-width: 90%
p_lda <- lda(species ~ ., p_tidy[,1:5], prior = c(1/3, 1/3, 1/3))
p_lda_pred <- predict(p_lda, p_tidy)
p_lda_pred_x1 <- data.frame(p_lda_pred$x)
p_lda_pred_x1$species <- p_tidy$species
ggplot(p_lda_pred_x1, 
       aes(x=LD1, y=LD2, 
           colour=species)) +
  geom_point(alpha=0.5) +  
  scale_color_discrete_divergingx("Zissou 1") +
#  scale_shape_manual(values=c(1, 2, 3)) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) 


## ---------------------------------------------------------------------------
data(bushfires)

bushfires_sub <- bushfires[,c(5, 8:45, 48:55, 57:60)] |>
  mutate(cause = factor(cause))

bushfires_rf <- randomForest(cause~.,
                             data=bushfires_sub,
                             importance=TRUE)
bushfires_rf


## ---------------------------------------------------------------------------
#| eval: false
# # Create votes matrix data
# bushfires_rf_votes <- bushfires_rf$votes |>
#   as_tibble() |>
#   mutate(cause = bushfires_sub$cause)
# 
# # Project 4D into 3D
# proj <- t(geozoo::f_helmert(4)[-1,])
# b_rf_v_p <- as.matrix(bushfires_rf_votes[,1:4]) %*% proj
# colnames(b_rf_v_p) <- c("x1", "x2", "x3")
# b_rf_v_p <- b_rf_v_p |>
#   as.data.frame() |>
#   mutate(cause = bushfires_sub$cause)
# 
# # Add simplex
# simp <- simplex(p=3)
# sp <- data.frame(simp$points)
# colnames(sp) <- c("x1", "x2", "x3")
# sp$cause = ""
# b_rf_v_p_s <- bind_rows(sp, b_rf_v_p) |>
#   mutate(cause = factor(cause))
# labels <- c("accident" , "arson",
#                 "burning_off", "lightning",
#                 rep("", nrow(b_rf_v_p)))
# 
# animate_xy(b_rf_v_p_s[,1:3], col = b_rf_v_p_s$cause,
#            axes = "off", half_range = 1.3,
#            edges = as.matrix(simp$edges),
#            obs_labels = labels)


## ---------------------------------------------------------------------------
#| eval: false
# library(crosstalk)
# library(plotly)
# library(viridis)
# p_cl <- p_tidy_std |>
#   mutate(pspecies = p_lda_pred$class) |>
#   dplyr::select(bl:bm, species, pspecies) |>
#   mutate(sp_jit = jitter(as.numeric(species), 0.5),
#          psp_jit = jitter(as.numeric(pspecies), 0.5))
# p_cl_shared <- SharedData$new(p_cl)
# 
# detour_plot <- detour(p_cl_shared, tour_aes(
#   projection = bl:bm,
#   colour = species)) |>
#     tour_path(grand_tour(2),
#                     max_bases=50, fps = 60) |>
#        show_scatter(alpha = 0.9, axes = FALSE,
#                     width = "100%", height = "450px")
# 
# conf_mat <- plot_ly(p_cl_shared,
#                     x = ~psp_jit,
#                     y = ~sp_jit,
#                     color = ~species,
#                     colors = viridis_pal(option = "D")(3),
#                     height = 450) |>
#   highlight(on = "plotly_selected",
#               off = "plotly_doubleclick") |>
#     add_trace(type = "scatter",
#               mode = "markers")
# 
# bscols(
#      detour_plot, conf_mat,
#      widths = c(5, 6)
#  )


## ---------------------------------------------------------------------------
#| fig-width: 5
#| fig-height: 4
#| out-width: 80%
w <- matrix(runif(48*40), ncol=40) |>
  as.data.frame() |>
  mutate(cl = factor(rep(c("A", "B", "C", "D"), rep(12, 4))))
w_lda <- lda(cl~., data=w)
w_pred <- predict(w_lda, w, dimen=2)$x
w_p <- w |>
  mutate(LD1 = w_pred[,1],
         LD2 = w_pred[,2])
ggplot(w_p, aes(x=LD1, y=LD2, colour=cl)) + 
  geom_point() +
  scale_colour_discrete_divergingx(palette = "Zissou 1",
                                   nmax=4, rev=TRUE) 


## ---------------------------------------------------------------------------
#| eval: false
# animate_xy(w[,1:40], guided_tour(lda_pp(w$cl)),
#            col=w$cl,
#            sphere=TRUE,
#            axes="off",
#            half_range=4)


## ---------------------------------------------------------------------------
set.seed(951)
ws <- w |>
  mutate(cl = sample(cl))


## ---------------------------------------------------------------------------
#| fig-width: 5
#| fig-height: 4
#| out-width: 80%
ws_lda <- lda(cl~., data=ws)
ws_pred <- predict(ws_lda, ws, dimen=2)$x
ws_p <- ws |>
  mutate(LD1 = ws_pred[,1],
         LD2 = ws_pred[,2])
ggplot(ws_p, aes(x=LD1, y=LD2, colour=cl)) + 
  geom_point() +
  scale_colour_discrete_divergingx(palette = "Zissou 1",
                                   nmax=4, rev=TRUE) 

