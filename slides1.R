## ----include = FALSE--------------------------------------------------------
#| label: libraries-for-participants
library(tidyverse)
library(colorspace)
library(patchwork)
library(broom)
library(palmerpenguins)
library(GGally)
library(mulgar)
library(conflicted)
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


## ----echo=FALSE-------------------------------------------------------------
plan <- tribble(~time, ~topic,
                "15", "Introduction: What is high-dimensional data, why visualise and overview of methods",
                "20", "Basics of linear projections, and recognising high-d structure", 
                "25", "Effectively reducing your data dimension, in association with non-linear dimension reduction")
knitr::kable(plan)


## ---------------------------------------------------------------------------
#| label: load-penguins
#| fig-width: 6
#| fig-height: 6
#| echo: false
library(palmerpenguins)
p_tidy <- penguins |>
  select(species, bill_length_mm:body_mass_g) |>
  rename(bl=bill_length_mm,
         bd=bill_depth_mm,
         fl=flipper_length_mm,
         bm=body_mass_g) 
ggpairs(p_tidy, columns=2:5) +
  theme(axis.text = element_blank())


## ---------------------------------------------------------------------------
#| code-fold: true
#| echo: false
#| label: hiding
set.seed(946)
d <- tibble(x1=runif(200, -1, 1), 
            x2=runif(200, -1, 1), 
            x3=runif(200, -1, 1))
d <- d |>
  mutate(x4 = x3 + runif(200, -0.1, 0.1))
d <- bind_rows(d, c(x1=0, x2=0, x3=-0.5, x4=0.5))

d_r <- d |>
  mutate(x1 = cos(pi/6)*x1 + sin(pi/6)*x3,
         x3 = -sin(pi/6)*x1 + cos(pi/6)*x3,
         x2 = cos(pi/6)*x2 + sin(pi/6)*x4,
         x4 = -sin(pi/6)*x2 + cos(pi/6)*x4)



## ---------------------------------------------------------------------------
#| label: visible
#| fig-width: 4
#| fig-height: 4
#| out-width: 80%
#| echo: false
ggpairs(d) +
  theme(axis.text = element_blank())


## ---------------------------------------------------------------------------
#| label: invisible
#| fig-width: 4
#| fig-height: 4
#| out-width: 80%
#| echo: false
ggpairs(d_r) +
  theme(axis.text = element_blank())


## ---------------------------------------------------------------------------
#| eval: false
#| echo: false
# # Code to make the plots
# ggscatmat(d)
# animate_xy(d)
# render_gif(d,
#            grand_tour(),
#            display_xy(
#              axes="bottomleft", cex=2.5),
#            gif_file = "gifs/anomaly1.gif",
#            start = basis_random(4, 2),
#            apf = 1/60,
#            frames = 1500,
#            width = 500,
#            height = 400)
# ggscatmat(d_r)
# animate_xy(d_r)
# render_gif(d_r,
#            grand_tour(),
#            display_xy(
#              axes="bottomleft", cex=2.5),
#            gif_file = "gifs/anomaly2.gif",
#            start = basis_random(4, 2),
#            apf = 1/60,
#            frames = 1500,
#            width = 500,
#            height = 400)
# 
# dsq <- tibble(x1=runif(200, -1, 1),
#             x2=runif(200, -1, 1),
#             x3=runif(200, -1, 1))
# dsq <- dsq |>
#   mutate(x4 = x3^2 + runif(200, -0.1, 0.1))
# dsq <- bind_rows(dsq, c(x1=0, x2=0, x3=0, x4=1.1))
# dsq <- bind_rows(dsq, c(x1=0, x2=0, x3=0.1, x4=1.05))
# dsq <- bind_rows(dsq, c(x1=0, x2=0, x3=-0.1, x4=1.0))
# ggscatmat(dsq)
# animate_xy(dsq, axes="bottomleft")
# dsq_r <- dsq |>
#   mutate(x1 = cos(pi/6)*x1 + sin(pi/6)*x3,
#          x3 = -sin(pi/6)*x1 + cos(pi/6)*x3,
#          x2 = cos(pi/6)*x2 + sin(pi/6)*x4,
#          x4 = -sin(pi/6)*x2 + cos(pi/6)*x4)
# ggscatmat(dsq_r)
# animate_xy(dsq_r, axes="bottomleft")


## ---------------------------------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 70%
#| fig-alt: "Scatterplot showing the 2D data having two clusters."
data("simple_clusters")

ggplot(simple_clusters, aes(x=x1, y=x2)) +
  geom_point(size=2, alpha=0.8, colour="#EC5C00") 


## ---------------------------------------------------------------------------
#| echo: false
#| eval: false
# library(tourr)
# library(geozoo)
# set.seed(1351)
# d <- torus(3, n=4304)$points
# d <- apply(d, 2, function(x) (x-mean(x))/sd(x))
# colnames(d) <- paste0("x", 1:3)
# d <- data.frame(d)
# animate_xy(d, axes="bottomleft")
# animate_slice(d, axes="bottomleft")
# set.seed(606)
# path_t2 <- save_history(d, little_tour(), 4)
# render_gif(d,
#            planned_tour(path_t2),
#            display_xy(col="#EC5C00",
#              half_range=3,
#              axes="bottomleft"),
#            gif_file = "gifs/torus.gif",
#            apf = 1/75,
#            frames = 1000,
#            width = 400,
#            height = 300)
# render_gif(d,
#            planned_tour(path_t2),
#            display_slice(col="#EC5C00",
#              half_range=3,
#              axes="bottomleft"),
#            gif_file = "gifs/torus_slice.gif",
#            apf = 1/75,
#            frames = 1000,
#            width = 400,
#            height = 300)


## ---------------------------------------------------------------------------
#| echo: false
penguins <- penguins |>
  na.omit() # 11 observations out of 344 removed
# use only vars of interest, and standardise
# them for easier interpretation
penguins_sub <- penguins |> 
  select(bill_length_mm,
         bill_depth_mm,
         flipper_length_mm,
         body_mass_g,
         species, 
         sex) |> 
  mutate(across(where(is.numeric),  ~ scale(.)[,1])) |>
  rename(bl = bill_length_mm,
         bd = bill_depth_mm,
         fl = flipper_length_mm,
         bm = body_mass_g)


## ---------------------------------------------------------------------------
#| eval: false
#| echo: false
# set.seed(645)
# render_gif(penguins_sub[,1:4],
#            grand_tour(),
#            display_xy(col="#EC5C00",
#              half_range=3.8,
#              axes="bottomleft", cex=2.5),
#            gif_file = "gifs/penguins1.gif",
#            apf = 1/60,
#            frames = 1500,
#            width = 500,
#            height = 400)


## ---------------------------------------------------------------------------
#| label: invisible
#| fig-width: 4
#| fig-height: 4
#| out-width: 70%
#| echo: false
ggpairs(d_r) +
  theme(axis.text = element_blank())


## ----eval=FALSE-------------------------------------------------------------
# library(tourr)
# animate_xy(flea[, 1:6], rescale=TRUE)


## ---------------------------------------------------------------------------
#| echo: false
flea |> slice_head(n=3)


## ----eval=FALSE-------------------------------------------------------------
# animate_xy(flea[, 1:6],
#            col = flea$species,
#            rescale=TRUE)


## ----eval=FALSE-------------------------------------------------------------
# animate_xy(flea[, 1:6],
#            tour_path = guided_tour(holes()),
#            col = flea$species,
#            rescale = TRUE,
#            sphere = TRUE)


## ----eval=FALSE-------------------------------------------------------------
# set.seed(915)
# animate_xy(flea[, 1:6],
#            radial_tour(basis_random(6, 2),
#                        mvar = 6),
#            rescale = TRUE,
#            col = flea$species)


## ---------------------------------------------------------------------------
#| eval: false
# set.seed(645)
# render_gif(penguins_sub[,1:4],
#            grand_tour(),
#            display_xy(col="#EC5C00",
#              half_range=3.8,
#              axes="bottomleft", cex=2.5),
#            gif_file = "gifs/penguins1.gif",
#            apf = 1/60,
#            frames = 1500,
#            width = 500,
#            height = 400)


## ----eval=FALSE-------------------------------------------------------------
# library(tourr)
# library(mulgar)
# animate_xy(c1)


## ----eval=FALSE, echo=FALSE-------------------------------------------------
# load("data/auswt20.rda")
# animate_xy(auswt20[,6:16], rescale=TRUE)


## ---------------------------------------------------------------------------
#| echo: false
set.seed(6045)
x1 <- runif(123)
x2 <- runif(123)
x3 <- x1 + rnorm(123, sd=0.1)
x4 <- rnorm(123, sd=0.2)
df <- tibble(x1 = (x1-mean(x1))/sd(x1), 
             x2 = (x2-mean(x2))/sd(x2),
             x3 = (x3-mean(x3))/sd(x3),
             x4, 
             x4scaled = (x4-mean(x4))/sd(x4))


## ---------------------------------------------------------------------------
#| echo: false
#| warning: false
#| message: false
dp1 <- ggplot(df) + 
  geom_point(aes(x=x1, y=x2)) +
  xlim(-2.5, 2.5) + ylim(-2.5, 2.5) +
  annotate("segment", x=0, xend=2, y=0, yend=0) +
  annotate("segment", x=0, xend=0, y=0, yend=2) +
  annotate("text", x=2.3, y=0, label="x1") +
  annotate("text", x=0, y=2.3, label="x2") +
  ggtitle("(a) Fully 2D") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
dp2 <- ggplot(df) + 
  geom_point(aes(x=x1, y=x3)) +
  xlim(-2.5, 2.5) + ylim(-2.5, 2.5) +
  annotate("segment", x=0, xend=2, y=0, yend=0) +
  annotate("segment", x=0, xend=0, y=0, yend=2) +
  annotate("text", x=2.3, y=0, label="x1") +
  annotate("text", x=0, y=2.3, label="x3") +
  ggtitle("(b) Reduced dimension") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
dp3 <- ggplot(df) + 
  geom_point(aes(x=x1, y=x4)) +
  xlim(-2.5, 2.5) + ylim(-3.5, 3.5) +
  annotate("segment", x=0, xend=2, y=0, yend=0) +
  annotate("segment", x=0, xend=0, y=0, yend=3) +
  annotate("text", x=2.3, y=0, label="x1") +
  annotate("text", x=0, y=3.3, label="x4") +
  ggtitle("(c) Reduced variance") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
dp4 <- ggplot(df) + 
  geom_point(aes(x=x1, y=x4scaled)) +
  xlim(-2.5, 2.5) + ylim(-3.5, 3.5) +
  annotate("segment", x=0, xend=2, y=0, yend=0) +
  annotate("segment", x=0, xend=0, y=0, yend=3) +
  annotate("text", x=2.3, y=0, label="x1") +
  annotate("text", x=0, y=3.3, label="x4") +
  ggtitle("(d) Rescaled") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


## ---------------------------------------------------------------------------
#| echo: false
#| fig-width: 9
#| fig-height: 3
#| out-width: 100%
dp1 + dp2 + dp3 + dp4 + plot_layout(ncol=4)


## ----echo=FALSE-------------------------------------------------------------
track <- read_csv("https://raw.githubusercontent.com/numbats/iml/master/data/womens_track.csv")


## ---------------------------------------------------------------------------
#| echo: false
#| out.width: "100%"
#| fig.width: 8
#| fig.height: 8
ggscatmat(track[,1:7]) +
  theme(axis.text = element_blank())


## ----eval=FALSE-------------------------------------------------------------
#| echo: false
# render_gif(track[,1:7],
#            grand_tour(),
#            display_xy(col="#EC5C00",
#              cex=2),
#            rescale=TRUE,
#            gif_file = "gifs/track.gif",
#            apf = 1/30,
#            frames = 1500,
#            width = 400,
#            height = 400)


## ----echo=FALSE-------------------------------------------------------------
options(digits=2)
track_pca <- prcomp(track[,1:7], center=TRUE, scale=TRUE)


## ----echo=FALSE-------------------------------------------------------------
track_pca$sdev^2


## ----echo=FALSE-------------------------------------------------------------
track_pca$rotation[,1:4]


## ----echo=FALSE-------------------------------------------------------------
ggscree(track_pca, q=7)


## ---------------------------------------------------------------------------
#| echo: false
#| out-width: 100%
#| fig-width: 5
#| fig-height: 5
library(ggfortify)
autoplot(track_pca, loadings=TRUE, loadings.label=TRUE)


## ----echo=FALSE, eval=FALSE-------------------------------------------------
# track_std <- track |>
#   mutate_if(is.numeric, function(x) (x-
#       mean(x, na.rm=TRUE))/
#       sd(x, na.rm=TRUE))
# track_std_pca <- prcomp(track_std[,1:7],
#                scale = FALSE,
#                retx=TRUE)


## ----eval=FALSE-------------------------------------------------------------
# track_model <- mulgar::pca_model(track_std_pca, d=2, s=2)
# track_all <- rbind(track_model$points, track_std[,1:7])
# animate_xy(track_all, edges=track_model$edges,
#            edges.col="#E7950F",
#            edges.width=3,
#            axes="off")


## ----echo=FALSE, eval=FALSE-------------------------------------------------
# render_gif(track_all,
#            grand_tour(),
#            display_xy(
#                       edges=track_model$edges,
#                       edges.col="#E7950F",
#                       edges.width=3,
#                       axes="off",
#                       half_range = 5),
#            gif_file="gifs/track_model.gif",
#            frames=500,
#            width=400,
#            height=400,
#            loop=FALSE)


## ---------------------------------------------------------------------------
#| label: penguins-umap
#| message: false
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 70%
library(uwot)
p_tidy_std <- p_tidy |> 
  na.omit() |>
  mutate_if(is.numeric, function(x) (x-mean(x))/sd(x))

set.seed(253)
p_tidy_umap <- umap(p_tidy_std[,2:5], init = "spca")
p_tidy_umap_df <- p_tidy_umap |>
  as_tibble() |>
  rename(UMAP1 = V1, UMAP2 = V2) 
ggplot(p_tidy_umap_df, aes(x = UMAP1, 
                           y = UMAP2)) +
  geom_point(colour = "#EC5C00") 


## ----eval=FALSE-------------------------------------------------------------
# library(uwot)
# set.seed(253)
# p_tidy_umap <- umap(p_tidy_std[,2:5], init = "spca")


## ---------------------------------------------------------------------------
#| label: pbmc
#| message: false
#| eval: true
#| echo: false
#| fig-width: 9
#| fig-height: 4
#| out-width: 70%
pbmc <- readRDS("data/pbmc_pca_50.rds")

# t-SNE
set.seed(1041)
p_tsne <- Rtsne::Rtsne(pbmc[,1:9])
p_tsne_df <- data.frame(tsneX = p_tsne$Y[, 1], tsneY = p_tsne$Y[, 2])
p1 <- ggplot(p_tsne_df, aes(x=tsneX, y=tsneY)) + geom_point()

# UMAP
set.seed(1045)
p_umap <- uwot::umap(pbmc[,1:9])
p_umap_df <- data.frame(umapX = p_umap[, 1], umapY = p_umap[, 2])
p2 <- ggplot(p_umap_df, aes(x=umapX, y=umapY)) + geom_point()

p1 + p2 + plot_layout(ncol=2)


## ----echo=TRUE, eval=FALSE--------------------------------------------------
# pbmc <- readRDS("data/pbmc_pca_50.rds")
# animate_xy(pbmc[,1:9])

