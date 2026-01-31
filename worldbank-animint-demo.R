# ==== WORLD BANK INTERACTIVE VISUALIZATION ====

# Install required packages if not already installed
packages <- c("animint2", "data.table", "ggplot2")
for(p in packages) if(!requireNamespace(p, quietly=TRUE)) install.packages(p)

library(animint2)
library(data.table)
library(ggplot2)

# Load World Bank data
data(WorldBank)
wb <- data.table(WorldBank)
wb$Region <- sub(" (all income levels)", "", wb$region, fixed = TRUE)

# Fill missing population with a placeholder
wb[is.na(population), population := 1700000]

# Helper functions for facets
FACETS <- function(df, top, side){
  data.frame(df,
             top=factor(top, c("Fertility rate", "Years")),
             side=factor(side, c("Years", "Life expectancy")))
}

TS.LIFE <- function(df) FACETS(df, "Years", "Life expectancy")
SCATTER <- function(df) FACETS(df, "Fertility rate", "Life expectancy")
TS.FERT <- function(df) FACETS(df, "Fertility rate", "Years")

# Unique years for selection
years <- unique(wb$year)

# Life expectancy time series
ts.life <- ggplot() +
  geom_tallrect(aes(xmin=year-0.5, xmax=year+0.5),
                clickSelects = "year",
                data = TS.LIFE(data.table(year = years)),
                alpha = 0.3) +
  geom_line(aes(x = year, y = life.expectancy, group = country, color = Region),
            clickSelects = "country",
            data = TS.LIFE(wb),
            size = 2, alpha = 0.6)

# Add faceting and theme
ts.facet <- ts.life +
  theme_bw() +
  theme(panel.margin = grid::unit(0, "lines")) +
  facet_grid(side ~ top, scales = "free") +
  xlab("") + ylab("")

# Scatter: fertility vs life expectancy
scatter.df <- SCATTER(wb)  # preserve interactive columns
scatter.df$country <- wb$country
scatter.df$year    <- wb$year

scatter.plot <- ts.facet +
  geom_point(aes(fertility.rate, life.expectancy,
                 color = Region,
                 size = population,
                 key = country),
             clickSelects = "country",
             showSelected = "year",
             data = scatter.df) +
  scale_size_animint(pixel.range = c(2, 20), breaks = 10^(5:9))

# Fertility time series
fert.df <- TS.FERT(wb)
fert.df$country <- wb$country
fert.df$year    <- wb$year

scatter.complete <- scatter.plot +
  geom_widerect(aes(ymin=year-0.5, ymax=year+0.5),
                clickSelects = "year",
                data = TS.FERT(data.table(year = years)),
                alpha = 0.3) +
  geom_path(aes(fertility.rate, year, group = country, color = Region),
            clickSelects = "country",
            data = fert.df,
            size = 2, alpha = 0.6)

viz <- animint(
  title = "World Bank Interactive Demo",
  scatter = scatter.complete + theme_animint(width = 900, height = 600),
  duration = list(year = 1000),
  time = list(variable = "year", ms = 3000),
  first = list(year = 1975, country = c("United States", "China", "India")),
  selector.types = list(country = "multiple")
)

animint2dir(viz, out.dir = "WorldBank_animint_demo", open.browser = TRUE)