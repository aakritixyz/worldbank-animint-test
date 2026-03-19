rm(list = ls())
library(animint2)
library(data.table)

# Load and Clean Data (As per documentation 8.1)
data(WorldBank)
wb <- data.table(WorldBank)
wb$Region <- sub(" (all income levels)", "", wb$region, fixed=TRUE)

# Remove NAs and fix Kuwait population (Gulf War adjustment)
not.na <- wb[!(is.na(life.expectancy) | is.na(fertility.rate))]
not.na[is.na(population), population := 1700000]

# Helper Functions (As per documentation 8.1)
FACETS <- function(df, top, side){
  data.frame(df,
             top=factor(top, c("Fertility rate", "Years")),
             side=factor(side, c("Years", "Life expectancy")))
}

TS.LIFE <- function(df) FACETS(df, "Years", "Life expectancy")
SCATTER <- function(df) FACETS(df, "Fertility rate", "Life expectancy")
TS.FERT <- function(df) FACETS(df, "Fertility rate", "Years")
YEAR.TEXT <- function(df) FACETS(df, "Years", "Years") # For the empty panel

years <- unique(not.na[, .(year)])

# Constructing the Visualization
viz_plot <- ggplot() +
  theme_bw() +
  theme(panel.margin = grid::unit(0, "lines")) +
  facet_grid(side ~ top, scales = "free") +
  xlab("") + ylab("")

# --- PANEL: BOTTOM-LEFT (Scatter Plot) ---
viz_plot <- viz_plot +
  geom_point(aes(x = fertility.rate, y = life.expectancy,
                 color = Region, size = population, key = country),
             clickSelects = "country", showSelected = "year",
             data = SCATTER(not.na)) +
  scale_size_animint(pixel.range = c(2, 20), breaks = 10^(5:9))

# --- PANEL: BOTTOM-RIGHT (Life Expectancy Time Series) ---
viz_plot <- viz_plot +
  geom_tallrect(aes(xmin = year - 0.5, xmax = year + 0.5),
                clickSelects = "year",
                data = TS.LIFE(years), alpha = 0.5) +
  geom_line(aes(x = year, y = life.expectancy, group = country, color = Region),
            clickSelects = "country",
            data = TS.LIFE(not.na), size = 1, alpha = 0.6)

# --- PANEL: TOP-LEFT (Fertility Time Series) ---
viz_plot <- viz_plot +
  geom_widerect(aes(ymin = year - 0.5, ymax = year + 0.5),
                clickSelects = "year",
                data = TS.FERT(years), alpha = 0.5) +
  geom_path(aes(x = fertility.rate, y = year, group = country, color = Region),
            clickSelects = "country",
            data = TS.FERT(not.na), size = 1, alpha = 0.6)

# put a big Year label in the Years/Years facet which is currently empty.
mid_year <- median(years$year)
year_label_df <- data.frame(year = years$year, x = mid_year, y = mid_year)

viz_plot <- viz_plot +
  geom_text(aes(x = x, y = y, label = year),
            showSelected = "year",
            size = 30, color = "gray70", alpha = 0.3,
            data = YEAR.TEXT(year_label_df))

# Rendering the Animation
viz_final <- animint(
  title = "World Bank Interactive (Final Fix)",
  scatter = viz_plot + theme_animint(width = 800, height = 800),
  duration = list(year = 1000), # Smooth transitions
  time = list(variable = "year", ms = 3000), # Auto-play
  first = list(year = 1975, country = c("India", "China", "United States")),
  selector.types = list(country = "multiple")
)

# Output
animint2dir(viz_final, out.dir = "WorldBank_Suhaani_Fix", open.browser = TRUE)