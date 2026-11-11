#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Plots for the Ambilogue citations (how often the tools are cited), drawn
# from the OpenAlex data written by citations_over_time.py.
#
# The tool plots are drawn by plot_ambilogue.R.
#
# Input : citations_by_year_long.csv, citations_by_year_overall.csv, ../Ambilogue.csv (Publication_Status)
# Output: figures in --out (.png)
#
# Usage:
#   Rscript plot_citations.R
#   Rscript plot_citations.R --dir=path/to/csvs --out=path/to/figures --format=png,svg --csv=path/to/Ambilogue.csv
#
# Defaults: --dir = the folder containing this script, --out = <script folder>/plots,
#           --format = png.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
  library(svglite)
  library(dplyr)
  library(ggtext)
})

# ARGUMENTS --------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
}
script_dir <- local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
in_dir   <- normalizePath(get_arg("dir", script_dir), mustWork = TRUE)
out_dir  <- get_arg("out", file.path(script_dir, "plots"))
formats  <- strsplit(get_arg("format", "png"), ",")[[1]]
csv_path <- get_arg("csv", file.path(script_dir, "..", "Ambilogue.csv"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# DATA -------------------------------------------------------------------------

read_data <- function(name) {
  path <- file.path(in_dir, name)
  if (!file.exists(path)) stop("Missing ", path, " - run citations_over_time.py first.", call. = FALSE)
  data <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  data$Year_to_date <- as.logical(data$Year_to_date)
  data
}

# One row per tool and year
long <- read_data("citations_by_year_long.csv")

# Preprint or Published for each tool
ambilogue <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE,
                      fileEncoding = "UTF-8-BOM")

# One row per year, all tools together
overall <- read_data("citations_by_year_overall.csv")

# Written to citations_last_updated.txt (shown on plots.html), not drawn on the plots
RETRIEVED_DATE <- unique(long$Retrieved)[1]

all_years <- sort(unique(long$Year))
facet_years <- seq(min(all_years), max(all_years))

# The dashed lines start from the last full year
last_full_year <- if (any(long$Year_to_date)) max(long$Year[long$Year_to_date]) - 1 else Inf

# Tools in publication order (oldest first)
tools_df <- long %>%
  group_by(Tool, Publication_Date, Publication_Year, Preprint_Date) %>%
  summarise(Total = sum(Citations), .groups = "drop") %>%
  arrange(as.Date(Publication_Date))

# Year of the preprint (if there was one) and the year the tool first appeared
tools_df$Preprint_Year <- as.integer(format(as.Date(tools_df$Preprint_Date), "%Y"))
tools_df$First_Year <- pmin(tools_df$Preprint_Year, tools_df$Publication_Year, na.rm = TRUE)
tools_df$Still_preprint <- ambilogue$Publication_Status[match(tools_df$Tool, ambilogue$Tool)] == "Preprint"
tools_df$Still_preprint[is.na(tools_df$Still_preprint)] <- FALSE
# Preprint-only tools stay orange to the end of the plot
tools_df$Preprint_End <- ifelse(tools_df$Still_preprint, Inf, tools_df$Publication_Year - 0.5)
tools_df$Panel <- sprintf("%s\n(total=%s)", tools_df$Tool,
                            format(tools_df$Total, big.mark = ",", trim = TRUE))
tools_df$Panel <- factor(tools_df$Panel, levels = tools_df$Panel)
long$Panel <- tools_df$Panel[match(long$Tool, tools_df$Tool)]

n_tools <- nrow(tools_df)
message("Read ", n_tools, " tools from ", in_dir)

# PLOT STYLING -----------------------------------------------------------------

# Styling follows the paper figures (analysis/ambilogue_paper/ambilogue_paper.R): pastel fills with
# black outlines, a black border around the plot area and the standard sans serif font.

OUTLINE_WIDTH <- 1.2        # black outline on bars and pies (ggplot default is 0.5)
PANEL_BORDER_WIDTH <- 3     # black border around the plot area on the bar-type plots
PIE_SMALL_SLICE <- 0.10     # pie slices smaller than this share get their label outside the pie
LEGEND_WRAP <- 40           # long legend labels wrap after this many characters, keeping the legend inside the plot area
PAIR_FIG_WIDTH <- 18        # figure size (inches) of the timeline and overall citations plots, so they match
PAIR_FIG_HEIGHT <- 13
PAIR_PANEL_WIDTH <- 14      # size (inches) of the plot area inside each of those two figures
PAIR_PANEL_HEIGHT <- 7.5
PIE_SIZE <- 5.5             # width and height (inches) of each pie's plot area, so both pies match
FONT_FAMILY <- "Open Sans"  # the font used on the website (site.css); install it on your computer, or R falls back to the default sans serif
FONT_SCALE <- 1.5           # multiplies every font size below (1 = original website sizes)

# Text is measured on a Cairo device, the same one ggsave() uses for the PNGs, so legend widths
# are worked out with the real fonts (Open Sans) rather than the PDF device's built-in ones.
if (capabilities("cairo")) grDevices::png(tempfile(fileext = ".png"), type = "cairo")

# Bold text (axis titles, legend titles, the numbers on the plots) needs the bold face of the font.
# If only the regular face is installed, R silently draws everything in regular weight, so warn.
tryCatch(local({   # the check itself must never stop the script
  plain <- systemfonts::match_fonts(FONT_FAMILY, weight = "normal")$path
  bold  <- systemfonts::match_fonts(FONT_FAMILY, weight = "bold")$path
  if (identical(plain, bold))
    warning("No bold face of '", FONT_FAMILY, "' found, so bold text will be drawn in regular weight. ",
            "Install the static 'Open Sans Bold' (e.g. from the 'static' folder of the Google Fonts download, ",
            "not only the variable font), restart R and run the script again.", call. = FALSE)
}), error = function(e) NULL)

# Text drawn with geom_text (the numbers on the plots) uses the same font
update_geom_defaults("text", list(family = FONT_FAMILY))

# Fixes the plot area (panel) to width x height inches (height defaults to width). Used so plots
# come out the same size however wide their legends are. Returns a gtable (draw it with grid.draw).
set_panel_size <- function(p, width, height = width) {
  g <- ggplotGrob(p)
  panel <- g$layout[g$layout$name == "panel", ]
  g$widths[panel$l]  <- unit(width, "in")
  g$heights[panel$t] <- unit(height, "in")
  g
}

# Wrap text at `width` characters (used for legend titles and labels, which now sit on the right)
wrap_text <- function(x, width) {
  vapply(x, function(v) paste(strwrap(v, width), collapse = "\n"), character(1), USE.NAMES = FALSE)
}

# Width (points) of a legend title, measured with the plot font.
title_width <- function(x, size) {
  tg <- grid::textGrob(x, gp = grid::gpar(fontfamily = FONT_FAMILY, fontsize = size, fontface = "bold"))
  grid::convertWidth(grid::grobWidth(tg), "pt", valueOnly = TRUE)
}

# Pads `title` on the right with non-breaking spaces until it is as wide as `target`, so the
# entries of two stacked legends start at the same left edge. `size` is the title font size (pt).
pad_title <- function(title, target, size) {
  for (i in 1:60) {
    if (title_width(title, size) >= title_width(target, size)) break
    title <- paste0(title, "\u00a0")
  }
  title
}


# Full year (FALSE) and year-to-date (TRUE) bars
CURRENT_YEAR_COLOURS <- c("TRUE" = "#E4F0FC",
                          "FALSE" = "#C1DAFB")

UNIQUE_COLOUR <- "black"
SUM_COLOUR <- "#E57373"
TOOL_LINE_COLOUR <- "#222222"

UNIQUE_LABEL <- "Cumulative number of unique papers citing at least one tool"
SUM_LABEL <- "Cumulative total of tool citations (double-counts papers citing more than one tool)"

TOOL_LINE_LABEL <- "Full year"
BEFORE_PUBLICATION_LABEL <- "Before preprint or publication"
PREPRINT_LABEL <- "Preprint only"
YEAR_TO_DATE_LABEL <- "Year-to-date"

PREPRINT_COLOUR <- "#F4C7A1"

# Number of tools across in the plot of each tool
TOOL_COLUMNS <- 4

theme_ambilogue_bar <- function(base_size = 11) {
  theme_linedraw(base_size = base_size) +
  theme(text = element_text(family = FONT_FAMILY),
        plot.background = element_rect(fill = NA, colour = NA),   # transparent background
        legend.background = element_rect(fill = NA, colour = NA),
        legend.key = element_rect(fill = NA, colour = NA),
        legend.box.background = element_rect(fill = NA, colour = NA),
        plot.title = element_markdown(face = "bold", size = 18 * FONT_SCALE, family = FONT_FAMILY, hjust = 0.5),
        plot.subtitle = element_markdown(face = "italic", size = 12 * FONT_SCALE, family = FONT_FAMILY),
        legend.title = element_text(size = 15 * FONT_SCALE, family = FONT_FAMILY, face = "bold"),
        legend.text = element_text(size = 13 * FONT_SCALE, family = FONT_FAMILY),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 16 * FONT_SCALE),
        axis.title = element_text(size = 18 * FONT_SCALE, family = FONT_FAMILY, face = "bold"),
        axis.text = element_text(size = 16 * FONT_SCALE, family = FONT_FAMILY),
        panel.border          = element_rect(colour = "black", fill = NA, linewidth = PANEL_BORDER_WIDTH),   # solid black border around the plot area (raise linewidth for a bolder line)
        panel.background      = element_blank(),
        panel.grid.major.x    = element_blank(),
        panel.grid.minor      = element_blank(),
        axis.line             = element_blank(),   # the panel border already draws these edges
        plot.title.position = "panel",   # centre the title over the plot area, not the legend
        legend.justification = "left",   # legend sits at the left edge of the plot area
        legend.box.just = "left",        # stacked legends line up on the left
        legend.title.position = "left",  # titles beside their entries
        legend.position = "bottom")
}

# Large text so the plot stays readable when shown at page width
theme_ambilogue_facet <- function(base_size = 11) {
  theme_ambilogue_bar(base_size) +
  theme(axis.text = element_text(size = 18, family = FONT_FAMILY),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 16),
        axis.title = element_text(size = 21, family = FONT_FAMILY, face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, face = "bold", hjust = 0, family = FONT_FAMILY, colour = "black"),
        legend.title = element_text(size = 19, family = FONT_FAMILY, face = "bold"),
        legend.text = element_text(size = 19, family = FONT_FAMILY),
        legend.key.size = unit(1.3, "lines"),
        legend.box = "vertical",
        legend.justification = "center",   # legend in the middle of the figure (the legends stay left-aligned with each other)
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5),   # thinner border for the small panels
        panel.spacing = unit(1.4, "lines"))
}

# Only whole numbers on the y axis
whole_number_breaks <- function(limits) {
  breaks <- pretty(limits, n = 4)
  breaks[breaks == floor(breaks)]
}

format_number <- function(x) format(x, big.mark = ",")

save_plot <- function(p, name, width, height) {
  for (fmt in formats) {
    f <- file.path(out_dir, paste0(name, ".", fmt))
    ggsave(f, p, width = width, height = height, dpi = 200, bg = "white")
    message("Wrote ", f)
  }
}

# Cumulative line: solid up to the last full year, dashed from there into the
# year to date point (open circle). With a label the line gets a legend entry,
# with all_points every full year gets a dot too, and with one_legend the solid
# and dashed lines share a single legend (needs matching colour and linetype scales).
cumulative_layers <- function(data, column, colour, linewidth = 1, point_size = 2.5,
                              label = NULL, all_points = FALSE, one_legend = FALSE) {
  full_years <- data[!data$Year_to_date, ]
  if (is.null(label)) {
    solid_line <- geom_line(data = full_years, aes(y = .data[[column]]),
                            colour = colour, linewidth = linewidth)
  } else if (one_legend) {
    solid_line <- geom_line(data = full_years,
                            aes(y = .data[[column]], colour = label, linetype = label),
                            linewidth = linewidth)
  } else {
    solid_line <- geom_line(data = full_years, aes(y = .data[[column]], colour = label),
                            linewidth = linewidth)
  }
  year_to_date_data <- data[data$Year >= last_full_year, ]
  if (one_legend) {
    dashed_line <- geom_line(data = year_to_date_data,
                             aes(y = .data[[column]], colour = YEAR_TO_DATE_LABEL,
                                 linetype = YEAR_TO_DATE_LABEL),
                             linewidth = linewidth)
  } else {
    dashed_line <- geom_line(data = year_to_date_data,
                             aes(y = .data[[column]], linetype = YEAR_TO_DATE_LABEL),
                             colour = colour, linewidth = linewidth)
  }
  layers <- list(solid_line,
                 dashed_line,
                 geom_point(data = data[data$Year_to_date, ], aes(y = .data[[column]]),
                            shape = 21, fill = "white", colour = colour, size = point_size, stroke = 0.8))
  if (all_points) {
    layers <- c(layers, list(geom_point(data = full_years, aes(y = .data[[column]]),
                                        colour = colour, size = point_size)))
  }
  layers
}

# PLOTS ------------------------------------------------------------------------

# CITATIONS BY TOOL
plot_df <- long
plot_df$First_Year <- tools_df$First_Year[match(plot_df$Tool, tools_df$Tool)]

# Makes each y axis start at zero
axis_df <- aggregate(Cumulative_citations ~ Panel, plot_df, function(x) max(x, 1))

ggplot(plot_df, aes(x = Year)) +
  # Grey out the years before the preprint (or the publication if there was no preprint)
  geom_rect(data = tools_df, aes(xmin = -Inf, xmax = First_Year - 0.5, ymin = -Inf, ymax = Inf,
                                   fill = BEFORE_PUBLICATION_LABEL),
            alpha = 0.4, inherit.aes = FALSE) +
  # Pale orange for the years between the preprint and the publication (or to the end if not published)
  geom_rect(data = tools_df[tools_df$Still_preprint | tools_df$First_Year < tools_df$Publication_Year, ],
            aes(xmin = First_Year - 0.5, xmax = Preprint_End, ymin = -Inf, ymax = Inf,
                fill = PREPRINT_LABEL),
            alpha = 0.4, inherit.aes = FALSE) +
  # White underlay so the shading never shows through the bars
  geom_col(aes(y = Citations), fill = "white") +
  # Year-to-date bars are drawn with alpha (and a dashed outline) so the shading can have its own
  # legend; over the white underlay the full-year bars look solid
  geom_col(aes(y = Citations, alpha = Year_to_date), fill = CURRENT_YEAR_COLOURS[["FALSE"]],
           colour = "black", linewidth = 0.6,
           linetype = ifelse(plot_df$Year_to_date, "dashed", "solid")) +
  # The cumulative line starts at the first year the tool was available
  cumulative_layers(plot_df[which(plot_df$Year >= plot_df$First_Year), ],
                    "Cumulative_citations", TOOL_LINE_COLOUR, linewidth = 1.3,
                    point_size = 4, label = TOOL_LINE_LABEL, all_points = TRUE,
                    one_legend = TRUE) +
  geom_blank(data = axis_df, aes(y = Cumulative_citations), inherit.aes = FALSE) +
  scale_alpha_manual(values = c("FALSE" = 1, "TRUE" = 0.45),
                     labels = c("Full year", "Year-to-date"),
                     name = "New citations per year") +
  scale_fill_manual(values = setNames(c("#E3E3E3", PREPRINT_COLOUR),
                                      c(BEFORE_PUBLICATION_LABEL, PREPRINT_LABEL)),
                    name = "Publication status") +
  scale_colour_manual(values = setNames(rep(TOOL_LINE_COLOUR, 2), c(TOOL_LINE_LABEL, YEAR_TO_DATE_LABEL)),
                      name = "Cumulative number of citations") +
  scale_linetype_manual(values = setNames(c("solid", "22"), c(TOOL_LINE_LABEL, YEAR_TO_DATE_LABEL)),
                        name = "Cumulative number of citations") +
  guides(alpha = guide_legend(order = 1),
         fill = guide_legend(order = 2),
         colour = guide_legend(order = 3),
         linetype = guide_legend(order = 3)) +
  scale_x_continuous(breaks = facet_years) +
  scale_y_continuous(breaks = whole_number_breaks, expand = expansion(mult = c(0.05, 0.08))) +
  facet_wrap(~Panel, ncol = TOOL_COLUMNS, scales = "free_y", axes = "all_x") +   # x axis on every panel
  labs(x = "Year", y = "Number of citing papers") +
  theme_ambilogue_facet() -> citations_by_tool

save_plot(citations_by_tool, "citations_by_tool", width=24,
          height=3.6 * ceiling(n_tools / TOOL_COLUMNS) + 3)

# CITATIONS OVERALL

plot_df <- overall
last_year_df <- plot_df[which.max(plot_df$Year), ]

ggplot(plot_df, aes(x = Year)) +
  # year-to-date bar has a dashed outline (one linetype per bar, so no extra legend)
  geom_col(aes(y = Unique_citing_works, fill = Year_to_date), colour = "black",
           linewidth = OUTLINE_WIDTH,
           linetype = ifelse(plot_df$Year_to_date, "dashed", "solid")) +
  cumulative_layers(plot_df, "Cumulative_sum", SUM_COLOUR, point_size = 4,
                    label = SUM_LABEL, all_points = TRUE) +
  cumulative_layers(plot_df, "Cumulative_unique", UNIQUE_COLOUR, point_size = 4,
                    label = UNIQUE_LABEL, all_points = TRUE) +
  geom_text(data = last_year_df, aes(y = Cumulative_unique, label = format_number(Cumulative_unique)),
            colour = UNIQUE_COLOUR, size = 5 * FONT_SCALE, vjust = -1.2, fontface = "bold") +
  geom_text(data = last_year_df, aes(y = Cumulative_sum, label = format_number(Cumulative_sum)),
            colour = SUM_COLOUR, size = 5 * FONT_SCALE, vjust = -1.2, fontface = "bold") +
  ylab("Number of citing papers") +
  xlab("Year") +
  scale_fill_manual(values = CURRENT_YEAR_COLOURS,
                    breaks = c("FALSE", "TRUE"),
                    labels = c("Full year", "Year-to-date"),
                    name = pad_title("New unique citing papers", "Cumulative number of citations", 15 * FONT_SCALE)) +
  scale_colour_manual(values = setNames(c(UNIQUE_COLOUR, SUM_COLOUR), c(UNIQUE_LABEL, SUM_LABEL)),
                      breaks = c(UNIQUE_LABEL, SUM_LABEL),
                      labels = function(x) wrap_text(x, LEGEND_WRAP), name = "Cumulative number of citations") +
  scale_linetype_manual(values = setNames("22", YEAR_TO_DATE_LABEL),
                        # blank title as wide as the other legend titles, so this entry lines up with theirs
                        name = pad_title("\u00a0", "Cumulative number of citations", 15 * FONT_SCALE)) +
  guides(fill = guide_legend(order = 1),
         colour = guide_legend(order = 2, nrow = 2),
         linetype = guide_legend(order = 3, override.aes = list(colour = "grey40"))) +
  scale_x_continuous(breaks = all_years) +
  scale_y_continuous(labels = format_number, expand = expansion(mult = c(0.05, 0.14))) +
  theme_ambilogue_bar() +
  theme(legend.box = "vertical") -> citations_overall

citations_overall_grob <- set_panel_size(citations_overall, PAIR_PANEL_WIDTH, PAIR_PANEL_HEIGHT)
save_plot(citations_overall_grob, "citations_overall", width = PAIR_FIG_WIDTH, height = PAIR_FIG_HEIGHT)

# Date the OpenAlex data was retrieved; plots.html reads this file.
writeLines(as.character(RETRIEVED_DATE), file.path(out_dir, "citations_last_updated.txt"))

invisible(grDevices::graphics.off())
message("Done: figures are in ", normalizePath(out_dir))
