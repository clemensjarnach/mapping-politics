## =============================================================================
## Script name: analysis.R
## Purpose: Analyse and Plot UK General Election Results 2010-2024
##
## Author: Clemens Jarnach
## =============================================================================

# ------------------------------------------------------------------------------
# Notes:
# ------------------------------------------------------------------------------
# This script performs the data preparation necessary for analysing and 
# plotting UK General Election results from 2010-2024. It reads historical 
# election data from Excel files, standardizes constituency codes using ONS 
# identifiers, calculates vote shares, and joins the results to geographic 
# shapefiles for spatial visualization. The output includes cleaned datasets 
# ready for creating choropleth maps, interactive visualizations, and 
# statistical analysis of voting patterns across UK constituencies.
#
# Key outputs:
# - data: Long-format election results with votes and vote shares by party
# - map_df_YYYY: Spatial dataframes for each election year (2010-2024)
# - Interactive leaflet maps and static ggplot visualizations
#
# Data sources:
# - House of Commons Library election results (1918-2024)
# - ONS constituency boundary shapefiles (Dec 2021 and July 2024)
#
# NOTE ON CONSTITUENCY BOUNDARIES ACROSS GENERAL ELECTIONS
# --------------------------------------------------------
# UK parliamentary constituency boundaries have been revised periodically, with
# major boundary reviews implemented in:
#   - 1997
#   - 2005
#   - 2010
#   - (2013 and 2018 reviews were conducted, but no boundary changes were enacted)
#   - 2024
#
# As a result, elections in 2001, 2005, and 2010 onward were held under 
# different constituency maps. To maintain a consistent spatial framework 
# for comparison over time, this script projects all election results 
# (2001–2019) onto the 2021 constituency boundaries. This approach ensures 
# visual and analytical consistency, but note that it does not reproduce 
# the exact historical constituency shapes or totals for earlier elections.
# Results should therefore be interpreted as historical election outcomes 
# projected onto current constituency boundaries.
# Updated constituency boundaries are used for the 2024 general election. 
#
# Note: the Data has the following boundary sets and identifying codes:
#   - 1997-2001: PCA codes
#   - 2005: PCA codes for England, Wales and Northern Ireland; ONS codes for Scotland. For comparison purposes, Scottish constituencies can be compared with the 2010-2017 set, while all other constituencies can be compared with the 1997-2001 set
#   - 2010-2019: ONS codes

# Commons Library: https://commonslibrary.parliament.uk/research-briefings/cbp-8647/ 



# ------------------------------------------------------------------------------
# Set-up# ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Check working directory
getwd()  

# Install and load required packages
want <- c("data.table", "fixest", "ggiraph", "gt", "here", "leaflet","lubridate", 
          "janitor", "purrr","readxl", "scales", "sf" ,"tidyverse")
have <- want %in% rownames(installed.packages())
if (any(!have)) install.packages(want[!have])
junk <- lapply(want, library, character.only = TRUE)
rm(have, want, junk)


# ------------------------------------------------------------------------------
# Load Data --------------------------------------------------------------------
# ------------------------------------------------------------------------------

# First, load the UK constituencies shapefile 
seats_table <- readxl::read_excel(
  here::here("data", "uk_general_election_seats_table.xlsx")
) %>%
  janitor::clean_names() 
print(seats_table)

seat_counts <- seats_table |> select(election,party,seats
)
print(seat_counts)

# Define official party colors
party_colors <- c(
  "Conservative" = "#0087DC",
  "Labour" = "#DC241f",
  "Lib Dem" = "#ffa500",
  "SNP" = "#dad420"
)


# ------------------------------------------------------------------------------
#  Seats Count figure ----------------------------------------------------------
# ------------------------------------------------------------------------------

# Create the visualization
seat_counts_plot <-  ggplot(seat_counts, aes(x = election, y = seats, color = party, group = party)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  geom_point(size = 4, alpha = 0.9) +
  geom_point(size = 2.5, color = "white") +  # White center for points
  
  # Add value labels at each point
  geom_text(aes(label = seats), 
            vjust = -1.2, 
            size = 3.5, 
            fontface = "bold",
            show.legend = FALSE) +
  
  # Styling
  scale_color_manual(values = party_colors, 
                     name = "Party",
                     labels = c("Conservative", "Labour", "Liberal Democrats", "SNP")) +
  scale_x_continuous(breaks = c(2010, 2015, 2017, 2019, 2024)) +
  scale_y_continuous(limits = c(0, 450), breaks = seq(0, 450, 50)) +
  
  # Labels and title
  labs(
    title = "UK General Election Results: Seat Distribution 2010-2024",
    subtitle = "The dramatic shift in British politics from coalition to Conservative dominance to Labour's 2024 landslide",
    x = "Election Year",
    y = "Number of Seats",
    caption = "Data: House of Commons Library | 650 total seats in House of Commons"
  ) +
  
  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, color = "grey40", margin = margin(b = 15)),
    plot.caption = element_text(color = "grey50", size = 9, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    plot.margin = margin(15, 15, 15, 15),
    axis.title = element_text(face = "bold", size = 11)
  )


# ------------------------------------------------------------------------------
#  Vote share figure  ----------------------------------------------------------
# ------------------------------------------------------------------------------
vote_share_data <- seats_table |> select(election,party,vote_share
)
print(vote_share_data)


# Define official party colors (same as before)
party_colors <- c(
  "Conservative" = "#0087DC",
  "Labour" = "#DC241f",
  "Lib Dem" = "#ffa500",
  "SNP" = "#dad420"
)

# Create the visualization
vote_share_plot <- ggplot(vote_share_data, aes(x = election, y = vote_share, color = party, group = party)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  geom_point(size = 4, alpha = 0.9) +
  geom_point(size = 2.5, color = "white") +  # White center for points
  
  # Add value labels at each point (with % symbol)
  geom_text(aes(label = paste0(round(vote_share, 1), "%")), 
            vjust = -1.2, 
            size = 3.5, 
            fontface = "bold",
            show.legend = FALSE) +
  
  # Styling
  scale_color_manual(values = party_colors, 
                     name = "Party",
                     labels = c("Conservative", "Labour", "Liberal Democrats", "SNP")) +
  scale_x_continuous(breaks = c(2010, 2015, 2017, 2019, 2024)) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10),
                     labels = function(x) paste0(x, "%")) +
  
  # Labels and title
  labs(
    title = "UK General Election Results: Vote Share 2010-2024",
    subtitle = "First-past-the-post distortion: vote share tells a different story than seat distribution",
    x = "Election Year",
    y = "Vote Share (%)",
    caption = "Data: House of Commons Library | Vote share percentages by party"
  ) +
  
  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, color = "grey40", margin = margin(b = 15)),
    plot.caption = element_text(color = "grey50", size = 9, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    plot.margin = margin(15, 15, 15, 15),
    axis.title = element_text(face = "bold", size = 11)
  )


# ------------------------------------------------------------------------------
# Bar Chart -----------------------
# ------------------------------------------------------------------------------

# Keep only relevant years
bar_years <- c(2010, 2015, 2017, 2019, 2024)
# Count seats per party per election
seat_counts <- seats_table %>% 
  select(election, party, seats)
seat_counts


# Define party colors (consistent with your line charts)

party_colors <- c(
  "Conservative" = "#0087DC",
  "Labour" = "#DC241f",
  "Lib Dem" = "#ffa500",
  "SNP" = "#dad420"
)

# Function to create bar chart for a single election

create_election_bar <- function(year) {
  data_year <- seat_counts %>% filter(election == year)
  
  ggplot(data_year, aes(x = party, y = seats, fill = party)) +
    geom_col(width = 0.7, alpha = 0.9) +
    geom_text(aes(label = seats), 
              vjust = -0.5, 
              size = 5, 
              fontface = "bold") +
    scale_fill_manual(values = party_colors) +
    scale_y_continuous(limits = c(0, 650),
                       breaks = seq(0, 650, 50)) +
    labs(
      title = paste0(year, " General Election Results"),
      subtitle = paste0("Total seats won by major parties (650 seats total)"),
      x = NULL,
      y = "Number of Seats",
      caption = "Data: House of Commons Library"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 12, color = "grey40", hjust = 0.5),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 11),
      axis.title.y = element_text(face = "bold", size = 12)
    )
}

# ------------------------------------------------------------------------------
# Create individual plots for each election ----
# ------------------------------------------------------------------------------
plot_2010 <- create_election_bar(2010)
plot_2015 <- create_election_bar(2015)
plot_2017 <- create_election_bar(2017)
plot_2019 <- create_election_bar(2019)
plot_2024 <- create_election_bar(2024)


# ------------------------------------------------------------------------------
# Create *interactive* individual plots for each election ----
# ------------------------------------------------------------------------------


create_election_bar_interactive <- function(year) {
  data_year <- seat_counts %>% 
    filter(election == year) %>%
    mutate(
      tooltip = paste0(
        party, ": ", seats, " seats\n(", year, ")"
      ),
      data_id = party
    )
  
  p <- ggplot(data_year, aes(x = party, y = seats, fill = party)) +
    geom_col_interactive(
      aes(tooltip = tooltip, data_id = data_id),
      width = 0.7,
      alpha = 0.9
    ) +
    geom_text(
      aes(label = seats), vjust = -0.5, size = 5, fontface = "bold"
    ) +
    scale_fill_manual(values = party_colors) +
    scale_y_continuous(limits = c(0, 650), breaks = seq(0, 650, 50)) +
    labs(
      title = paste0(year, " General Election Results"),
      subtitle = "Total seats won by major parties (650 total)",
      x = NULL,
      y = "Seats",
      caption = "Data: House of Commons Library"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 12, color = "grey40", hjust = 0.5),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 11),
      axis.title.y = element_text(face = "bold", size = 12)
    )
  
  girafe(
    ggobj = p,
    options = list(
      opts_hover(css = "fill:black;"),
      opts_hover_inv(css = "opacity:0.5;"),
#      opts_hover(css = "fill:black;stroke:black;cursor:pointer;"),
      opts_tooltip(css = "background:#222;color:white;padding:6px;border-radius:4px;")
    )
  )
}

plot_i_2010 <- create_election_bar_interactive(2010)
plot_i_2015 <- create_election_bar_interactive(2015)
plot_i_2017 <- create_election_bar_interactive(2017)
plot_i_2019 <- create_election_bar_interactive(2019)
plot_i_2024 <- create_election_bar_interactive(2024)





# ------------------------------------------------------------------------------
# Maps/Choropleth  ------------------------------------------------------------
# ------------------------------------------------------------------------------


# 2019, 2017, 2015, 2010: ---------

file <- here::here("data", "1918-2019election_results_by_pcon.xlsx")

sheets <- readxl::excel_sheets(file)

# Focus on the most recent 10 elections present
target_years <- intersect(
  c(
    # "1997", "2001", "2005",
    "2010",
    "2015",
    "2017",
    "2019"), 
  sheets
)

read_hoc_sheet <- function(sheet){
  
  raw <- readxl::read_excel(
    file,
    sheet = sheet,
    skip = 3,   # skips the title lines
    col_names = TRUE
  )
  
  raw <- janitor::clean_names(raw)
  
  # Identify party vote columns (pattern: partyname_votes or partyname_vote_share)
  vote_cols   <- grep("_votes$", names(raw),  value = TRUE)
  # share_cols  <- grep("_vote_share$", names(raw), value = TRUE)
  
  # Keep base ID columns and elections
  core <- raw %>%
    select(id, constituency, electorate, total_votes, turnout,
           all_of(vote_cols)) %>%
    mutate(election = as.integer(sheet))
  
  core
}

data_wide <- purrr::map_dfr(target_years, read_hoc_sheet)

# remove rows where the id cell is empty: 
data_wide<- data_wide |>
  filter(!is.na(id))


# Create vote_share: 
colnames(data_wide)

data_wide <- data_wide %>%
  mutate(
    across(
      .cols = ends_with("_votes"),
      .fns  = ~ .x / total_votes,
      .names = "{.col}_share"
    )
  )

data_wide %>%
  select(constituency, total_votes, matches("conservative|labour")) %>%
  head()


# Some constituencies have mroe than one id. Let's use the 
# standardized ONS (Office for National Statistics) codes where possible.

# Create list of id to constituency pairs: 
constituency_pairs_df <- data_wide |>
  select(id, constituency)|>
  unique()

# Let's use stringr::str_detect to check if the ID starts with a letter,
# sort based on that check, and then keep the first row (the best ID).
unique_constituency_map_prioritized <- constituency_pairs_df |>
  mutate(is_ons_code = str_detect(id, "^[A-Za-z]")) |>
  # Sort the data. Arrange puts TRUE values (ONS codes) first.
  # This ensures that if both ID types exist, the ONS code is on top.
  arrange(desc(is_ons_code)) |>
  group_by(constituency) |>
  # Keep the first row within each group (now guaranteed to be the ONS code if present)
  slice_head(n = 1) |>
  # Clean up the temporary column and grouping
  ungroup() |>
  select(-is_ons_code)


head(data_wide)

# We join the full data to the map on the shared constituency column.
data_wide <- data_wide |>
  # Join the original data ('data_to_update') to the lookup table ('unique_constituency_map_prioritized')
  left_join(unique_constituency_map_prioritized, 
            by = "constituency",
            # This suffix is important: it names the new ID column "id" pulled from the map and the old one "id_old"
            suffix = c("_old", ""))



# Pivot Longer & creates party, votes, and vote_share columns: 
data <- data_wide %>%
  pivot_longer(
    cols = matches("(votes$|votes_share$)"),
    names_to = c("party", ".value"),
    names_pattern = "(.*)_(votes|votes_share)$"
  ) %>%
  # Standardise column names
  rename(
    ons_code = id,
    vote_share = votes_share
  )

# Check if the data seems correct:  
data %>% count(election)  # rows per GE
data %>% filter(ons_code == "W07000049", election == 2019)

# Remove rows where party == total
data <- data |>
  filter(party != "total")

# Check structure
colnames(data)

# Clean column order and remove legacy id fields
# Reorder so ons_code and constituency come first
data <- data |>
  select(
    ons_code,
    constituency,
    everything()
  )

# Drop redundant ID columns
data <- data |>
  select(
    -id_old
  )

# ------------------------------------------------------------------------------
# Choropleth
# ------------------------------------------------------------------------------

# 2019, 2017, 2015, 2010 -------------------------------------------------------

# First, load the UK constituencies shapefile 
uk_const <- st_read(
  here::here("data", "Westminster_Parliamentary_Constituencies_Dec_2021_UK_BUC_2022_-1597611953006344616", "PCON_DEC_2021_UK_BUC.shp")
) %>%
  janitor::clean_names() 
#%>%
# st_transform(4326)

# saveRDS(
#   object = uk_const,
#   file = here("data", "processed", "uk_constituencies_2021_simplified.rds"),
#   compress = "xz"
# )

# inspect field names to identify the constituency code column:
names(uk_const)

# rename to ons_code: 
uk_const <- uk_const %>%
  rename(ons_code = pcon21cd)

# Create winner-by-constituency data per election:
winners <- data %>%
  group_by(election, ons_code) %>%
  slice_max(votes, n = 1, with_ties = FALSE) %>%
  ungroup()

# Join it to the map: 
map_df <- uk_const %>%
  left_join(winners, by = "ons_code")


# Add Party Colours: 
unique(data.frame(data$party))
party_cols <- c(
  "conservative" = "#0087DC",
  "labour" = "#DC241f",
  "liberal_democrats" = "#FDBB30",
  "snp" = "#FFFF00",
  "plaid_cymru" = "#005B54",
  "green" = "#52C152",
  "brexit" = "#12B6CF",
  "reform_uk" = "#00B5E2",
  "ukip" = "#6D3177",
  "independent" = "grey60",
  "dup" = "#D46A4C",
  "sinn_fein" = "#326760", 
  "sdlp" = "#2AA82C",
  "uup" = "#48A5EE",
  "alliance" = "#F6CB2F",
  "other" = "grey80"
)


# Plot a basic map  for 2019 as example: 
plot_2019 <- map_df %>%
  filter(election == 2019) %>%
  ggplot(aes(fill = party)) +
  geom_sf(colour = "white", linewidth = 0.1) +
  scale_fill_manual(values = party_cols, na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "2019 UK General Election 2019 - Winning Party by Constituency",
    fill = "Party"
  )


# Plot  2017:
plot_2017 <- map_df %>%
  filter(election == 2017) %>%
  ggplot(aes(fill = party)) +
  geom_sf(colour = "white", linewidth = 0.1) +
  scale_fill_manual(values = party_cols, na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "2017 UK General Election - Winning Party by Constituency",
    fill = "Party"
  )




# Plot  2015:
plot_2015 <- map_df %>%
  filter(election == 2015) %>%
  ggplot(aes(fill = party)) +
  geom_sf(colour = "white", linewidth = 0.1) +
  scale_fill_manual(values = party_cols, na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "2015 UK General Election - Winning Party by Constituency",
    fill = "Party"
  )

# Plot  2010:
plot_2010 <- map_df %>%
  filter(election == 2010) %>%
  ggplot(aes(fill = party)) +
  geom_sf(colour = "white", linewidth = 0.1) +
  scale_fill_manual(values = party_cols, na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "2010 UK General Election - Winning Party by Constituency",
    fill = "Party"
  )






# ------------------------------------------------------------------------------
# Interactive Map --------------------------------------------------------------
# ------------------------------------------------------------------------------


# Function to create interactive map for any election
create_interactive_map <- function(election_year) {
  
  party_cols <- c(
    "conservative" = "#0087DC",
    "labour" = "#DC241f",
    "liberal_democrats" = "#FDBB30",
    "snp" = "#FFFF00",
    "plaid_cymru" = "#005B54",
    "green" = "#52C152",
    "brexit" = "#12B6CF",
    "reform_uk" = "#00B5E2",
    "ukip" = "#6D3177",
    "independent" = "grey60",
    "dup" = "#D46A4C",
    "sinn_fein" = "#326760", 
    "sdlp" = "#2AA82C",
    "uup" = "#48A5EE",
    "alliance" = "#F6CB2F",
    "other" = "grey80"
  )
  
  map_year <- map_df %>%
    filter(election == election_year) %>%
    st_transform(4326)
  

  pal <- colorFactor(
    palette = party_cols,
    domain = map_year$party,
    levels = names(party_cols)  # This ensures correct mapping
  )
  
  leaflet(map_year) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~pal(party),
      fillOpacity = 0.7,
      color = "white",
      weight = 0.5,
      opacity = 1,
      highlight = highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = ~paste0(pcon21nm, ": ", party),
      popup = ~paste0(
        "<strong>", pcon21nm, "</strong><br/>",
        "Party: ", party, "<br/>",
        "Votes: ", format(votes, big.mark = ","), "<br/>",
        "Vote Share: ", round(vote_share, 1), "%"
      ),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = ~party,
      title = paste(election_year, "Winner"),
      opacity = 0.7
    )
}

# Create maps for each election
map_interactive_2010 <- create_interactive_map(2010)
map_interactive_2015 <- create_interactive_map(2015)
map_interactive_2017 <- create_interactive_map(2017)
map_interactive_2019 <- create_interactive_map(2019)




# ------------------------------------------------------------------------------
# 2024 -------------------------------------------------------------------------
# ------------------------------------------------------------------------------


file <- here::here("data", "1918-2019election_results_by_pcon.xlsx")

# Read and process data 
data_wide <- readxl::read_excel(
  file,
  sheet = "2024",
  skip = 3,
  col_names = TRUE
) %>%
  janitor::clean_names() %>%
  filter(!is.na(id)) %>%  # Remove empty rows 
  # Keep only necessary columns 
  select(id, constituency, electorate, total_votes, turnout, ends_with("_votes")) %>%
  # Create vote_share 
  mutate(
    election = 2024L,
    across(
      ends_with("_votes"),
      ~ .x / total_votes,
      .names = "{.col}_share"
    )
  )

# Simplify ID standardization 
data_wide <- data_wide %>%
  group_by(constituency) %>%
  # Keep ONS code (starts with letter) if available, otherwise keep first ID
  slice_min(order_by = !str_detect(id, "^[A-Za-z]"), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(ons_code = id)

# Pivot to long format
data <- data_wide %>%
  pivot_longer(
    cols = ends_with(c("_votes", "_votes_share")),
    names_to = c("party", ".value"),
    names_pattern = "(.*)_(votes|votes_share)$"
  ) %>%
  rename(vote_share = votes_share) %>%
  filter(party != "total") %>%  # Remove total rows immediately
  select(ons_code, constituency, election, party, votes, vote_share, everything())

# ------------------------------------------------------------------------------
# Choropleth  # ----------------------------------------------------------------
# ------------------------------------------------------------------------------

# Read shapefile once and transform

uk_const <- st_read(
  here::here("data", "Westminster_Parliamentary_Constituencies_July_2024_Boundaries_UK_BFC_8322311767861589132", "PCON_JULY_2024_UK_BFC.shp"),
  quiet = TRUE
) %>%
  janitor::clean_names() %>%
  rename(ons_code = pcon24cd) %>%
  st_transform(4326) %>%
  st_simplify(dTolerance = 100)  # Simplify geometry - adjust tolerance as needed

# saveRDS(
#   object = uk_const,
#   file = here("data", "processed", "uk_constituencies_2024_simplified.rds"),
#   compress = "xz"
# )

# Get winners (no need to filter by election if only 2024 exists)
winners <- data %>%
  group_by(ons_code) %>%
  slice_max(votes, n = 1, with_ties = FALSE) %>%
  ungroup()

# Join and create map data
map_df_2024 <- uk_const %>%
  left_join(winners, by = "ons_code")

# Define party colors
party_cols <- c(
  "conservative" = "#0087DC",
  "labour" = "#DC241f",
  "liberal_democrats" = "#FDBB30",
  "snp" = "#FFFF00",
  "plaid_cymru" = "#005B54",
  "green" = "#52C152",
  "brexit" = "#12B6CF",
  "reform_uk" = "#00B5E2",
  "ukip" = "#6D3177",
  "independent" = "grey60",
  "dup" = "#D46A4C",
  "sinn_fein" = "#326760",
  "sdlp" = "#2AA82C",
  "uup" = "#48A5EE",
  "alliance" = "#F6CB2F",
  "other" = "grey80"
)

# Static plot (simplified)
plot_2024 <- ggplot(map_df_2024, aes(fill = party)) +
  geom_sf(colour = "white", linewidth = 0.1) +
  scale_fill_manual(values = party_cols, na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "2024 UK General Election - Winning Party by Constituency",
    fill = "Party"
  )

# Interactive map 
pal <- colorFactor(
  palette = party_cols,
  domain = map_df_2024$party,
  levels = names(party_cols)  # ensure correct colour mapping
)

map_interactive_2024 <- leaflet(map_df_2024) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal(party),
    fillOpacity = 0.7,
    color = "white",
    weight = 0.5,
    opacity = 1,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    label = ~paste0(pcon24nm, ": ", party),
    popup = ~paste0(
      "<strong>", pcon24nm, "</strong><br/>",
      "Party: ", party, "<br/>",
      "Votes: ", format(votes, big.mark = ","), "<br/>",
      "Vote Share: ", round(vote_share, 1), "%"
    ),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "13px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~party,
    title = "Winning Party",
    opacity = 0.7
  )

