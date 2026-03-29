library(tidyverse)
library(compmus)
stussynew <- read_csv("Stussy_NEW.csv")
stussyold <- read_csv("Stussy_OLD (2).csv")

bothplaylists <- 
  bind_rows(
    stussyold |> mutate(Category = "OLD"),
    stussynew |> mutate(Category = "NEW")
  ) |>
  mutate(Category = factor(Category, levels = c("OLD", "NEW")))

stussynew |> ggplot(aes(x = Energy)) + geom_histogram(binwidth = 0.1)
bothplaylists |>
  ggplot(aes(x = Energy)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~Category)
bothplaylists |>
  ggplot(aes(x = Category, y = Energy)) +
  geom_violin()





bothplaylists |>                    # Start with awards.
  mutate(
    Mode = ifelse(Mode == 0, "Minor", "Major")
  ) |>
  ggplot(                     # Set up the plot.
    aes(
      x = Valence,
      y = Energy,
      size = Loudness,
      colour = Mode
    )
  ) +
  geom_point() +              # Scatter plot.
  geom_rug(linewidth = 0.1) + # Add 'fringes' to show data distribution.
  facet_wrap(~ Category) +    # Separate charts per playlist.
  scale_x_continuous(         # Fine-tune the x axis.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),   # Use grid-lines for quadrants only.
    minor_breaks = NULL       # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(         # Fine-tune the y axis in the same way.
    limits = c(0.5, 1),
    breaks = c(0, 0.50, 1),
    minor_breaks = NULL
  ) +
  scale_colour_brewer(        # Use the Color Brewer to choose a palette.
    type = "qual",            # Qualitative set.
    palette = "YlOrRd"        # Name of the palette is 'Paired'.
  ) +
  scale_size_continuous(      # Fine-tune the sizes of each point.
    trans = "exp",            # Use an exp transformation to emphasise loud.
    guide = "none"            # Remove the legend for size.
  ) +
  theme_light() +             # Use a simpler theme.
  labs(                       # Make the titles nice.
    x = "Valence",
    y = "Energy",
    colour = "Mode"
  )

remotes::install_github('jaburgoyne/compmus')

believe <- read_csv("Believe.csv")

believe |>
  compmus_wrangle_chroma() |> 
  mutate(pitches = map(pitches, compmus_normalise, "chebyshev")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(
    title = "Believe in Yourself (27 March 2026)",
    x = "Time (s)", 
    y = NULL, 
    fill = "Magnitude"
  ) +
  theme_minimal() +
  scale_fill_viridis_c()

aboutus <- read_csv("Aboutus.csv")

aboutus |>
  compmus_wrangle_chroma() |> 
  mutate(pitches = map(pitches, compmus_normalise, "chebyshev")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(
    title = "It's About us (31 January 2025)",
    x = "Time (s)", 
    y = NULL, 
    fill = "Magnitude"
  ) +
  theme_minimal() +
  scale_fill_viridis_c()

Allnight <- read_csv("AllnightChroma.csv")

Allnight |>
  compmus_wrangle_chroma() |> 
  mutate(pitches = map(pitches, compmus_normalise, "chebyshev")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(
    title = "All Night Long (6 June 2023)",
    x = "Time (s)", 
    y = NULL, 
    fill = "Magnitude"
  ) +
  theme_minimal() +
  scale_fill_viridis_c()

TravellerChroma <- read_csv("TravellerChroma.csv")

TravellerChroma |>
  compmus_wrangle_chroma() |> 
  mutate(pitches = map(pitches, compmus_normalise, "chebyshev")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(
    title = "A Traveller on the Dusty Road (3 June 2022)",
    x = "Time (s)", 
    y = NULL, 
    fill = "Magnitude"
  ) +
  theme_minimal() +
  scale_fill_viridis_c()

LimerenceChroma <- read_csv("LimerenceChroma.csv")

LimerenceChroma |>
  compmus_wrangle_chroma() |> 
  mutate(pitches = map(pitches, compmus_normalise, "chebyshev")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(
    title = "Limerence (25 January 2021)",
    x = "Time (s)", 
    y = NULL, 
    fill = "Magnitude"
  ) +
  theme_minimal() +
  scale_fill_viridis_c()
