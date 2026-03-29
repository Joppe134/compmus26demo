TravellerMFCC <- read_csv("TravellerMFCC.csv")

TravellerMFCC |>
  compmus_wrangle_timbre() |> 
  mutate(timbre = map(timbre, compmus_normalise, "manhattan")) |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = mfcc,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude", title = "A Traveller on the Dusty Road (3 June 2022) - Timbre") +
  scale_fill_viridis_c() +                              
  theme_classic() + theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold")
  )

TravellerMFCC |>
  compmus_wrangle_timbre() |> 
  filter(row_number() %% 50L == 0L) |> 
  mutate(timbre = map(timbre, compmus_normalise, "manhattan")) |>
  compmus_self_similarity(timbre, "cosine") |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = 50 * xduration,
      y = ystart + yduration / 2,
      height = 50 * yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(x = "", y = "", title = "A Traveller on the Dusty Road (3 June 2022) - Timbre")+ theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold")
  )


AllnightMFCC <- read_csv("AllnightMFCC.csv")

AllnightMFCC |>
  compmus_wrangle_timbre() |> 
  mutate(timbre = map(timbre, compmus_normalise, "manhattan")) |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = mfcc,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude",title = "All Night Long (6 June 2023)") +
  scale_fill_viridis_c() +                              
  theme_classic()

AllnightMFCC |>
  compmus_wrangle_timbre() |> 
  filter(row_number() %% 50L == 0L) |> 
  mutate(timbre = map(timbre, compmus_normalise, "chebyshev")) |>
  compmus_self_similarity(timbre, "cosine") |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = 50 * xduration,
      y = ystart + yduration / 2,
      height = 50 * yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(x = "", y = "", title = "All Night Long (6 June 2023) - Timbre")+ theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold")
  )



BelieveMFCC <- read_csv("BelieveMFCC.csv")

BelieveMFCC |>
  compmus_wrangle_timbre() |> 
  mutate(timbre = map(timbre, compmus_normalise, "manhattan")) |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = mfcc,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude", title = "Believe in Yourself (27 March 2026)") +
  scale_fill_viridis_c() +                              
  theme_classic()


mutate(timbre = map(timbre, compmus_normalise, "euclidean")) |>
  compmus_self_similarity(timbre, "cosine")


BelieveMFCC |>
  compmus_wrangle_timbre() |> 
  filter(row_number() %% 50L == 0L) |> 
  mutate(timbre = map(timbre, compmus_normalise, "chebyshev")) |>
  compmus_self_similarity(timbre, "cosine") |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = 50 * xduration,
      y = ystart + yduration / 2,
      height = 50 * yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(x = "", y = "", title = "Believe in Yourself (29 March 2026) - Timbre")+ theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold")
  )

AboutusMFCC <- read_csv("AboutusMFCC.csv")

AboutusMFCC |>
  compmus_wrangle_timbre() |> 
  mutate(timbre = map(timbre, compmus_normalise, "manhattan")) |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = mfcc,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude", title = "It's About us (31 January 2025)") +
  scale_fill_viridis_c() +                              
  theme_classic()

AboutusMFCC |>
  compmus_wrangle_timbre() |> 
  filter(row_number() %% 50L == 0L) |> 
  mutate(timbre = map(timbre, compmus_normalise, "chebyshev")) |>
  compmus_self_similarity(timbre, "cosine") |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = 50 * xduration,
      y = ystart + yduration / 2,
      height = 50 * yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(x = "", y = "", title = "It's About Us (31 January 2025) - Timbre")+ theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold")
  )


LimerenceMFCC <- read_csv("LimerenceMFCC.csv")

LimerenceMFCC |>
  compmus_wrangle_timbre() |> 
  mutate(timbre = map(timbre, compmus_normalise, "manhattan")) |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = mfcc,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude", title = "Limerence (25 January 2021)") +
  scale_fill_viridis_c() +                              
  theme_classic()

LimerenceMFCC |>
  compmus_wrangle_timbre() |> 
  filter(row_number() %% 50L == 0L) |> 
  mutate(timbre = map(timbre, compmus_normalise, "chebyshev")) |>
  compmus_self_similarity(timbre, "cosine") |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = 50 * xduration,
      y = ystart + yduration / 2,
      height = 50 * yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +

  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(x = "", y = "", title = "Limerence (25 January 2021) - Timbre")+ theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold")
  )
