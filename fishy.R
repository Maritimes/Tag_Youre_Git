# Install required packages if needed
if (!require("gganimate")) install.packages("gganimate")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gifski")) install.packages("gifski")

library(ggplot2)
library(gganimate)

# Function to create fish shape
create_fish <- function(x_pos, y_pos, size = 1, flip = FALSE) {
    # Fish body (ellipse)
    theta <- seq(0, 2 * pi, length.out = 100)
    body_x <- x_pos + size * 0.5 * cos(theta) * 5.2
    body_y <- y_pos + size * 0.3 * sin(theta)

    # Tail
    tail_x <- c(x_pos - size * 0.75, x_pos - size * 1.3, x_pos - size * 0.75)
    tail_y <- c(y_pos - size * 0.2, y_pos, y_pos + size * 0.2)
    
    tail_x <- c(x_pos - size * 2, x_pos - size * 2.2, x_pos - size * 2.7, x_pos - size * 2.3)
    tail_y <- c(y_pos, y_pos + size * 0.7, y_pos + size * 0.7, y_pos)

    # Top fin
    fin_top_x <- c(x_pos + size * 0.2, x_pos + size * 0.3, x_pos + size * 0.4)
    fin_top_y <- c(y_pos, y_pos + size * 0.5, y_pos)
    
    fin_top_x <- c(x_pos + size * 2.4, x_pos + size * 2.4, x_pos + size * 1.4, x_pos + size * 1.4)
    fin_top_y <- c(y_pos - size * 0.25, y_pos + size * 0.25, y_pos + size * 0.25, y_pos - size * 0.25)

    # Bottom fin
    fin_bottom_x <- c(x_pos - size * 0.1, x_pos, x_pos + size * 0.1)
    fin_bottom_y <- c(y_pos, y_pos - size * 0.4, y_pos)
    
    fin_bottom_x <- c(x_pos - size * 0, x_pos, x_pos + size * 0)
    fin_bottom_y <- c(y_pos, y_pos - size * 0.4, y_pos + size * 0.4)
    # Eye
    eye_x <- x_pos + size * 2.4
    eye_y <- y_pos + size * 0

    if (flip) {
        body_x <- -body_x + 2 * x_pos
        tail_x <- -tail_x + 2 * x_pos
        fin_top_x <- -fin_top_x + 2 * x_pos
        fin_bottom_x <- -fin_bottom_x + 2 * x_pos
        eye_x <- -eye_x + 2 * x_pos
    }

    data.frame(
        x = c(body_x, tail_x, fin_top_x, fin_bottom_x, eye_x),
        y = c(body_y, tail_y, fin_top_y, fin_bottom_y, eye_y),
        part = c(
            rep("body", length(body_x)),
            rep("tail", length(tail_x)),
            rep("fin_top", length(fin_top_x)),
            rep("fin_bottom", length(fin_bottom_x)),
            rep("eye", 1)
        )
    )
}

# Create animation frames
n_frames <- 100
fish_data <- data.frame()

for (i in 1:n_frames) {
    # Fish swims in a wavy pattern
    x <- i * 0.15
    y <- 5 + sin(i * 0.3) * 2

    # Tail wag effect
    tail_offset <- sin(i * 0.5) * 0.2

    fish <- create_fish(x, y, size = 1)
    fish$frame <- i
    fish$tail_wag <- tail_offset

    fish_data <- rbind(fish_data, fish)
}

# Add bubbles
bubble_data <- data.frame(
    x = runif(20, 0, 15),
    y = runif(20, 0, 10),
    size = runif(20, 0.1, 0.4),
    frame = rep(1:n_frames, length.out = 20)
)

# Move bubbles up
for (i in seq_len(nrow(bubble_data))) {
    bubble_data$y[i] <- bubble_data$y[i] + (bubble_data$frame[i] * 0.05)
    if (bubble_data$y[i] > 10) bubble_data$y[i] <- bubble_data$y[i] - 10
}

# Create the plot
p <- ggplot() +
    # Ocean background
    theme_void() +
    theme(
        plot.background = element_rect(fill = "#1e90ff"),
        panel.background = element_rect(fill = "#1e90ff")
    ) +
    # Bubbles
    geom_point(
        data = bubble_data,
        aes(x = x, y = y, size = size),
        color = "lightblue", alpha = 0.6, shape = 21, fill = "white"
    ) +
    scale_size_continuous(range = c(1, 15), guide = "none") +
    # Fish body
    geom_polygon(
        data = fish_data[fish_data$part == "body", ],
        aes(x = x, y = y, group = frame),
        fill = "yellow", color = "yellow", size = 1
    ) +
    # Tail
    geom_polygon(
        data = fish_data[fish_data$part == "tail", ],
        aes(x = x, y = y, group = frame),
        fill = "yellow", color = "yellow", size = 1
    ) +
    # Fins
    geom_polygon(
        data = fish_data[fish_data$part == "fin_top", ],
        aes(x = x, y = y, group = frame),
        fill = "black", color = "black", size = 1
    ) +
    geom_polygon(
        data = fish_data[fish_data$part == "fin_bottom", ],
        aes(x = x, y = y, group = frame),
        fill = "purple", color = "black", size = 1
    ) +
    # Eye
    geom_point(
        data = fish_data[fish_data$part == "eye", ],
        aes(x = x, y = y, group = frame),
        size = 8, color = "black"
    ) +
    coord_fixed(xlim = c(0, 15), ylim = c(0, 10)) +
    # Animate
    transition_manual(frame)

# Render animation
animate(p,
    nframes = n_frames, fps = 20, width = 600, height = 400,
    renderer = gifski_renderer("swimming_fish.gif")
)

print("Animation saved as 'swimming_fish.gif'!")


