library(qrcode)
library(tidyverse)

# Create a temporary file path for the QR code image
output_path <- "./vignettes/presentation/qrcode.png"

# Step 1: Generate the QR code matrix
qr_data <- qr_code("https://murraylax.org/ecodata/")

# Step 2: Convert the matrix to a data frame for ggplot2
qr_df <- as.data.frame(which(qr_data == 1, arr.ind = TRUE))

# Step 3: Plot the QR code with ggplot2
ggplot(qr_df, aes(x = col, y = -row)) +
  geom_tile(fill = "dodgerblue4") +  # Fill the QR code with dodgerblue4
  theme_void() +                     # Remove all axis elements
  theme(
    plot.background = element_rect(
      fill = "white", colour = "dodgerblue4", size = 15  # Add a thick black border
    ),
    plot.margin = margin(20, 20, 20, 20, unit = "pt")  # Add white space (20 pt)
  ) +
  coord_fixed()  # Ensure the QR code has square proportions

ggsave(filename = output_path, width = 5, height = 5, units = "in", dpi = 300)
