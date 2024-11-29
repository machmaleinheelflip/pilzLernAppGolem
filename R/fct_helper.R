# Function to resize and encode the image
encode_resized_image_from_url <- function(url, max_width = 600, max_height = 600) {
  image <- image_read(url) %>%
    image_resize(paste(max_width, "x", max_height)) %>%
    image_convert(format = "jpeg")

  base64_str <- base64enc::base64encode(image_write(image, path = NULL, format = "jpeg"))
  paste0("data:image/jpeg;base64,", base64_str)
}
