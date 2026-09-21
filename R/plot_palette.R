# Default colours shared by every plot() method in the package.
#
# Point-range displays (error bars) are plain black; ribbon displays use a
# muted steel blue, with a lighter tint for the band so it stays readable
# behind the line without competing with it.
.fixes_palette <- function() {
  list(
    point = "#000000",
    line = "#3D5A80",
    ribbon = "#8DA9C4",
    alpha = 0.35
  )
}
