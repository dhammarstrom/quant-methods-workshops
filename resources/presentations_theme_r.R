

## Presentations settings

colors <- c("#f62a3e", "#b08ad2", "#f9a170", "#91a0c0", "#6d97d9", "#034a58", "#000a0a")

background_color_plots <- "#fdf3e3"


### Schema theme

schema_theme <- function() {
        
        theme_void() + 
                theme(plot.background = element_rect(fill = background_color_plots, color = background_color_plots))
}





### Fixed ggplot2 labels

library(ggplot2)
library(grid)
library(stringi)
ggname <- function (prefix, grob) {
        grob$name <- grobName(grob, prefix)
        grob
}

geom_label2 <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        parse = FALSE,
                        nudge_x = 0,
                        nudge_y = 0,
                        label.padding = unit(0.25, "lines"),
                        label.r = unit(0.15, "lines"),
                        label.size = 0.25,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
        if (!missing(nudge_x) || !missing(nudge_y)) {
                if (!missing(position)) {
                        stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
                }
                
                position <- position_nudge(nudge_x, nudge_y)
        }
        
        layer(
                data = data,
                mapping = mapping,
                stat = stat,
                geom = GeomLabel2,
                position = position,
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list(
                        parse = parse,
                        label.padding = label.padding,
                        label.r = label.r,
                        label.size = label.size,
                        na.rm = na.rm,
                        ...
                )
        )
}

GeomLabel2 <- ggproto("GeomLabel2", Geom,
                      required_aes = c("x", "y", "label"),
                      
                      default_aes = aes(
                              colour = "black", fill = "white", size = 3.88, angle = 0,
                              hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
                              lineheight = 1.2
                      ),
                      
                      draw_panel = function(self, data, panel_params, coord, parse = FALSE,
                                            na.rm = FALSE,
                                            label.padding = unit(0.25, "lines"),
                                            label.r = unit(0.15, "lines"),
                                            label.size = 0.25) {
                              lab <- data$label
                              if (parse) {
                                      lab <- parse(text = as.character(lab))
                              }
                              
                              data <- coord$transform(data, panel_params)
                              if (is.character(data$vjust)) {
                                      data$vjust <- compute_just(data$vjust, data$y)
                              }
                              if (is.character(data$hjust)) {
                                      data$hjust <- compute_just(data$hjust, data$x)
                              }
                              
                              grobs <- lapply(1:nrow(data), function(i) {
                                      row <- data[i, , drop = FALSE]
                                      labelGrob2(lab[i],
                                                 x = unit(row$x, "native"),
                                                 y = unit(row$y, "native"),
                                                 just = "center",
                                                 padding = label.padding,
                                                 r = label.r,
                                                 text.gp = gpar(
                                                         col = row$colour,
                                                         fontsize = row$size * .pt,
                                                         fontfamily = row$family,
                                                         fontface = row$fontface,
                                                         lineheight = row$lineheight
                                                 ),
                                                 rect.gp = gpar(
                                                         col = row$colour,
                                                         fill = alpha(row$fill, row$alpha),
                                                         lwd = label.size * .pt
                                                 )
                                      )
                              })
                              class(grobs) <- "gList"
                              
                              ggname("geom_label", grobTree(children = grobs))
                      },
                      
                      draw_key = draw_key_label
)

labelGrob2 <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                       just = "center", padding = unit(0.25, "lines"), r = unit(0.1, "snpc"),
                       default.units = "npc", name = NULL,
                       text.gp = gpar(), rect.gp = gpar(fill = "white"), vp = NULL) {
        
        stopifnot(length(label) == 1)
        
        if (!is.unit(x))
                x <- unit(x, default.units)
        if (!is.unit(y))
                y <- unit(y, default.units)
        
        gTree(label = label, x = x, y = y, just = just, padding = padding, r = r,
              name = name, text.gp = text.gp, rect.gp = rect.gp, vp = vp, cl = "labelgrob2")
}

makeContent.labelgrob2 <- function(x) {
        hj <- resolveHJust(x$just, NULL)
        vj <- resolveVJust(x$just, NULL)
        
        t <- textGrob(
                x$label,
                x$x + 1 * (0.55 - hj) * unit(5, "mm"),
                x$y + 2 * (0.55 - vj) * x$padding,
                just = "center",
                gp = x$text.gp,
                name = "text"
        )
        
        r <- roundrectGrob(x$x, x$y, default.units = "native",
                           width =  1.5 * unit(max(stri_width(x$x)) + 8, "mm"),
                           height = grobHeight(t) + 2 * x$padding,
                           just = c(hj, vj),
                           r = x$r,
                           gp = x$rect.gp,
                           name = "box"
        )
        
        setChildren(x, gList(r, t))
}
