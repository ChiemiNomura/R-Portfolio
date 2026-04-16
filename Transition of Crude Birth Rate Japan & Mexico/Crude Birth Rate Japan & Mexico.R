library(tidyverse)
library(stringr) 

# df: Time-series Crude Birth Rate data for MX and JP
# events: Historical event markers for background annotation
df <- read_csv("TotalSheets.csv")
events <- read_csv("EventosNatalidad.csv")

# Prepare event guide for the plot caption
all_events <- paste0(events$Num, ":", events$Name)
col_left <- all_events[1:6]
col_right <- c(all_events[7:11], "") 

# Adjust string padding
col_left_fixed <- str_pad(col_left, width = 30, side = "right")
event_guide <- paste(paste0(col_left_fixed, col_right), collapse = "\n")


ggplot() +
  #Events
  geom_rect(data = events, alpha = 0.5,
            aes(xmin = Xmin, xmax = Xmax, ymin = -Inf, ymax = Inf, fill = Color))+
  geom_text(data = events, size = 6,
            aes(x = ifelse(Number == "⑨", ((Xmin + Xmax) / 2) + 2, (Xmin + Xmax) / 2), 
                y = 3, label = Number))+
  #Line
  geom_line(data = df %>% filter(!is.na(TBN_JP)), aes(x= year, y = TBN_JP, color = "Japón"), size = 1)+
  geom_line(data = df %>% filter(!is.na(TBN_MX)), aes(x= year, y = TBN_MX, color = "México"), size = 1) +
  
  #scale
  scale_x_continuous(breaks = seq(1900, 2025, by = 10), expand = c(0, 0))+
  scale_y_continuous(limits = c(0, 55), breaks = seq(0,60, by = 10), expand = c(0, 0))+
  
  #Legend events
  scale_fill_manual(values = c("lightgreen" = "lightgreen", "pink" = "pink", "gray" = "gray"),
                    labels = c("lightgreen" = "México", "pink" = "Japón", "gray" = "Inter-\nnacional"),
                    breaks = c("pink", "lightgreen", "gray"),
                    name = NULL) +
  #Legend line
  scale_color_manual(values = c("México" = "darkgreen","Japón" = "navy"), name = NULL) +
  labs( x = "Año", y = "TBN", caption = event_guide,
        title = "Transición de la Tasa Bruta de Natalidad de México y Japón: 1900-2024",) +
  
  theme_minimal()+
  theme(legend.position = "bottom", aspect.ratio = 0.5, legend.margin = margin(t = 4),
        legend.justification = "left",legend.box = "horizontal",
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 9, hjust = 0, family = "mono", lineheight = 1.1, 
                                    margin = margin(t = 20, l = 20)),
        plot.caption.position = "plot",
        
        #memori
        axis.ticks = element_line(colour = "black", linewidth = 0.5),
        axis.ticks.length = unit(0.15, "cm"),
        #frame
        plot.background = element_rect(colour = "black", fill = NA, size = 1),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        panel.border = element_rect(colour = "black", size = 0.5))

