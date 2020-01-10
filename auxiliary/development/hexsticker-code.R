#Hex sticker for MADA course
library('hexSticker')

library(ggplot2)

p <- ggplot() 
p <- p + annotate("text", x = 1, y=1, hjust = 0, parse=T, label='"Pon" * phantom("tiac Firebird")', color="red") +
         annotate("text", x = 1, y=1, hjust = 0, parse=T, label='phantom("Pon") * "tiac Firebird"', color="black")
p <- p + theme_void() + theme_transparent() 

print(p)

sticker(p, package="", p_size=20, s_x=1, s_y=.75, s_width=1.3, s_height=1, filename="sticker.png")
