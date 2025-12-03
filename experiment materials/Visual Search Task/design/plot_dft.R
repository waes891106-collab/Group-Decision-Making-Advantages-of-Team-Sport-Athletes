library(pacman)
pacman::p_load(agricolae, ggpubr, cowplot, ggplot2, plyr, dplyr, mnormt, fdrtool, tidyverse, gridExtra, crayon,
               reshape2, ggthemes, devtools, gridExtra)


#
xpos<-cos(seq(0,2*pi,by=2*pi/5))
ypos<-sin(seq(0,2*pi,by=2*pi/5))
dta <- cbind(xpos, ypos)
dta <- as.data.frame(dta)

#
seg_df <- data.frame(x=c(1.00, 0.309, -0.809,  0.309,  1.00),
                      y=c(0, 9.510565e-01, 5.877853e-01, -9.510565e-01, -2.449294e-16),
                      xend=c(0.309, -0.809,  0.309,  1.00, 1.00),
                      yend=c(9.510565e-01, 5.877853e-01, -9.510565e-01, -2.449294e-16, 0))

seg_df1 <- sample_n(seg_df, 2, replace = F) #
seg_df2 <- sample_n(seg_df, 2, replace = T) #
seg_df3 <- sample_n(seg_df, 3, replace = F) #
seg_df4 <- sample_n(seg_df, 2, replace = F) 
seg_df5 <- sample_n(seg_df, 3, replace = F) 
seg_df6 <- sample_n(seg_df, 4, replace = F)
seg_df7 <- sample_n(seg_df, 4, replace = F)
seg_df8 <- sample_n(seg_df, 3, replace = T)
seg_df9 <- sample_n(seg_df, 3, replace = T) 
seg_df10 <- sample_n(seg_df, 4, replace = F)



f1 <- ggplot(dta, aes(x=xpos, y=ypos)) +
  geom_segment(data=seg_df1, aes(x, y, xend=xend, yend=yend), colour = "red", size = 2) +
  geom_point(shape = 20, size = 5, stroke=1) +
  scale_x_continuous(limits=c(-1.5, 1.5)) + 
  scale_y_continuous(limits=c(-1.5, 1.5)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 1))

f2 <- ggplot(dta, aes(x=xpos, y=ypos)) +
  geom_segment(data=seg_df2, aes(x, y, xend=xend, yend=yend), colour = "red", size = 2) +
  geom_point(shape = 20, size = 5, stroke=1) +
  scale_x_continuous(limits=c(-1.5, 1.5)) + 
  scale_y_continuous(limits=c(-1.5, 1.5)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 1))

f3 <- ggplot(dta, aes(x=xpos, y=ypos)) +
  geom_segment(data=seg_df3, aes(x, y, xend=xend, yend=yend), colour = "red", size = 2) +
  geom_point(shape = 20, size = 5, stroke=1) +
  scale_x_continuous(limits=c(-1.5, 1.5)) + 
  scale_y_continuous(limits=c(-1.5, 1.5)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 1))

f4 <- ggplot(dta, aes(x=xpos, y=ypos)) +
  geom_segment(data=seg_df4, aes(x, y, xend=xend, yend=yend), colour = "red", size = 2) +
  geom_point(shape = 20, size = 5, stroke=1) +
  scale_x_continuous(limits=c(-1.5, 1.5)) + 
  scale_y_continuous(limits=c(-1.5, 1.5)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 1))

f5 <- ggplot(dta, aes(x=xpos, y=ypos)) +
  geom_segment(data=seg_df5, aes(x, y, xend=xend, yend=yend), colour = "red", size = 2) +
  geom_point(shape = 20, size = 5, stroke=1) +
  scale_x_continuous(limits=c(-1.5, 1.5)) + 
  scale_y_continuous(limits=c(-1.5, 1.5)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 1))

f6 <- ggplot(dta, aes(x=xpos, y=ypos)) +
  geom_segment(data=seg_df6, aes(x, y, xend=xend, yend=yend), colour = "red", size = 2) +
  geom_point(shape = 20, size = 5, stroke=1) +
  scale_x_continuous(limits=c(-1.5, 1.5)) + 
  scale_y_continuous(limits=c(-1.5, 1.5)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 1))

f7 <- ggplot(dta, aes(x=xpos, y=ypos)) +
  geom_segment(data=seg_df7, aes(x, y, xend=xend, yend=yend), colour = "red", size = 2) +
  geom_point(shape = 20, size = 5, stroke=1) +
  scale_x_continuous(limits=c(-1.5, 1.5)) + 
  scale_y_continuous(limits=c(-1.5, 1.5)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 1))

f8 <- ggplot(dta, aes(x=xpos, y=ypos)) +
  geom_segment(data=seg_df8, aes(x, y, xend=xend, yend=yend), colour = "red", size = 2) +
  geom_point(shape = 20, size = 5, stroke=1) +
  scale_x_continuous(limits=c(-1.5, 1.5)) + 
  scale_y_continuous(limits=c(-1.5, 1.5)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 1))

f9 <- ggplot(dta, aes(x=xpos, y=ypos)) +
  geom_segment(data=seg_df9, aes(x, y, xend=xend, yend=yend), colour = "red", size = 2) +
  geom_point(shape = 20, size = 5, stroke=1) +
  scale_x_continuous(limits=c(-1.5, 1.5)) + 
  scale_y_continuous(limits=c(-1.5, 1.5)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 1))

f10 <- ggplot(dta, aes(x=xpos, y=ypos)) +
  geom_segment(data=seg_df10, aes(x, y, xend=xend, yend=yend), colour = "red", size = 2) +
  geom_point(shape = 20, size = 5, stroke=1) +
  scale_x_continuous(limits=c(-1.5, 1.5)) + 
  scale_y_continuous(limits=c(-1.5, 1.5)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 1))


grid.arrange(f1, f2, f3, f4, f5,
             f6, f7, f8, f9, f10, ncol=5)  

####################################################
x<-c(6,8,8,10)
y<-c(8,8,0,8)
dta_HH <- cbind(x, y)
dta_HH <- as.data.frame(dta_HH)

seg_dtaHH <- data.frame(x=c(6, 8),
                     y=c(8, 8),
                     xend=c(10, 8),
                     yend=c(8, 0))

fHH <- ggplot(dta_HH, aes(x=x, y=y)) +
  geom_segment(data=seg_dtaHH, aes(x, y, xend=xend, yend=yend), size = 5) +
  geom_point(shape = 15, size = 4.7) +
  scale_x_continuous(limits=c(-4, 20)) + 
  scale_y_continuous(limits=c(-2, 10)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 0.5))

#
x<-c(6,8,8,10)
y<-c(8,8,4,8)
dta_HX <- cbind(x, y)
dta_HX <- as.data.frame(dta_HX)

seg_dtaHX <- data.frame(x=c(6, 8),
                       y=c(8, 8),
                       xend=c(10, 8),
                       yend=c(8, 4))

fHX <- ggplot(dta_HX, aes(x=x, y=y)) +
  geom_segment(data=seg_dtaHX, aes(x, y, xend=xend, yend=yend), size = 5) +
  geom_point(shape = 15, size = 4.7) +
  scale_x_continuous(limits=c(-4, 20)) + 
  scale_y_continuous(limits=c(-2, 10)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 0.5))

#
x<-c(6,8,6,10)
y<-c(8,8,0,8)
dta_XH <- cbind(x, y)
dta_XH <- as.data.frame(dta_XH)

seg_dtaXH <- data.frame(x=c(6, 6),
                        y=c(8, 8),
                        xend=c(10, 6),
                        yend=c(8, 0))

fXH <- ggplot(dta_XH, aes(x=x, y=y)) +
  geom_segment(data=seg_dtaXH, aes(x, y, xend=xend, yend=yend), size = 5) +
  geom_point(shape = 15, size = 4.7) +
  scale_x_continuous(limits=c(-4, 20)) + 
  scale_y_continuous(limits=c(-2, 10)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 0.5))

#
x<-c(6,8,6,10)
y<-c(8,8,4,8)
dta_XX <- cbind(x, y)
dta_XX <- as.data.frame(dta_XX)

seg_dtaXX <- data.frame(x=c(6, 6),
                        y=c(8, 8),
                        xend=c(10, 6),
                        yend=c(8, 4))

fXX <- ggplot(dta_XX, aes(x=x, y=y)) +
  geom_segment(data=seg_dtaXX, aes(x, y, xend=xend, yend=yend), size = 5) +
  geom_point(shape = 15, size = 4.7) +
  scale_x_continuous(limits=c(-4, 20)) + 
  scale_y_continuous(limits=c(-2, 10)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 0.5))

#
grid.arrange(fHH, fXH, fHX, fXX, ncol=2)

#
x<-c(6,8,6,10)
y<-c(8,8,2,8)
dta_XL <- cbind(x, y)
dta_XL <- as.data.frame(dta_XL)

seg_dtaXL <- data.frame(x=c(6, 6),
                        y=c(8, 8),
                        xend=c(10, 6),
                        yend=c(8, 2))

fXL <- ggplot(dta_XL, aes(x=x, y=y)) +
  geom_segment(data=seg_dtaXL, aes(x, y, xend=xend, yend=yend), size = 5) +
  geom_point(shape = 15, size = 4.7) +
  scale_x_continuous(limits=c(-4, 20)) + 
  scale_y_continuous(limits=c(-2, 10)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 0.5))

#
x<-c(6,7,7,10)
y<-c(8,8,4,8)
dta_LX <- cbind(x, y)
dta_LX <- as.data.frame(dta_LX)

seg_dtaLX <- data.frame(x=c(7, 7),
                        y=c(8, 8),
                        xend=c(10, 7),
                        yend=c(8, 4))

fLX <- ggplot(dta_LX, aes(x=x, y=y)) +
  geom_segment(data=seg_dtaLX, aes(x, y, xend=xend, yend=yend), size = 5) +
  geom_point(shape = 15, size = 4.7) +
  scale_x_continuous(limits=c(-4, 20)) + 
  scale_y_continuous(limits=c(-2, 10)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 0.5))

#
x<-c(6,7,7,10)
y<-c(8,8,0,8)
dta_LH <- cbind(x, y)
dta_LH <- as.data.frame(dta_LH)

seg_dtaLH <- data.frame(x=c(7, 7),
                        y=c(8, 8),
                        xend=c(10, 7),
                        yend=c(8, 0))

fLH <- ggplot(dta_LH, aes(x=x, y=y)) +
  geom_segment(data=seg_dtaLH, aes(x, y, xend=xend, yend=yend), size = 5) +
  geom_point(shape = 15, size = 4.7) +
  scale_x_continuous(limits=c(-4, 20)) + 
  scale_y_continuous(limits=c(-2, 10)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 0.5))

#
x<-c(6,8,8,10)
y<-c(8,8,2,8)
dta_HL <- cbind(x, y)
dta_HL <- as.data.frame(dta_HL)

seg_dtaHL <- data.frame(x=c(6, 8),
                        y=c(8, 8),
                        xend=c(10, 8),
                        yend=c(8, 2))

fHL <- ggplot(dta_HL, aes(x=x, y=y)) +
  geom_segment(data=seg_dtaHL, aes(x, y, xend=xend, yend=yend), size = 5) +
  geom_point(shape = 15, size = 4.7) +
  scale_x_continuous(limits=c(-4, 20)) + 
  scale_y_continuous(limits=c(-2, 10)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 0.5))

#
x<-c(6,7,7,10)
y<-c(8,8,2,8)
dta_LL <- cbind(x, y)
dta_LL <- as.data.frame(dta_LL)

seg_dtaLL <- data.frame(x=c(7, 7),
                        y=c(8, 8),
                        xend=c(10, 7),
                        yend=c(8, 2))

fLL <- ggplot(dta_LL, aes(x=x, y=y)) +
  geom_segment(data=seg_dtaLL, aes(x, y, xend=xend, yend=yend), size = 5) +
  geom_point(shape = 15, size = 4.7) +
  scale_x_continuous(limits=c(-4, 20)) + 
  scale_y_continuous(limits=c(-2, 10)) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 0.5))

grid.arrange(fHH, fLH, fXH,
             fHL, fLL, fXL,
             fHX, fLX, fXX, ncol=3)

