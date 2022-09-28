# gerar dois vetores N(0,1), independentes
v1 = rnorm(1500)
v2 = rnorm(1500)

x = v1*2
y = v1+v2

z = rep(c('a','b'), 750)

df <- data.frame(x=x,y=y,z=z)
head(df)

par(mfrow = c(1,3), par(oma=c(0,3,3,0)))
hist(df$y,probability = T, main = 'sdsd',col=3)
box()
lines(density(df$y), lwd=3, col=4)
plot(y~z, df, col = c(3,4), main='kjsf')
mtext("Worm-Plot", side=3, line=3, cex=1 )
plot(y~x,df, col=c(3,4), main='kjsf')
abline(lm(y~x, df), lwd=3)

library(ggplot2)

# Densidade
ggplot(data=df, mapping = aes(x=y))+geom_density()
ggplot(data=df, mapping = aes(x=y))+geom_density()+geom_rug()
ggplot(data=df, mapping = aes(x=x))+geom_density(col=4)+geom_rug()
ggplot(data=df, mapping = aes(x=y))+geom_density(col=4, fill=6)+geom_rug()
ggplot(data=df, mapping = aes(x=y))+geom_density(col=4, fill=6, alpha=0.3)+geom_rug()

# Histograma
ggplot(data=df, mapping = aes(x=x))+geom_histogram()
ggplot(data=df, mapping = aes(x=y))+geom_histogram(col=6, fill=6)+geom_rug()
ggplot(data=df, mapping = aes(x=y))+geom_histogram(col=6, fill=6, alpha=0.3)

# Boxplot
ggplot(data=df, mapping = aes(y=x))+geom_boxplot()
ggplot(data=df, mapping = aes(y=y))+geom_boxplot(col=3, fill=4, alpha=0.4)

# Gráficos de dispersão
ggplot(data=df, mapping = aes(x=x,y=y))+geom_point()

ggplot(data=df, mapping = aes(x=x,y=y,col=z))+geom_point()
ggplot(data=df, mapping = aes(x=x,y=y,col=z))+geom_point(alpha=0.5)

# boxplot para níveis de um fator
ggplot(data=df, mapping = aes(x=z,y=y,col=z))+geom_boxplot()

ggplot(data=df, mapping = aes(x=z,y=y,col=z))+geom_boxplot(col=c(3,4),
                                                           fill=c(3,4),
                                                           alpha=0.3)

# Eixos e título

ggplot(data=df, mapping = aes(x=x,y=y))+geom_point(alpha=0.5)+
  ggtitle('Título')+xlab('Eixo x')+ylab('Eixo y')


# facet wrap

ggplot(data=df, mapping = aes(x=x,y=y,col=z))+geom_point(alpha=0.5)+
  geom_smooth(se=F, lwd=1.5, col=1)+facet_wrap(~z)

# gráficos em painel

library(gridExtra)

g1 <- ggplot(data=df, mapping = aes(x=y))+geom_density(col=2, fill=2, alpha=0.3)
g2 <- ggplot(data=df, mapping = aes(x=y))+geom_histogram(col=6, fill=6, alpha=0.3)
g3 <- ggplot(data=df, mapping = aes(x=x,y=y,col=z))+geom_point(alpha=0.5)+geom_smooth(col=1)
g4 <- ggplot(data=df, mapping = aes(x=z,y=y,col=z))+geom_boxplot(col=c(3,4),
                                                           fill=c(3,4),
                                                           alpha=0.3)

grid.arrange(g1,g2,g3,g4,nrow=2,ncol=2)

# Temas

graf <- ggplot(data=df, mapping = aes(x=y))+
  geom_histogram(col=1)+xlab('')+
  ylab('')


a <- g4+theme_bw()+ggtitle('bw')
b <- g4+theme_classic()+ggtitle('classic')
c <- g4+theme_dark()+ggtitle('dark')
d <- g4+theme_gray()+ggtitle('gray')
e <- g4+theme_light()+ggtitle('light')
f <- g4+theme_minimal()+ggtitle('minimal')
g <- g4+theme_test()+ggtitle('test')
h <- g4+theme_void()+ggtitle('void')

grid.arrange(a,b,c,d,e,f,g,h, nrow=3,ncol=3)

# Gráficos interativos

library(plotly)
ggplotly(g1)
ggplotly(g2)
ggplotly(g3)
ggplotly(g4)
