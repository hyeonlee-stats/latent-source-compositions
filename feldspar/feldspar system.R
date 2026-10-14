fp.color = 1:8
fp.shape = 1:8

## straights
df = rbind(c(0, 0.9, 0.1, 0.9*0.7, 0.9*0.3, 0.1),
           c(0.7, 0.3, 0, 0.9*0.7, 0.9*0.3, 0.1),
           c(0.5, 0.5, 0, 0.9*0.5, 0.9*0.5, 0.1),
           c(0.3, 0.7, 0, 0.9*0.3, 0.9*0.7, 0.1),
           c(0.1, 0.9, 0, 0.9*0.1, 0.9*0.9, 0.1),
           c(0, 0.1, 0.9, 0.58, 0.1, 0.32),
           c(0.63, 0, 0.37, 0.58, 0.1, 0.32),
           c(0.9, 0.1, 0, 0.8, 0.1, 0.1),
           c(0.9, 0, 0.1, 0.8, 0.1, 0.1),
           c(0.8, 0.1, 0.1, 0.65, 0.2, 0.15))
colnames(df) = c('al', 'an', 'or', 'alend', 'anend', 'orend')

## curve
start   <- c(0.58, 0.1, 0.32)
end     <- c(0.63, 0.27, 0.1)
control <- c(0.7, 0.13, 0.17)

t <- seq(0, 1, length.out = 100)
curve_df <- as.data.frame(
  outer((1 - t)^2, start) +
    outer(2 * (1 - t) * t, control) +
    outer(t^2, end)
)
names(curve_df) <- c('al','an','or')

## text
df_text = as.data.frame(rbind(c('Sanidine',0.25, 0.05, 0.7),
                c('Anorthoclase',0.75, 0.05, 0.2),
                c('Albite', 0.9, 0.05, 0.05),
                c('Oligoclase', 0.75, 0.2, 0.05),
                c('Andesine', 0.575, 0.375, 0.05),
                c('Labradorite', 0.375, 0.575, 0.05),
                c('Bytownite', 0.2, 0.75, 0.05),
                c('Anorthite', 0.05, 0.9, 0.05)))
df_text[,2] = as.numeric(df_text[,2])
df_text[,3] = as.numeric(df_text[,3])
df_text[,4] = as.numeric(df_text[,4])
colnames(df_text) = c('label','al','an','or')

p1 = ggtern(df, aes(x=al, y=or, z=an)) +
  geom_segment(aes(xend = alend, yend = orend, zend = anend),
               linewidth = 1, col = 'gray') +
  geom_path(data = curve_df,
    mapping = aes(x = al, y = or, z = an),
    inherit.aes = FALSE,
    linewidth = 1, col = 'gray') +
  geom_text(data = df_text, mapping = aes(label = label, x= al, y = or, z=an),
            position = "identity",
            col = fp.color,
            angle = c(60,60,0,0,0,0,0,0),
            size = 3.5) +
  labs(x = expression(atop('Albite','(NaAlSi3O8)')),
       y = expression(atop('Orthoclase','(KAlSi3O8)')),
       z = expression(atop('Anorthite','(CaAl2Si2O8)'))) +
  annotate('text', x = 0.25, y = 0.4, z = 0.35,
           label = 'Miscibility gap\n(no felspars with\nthese compositions)',
           col = 'gray', size = 3.5) +
  theme_nomask() +
  theme(tern.axis.title.L = element_text(hjust = 0.15, vjust = 1),
        tern.axis.title.R = element_text(hjust = 0.85, vjust = 1))

saveRDS(p1, 'feldspar/feldspar_system.RDS')

