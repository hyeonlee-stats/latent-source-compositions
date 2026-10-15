# ==== read data ====
# blt.source contains source lithology of experiments.
# blt.melt contains the chemical compositions of basaltic melts of the above
#     source lithologies under various environmental factors.
# blt.source$type: type from source lithology
# blt.melt$type.yang: type inferred by Yang et al. (2019)

blt.melt = read.csv('basalt/yang 2019 basalt melt labels.csv', strip.white = T) %>%
  dplyr::rename(type.yang = Type) %>%
  mutate(type.yang = factor(type.yang,
                            levels = c('mafic','transitional','peridotite','harburgite',
                                       'carbonated mafic','carbonated transitional','carbonated peridotite')))
blt.source = read.csv('basalt/yang 2019 basalt source labels.csv', strip.white = T)

# ==== assign source type ====
# map lithology to type based on Yang et al. (2019) paper
mapping <- c(
  "Lherzolite"                     = "Peridotite",
  "Harburgite"                     = "Harzburgite",
  "Olivine websterite"             = "Peridotite",
  "lherzolite"                     = "Peridotite",
  "Olivine pyroxenite"             = "Mafic",
  "Garnet pyroxenite"              = "Mafic",
  "Garnet websterite"              = "Mafic",
  "Garnet-olivine clinopyroxenite" = "Mafic",
  "Silica-excess pyroxenite"       = "Mafic",
  "Bimineralic eclogite"           = "Mafic",
  "Olivive Hb Clino"               = "Mafic",
  "Silica-deficient pyroxenite"    = "Mafic",
  "Hornblendite"                   = "Mafic",
  "Clinopyroxene-hornblendite"     = "Mafic",
  "Carbonated eclogite"            = "Mafic"
)

source2type <- data.frame(
  Lithology = names(mapping),
  type = unname(mapping)
)

blt.source = blt.source %>%
  left_join(source2type, by = 'Lithology') %>%
  mutate(carbonated = sapply(Volatiles, function(x){ifelse('C' %in% strsplit(x, ', ')[[1]], 'Carbonated ','')}),
         type = factor(paste0(carbonated, type),
                       levels = c('Mafic','Peridotite','Harzburgite',
                                  'Carbonated Mafic','Carbonated Peridotite'))) %>%
  select(References, Source.composition, type) %>%
  unique()

blt.melt = left_join(blt.melt, blt.source,
                     by = c('References','Source.composition'), relationship = 'many-to-many')

# check how many melts are not mapped to source
setdiff(blt.melt$Source.composition, blt.source$Source.composition)
setdiff(blt.melt$References, blt.source$References)

# check if mapping was compatible with Yang et al. (2019)
table(blt.melt$type.yang, blt.melt$type)

# ==== remove missing/outliers ====
blt.melt$Cr2O3[is.na(blt.melt$Cr2O3)] = 0
blt.melt = blt.melt[blt.melt$P2O5 < 10,]

# ==== extract melt composition as matrix ====
# sort variables in decreasing mean proportion
oxide.mass = c(60.08, 79.87, 101.96, 151.99, 71.84, 70.94, 40.30, 56.08, 61.98, 94.20, 74.69, 141.94)
#               SiO2   TiO2   Al2O3   Cr2O3   FeO    MnO    MgO    CaO    Na2O   K2O    NiO    P2O5
cation.mass = c(28.09, 47.87, 53.96, 103.99, 55.85, 54.94, 24.31, 40.08, 45.98, 78.20, 58.69, 61.95)
#               Si     Ti     Al2     Cr2     Fe     Mn     Mg     Ca     Na2    K2     Ni     P2
varnames = setNames(c("Si",  "Ti",  "Al",   "Cr",   "Fe", "Mn", "Mg", "Ca", "Na",  "K",  "Ni", "P"),
                    c("SiO2","TiO2","Al2O3","Cr2O3","FeO","MnO","MgO","CaO","Na2O","K2O","NiO","P2O5"))

blt.X = as.matrix(blt.melt[,8:19]) %>%
  sweep(2, cation.mass/oxide.mass, '*') %>%
  sweep(1, rowSums(.), '/')
colnames(blt.X) = varnames[colnames(blt.X)]

# reorder elements by mean proportion in descending order
var_order = blt.X %>%
  colMeans() %>%
  sort(decreasing = T) %>%
  names()
blt.X = blt.X[,var_order]
blt.X = blt.X[,c('Si','Mg','Ca','Al','Fe','Na','K','Ti')]

blt.type = blt.melt$type

saveRDS(blt.X, 'basalt/blt.X.RDS')
saveRDS(blt.type, 'basalt/blt.type.RDS')
