# Consideracoes iniciais:
# 1. Foi programado para rodar uns comandos no terminal linux,
#    portanto nao sei se funciona em windows.
# 2. Abra o jogo e em opcoes desabilite fullscreen.
# 3. Feche o jogo e rode o script abaixo.

library(rMouse)
library(tesseract)

# abrir o jogo no modo janela
system('steam steam://rungameid/257350')

# iniciar contrucao do personagem
reroll_1 <- coord() # definir a posicao reroll
reroll_2 <- coord()
store <- coord() # definir a posicao store

# funcao para dar roll
get_roll <- function(reroll_1, reroll_2) {
  x <- trunc(runif(1, reroll_1$x, reroll_2$x))
  y <- trunc(runif(1, reroll_1$y, reroll_2$y))
  move(x, y)
  left()
}

# funcao para pegar atributo
get_atr <- function() {
  
  move(506, 644)
  for (j in 1:10) left()
  move(463, 592)
  for (j in 1:10) left()
  
  get_print()
  
  atual <- ocr('totalroll.png') # tomar o texto.
  atual <- gsub('\\D+', '', atual) # tomar apenas numeros.
  atual <- as.numeric(atual) # converter para numero. se nao der entao vira NA.
  atual <- ifelse(is.na(atual), 0, atual) # se nao conseguir entao aplicar zero.
  
  forca <- ocr('totalforca.png') # tomar o texto.
  forca <- gsub('\\D+', '', forca) # tomar apenas numeros.
  forca <- as.numeric(forca) # converter para numero. se nao der entao vira NA.
  forca <- ifelse(is.na(forca), 0, forca) # se nao conseguir entao aplicar zero.
  
  return(c(atual, forca))
  
}

# funcao para armazenar
get_store <- function() {
  move(store$x, store$y)
  left()
}

# funcao pegar print e recortar
get_print <- function() {
  system('rm *.png')
  # tomar um print da tela
  system('import -window root screenshot.png')
  # recortar o total roll
  system('convert screenshot.png -crop 75x20+350+898 -set colorspace Gray -separate -average -negate totalroll.png')
  # recortar o total forca
  system('convert screenshot.png -crop 75x20+361+585 -set colorspace Gray -separate -average -negate totalforca.png')
}

# quando chegar em habilidades ------------------------------------------------

db <- NULL
maior_atual <- 0
maior_forca <- 0
for (i in 1:1000) {
  
  res <- get_atr() # pegar valor do atributo
  atual <- res[1]
  forca <- ifelse(res[2] > 1899 | res[2] == 1800, 2999, res[2])
  
  if (maior_atual <= atual & maior_forca < forca) {
    maior_atual <- atual
    maior_forca <- forca
    cat('\nNovo registro')
    get_store()
  } else {
    get_roll(reroll_1, reroll_2)
  }
  
  cat('\nmaior atr:', maior_atual, 
      '\natr sort :', atual, 
      '\nmaior força:', maior_forca, 
      '\nforça sort :', forca, 
      '\ni:', i, '\n')
  db <- rbind.data.frame(
    db,
    cbind.data.frame(
      'total_roll' = atual,
      'atributo' = forca
    )
  )
  
}


# distribuicao ----------------------------------------------------------------

db$total_roll <- ifelse(db$total_roll == 0, 77, db$total_roll)

hist(db$total_roll, probability = T)
library(fitdistrplus)
fit <- fitdist(db$total_roll, distr = 'exp')
summary(fit)
gofstat(fit)
plot(fit)
barplot(table(db$total_roll))
