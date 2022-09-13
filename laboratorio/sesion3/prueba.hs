listaInfinita = zip (concat [[0..n] | n<-[0..]]) (concat [reverse [0..n] | n<-[0..]]) 

lista = concat ([zip [0..n] (reverse [0..n])| n<-[0..]])