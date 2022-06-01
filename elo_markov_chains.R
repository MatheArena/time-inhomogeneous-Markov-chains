#### elo.R ####
#
# Elo-Simulator of skill ranking numbers
#
###########################################
#
# Authors: Florian Sobieczky - Eva-Maria Infanger - Gerald Infanger - Mai, 2022
#
# Usage: 
#  M <- do_elo(80)   .... für 80 Zeitschritte, oder:
#  M <- do_elo(50, m=10)  .... für 4 Aufgaben oder:
#  M <- do_elo(80, exlevel=1000) .... für Anfangswert (von allen)=1000, oder
#  M <- do_elo(80, KE_Faktor=0) .... ohne Konvergenzerzeugenden Faktor
#  M <- do_elo(80, ZSZ=5) ... 5 'Zeitschritte Zurück' für Running Mean.
#
# Wenn ggplot2 als library installiert ist, das folgende auskommentieren,
# library(ggplot2)
# und mit nice_plot(M) den schönen Plot des Ergebnisses genießen!
#
# In Arbeite: Eine Routine 'do_elo_stats()' in der die Verteilung der
# Ränge im n-ten Zeitschritt ausgewertet wird. Kann man dann für die
# Optimierung der Kombination KE_Faktor+ZSZ Wert benutzen.
###################################################################

library(ggplot2)

F <- function(x, beta) 1/(1 + 10^(-beta*x))   # Sigmoid

do_elo <- function(n, m=5, beta=0.1, k=1, e=1, exlevel0=7, verbose=TRUE, title='Elo-Numbers (Magenta: Learner)',  EloTarget=5, EA_echt=rep(0.7,m), seed=0, ZSZ=10, KE_Faktor=0, returnW=TRUE) {
    if(seed==0)
        random_seed()    # Setzen einer zufälligen Saat.
    else
        set.seed(seed)   # Setzen der gleichen Saat, für Vergleichbarkeit
    
    
    v <- rep(exlevel0,m)      # Vektor mit Elo-Zahlen (initial).
    v[1] <- 7                 # Erster Eintrag ist Lernende(r)/Spieler(in)
    EA_echt[1] <- 0.3         # Das ist die eine schwierige Aufgabe.
                              # Die wird im Mittel nur in 30% der Fälle gelöst.

    k0 <- k
    S <- rep(0, m) # This holds the empirical distribution
    
    M <- matrix(rep(0, m*n), ncol=m)# Hier werden dann Elo-Vektoren für jede Zeit abgesp.
    W <- matrix(rep(0, m*n), ncol=m)# Hier werden dann Elo-Vektoren für jede Zeit abgesp.    
    M[1,] <- v                      # Start-verteilung.
    W[1,] <- v
    
    for(t in 2:n) {                 # Diskrete Zeitvariable hier: t.
        i<-1                        # Hier ursprünglich noch Schleife, aber

        for( j in (i+1):m ) {       # die 'anderen Spieler' treten nicht gegeneinander
                                    # an, weil sie 'Aufgaben' sind.
            EA <- F(v[i]-v[j], beta) # Prob zu 'gewinnen' Spieler A.
            EB <- F(v[j]-v[i], beta) # Prob zu 'gewinnen' Spieler A.

            x <- runif(1)           # Der Zufallsgenerator (Saat oben gesetzt!)

            #            if(x < EA) {            # Entscheid über Erfolg.

            if(x < EA_echt[j-1]) {            # Entscheid über Erfolg.
                SA <- 1
                SB <- 0
            }
            else {                  # Entscheid über Misserfolg.
                SA <- 0
                SB <- 1
            }

	    if(KE_Faktor==0)    # Hier sind die verschiedenen Konvergenz
	       k<-1             # erzeugenden Faktoren: 0 ist Konstant,
	    if(KE_Faktor==1)
	       k <- 4/(1+1*t)	   # 1 ist mit O(t^-1) abfallend,
	    if(KE_Faktor==2)
               k <- exp(-0.06*t)   # 2 ist exponentiell abfallend,
	    if(KE_Faktor==3)
	       k <- 2/(log(1+t)) # 3 ist logarithmisch abfallend.

            v[i] <- v[i] + k*(SA - EA) # Elo-Regel zur Adaption der Elozahlen.
            v[j] <- v[j] + k*(SB - EB)

            M[t, i] <- v[i]         # Abspeichern des aktuellen Resultats
            M[t, j] <- v[j]

            W[t, i] <- mean(M[max(1,(t-ZSZ)):t, i])   # Abspeichern der Running Means
            W[t, j] <- mean(M[max(1,(t-ZSZ)):t, j])   # Das max sichert nur, dass kein Wert unter eins als Index genommen wird.
        }
      
    }

    A <- as.data.frame(M)
    names(A)[1] <- 'Emmy'
    names(A)[2:m] <- paste0('ItemLevel_',1:(m-1))        
    A$time <- 1:(dim(A)[1])


    if(verbose==TRUE) {             # Plotten wenn gewünscht ....
        plot(M[,1], col='magenta', lwd=2, ylab='Elo', ylim=c(4,12), t='l', xlab='Time Step n', main=title)
        
        for(j in 2:m)
            lines(M[,j], col=j, lwd=3)
        
        lines(W[,1], col='magenta', lwd=5, ylab='Elo', ylim=c(0,36), t='l', xlab='Time Step n', main=title)
        for(j in 2:m)
            lines(W[,j], col=j, lwd=10)
        
        grid()
    }

    if(returnW==TRUE)  # Entweder M oder W wiedergeben.
        res <- W
    else
        res <- M

    res
}



as_A <- function(M) {
	  A <- as.data.frame(M)
	  m <- dim(A)[2]
	  n <- dim(A)[1]
	  
	  names(A)[1] <- 'Emmy'
    	  names(A)[2:m] <- paste0('ItemLevel_',1:(m-1))        
          A$time <- 1:(dim(A)[1])

	  A
}

nice_plot <- function(M) {
    A<- as_A(M)

    p <-  ggplot(A) + aes(x=time, y=Emmy) + geom_line() + geom_smooth() + ylim(1,17) + xlab('number of solved tasks') + ylab('Elo Ranking') + geom_line(aes(x=time, y=ItemLevel_1, col='ItemLevel_1')) + geom_line(aes(x=time, y=ItemLevel_2, col='ItemLevel_2')) + geom_line(aes(x=time, y=ItemLevel_3, col='ItemLevel_3')) + geom_line(aes(x=time, y=ItemLevel_4, col='ItemLevel_4')) + ggtitle("Elo - Ranking for Users and Tasks")

    p
}



                                    # Statistik für mehrere Durchänge.
                                    # Hier werden in N Iteration Statistiken
                                    # der Elo-Zahlen nach n Zeitschritten
                                    # berechnet. 
do_elo_stats <- function(N=100, n=80, m=10, beta=0.1, k=1, exlevel0=5, verbose=TRUE, KE_Faktor=0, ZSZ=10, true_values=c(8, 12, 4.5, 4.5, 4.5)) {

    FV <- matrix(rep(0, N*m ), ncol=m) # Final Value Matrix

    for(k in 1:N){
        W <- do_elo(n, m=5, seed=k, returnW=TRUE, KE_Faktor=KE_Faktor, ZSZ=ZSZ)
        FV[k,] <- W[n,]   # Last Line of M is saved into FV[k,1:m]
    }

    A <- as_A(FV)   # Turns the matrix into a data.frame.
                   # This is just like a matrix, except that you can
                   # reach columns with the dollar symbol: A$Aufgabe_2
                   # is the third (sic!) column, if names(A)[3] = 'Augabe_2'.
                   # See as_A() how A is defined.
                   # I am using it because some plot commands want dafaframes
                   # instead of Matrices....

    if(verbose==TRUE) {  # in this case show graphics
      
       par(mfrow=c(2,1)) # 2 x 2 Graphics window
        boxplot(data.frame(A[,1:5]), main='Elo-Numbers', ylim=c(1,16));grid()
        abline(h=true_values[1:3], col=c('magenta', rgb(1,0,0,0.5),rgb(0,1,0,0.5),rgb(0,0,1,0.5)))
#       boxplot(data.frame(result_ItemLevel1, result_ItemLevel2), main='Mittl. Aufgabenniveau');grid()
       h0 <- hist(A$Emmy, 30, plot=FALSE)
       h1 <- hist(A$ItemLevel_1, 30, plot=FALSE)
       h2 <- hist(A$ItemLevel_2, 30, plot=FALSE)
       h3 <- hist(A$ItemLevel_3, 30, plot=FALSE)
       h4 <- hist(A$ItemLevel_4, 30, plot=FALSE)
       plot(h0, xlim=c(4,12), col='magenta', main='Histogram of Final Values Emmy & Exercises', xlab='Elo-Rank')
       plot(h1, add=TRUE, col=rgb(1,0,0,0.5), xlim=c(2,16))
       plot(h2, add=TRUE, col=rgb(0,1,0,0.5), xlim=c(2,16))
       plot(h3, add=TRUE, col=rgb(0,0,1,0.5), xlim=c(2,16))
       plot(h4, add=TRUE, col=rgb(0.3,0.3,0,0.5), xlim=c(2,16))

        
    }			   

    print( c(sd(A$Emmy), sqrt(mean((A$Emmy-true_values[1])^2)), sd(A$ItemLevel_1), sqrt(mean((A$ItemLevel_2-true_values[3])^2)),  sd(A$ItemLevel_2), sqrt(mean((A$ItemLevel_2-true_values[3])^2)), sd(A$ItemLevel_3), sqrt(mean((A$ItemLevel_3-true_values[4])^2)), sd(A$ItemLevel_4), sqrt(mean((A$ItemLevel_4-true_values[5])^2))))

    A        
}

nice_stats_plot <- function(A, KE_Faktor=0, png=FALSE) {
    B <- as.data.frame(c(A$Emmy, A$ItemLevel_1, A$ItemLevel_2, A$ItemLevel_3, A$ItemLevel_4 ))
    B$User <- c(rep('Emmy', 100), rep('ItemLevel_1', 100), rep('ItemLevel_2', 100), rep('ItemLevel_3', 100), rep('ItemLevel_4', 100))
    names(B)<-c('Elo', 'UserOrExercise')

    if(KE_Faktor==0){
        if(png==TRUE)
            png(filename('const_plot.png'))
        readline()
        ggplot(B, aes(x=UserOrExercise, y=Elo)) + geom_violin(trim=TRUE)  + ggtitle('Elo Ranking after n=80 timesteps with a constant learning rate') + geom_boxplot(width=0.1, col=c('black', 'red', 'cyan', 'green', 'magenta'))
        readline()
        if(png==TRUE)
           dev.off()
    }     
    if(KE_Faktor==3){
        if(png==TRUE)
            png(filename('log_plot.png'))
        ggplot(B, aes(x=UserOrExercise, y=Elo)) + geom_violin(trim=TRUE)  + ggtitle('Elo Ranking after n=80 timesteps with a logarithmic learning rate') + geom_boxplot(width=0.1, col=c('black', 'red', 'cyan', 'green', 'magenta'))
        if(png==TRUE)
            dev.off()                 
    }
}


random_seed <- function(){
    set.seed(strsplit(strsplit(as.character(Sys.time()),' ')[[1]][2],':')[[1]][3])
}

pick_nice <- function() {

}
