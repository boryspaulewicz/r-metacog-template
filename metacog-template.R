## Skrypt do analizy danych z decyzjami pierwszego i drugiego stopnia
## (metapoznawczymi).

## Ten pakiet służy do dopasowywania modeli mieszanych
library(lme4)
## Ten pakiet radzi sobie z improtowaniem danych z wielu formatów:
library(rio)
## A ten daje łasne rysunki:
library(ggplot2)
library(lattice)

######################################################################
## Moje ulubione funkcje pomocnicze

aggregate.expand = function(dv, f, fun = mean){
    res = aggregate(dv ~ f, FUN = fun)
    rownames(res) = as.character(res[,1])
    return(res[as.character(f), 2])
}

load('d')

## Najpierw patrzymy, czy to są nasze dane:
str(d)
head(d)
tail(d)

## Potem patrzymy, czy zgadzają się liczby prób u poszczególnych osób
## (np. czasem ten sam identyfikator pojawia się dwa razy):
res = aggregate(acc ~ id, d, length)
plot(sort(res$acc))
## Znajdujemy osoby do wyrzucenia:
res
## ... i ewentualnie wyrzucamy je
d = d[d$id != 'Marta:PAS:DS',]

## Sprawdzamy, czy nie ma osób odstających pod względem poprawności
## lub użycia skali:
res = aggregate(acc ~ id, d, mean)
dotplot(id ~ acc, res)
res = aggregate(ratingd ~ id, d, sd)
dotplot(id ~ ratingd, res)
## ... i ewentualnie wywalamy osoby problematyczne
d$macc = aggregate.expand(d$acc, d$id)
d = d[d$macc > .6,]
## dosyć arbitralny próg, warto przemyśleć

######################################################################
## Analizy

## glmer to funkcja z pakietu lme4, która dopasowuje uogólnione
## liniowe modele mieszane.

## Tak się cudownie składa dla decyzji meta-poznawczej, że nachylenie
## ocen względem poprawności jest dokładnie równe d' tej decyzji
## (metapoznawczej), a punkt przecięcia dla tego efektu jest dokładnie
## równy kryterium.

## Sprawdzamy, czy warunki, które nie powinny się pod tym względem
## różnić, nie różnią się pod względem biasu (kryterium w znaczeniu
## SDT) [nie jest oczywiste, że time:order tutaj pokazuje różnice w
## kryterium]. Jednocześnie to jest nasz podstawowy model
summary(glmer(acc ~ -1 + time / (ratingd * order) + (1|id), d, family = binomial))
##! UWAGA, model się nie zbiegł, spróbujemy to wymusić:
summary((m = glmer(acc ~ -1 + time / (ratingd * order) + (1|id), d, family = binomial,
              control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))))

## Random effects:
##  Groups Name        Variance Std.Dev.
##  id     (Intercept) 0.7587   0.871   
## Number of obs: 2679, groups:  id, 21

## Fixed effects:
##                         Estimate Std. Error z value Pr(>|z|)    
## time160                   1.3963     0.8813   1.584  0.11311    
## time32                    1.0154     0.4178   2.430  0.01509 *  
## time48                    0.8507     0.5337   1.594  0.11094    
## time80                    0.6750     0.5478   1.232  0.21794    
## time160:ratingd           3.5218     1.1778   2.990  0.00279 ** 
## time32:ratingd            3.8120     0.8341   4.570 4.88e-06 ***
## time48:ratingd            4.6454     1.0717   4.335 1.46e-05 ***
## time80:ratingd            3.1200     0.7777   4.012 6.02e-05 ***

## Powyżej mamy dla wszystkich czasów osobno (time / cośtam) punkty
## przecięcia i nachylenia linii regresji wyrażającej efekt ratingu
## dla warunku DS (czynnik order, najpierw Decyzja, potem Skala).

## Poniżej mamy różnice SD vs DS dla punktów przecięcia (pierwsze 4
## rzędy) i nachyleń

## time160:orderSD          -0.9537     1.0761  -0.886  0.37549    
## time32:orderSD            0.2699     0.5890   0.458  0.64675    
## time48:orderSD            0.6815     0.6840   0.996  0.31908    
## time80:orderSD           -0.2463     0.7540  -0.327  0.74399    
## time160:ratingd:orderSD   0.6837     1.5019   0.455  0.64896    
## time32:ratingd:orderSD   -1.1367     1.1251  -1.010  0.31233    
## time48:ratingd:orderSD   -3.0579     1.2563  -2.434  0.01493 *  
## time80:ratingd:orderSD    1.2829     1.1884   1.080  0.28036    

## Sprawdzamy, czy niektóre warunki różnią się poprawnością:
summary(glmer(acc ~ -1 + time / order + (1|id), d, family = binomial,
              control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))))

## Fixed effects:
##                 Estimate Std. Error z value Pr(>|z|)    
## time160           3.8634     0.4682   8.251  < 2e-16 ***
## time32            2.3779     0.3715   6.400 1.55e-10 ***
## time48            2.8906     0.3930   7.354 1.92e-13 ***
## time80            2.5318     0.3769   6.718 1.85e-11 ***
## time160:orderSD  -0.6017     0.6072  -0.991    0.322    
## time32:orderSD    0.0765     0.5151   0.149    0.882    
## time48:orderSD   -0.4835     0.5300  -0.912    0.362    
## time80:orderSD    0.3745     0.5290   0.708    0.479    

## Próbujemy dopasować pełny ("najuczciwszy") model z wszystkimi
## możliwymi efektami losowymi
## summary((mf = glmer(acc ~ -1 + time / (ratingd * order) + (-1 + time / ratingd |id), d, family = binomial,
##              control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))))
## Takie wywołanie funkcji glmer powoduje, że model jest najpierw
## zapisany w zmiennej mf, a dopiero potem dostajemy z niego summary
## save(mf, file = 'mf')

## Ładuję zapisany wcześniej wolno dopasowujący się model i patrzę na
## jego dopasowanie
load('mf')
summary(mf)

######################################################################
## Wykres

## Pamiętamy wszyscy, że w regresji logistycznej nie modelujemy wcale
## średniej zmiennej zależnej, tylko funkcję logistyczną średniej zmiennej zależnej.

## Dodajemy zmienną z predykcjami dopasowanego modelu do naszego
## zbioru danych:
d$fit = fitted(m)

## Patrzymy, jak to wygląda na wykresie. Najpierw uśredniamy
## predykcje po osobo-warunkach.
res = aggregate(fit ~ id + order + time + ratingd, d, mean)
ggplot(res, aes(x = ratingd, y = fit, group = order, color = order)) + geom_line() + facet_grid(.~time)

## Wyliczamy predykcje dopasowanego modelu pomijając efekt osób, a
## właściwie po prostu uwzględniając jedynie interesujące nas efekty
## ustalone
d$fit = (model.matrix(m) %*% fixef(m))[,1]
res = aggregate(fit ~ order + time + ratingd, d, mean)
ggplot(res, aes(x = ratingd, y = fit, group = order, color = order)) + geom_line() + facet_grid(.~time)

## A tutaj pokazujemy dopasowanie na oryginalnej skali (poprawności)
ggplot(res, aes(x = ratingd, y = binomial()$linkinv(fit), group = order, color = order)) +
    geom_line() + facet_grid(.~time) + labs(y = 'Accuracy')
