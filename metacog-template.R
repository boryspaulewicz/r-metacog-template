## Skrypt do analizy danych z decyzjami pierwszego i drugiego stopnia
## (metapoznawczymi).

## Ten pakiet służy do dopasowywania modeli mieszanych
library(lme4)
## Ten pakiet radzi sobie z improtowaniem danych z wielu formatów:
library(rio)
## A ten daje łasne rysunki:
library(ggplot2)

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
d = d[d$macc > .6,] ## dosyć arbitralny próg, warto przemyśleć

## Sprawdzamy, czy warunki, które nie powinny się pod tym względem
## różnić, nie różnią się pod względem biasu (kryterium w znaczeniu
## SDT) [nie jest oczywiste, że time:order tutaj pokazuje różnice w
## kryterium]:
summary(glmer(acc ~ -1 + time / (ratingd * order) + (1|id), d, family = binomial))
##! UWAGA, model się nie zbiegł, spróbujemy to wymusić:
summary(glmer(acc ~ -1 + time / (ratingd * order) + (1|id), d, family = binomial,
              control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))))

## Sprawdzamy, czy niektóre warunki różnią się poprawnością:
summary(glmer(acc ~ -1 + time / order + (1|id), d, family = binomial,
              control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))))

## Dopasowujemy podstawowy model, na początek w najbardziej czytelnej
## parametryzacji, to znaczy, wszystkie interesujące nas efekty są
## zagnieżdżone w efektach, które nas mniej interesują:
summary((m = glmer(acc ~ -1 + time / (ratingd * order) + (1|id), d, family = binomial,
              control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))))

## Próbujemy dopasować pełny ("najuczciwszy") model z wszystkimi
## możliwymi efektami losowymi
summary((mf = glmer(acc ~ -1 + time / (ratingd * order) + (-1 + time / ratingd |id), d, family = binomial,
              control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))))
