## Skrypt do analizy danych z decyzjami pierwszego i drugiego stopnia
## (metapoznawczymi).

## Ten pakiet radzi sobie z improtowaniem danych z wielu formatów:
library(rio)

## Najpierw patrzymy, czy to są nasze dane:
str(d)
head(d)

## Potem patrzymy, czy zgadzają się liczby prób u poszczególnych osób
## (np. czasem ten sam identyfikator pojawia się dwa razy):
res = aggregate(time ~ id, d, length)
plot(sort(res$time))
## Znajdujemy osoby do wyrzucenia:
res
## ... i ewentualnie wyrzucamy je

## Sprawdzamy, czy nie ma osób odstających pod względem poprawności
## lub użycia skali:

## ... i ewentualnie wywalamy osoby problematyczne

## Sprawdzamy, czy warunki, które nie powinny się pod tym względem
## różnić, nie różnią się pod względem biasu (kryterium w znaczeniu
## SDT):

## Sprawdzamy, czy niektóre warunki różnią się poprawnością:

## Dopasowujemy podstawowy model, na początek w najbardziej czytelnej
## parametryzacji, to znaczy, wszystkie interesujące nas efekty są
## zagnieżdżone w efektach, które nas mniej interesują:
