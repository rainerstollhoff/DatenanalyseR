# Mit einem Hashtag kann man Kommentare einfügen

## R Skript für das Komponentenmodell
## R. Stollhoff, Datum,....

# Der Datensatz CO2
co2

# eine Beschreibung mit der Hilfefunktion
?co2

# Der Datentyp von co2
typeof(co2)

# erstellen einer Zeitreihe mit den Daten aus co2
ts(co2,start=1959,frequency = 12)

# direkte Übergabe an die plot() Funktion
plot(ts(co2,start=1959,frequency = 12))

# für die weitere Verarbeitung legen wir eine neue Variable an
co2_ts <- ts(co2,start=1959,frequency = 12)
plot(co2_ts) # nichts passiert

# Nur 4 Werte pro Jahr anzeigen
window(co2_ts,frequency=4)

# Nur 2 Werte pro Jahr anzeigen und mit lines dem bestehenden Plot hinzufügen
lines(window(co2_ts,frequency=2),col=2)

# Holt-Winters-Modell schätzen, plotten und vorhersagen
co2_HW <- HoltWinters(co2_ts)
plot(co2_HW)
predict(co2_HW)

# den Graphenbereich mit xlim und ylim erweitern
plot(co2_HW,xlim = c(1959,2010),ylim=c(300,400))
# Vorhersage für die nächsten zehn Jahre hinzufügen
lines(predict(co2_HW,n.ahead = 120),col="green")



