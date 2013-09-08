Beispielanwendung für die LRC
=============================

In diesem Ordner werden alle notwendigen Dateien für die Erstellung einer 
Beispielanwendung zur Verfügung gestellt.

**Inhaltsübersicht:**    

1. example.exe    
2. Lanugages    


example.exe
-----------

Die Beispielanwendung *example.exe* soll demonstrieren, wie mit den LazarusRegistryControls (LRC), auf 
einfache Art und Weise ein Zugriff auf die Registry hergestellt werden kann, ohne dabei
viel Code schreiben zu müssen (möglichst keinen).    
Die notwendigen Beispieleinträge in der Registry werden über die Funktion *Datei\Beispieleinträge erstellen*
angelegt und können über die Funktion *Datei\Beispieleinträge entfernen* entfernt werden.

Languages
---------

Das Unterverzeichnis *Languages* enhält Übersetzungsdateien *`*.po`-Dateien*, mit denen
die Resourcestrings der Beispielanwendung angepasst werden können. Dabei wird ein 
besonderes Augenmerk auf die Maske *TEditSettings* gesetzt, die nich direkt Teil
der Beispielanwendung ist, sondern über die Komponenten zu Verfügung gestellt wird.