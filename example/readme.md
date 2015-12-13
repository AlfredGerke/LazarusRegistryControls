Beispielanwendung für die LRC
=============================

In diesem Ordner werden alle notwendigen Dateien für die Erstellung einer 
Beispielanwendung zur Verfügung gestellt.

**Inhaltsübersicht:**    

0. v0.9-M1 (unbedingt beachten!)
1. example.exe    
2. Lanugages    

v0.9-M1
-------
Die Sourcen dieser Beispielanwendung sind nur mit dem Pre-release *v0.9-M1* lauffähig.
Mit der Version *v0.9-M2* wurden grundlegende Änderungen eingeführt die in dieser Beispielanwednung
nicht Berücksichtigt werden. Es sind auch keine Anpassungen an diese Beispielanwendung vorgesehen.
Für die Version *v0.9-M2* wird eine komplett neue Beispielanwendung eingeführt.

* [s. readme.md aus example v0.9-M.2](../example v0.9-M.2/readme.md "readme.md")

example.exe
-----------
Die Beispielanwendung *example.exe* soll demonstrieren, wie mit den LazarusRegistryControls (LRC), auf 
einfache Art und Weise ein Zugriff auf die Registry hergestellt werden kann, ohne dabei
viel Code schreiben zu müssen (möglichst keinen).    

Die notwendigen Beispieleinträge in der Registry werden über die Funktion *Datei\Beispieleinträge erstellen*
angelegt und können über die Funktion *Datei\Beispieleinträge entfernen* entfernt werden.     
Das Beispiel ermöglicht das Analgen von Standards in `HKEY_LOCAL_MACHINE`. Zu diesem
Zweck wurde für Windowsplattformen > **WinXP** für die UAC die höchst verfügbare Ausfürhungsebenene gewählt.
Die Anwendung verlangt beim Start eine Autorisierung durch den Aufrufer.

* [s. Lazarus Wiki](http://wiki.lazarus.freepascal.org/GDB_Debugger_Tips#Debugging_applications_with_Administrative_privileges "Lazarus Wiki") 


Languages
---------
Das Unterverzeichnis *Languages* enhält Übersetzungsdateien *`*.po`-Dateien*, mit denen
die Resourcestrings der Beispielanwendung angepasst werden können. Dabei wird ein 
besonderes Augenmerk auf die Maske *TEditSettings* gesetzt, die nich direkt Teil
der Beispielanwendung ist, sondern über die Komponenten zu Verfügung gestellt wird.