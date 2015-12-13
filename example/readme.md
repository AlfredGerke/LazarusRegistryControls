Beispielanwendung f�r die LRC
=============================

In diesem Ordner werden alle notwendigen Dateien f�r die Erstellung einer 
Beispielanwendung zur Verf�gung gestellt.

**Inhalts�bersicht:**    

0. v0.9-M1 (unbedingt beachten!)
1. example.exe    
2. Lanugages    

v0.9-M1
-------
Die Sourcen dieser Beispielanwendung sind nur mit dem Pre-release *v0.9-M1* lauff�hig.
Mit der Version *v0.9-M2* wurden grundlegende �nderungen eingef�hrt die in dieser Beispielanwednung
nicht Ber�cksichtigt werden. Es sind auch keine Anpassungen an diese Beispielanwendung vorgesehen.
F�r die Version *v0.9-M2* wird eine komplett neue Beispielanwendung eingef�hrt.

* [s. readme.md aus example v0.9-M.2](../example v0.9-M.2/readme.md "readme.md")

example.exe
-----------
Die Beispielanwendung *example.exe* soll demonstrieren, wie mit den LazarusRegistryControls (LRC), auf 
einfache Art und Weise ein Zugriff auf die Registry hergestellt werden kann, ohne dabei
viel Code schreiben zu m�ssen (m�glichst keinen).    

Die notwendigen Beispieleintr�ge in der Registry werden �ber die Funktion *Datei\Beispieleintr�ge erstellen*
angelegt und k�nnen �ber die Funktion *Datei\Beispieleintr�ge entfernen* entfernt werden.     
Das Beispiel erm�glicht das Analgen von Standards in `HKEY_LOCAL_MACHINE`. Zu diesem
Zweck wurde f�r Windowsplattformen > **WinXP** f�r die UAC die h�chst verf�gbare Ausf�rhungsebenene gew�hlt.
Die Anwendung verlangt beim Start eine Autorisierung durch den Aufrufer.

* [s. Lazarus Wiki](http://wiki.lazarus.freepascal.org/GDB_Debugger_Tips#Debugging_applications_with_Administrative_privileges "Lazarus Wiki") 


Languages
---------
Das Unterverzeichnis *Languages* enh�lt �bersetzungsdateien *`*.po`-Dateien*, mit denen
die Resourcestrings der Beispielanwendung angepasst werden k�nnen. Dabei wird ein 
besonderes Augenmerk auf die Maske *TEditSettings* gesetzt, die nich direkt Teil
der Beispielanwendung ist, sondern �ber die Komponenten zu Verf�gung gestellt wird.