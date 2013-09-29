Dokumentation für die LRC
=========================

In diesem Ordner wird die generierte Dokumentation im CHM-Format hinterlegt,
sowie Batchdateien zum Erstellen der CHM- und HTML-Dokumentation:    
  
* createDocu.bat
* makeDocu.bat


## createDocu.bat
In dieser Batchdatei wird dei Batch *makeDocu.bat* mit den notwendigen Parametern aufgerufen.    
Der Aufruf von *makeDocu.bat* muss wie folgt lauten: 
   
`makeDocu {Quelle der Beschreibungsdateien} {Quelle der Quelltextdateien} {Ziel der Dokumentation} {html | chm}`


## makeDocu.bat
Diese Batchdatei erstellt mit Hilfe von *FPDoc.exe* anhand der Übergabeparameter eine Dokumentation.


## FPDoc
*FPDoc.exe* wird in den beiden Batchdateien ohne Pfadangabe aufgerufen. Die *FPDoc.exe* sollte
sich in der Lazarus-Installation im Verzeichnis *`\lazarus\fpc\2.6.2\bin\i386-win32\`* (Windowsinstallation) befinden.
Entweder wird in den Batchdateien der Pfad ergänzt oder wie in diesem Fall geschen,
der Pfad zur *FPDoc.exe* in den globalen Pfad übernommen.   
  

## Absolute Pfade    
Bei der Entwicklung wurde auf relative Pfadangaben für die IDE-Umgebung geachtet.
Trotzdem haben sich in einigen Dateien absoulte Pfade eingefunden. Dabei handelt 
es sich immer um Pfadangaben für die Dokumentation. Der Pfadanteil 
*`C:\Sourcen\Projekte\SaE\Lazarus\regcontrols\`* muss in folgenden Dateien angepasst 
werden:    

* createDocu.bat       