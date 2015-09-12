�bersetzung von Dialogen zur Laufzeit
=====================================

* lclstrconsts.po
* TEditSettings   

### lclstrconsts.po    
Standard�bersetzungen wie z.B. TBitBtn.Caption oder Buttons.Caption aus MessageDLG 
(etc.), welche von Elementen der LRC verwendet werden, werden nicht von der LRC
�bersetzt. Sie m�ssen aus der entsprechende �bersetzungsdatei ***lclstrconsts.LANG_ID.po***
der LCL in die `*.po` Datei des Hauptprogrammes �bernommen werden. F�r eine �bersetung 
ins Deutsche w�ren das die Stringressourcen aus der Datei: *lclstrconsts.de.po*.     

### TEditSettings       
RootKeys k�nnen �ber die Methode: *ShowClientEditDialog* der RegistrySource f�r
jedes beliebige Steuerelement zur Laufzeit aufgerufen werden. Will man die Strings in der
Maske von ***TEditSettings*** �bersetzen, muss man nur der `*.po` Datei des
Hauptprogrammes die �bersetzten Stringressourcen aus der Datei: *dlgeditsettings.LANG_ID.po*
hinzuf�gen. F�r eine �bersetung ins Deutsche w�ren das die Stringressourcen aus der 
Datei: *dlgeditsettings.de.po* .