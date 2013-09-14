Übersetzung von Dialogen zur Laufzeit
=====================================

* lclstrconsts.po
* TEditSettings   

### lclstrconsts.po    
Standardübersetzungen wie z.B. TBitBtn.Caption oder Buttons.Caption aus MessageDLG 
(etc.), welche von Elementen der LRC verwendet werden, werden nicht von der LRC
übersetzt. Sie müssen aus der entsprechende Übersetzungsdatei ***lclstrconsts.LANG_ID.po***
der LCL in die `*.po` Datei des Hauptprogrammes übernommen werden. Für eine Übersetung 
ins Deutsche wären das die Stringressourcen aus der Datei: *lclstrconsts.de.po*.     

### TEditSettings       
RootKeys können über die Methode: *ShowClientEditDialog* der RegistrySource für
jedes beliebige Steuerelement zur Laufzeit aufgerufen werden. Will man die Strings in der
Maske von ***TEditSettings*** übersetzen, muss man nur der `*.po` Datei des
Hauptprogrammes die übersetzten Stringressourcen aus der Datei: *dlgeditsettings.LANG_ID.po*
hinzufügen. Für eine Übersetung ins Deutsche wären das die Stringressourcen aus der 
Datei: *dlgeditsettings.de.po* .