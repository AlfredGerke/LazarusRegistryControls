***(in Progress...)***

LazarusRegistryControls (LRC)
=============================

Sammlung von Lazarus-Steuerelementen, welche direkt mit der Registry verbunden 
werden (lesen/schreiben)

**Inhaltsübersicht:**

- 1     Einleitung
- 2     Packagenamen
- 3     Installation
- 3.1   Versionen
- 3.2   Reihenfolge
- 3.3   Absolute Pfade
- 4     LazarusRegistryControls (LRC) Funktionalität
- 4.1   `HKEY_CURRENT_USER`
- 4.2   Standardwerte
- 4.3   Synchronisierung und Gruppierung
- 4.3.1 Synchronisierung
- 4.3.2 Gruppierung


1 Einleitung
------------

Die LazarusRegistryControls (LRC) sind direkte Ableitung einer Teilmenge aus den 
Standard- und den Additional-Steuerelemte der LCL von Lazarus. Sie werden auf einem 
eigenen Reiter der Komponentenpalette "Registry Controls" installiert. Die Auswahl 
der abgeleiteten Steuerelement richtet sich nach der Häufigkeit ihrer Nutzung 
(TEdit, TComboBox, TCheckBox, TListBox, TRadioButton, TRadioGroup, TCheckListBox, 
TCheckGroup, TValueListEditor). Diese Steuerelemente sollten nur genutzt werden, 
um Werte in der Registry zu verwalten. Sie sind in der vorliegenden Version nur
für den Einsatz unter Windows sinnvoll.     


2 Packagenamen
--------------

- registrysource.lpk:        
  
  * TRegistrySource
  * Erweiterung der Eigenschaften
  * Property- und Komponenteneditoren

- registrycontrols.lpk:        
  
  * TRegEdit
  * TRegLabel
  * TRegComboBox
  * TRegCheckBox
  * TRegListBox
  * TRegRadioButton
  * TRegRadioGroup
  * TRegCheckListBox
  * TRegCheckGroup
  * TRegValueListEditor 


3 Installation
--------------

Für die Installation sind keine zusätzlichen Packages (etc.) notwendig. In der 
vorliegenden Version nur unter Windows sinnvoll.   

## 3.1 Versionen       
Lazarus: 1.0.10 / 1.0.12   
FPC: 2.6.2     
LazarusRegistryControls (LRC): *?.?.? (in Progress)*            

## 3.2 Reihenfolge    
- registrysource.lpk
- registrycontrols.lpk

## 3.3 Absolute Pfade    
Bei der Entwicklung wurde auf relative Pfadangaben für die IDE-Umgebung geachtet.
Trotzdem haben sich in einigen Dateien absoulte Pfade eingefunden. Dabei handelt 
es sich immer um Pfadangaben für die Dokumentation. Der Pfadanteil 
C:\Sourcen\Projekte\SaE\Lazarus\regcontrols\ muss in folgenden Dateien angepasst 
werden:        

- docuProject.ldp in ..\doc\    
- registrycontrols.lpk in ..\package\    
- registrysource.lpk in ..\package\    
- _makeskel.bat in ..\soruce\   
- makeChm.bat in ..\documentation\ 


4 LazarusRegistryControls (LRC) Funktionalität
----------------------------------------------  

Die abgeleiteten Steuerelement werden über eine zentrale Komponente 
(TRegistrySource) mit der Registry verbunden. Über eine Erweiterung der
Eigenschaften (RegistrySettings) der abgeleiteten Steuerelement können spezifische 
Informationen für Schlüssel der Registry hinterlegt werden, welche genutzt werden, 
um Daten aus der Registry zu lesen und in die Registry zu schreiben. 

## 4.1 `HKEY_CURRENT_USER`

Die Steuerelemente schreiben und lesen immer in das Registryroot: `HKEY_CURRENT_USER`.
Unterhalb vom Registryroot wird eine Grundschlüssel eingerichtet. Dieser Grundschlüssel
sollte immer in den Schlüssel *Software* verweisen.     
Unterhalb des Schlüssel *Software* sollte der Name der Firma oder der Organisation,
welche das Projekt/Programm vertreibt aufgeführt werden.     
Unterhalb des Schlüssels für die Firma/Organisation sollte eine Bezeichnung für das 
Projekt eingeführt werden.     
Um meherere Versionen eines Projektes unter Umständen mit verschiedenen Schlüssel 
versorgen zu können sollte, unterhalb des Schlüssels für das Projekt eine eindeutige 
Kennung angegeben werden. Eine solche eindeutige Kennung könnte eine GUID sein.
In der RegistrySource (TRegistrySource) werden für diese Einzelelemente Eigenschaften
angeboten *(Eigenschaften: GUID, ORGANISATION, PROJECT)*.     
Diese Eigenschaften können in einem Grundschlüssel als Variablen hinterlegt werden. 
*(Variablen: %%GUID%%, %%ORGANISATION%%, %%PROJECT%%)*    
Ein Grundschlüssel könnte also wie folgt aussehen:    
`SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\%%GUID%%`    
In dem Beispielprogramm wird daraus:    
`Software\ExampleFactory\LazarusRegistryControls\{51D8EEB4-A549-4B08-BFCB-731DEC3E82AE}`    


## 4.2 Standardwerte

Standardwerte für nicht erfolgreiche Zugriffe auf die Registry (in der Regel 
Schlüssel nicht vorhanden) können direkt am Steuerelement in den RegistrySettings 
(Eigenschaft: Default) definiert werden. Als zweite Möglichkeit kann man am 
Steuerelement einen Schlüsselpfad definieren, über den Defaults in der Registry 
verwaltet werden können. Dies ermöglicht personalisierte Standardwerte, die z.B. 
durch die Installation oder einem Update eingerichtet werden. Ein Administarionswerkzeug 
könnte ebenfalls diese Standards bearbeiten.    
Der Grundschlüssel für Standards, sollte nach ähnlichen Regeln aufgebaut werden,
wie der Grundschlüssel im `HKEY_CURRENT_USER`. Die Eigenschaften: *GUID, ORGANISATION, PROJECT* und
deren Einsatz durch die entsprechenden Variablen stehen in gleichwer Weise zur Verfügung.       
Ein Grundschlüssel könnte also wie folgt aussehen:        
`SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\DEFAULTS\%%GUID%%`        
In dem Beispielprogramm wird daraus:        
`Software\ExampleFactory\LazarusRegistryControls\DEFAULTS\{51D8EEB4-A549-4B08-BFCB-731DEC3E82AE}`       
   

## 4.3 Synchronisierung und Gruppierung

### 4.3.1 Synchronisierung    
Auf Wunsch können Steuerelemente automatisch veranlasst werden, sich zu aktualisieren,
wenn der Inhalt eines beliebigen Steuerelementes geändert wurde. Voraussetzung ist,
das alle diese Steuerelement mit der selben RegistrySoruce (TRegistrySource) 
verbunden sind, wie das Steuerelement dessen Wert geändert wurde. Jedes Steuerelement
kann über die Eigenschaft DoSyncData bestimmen, ob es an einer automatischen
Synchronisierung teilnimmt. Die Eigenschaft DoSyncData an der zentralen Komponente 
(TRegistrySource) bestimmt ob überhaupt synchronisiert werden soll.

### 4.3.2 Gruppierung    
Über die Eigenschaft GroupIndex der erweiterten Eigenschaften (RegistrySettings)
können Steuerelemente zu Gruppen zusammengefasst werden. Der GroupIndex=0 steht 
dabei immer für "keine Zuordnung zu einer Gruppe".

### Beispiel 1:     
Wenn ein Steuerelement mit dem GroupIndex=0 seinen Wert ändert, die Synchronisierung 
überall eingeschaltet ist, dann werden auch alle anderen Steuerelemente veranlasst, 
ihre Daten zu aktualisieren, unabhängig ob deren GroupIndex<>0 ist.

### Beispiel 2:     
Wenn ein Steuerelement mit dem GroupIndex=1 seinen Wert ändert, dann werden nur 
die Steuerelemente zur Synchronisierung veranlasst, welche ebenfalls den GroupIndex=1 
besitzen.     
