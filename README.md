(in Progress...)

LazarusRegistryControls (LRC)
=============================

Sammlung von Lazarus-Steuerelementen, welche direkt mit der Registry verbunden 
werden (lesen/schreiben)

Inhalts�bersicht:

- 1     Einleitung
- 2     Packagenamen
- 3     Installation
- 4     LazarusRegistryControls (LRC) Funktionalit�t
- 4.1   HKEY_CURRENT_USER
- 4.2   Standardwerte
- 4.3   Synchronisierung und Gruppierung
- 4.3.1 Synchronisierung
- 4.3.2 Gruppierung


1 Einleitung

Die LazarusRegistryControls (LRC) sind direkte Ableitung einer Teilmenge aus den 
Standard- und den Additional-Steuerelemte. Sie werden auf einem eigenen Reiter 
der Komponentenpalette "Registry Controls" installiert. Die Auswahl der abgeleiteten
Steuerelement richtet sich nach der H�ufigkeit ihrer Nutzung (TEdit, TComboBox,
TCheckBox, TListBox, TRadioButton, TRadioGroup, TCheckListBox, TCheckGroup, 
TValueListEditor). Diese Steuerelemente sollten nur genutzt werden, um Werte in 
der Registry zu verwalten. 


2 Packagenamen

- registrysource.lpk
  Inhalt: TRegistrySource, Erweiterung der Eigenschaften, Property- und Komponenteneditoren

- registrycontrols.lpk
  Inhalt: TRegEdit, TRegComboBox, TRegCheckBox, TRegListBox, TRegRadioButton, 
  TRegRadioGroup, TCheckListBox, TRegCheckGroup, TRegValueListEditor 


3 Installation

in folgender Reihenfolge:
- registrysource.lpk
- registrycontrols.lpk


4 LazarusRegistryControls (LRC) Funktionalit�t  

Die abgeleiteten Steuerelement werden �ber eine zentrale Komponente 
(TRegistrySource) mit der Registry verbunden. �ber eine Erweiterung der
Eigenschaften (RegistrySettings) der abgeleiteten Steuerelement k�nnen spezifische 
Informationen f�r Schl�ssel der Registry hinterlegt werden, welche genutzt werden, 
um Daten aus der Registry zu lesen und in die Registry zu schreiben. 

4.1 HKEY_CURRENT_USER

Die Steuerelemente schreiben und lesen immer in das Registryroot: HKEY_CURRENT_USER.

4.2 Standardwerte

Standardwerte f�r nicht erfolgreiche Zugriffe auf die Registry (in der Regel 
Schl�ssel nicht vorhanden) k�nnen zum einen statisch am Steuerelement definiert
werden. Als zweite M�glichkeit kann man am Steuerelement einen Schl�sselpfad 
definieren, �ber den Defaults in der Registry verwaltet werden k�nnen.

4.3 Synchronisierung und Gruppierung

4.3.1 Synchronisierung
Auf Wunsch k�nnen Steuerelemente automatisch veranlasst werden, sich zu aktualisieren,
wenn der Inhalt eines beliebigen Steuerelementes ge�ndert wurde. Voraussetzung ist,
das alle diese Steuerelement mit der selben RegistrySoruce (TRegistrySource) 
verbunden sind, wie das Steuerelement dessen Wert ge�ndert wurde. Jedes Steuerelement
kann �ber die Eigenschaft DoSyncData bestimmen, ob es an einer automatischen
Synchronisierung teilnimmt. Die Eigenschaft DoSyncData an der zentralen Komponente 
(TRegistrySource) bestimmt ob �berhaupt synchronisiert werden soll.

4.3.2 Gruppierung
�ber die Eigenschaft GroupIndex der erweiterten Eigenschaften (RegistrySettings)
k�nnen Steuerelemente zu Gruppen zusammengefasst werden. Der GroupIndex=0 steht 
dabei immer f�r "keine Zuordnung zu einer Gruppe".

Beispiel 1: 
Wenn ein Steuerelement mit dem GroupIndex=0 seinen Wert �ndert, die Synchronisierung 
�berall eingeschaltet ist, dann werden auch alle anderen Steuerelemente veranlasst, 
ihre Daten zu aktualisieren, unabh�ngig ob deren GroupIndex<>0 ist.

Beispiel 2: 
Wenn ein Steuerelement mit dem GroupIndex=1 seinen Wert �ndert, dann werden nur 
die Steuerelemente zur Synchronisierung veranlasst, welche ebenfalls den GroupIndex=1 
besitzen.     
