cls

if "%1"=="" goto Anwendung
if "%2"=="" goto Anwendung
if "%3"=="" goto Anwendung
if "%4"=="" goto Anwendung 


fpdoc --package=RegistrySource --output=%3\registrysource --format=%4 --show-private --content --descr=%1\regbaseform.xml --descr=%1\dlgaboutcomponent.xml --descr=%1\dlgeditsettings.xml --descr=%1\dlgtruefalse.xml --descr=%1\regconst.xml --descr=%1\regmsg.xml --descr=%1\regpropedits.xml --descr=%1\regresstrings.xml --descr=%1\regsourcen.xml --descr=%1\regtype.xml --descr=%1\regutils.xml --input=%2\regbaseform.pas --input=%2\dlgaboutcomponent.pas --input=%2\dlgeditsettings.pas --input=%2\dlgtruefalse.pas --input=%2\regconst.pas --input=%2\regmsg.pas --input=%2\regpropedits.pas --input=%2\regresstrings.pas --input="-dfpdoc %2\regsourcen.pas" --input="-dfpdoc %2\regtype.pas" --input=%2\regutils.pas > %4.log
fpdoc --package=RegistryControls --output=%3\registrycontrols --format=%4 --show-private --content --descr=%1\regcheckbox.xml --descr=%1\regedit.xml --descr=%1\regradiobutton.xml --descr=%1\reglabel.xml --descr=%1\regcombobox.xml --descr=%1\reglistbox.xml --descr=%1\regradiogroup.xml --descr=%1\regchecklistbox.xml --descr=%1\regcheckgroup.xml --descr=%1\regvaluelisteditor.xml --input=%2\regcheckbox.pas --input=%2\regedit.pas --input=%2\regradiobutton.pas --input=%2\reglabel.pas --input=%2\regcombobox.pas --input=%2\reglistbox.pas --input=%2\regradiogroup.pas --input=%2\regchecklistbox.pas --input=%2\regcheckgroup.pas --input=%2\regvaluelisteditor.pas >> %4.log

goto ENDE1

:Anwendung
echo.
echo    Das Batchprogramm zur Erstellung einer Doku wird wie folgt aufgerufen:  
echo.
echo    makeDocu {Quelle der Beschreibungsdateien} {Quelle der Quelltextdateien} {Ziel der Dokumentation} {html | chm}
echo.

:ENDE1

CLS
echo.
echo    Die Dokumentation wurde erstellt 
echo.