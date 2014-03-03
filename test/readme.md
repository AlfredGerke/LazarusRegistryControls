Automatisierter Test für die LRC
================================

In diesem Ordner befindet sich ein FPCUnit-Test um einen automatisierten Test für 
die LazarusRegistryControls zur Verfügung zu stellen.

**Inhaltsübersicht:**

- 1     Zielvorgabe
- 2     Wrapper
- 3     Testfälle
- 4     GUI-Test
- 5     Bugs


1 Zielvorgabe
-------------
Der automatisierte Test überprüft alle Eigenschaften der LR-Controls, angefangen
mit der TRegistrySource und den Zugriff auf die darunter liegende Verarbeitung-Unit, 
bis hin zu jedem einzelnem Property eines jeden LR-Controls, welches im Zusammenhang 
mit der Verarbeitung von Daten aus der Registry zusammenhängt.       

Zusätzlich sollen Bugs ebenfalls als eigenständige Tests in das Projekt aufgenommen 
werden.      

Die Mindestanforderung an die LRC muss sein das alle implementierten Testmethoden
mit Erfolg abgeschlossen werden können.

2 Wrapper
---------
Zu testende LR-Controls sollen nicht direkt in Testfälle eingebunden werden,
sondern werden über Wrapper einem Testfall zur Verfügung gestellt. 
Für jedes LR-Control wird ein Wrapper zur Verfügung gestellt.

###Grundklassen       
Alle Wrapper werden von einer Grundklasse TWrapper abgeleitet. TWrapper ist eine 
generische Klasse und besitzt noch keine spezifischen Informationen zu einem LR-Control.
Von der Grundklasse TWrapper können beliebige Ableitung eingeführt werden. So 
wird für LR-Controls mit CaptionSettings eine Wrapper-Klasse TWrapperCS eingeführt, 
welche direkt von TWrapper ableitet wird und ebenfalls eine generische Klasse ist.                                 
Ziel ist es Testmethoden in verschiedenen Ebenen zu implementieren, abhängig von 
der Häufigkeit mit der sie zur Anwendung kommen. In der Grundklasse für alle Wrapper (TWrapper) 
werden Testmethoden implementiert, welche für alle LR-Controls und in beliebig 
vielen Testfällen zur Anwendung kommen. Für diese Testmethoden ist es unerheblich 
um welches LR-Control es sich im speziellen handelt, das getestet werden soll. 
In der abgeleiteten Klasse TWrapperCS werden Testmethoden implementiert, welche 
für alle LR-Controls zur Anwendung kommen, welche CaptionSettings besitzen.      

###Wrapper mit LR-Control
Mit der Zuweisung eines LR-Controls (z.B.: TRegEdit) wird aus dem generischen 
Wrapper eine spezialisierter Wrapper. In dieser Klasse werden alle Testmethoden
implementiert, welche sich speziell auf ein bestimmtes LR-Control beziehen, aber 
in beliebig vielen Testfällen zur Anwendung kommen können. 

3 Testfall
----------
Jedes LR-Control bekommt mindestens einen eigenen Testfall in dem alle Testmethoden 
aus den Wrappern (spezialisierte Klasse und generische Klasse) implementiert werden.
Hinzu kommen beliebig viele Testmethoden, die nur ein einziges Mal überprüft werden 
müssen und somit in den Wrappern nicht aufgeführt werden sollen. Diese Tests stellen 
die Grundlage des automatisierten Test dar.     

Zusätzlich zu den Grund-Testfällen für ein Control sollen erweiterte Testfälle 
geschaffen werden, die die LR-Controls in Kombination miteinander und mit fremden 
Controls testen sollen. 

Auch hier gilt: Testmethoden, welche mehrfach zur Anwendung kommen in beliebig 
vielen Testfällen, werden in den Wrappern implementiert. Ist ein Zugriff auf ein 
bestimmtes LR-Control (z.B.: TRegEdit) notwendig, wird der zugehörigen spezialisierte 
Wrapper erweitert, ansonsten der generische Wrapper. Nur Testmethoden welche einmalig
zur Anwendung kommen, werden direkt im Testfall implementiert.     

4 GUI-Test
----------
LR-Controls sollen auch in einer GUI-Umgebung getestet werden. Zu diesem Zweck
wird ein einfaches Formular entworfen, auf dem ein oder mehrere LR-Controls platziert werden.
Es sollen Benutzereingabe simuliert werden und Ergebnisse überprüft werden.

Konzept: ???

5 Bugs
------

Für jeden Bug soll ein eigener Testfall eingerichtet werden.