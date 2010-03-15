# Microsoft Developer Studio Project File - Name="wspwin" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=wspwin - Win32 Release
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "wspwin.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "wspwin.mak" CFG="wspwin - Win32 Release"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "wspwin - Win32 Release" (basierend auf  "Win32 (x86) Application")
!MESSAGE "wspwin - Win32 Debug" (basierend auf  "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "wspwin - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\Release"
# PROP BASE Intermediate_Dir ".\Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\Output\Tmp\Release\Wspwin"
# PROP Intermediate_Dir "..\..\Output\Tmp\Release\Wspwin"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /I "Include" /I ".." /I "xvt/include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /FD /c
# SUBTRACT CPP /YX
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /i "Resource" /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 lz32.lib xvt/lib/xvtnmapi.lib xvt/lib/xvtnmhn.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /machine:I386
# SUBTRACT LINK32 /map /debug /nodefaultlib /force
# Begin Special Build Tool
TargetPath=\WspHead\Output\Tmp\Release\Wspwin\wspwin.exe
SOURCE="$(InputPath)"
PostBuild_Desc=Ausgabedatei wird kopiert...
PostBuild_Cmds=copy $(TargetPath) ..\..\Output\Release
# End Special Build Tool

!ELSEIF  "$(CFG)" == "wspwin - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\Debug"
# PROP BASE Intermediate_Dir ".\Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\Output\Tmp\Debug\Wspwin"
# PROP Intermediate_Dir "..\..\Output\Tmp\Debug\Wspwin"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "Include" /I ".." /I "xvt/include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX"wspwin.h" /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /i "Resource" /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 lz32.lib xvt/lib/xvtnmapi.lib xvt/lib/xvtnmhn.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# SUBTRACT LINK32 /profile /incremental:no /nodefaultlib
# Begin Special Build Tool
TargetPath=\WspHead\Output\Tmp\Debug\Wspwin\wspwin.exe
SOURCE="$(InputPath)"
PostBuild_Desc=Ausgabedatei wird kopiert...
PostBuild_Cmds=copy $(TargetPath) ..\..\Output\Debug
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "wspwin - Win32 Release"
# Name "wspwin - Win32 Debug"
# Begin Group "Ressourcendateien"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\Resource\CUR13.CUR
# End Source File
# Begin Source File

SOURCE=.\Resource\CUR2.CUR
# End Source File
# Begin Source File

SOURCE=.\Resource\ICO9014.ICO
# End Source File
# Begin Source File

SOURCE=.\Resource\interpol.bmp
# End Source File
# Begin Source File

SOURCE=.\Resource\Question.bmp
# End Source File
# Begin Source File

SOURCE=.\Resource\wsp.ico
# End Source File
# Begin Source File

SOURCE=.\Resource\wspwin.rc
# End Source File
# End Group
# Begin Group "Headerdateien"

# PROP Default_Filter "*.h"
# Begin Source File

SOURCE=.\Include\armco.h
# End Source File
# Begin Source File

SOURCE=.\Include\AUFNEHM.H
# End Source File
# Begin Source File

SOURCE=.\Include\AutoSketchDlg.h
# End Source File
# Begin Source File

SOURCE=.\Include\BCE_ALLG.H
# End Source File
# Begin Source File

SOURCE=.\Include\C_laengs.h
# End Source File
# Begin Source File

SOURCE=.\Include\configuration.h
# End Source File
# Begin Source File

SOURCE=.\Include\Da66.h
# End Source File
# Begin Source File

SOURCE=.\Include\DIS_PROF.H
# End Source File
# Begin Source File

SOURCE=.\Include\Dxf_edit.h
# End Source File
# Begin Source File

SOURCE=.\Include\Error1.h
# End Source File
# Begin Source File

SOURCE=.\Include\Flaeche.h
# End Source File
# Begin Source File

SOURCE=.\Include\Global.h
# End Source File
# Begin Source File

SOURCE=.\Include\global_defs.h
# End Source File
# Begin Source File

SOURCE=.\Include\global_types.h
# End Source File
# Begin Source File

SOURCE=.\Include\global_vars.h
# End Source File
# Begin Source File

SOURCE=.\Include\Jabron.h
# End Source File
# Begin Source File

SOURCE=.\Include\Jabron1.h
# End Source File
# Begin Source File

SOURCE=.\Include\L_typen.h
# End Source File
# Begin Source File

SOURCE=.\Include\Laengs1.h
# End Source File
# Begin Source File

SOURCE=.\Include\LAENGS3.H
# End Source File
# Begin Source File

SOURCE=.\Include\Leibo.h
# End Source File
# Begin Source File

SOURCE=.\Include\LESE_STR.H
# End Source File
# Begin Source File

SOURCE=.\Include\List.h
# End Source File
# Begin Source File

SOURCE=.\Include\LISTAUSW.H
# End Source File
# Begin Source File

SOURCE=.\Include\Paint.h
# End Source File
# Begin Source File

SOURCE=.\Include\Plot.h
# End Source File
# Begin Source File

SOURCE=.\Include\Printer.h
# End Source File
# Begin Source File

SOURCE=.\Include\PROFPRO2.H
# End Source File
# Begin Source File

SOURCE=.\Include\PROFPROJ.H
# End Source File
# Begin Source File

SOURCE=.\Include\profverl.h
# End Source File
# Begin Source File

SOURCE=.\Include\PRUEFLST.H
# End Source File
# Begin Source File

SOURCE=.\Include\Qlist.h
# End Source File
# Begin Source File

SOURCE=.\Include\Rauh.h
# End Source File
# Begin Source File

SOURCE=.\Include\READ.H
# End Source File
# Begin Source File

SOURCE=.\Include\READ_CFG.H
# End Source File
# Begin Source File

SOURCE=.\Include\Readprof.h
# End Source File
# Begin Source File

SOURCE=.\Include\resource.h
# End Source File
# Begin Source File

SOURCE=.\Include\SCHREIBE.H
# End Source File
# Begin Source File

SOURCE=.\Include\Slist.h
# End Source File
# Begin Source File

SOURCE=.\Include\Strang.h
# End Source File
# Begin Source File

SOURCE=.\Include\STRANG2.H
# End Source File
# Begin Source File

SOURCE=.\Include\TYPEN.H
# End Source File
# Begin Source File

SOURCE=.\Include\Util.h
# End Source File
# Begin Source File

SOURCE=.\Include\Util2.h
# End Source File
# Begin Source File

SOURCE=.\Include\VERLUSTE.H
# End Source File
# Begin Source File

SOURCE=.\Include\VERZWEIG.H
# End Source File
# Begin Source File

SOURCE=.\Include\VGL_LIST.H
# End Source File
# Begin Source File

SOURCE=.\Include\Volume1.h
# End Source File
# Begin Source File

SOURCE=.\Include\Volume2.h
# End Source File
# Begin Source File

SOURCE=.\Include\waspila.h
# End Source File
# Begin Source File

SOURCE=.\Include\Wspalloc.h
# End Source File
# Begin Source File

SOURCE=.\Include\Wspd143.h
# End Source File
# Begin Source File

SOURCE=.\Include\wspdde.h
# End Source File
# Begin Source File

SOURCE=.\Include\wsphilfe.h
# End Source File
# Begin Source File

SOURCE=.\Include\wsplist.h
# End Source File
# Begin Source File

SOURCE=.\Include\Wsplp2qp.h
# End Source File
# Begin Source File

SOURCE=.\Include\Wspwin.h
# End Source File
# End Group
# Begin Group "Quelcodedateien"

# PROP Default_Filter "*.cpp"
# Begin Source File

SOURCE=.\Source\Aufnehm.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\AutoSketchdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Bce_allg.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\C_laengs.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\configuration.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Da66.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Dis_prof.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Dxf_edit.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Error1.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\FLAECHE.CPP
# End Source File
# Begin Source File

SOURCE=.\Source\global_vars.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Jabron.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Jabron1.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Laengs1.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Laengs2.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Leibo.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Lese_str.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\List.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Listausw.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Paint.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Plot.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Plot100.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Plot101.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\PRINTER.CPP
# End Source File
# Begin Source File

SOURCE=.\Source\Profpro2.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Profproj.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\profverl.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\PRUEFLST.CPP
# End Source File
# Begin Source File

SOURCE=.\Source\Qwert.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Rauh.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Read.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Read_cfg.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Readprof.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Slist.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Strang.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Strang2.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Util.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Util2.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Verluste.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Verzweig.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Vgl_list.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Volume1.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Volume2.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\waspila.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspalloc.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd102.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd107.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\WSPD110.CPP
# End Source File
# Begin Source File

SOURCE=.\Source\WSPD122.CPP
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd128.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\WSPD135.CPP
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd136.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd137.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\WSPD138.CPP
# End Source File
# Begin Source File

SOURCE=.\Source\WSPD140.CPP
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd141.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd142.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd143.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\WSPD144.CPP
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd145.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd147.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd148.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd150.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd151.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\WSPD153.CPP
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd154.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd155.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd156.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd157.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd158.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd159.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd160.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd161.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd162.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\WSPD163.CPP
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd166.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd167.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\WSPD168.CPP
# End Source File
# Begin Source File

SOURCE=.\Source\WSPD169.CPP
# End Source File
# Begin Source File

SOURCE=.\Source\WSPD203.CPP
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd204.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd208.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd209.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd210.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd211.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd212.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd213.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd217.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd218.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd222.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\wspd337.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspd_exeute_extern.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\WspdArmc.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\wspdde.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wsplist.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wsplp2qp.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspm001.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspw116.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspw117.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspw120.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspw121.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspw122.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspw130.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspw131.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspw133.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Wspwin.cpp
# End Source File
# End Group
# Begin Source File

SOURCE=.\History.TxT
# End Source File
# End Target
# End Project
