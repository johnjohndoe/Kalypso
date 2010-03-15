# Microsoft Developer Studio Project File - Name="Stempel" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=Stempel - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "Stempel.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "Stempel.mak" CFG="Stempel - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "Stempel - Win32 Release" (basierend auf  "Win32 (x86) Application")
!MESSAGE "Stempel - Win32 Debug" (basierend auf  "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Stempel - Win32 Release"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\Output\Tmp\Release\Stempel"
# PROP Intermediate_Dir "..\..\Output\Tmp\Release\Stempel"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MD /W3 /GX /Ox /Ob2 /I "Include" /I "..\Common\Include" /I "..\Draw\Include" /I "..\Esri\Include" /I "..\WspPrj\Include" /I "..\\" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "_AFXDLL" /FR /YX"stdafx.h" /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x407 /i "Include" /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /machine:I386
# ADD LINK32 version.lib /nologo /subsystem:windows /incremental:yes /machine:I386
# Begin Special Build Tool
TargetPath=\Wsp\Output\Tmp\Release\Stempel\Stempel.exe
SOURCE="$(InputPath)"
PostBuild_Desc=Ausgabedatei wird kopiert...
PostBuild_Cmds=copy $(TargetPath) ..\..\Output\Release
# End Special Build Tool

!ELSEIF  "$(CFG)" == "Stempel - Win32 Debug"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Stempel_"
# PROP BASE Intermediate_Dir "Stempel_"
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\Output\Tmp\Debug\Stempel"
# PROP Intermediate_Dir "..\..\Output\Tmp\Debug\Stempel"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MDd /W3 /Gm /Gi /GX /ZI /Od /I "Include" /I "..\Common\Include" /I "..\Draw\Include" /I "..\Esri\Include" /I "..\WspPrj\Include" /I "..\\" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "_AFXDLL" /FR /YX"stdafx.h" /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x407 /i "Include" /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 version.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# SUBTRACT LINK32 /profile /incremental:no /map
# Begin Special Build Tool
TargetPath=\Wsp\Output\Tmp\Debug\Stempel\Stempel.exe
SOURCE="$(InputPath)"
PostBuild_Desc=Ausgabedatei wird kopiert...
PostBuild_Cmds=copy $(TargetPath) ..\..\Output\Debug
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "Stempel - Win32 Release"
# Name "Stempel - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\Source\Griddlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Newdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Stempel.cpp
# End Source File
# Begin Source File

SOURCE=.\source\stempelhelpmap.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Stplfrm.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Stplview.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\Include\buttons.h
# End Source File
# Begin Source File

SOURCE=.\Include\Griddlg.h
# End Source File
# Begin Source File

SOURCE=.\Include\Newdlg.h
# End Source File
# Begin Source File

SOURCE=.\Include\resource.h
# End Source File
# Begin Source File

SOURCE=.\include\stdafx.h
# End Source File
# Begin Source File

SOURCE=.\Include\Stempel.h
# End Source File
# Begin Source File

SOURCE=.\include\stempelhelpmap.h
# End Source File
# Begin Source File

SOURCE=.\Include\Stplfrm.h
# End Source File
# Begin Source File

SOURCE=.\Include\Stplview.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\Include\Res\bitmap48.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\magnify.cur
# End Source File
# Begin Source File

SOURCE=.\Include\Res\mainfram.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn01.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn02.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn03.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn04.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn05.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn06.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn07.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn08.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn09.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn10.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn11.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn12.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn13.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn14.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn15.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn16.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn17.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn18.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn19.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn20.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn21.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn22.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn23.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\pattrn24.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\Pencil.cur
# End Source File
# Begin Source File

SOURCE=.\Include\Res\sizediag.cur
# End Source File
# Begin Source File

SOURCE=.\Include\Res\Stempel.ico
# End Source File
# Begin Source File

SOURCE=.\Include\Stempel.rc
# End Source File
# Begin Source File

SOURCE=.\Include\Res\Stempel.rc2
# End Source File
# Begin Source File

SOURCE=.\Include\Res\Toolbar.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\Res\Toolbar1.bmp
# End Source File
# End Group
# Begin Group "Draw"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\Draw\Source\Cntritem.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\Cntritem.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\Colors.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\debugdlg.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\debugdlg.h
# End Source File
# Begin Source File

SOURCE=..\draw\source\distdlg.cpp
# End Source File
# Begin Source File

SOURCE=..\draw\include\distdlg.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\draw.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\draw.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\Drawdoc.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\drawdoc.h
# End Source File
# Begin Source File

SOURCE=..\Draw\source\DrawLayer.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\Drawobj.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\Drawobj.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\Drawtool.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\Drawtool.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\drawvw.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\drawvw.h
# End Source File
# Begin Source File

SOURCE=..\draw\source\dxfarc.cpp
# End Source File
# Begin Source File

SOURCE=..\draw\include\dxfarc.h
# End Source File
# Begin Source File

SOURCE=..\draw\source\dxfblkin.cpp
# End Source File
# Begin Source File

SOURCE=..\draw\include\dxfblkin.h
# End Source File
# Begin Source File

SOURCE=..\draw\source\dxfblock.cpp
# End Source File
# Begin Source File

SOURCE=..\draw\include\dxfblock.h
# End Source File
# Begin Source File

SOURCE=..\draw\source\dxfent.cpp
# End Source File
# Begin Source File

SOURCE=..\draw\include\dxfent.h
# End Source File
# Begin Source File

SOURCE=..\draw\source\dxfkreis.cpp
# End Source File
# Begin Source File

SOURCE=..\draw\include\dxfkreis.h
# End Source File
# Begin Source File

SOURCE=..\draw\source\dxflayer.cpp
# End Source File
# Begin Source File

SOURCE=..\draw\include\dxflayer.h
# End Source File
# Begin Source File

SOURCE=..\draw\source\dxflinie.cpp
# End Source File
# Begin Source File

SOURCE=..\draw\include\dxflinie.h
# End Source File
# Begin Source File

SOURCE=..\draw\source\dxfltype.cpp
# End Source File
# Begin Source File

SOURCE=..\draw\include\dxfltype.h
# End Source File
# Begin Source File

SOURCE=..\draw\source\dxfplin.cpp
# End Source File
# Begin Source File

SOURCE=..\draw\include\dxfplin.h
# End Source File
# Begin Source File

SOURCE=..\draw\source\dxfpunkt.cpp
# End Source File
# Begin Source File

SOURCE=..\draw\include\dxfpunkt.h
# End Source File
# Begin Source File

SOURCE=..\draw\source\dxftext.cpp
# End Source File
# Begin Source File

SOURCE=..\draw\include\dxftext.h
# End Source File
# Begin Source File

SOURCE=..\draw\source\dxfzeich.cpp
# End Source File
# Begin Source File

SOURCE=..\draw\include\dxfzeich.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\Graphdlg.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\Graphdlg.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\Ipframe.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\Linppage.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\Linppage.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\Mainfrm.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\mainfrm.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\mtoolbar.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\mtoolbar.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\Optdlg.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\Optdlg.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\propset.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\propset.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\Solppage.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\Solppage.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\splash.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\splash.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\splitfrm.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\splitfrm.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\statpage.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\statpage.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\Stpldoc.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\Stpldoc.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\styppage.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\styppage.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\summinfo.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\summinfo.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\summpage.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\summpage.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\Svritem.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\Svritem.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\Txtppage.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\Txtppage.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\zoombx.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\zoombx.h
# End Source File
# Begin Source File

SOURCE=..\Draw\Source\Zoomdlg.cpp
# End Source File
# Begin Source File

SOURCE=..\Draw\Include\Zoomdlg.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\history.txt
# End Source File
# End Target
# End Project
