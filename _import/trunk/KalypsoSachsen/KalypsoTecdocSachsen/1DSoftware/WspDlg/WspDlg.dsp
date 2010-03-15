# Microsoft Developer Studio Project File - Name="WspDlg" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=WspDlg - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "WspDlg.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "WspDlg.mak" CFG="WspDlg - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "WspDlg - Win32 Release" (basierend auf  "Win32 (x86) Dynamic-Link Library")
!MESSAGE "WspDlg - Win32 Debug" (basierend auf  "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "WspDlg - Win32 Release"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\Output\Tmp\Release\WspDlg"
# PROP Intermediate_Dir "..\..\Output\Tmp\Release\WspDlg"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /Ob2 /I "Include" /I ".." /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_USRDLL" /D "_WSPDLGDLL" /D "_WINDLL" /D "_AFXDLL" /FR /YX"stdafx.h" /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x407 /i "Include" /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 version.lib /nologo /base:"0x21000000" /subsystem:windows /dll /incremental:yes /machine:I386
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
TargetPath=\WspWin\Output\Tmp\Release\WspDlg\WspDlg.dll
SOURCE="$(InputPath)"
PostBuild_Desc=Ausgabedatei wird kopiert...
PostBuild_Cmds=copy $(TargetPath) ..\..\Output\Release
# End Special Build Tool

!ELSEIF  "$(CFG)" == "WspDlg - Win32 Debug"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\Output\Tmp\Debug\WspDlg"
# PROP Intermediate_Dir "..\..\Output\Tmp\Debug\WspDlg"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I ".." /I "Include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_USRDLL" /D "_WSPDLGDLL" /D "_WINDLL" /D "_AFXDLL" /FR /YX"stdafx.h" /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x407 /i "Include" /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 version.lib /nologo /base:"0x21000000" /subsystem:windows /dll /debug /machine:I386 /out:"..\..\Output\Tmp\Debug\WspDlg\wspdlgD.dll" /pdbtype:sept
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
TargetPath=\WspWin\Output\Tmp\Debug\WspDlg\wspdlgD.dll
SOURCE="$(InputPath)"
PostBuild_Desc=Ausgabedatei wird kopiert...
PostBuild_Cmds=copy $(TargetPath) ..\..\Output\Debug
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "WspDlg - Win32 Release"
# Name "WspDlg - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\Source\calcpg1.cpp
# ADD CPP /I "..\Include"
# End Source File
# Begin Source File

SOURCE=.\Source\calcpg2.cpp
# ADD CPP /I "..\Include"
# End Source File
# Begin Source File

SOURCE=.\Source\calcpg3.cpp
# ADD CPP /I "..\Include"
# End Source File
# Begin Source File

SOURCE=.\source\calcpg4.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\calcsht.cpp
# ADD CPP /I "..\Include"
# End Source File
# Begin Source File

SOURCE=.\source\datenbankdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\dirpage.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\export.cpp
# End Source File
# Begin Source File

SOURCE=.\source\KopfTxtDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\lossdlg.cpp
# ADD CPP /I "..\Include"
# End Source File
# Begin Source File

SOURCE=.\Source\optdlg.cpp
# ADD CPP /I "..\Include"
# End Source File
# Begin Source File

SOURCE=.\Source\prjprdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\prjsmdlg.cpp
# ADD CPP /I "..\Include"
# End Source File
# Begin Source File

SOURCE=.\Source\progdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\respage.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\strdpage.cpp
# ADD CPP /I "..\Include"
# End Source File
# Begin Source File

SOURCE=.\Source\WspDlg.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\Include\calcpg1.h
# End Source File
# Begin Source File

SOURCE=.\Include\calcpg2.h
# End Source File
# Begin Source File

SOURCE=.\Include\calcpg3.h
# End Source File
# Begin Source File

SOURCE=.\Include\calcpg4.h
# End Source File
# Begin Source File

SOURCE=.\Include\calcsht.h
# End Source File
# Begin Source File

SOURCE=.\include\datenbankdlg.h
# End Source File
# Begin Source File

SOURCE=.\Include\dirpage.h
# End Source File
# Begin Source File

SOURCE=.\Include\export.h
# End Source File
# Begin Source File

SOURCE=.\include\KopfTxtDlg.h
# End Source File
# Begin Source File

SOURCE=.\Include\lossdlg.h
# End Source File
# Begin Source File

SOURCE=.\Include\optdlg.h
# End Source File
# Begin Source File

SOURCE=.\Include\prjprdlg.h
# End Source File
# Begin Source File

SOURCE=.\Include\prjsmdlg.h
# End Source File
# Begin Source File

SOURCE=.\Include\progdlg.h
# End Source File
# Begin Source File

SOURCE=.\Include\resource.h
# End Source File
# Begin Source File

SOURCE=.\Include\respage.h
# End Source File
# Begin Source File

SOURCE=.\include\stateDlg.h
# End Source File
# Begin Source File

SOURCE=.\include\stdafx.h
# End Source File
# Begin Source File

SOURCE=.\Include\strdpage.h
# End Source File
# Begin Source File

SOURCE=.\Include\WspDlg.h
# End Source File
# Begin Source File

SOURCE=..\BCE\Include\WSPFeatures.h
# End Source File
# Begin Source File

SOURCE=.\include\wsphilfe.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\Include\res\abfluss.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\bauwerk.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\bewuchs.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\bmp00001.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\bordvoll.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\bslinks.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\bsrechts.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\calc.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\csec.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\deich_li.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\deich_re.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\director.ico
# End Source File
# Begin Source File

SOURCE=.\Include\res\dropdown.ico
# End Source File
# Begin Source File

SOURCE=.\Include\res\durch.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\eiprofil.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\gauss.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\height1.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\hoehe.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\hwert.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\icon1.ico
# End Source File
# Begin Source File

SOURCE=.\Include\res\kasten.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\kreis.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\kreisseg.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\laenge.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\linien1.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\lsec.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\manager_toolbar.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\maul.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\ok_bruec.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\ok_gelae.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\ok_wehr.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\project.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\project_neu.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\punkt_nr.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\rahmen.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\rauh.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\rwert.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\shoehe.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\state.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\trapez.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\uk_bruec.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\wassersp.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\water.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\wsp_brei.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\wsp_hoeh.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\wsp_mess.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\WspDlg.rc
# End Source File
# Begin Source File

SOURCE=.\Include\res\WspDlg.rc2
# End Source File
# Begin Source File

SOURCE=.\Include\res\wsphoehe.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\WSPMap.ico
# End Source File
# End Group
# Begin Source File

SOURCE=.\history.txt
# End Source File
# End Target
# End Project
