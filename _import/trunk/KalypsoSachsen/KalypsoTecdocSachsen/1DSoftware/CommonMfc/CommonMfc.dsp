# Microsoft Developer Studio Project File - Name="CommonMfc" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=CommonMfc - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "CommonMfc.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "CommonMfc.mak" CFG="CommonMfc - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "CommonMfc - Win32 Release" (basierend auf  "Win32 (x86) Static Library")
!MESSAGE "CommonMfc - Win32 Debug" (basierend auf  "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "CommonMfc - Win32 Release"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\Output\Tmp\Release\CommonMfc"
# PROP Intermediate_Dir "..\..\Output\Tmp\Release\CommonMfc"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "Include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /FR /YX"stdafx.h" /FD /c
# ADD BASE RSC /l 0x407 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x407 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "CommonMfc - Win32 Debug"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\Output\Tmp\Debug\CommonMfc"
# PROP Intermediate_Dir "..\..\Output\Tmp\Debug\CommonMfc"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /Yu"stdafx.h" /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "Include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /FR /YX"stdafx.h" /FD /GZ /c
# ADD BASE RSC /l 0x407 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x407 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "CommonMfc - Win32 Release"
# Name "CommonMfc - Win32 Debug"
# Begin Group "Header-Dateien"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\Include\BoxPager.h
# End Source File
# Begin Source File

SOURCE=.\Include\brushbox.h
# End Source File
# Begin Source File

SOURCE=.\Include\CellRange.h
# End Source File
# Begin Source File

SOURCE=.\Include\colorbox.h
# End Source File
# Begin Source File

SOURCE=.\Include\CommonMfc.rc
# End Source File
# Begin Source File

SOURCE=.\Include\commresource.h
# End Source File
# Begin Source File

SOURCE=.\Include\contexthelp.h
# End Source File
# Begin Source File

SOURCE=.\Include\CoolDialogBar.h
# End Source File
# Begin Source File

SOURCE=.\Include\DDEWnd.h
# End Source File
# Begin Source File

SOURCE=.\Include\dib.h
# End Source File
# Begin Source File

SOURCE=.\Include\DIBSectionLite.h
# End Source File
# Begin Source File

SOURCE=.\Include\executeextern.h
# End Source File
# Begin Source File

SOURCE=.\Include\FileDebugContext.h
# End Source File
# Begin Source File

SOURCE=.\Include\GridCtrl.h
# End Source File
# Begin Source File

SOURCE=.\Include\GridDropTarget.h
# End Source File
# Begin Source File

SOURCE=.\Include\grphbox.h
# End Source File
# Begin Source File

SOURCE=.\Include\Helper.h
# End Source File
# Begin Source File

SOURCE=.\Include\images.h
# End Source File
# Begin Source File

SOURCE=.\Include\InPlaceEdit.h
# End Source File
# Begin Source File

SOURCE=.\Include\InPlaceList.h
# End Source File
# Begin Source File

SOURCE=.\Include\itemChooser.h
# End Source File
# Begin Source File

SOURCE=.\Include\lctrlex.h
# End Source File
# Begin Source File

SOURCE=.\Include\lctrlex2.h
# End Source File
# Begin Source File

SOURCE=.\Include\linebox.h
# End Source File
# Begin Source File

SOURCE=.\Include\MapDouble.h
# End Source File
# Begin Source File

SOURCE=.\Include\MapUINTToString.h
# End Source File
# Begin Source File

SOURCE=.\Include\MemDC.h
# End Source File
# Begin Source File

SOURCE=.\Include\messagebox2.h
# End Source File
# Begin Source File

SOURCE=.\Include\messagebox5.h
# End Source File
# Begin Source File

SOURCE=.\Include\messageboxwelcher.h
# End Source File
# Begin Source File

SOURCE=.\Include\MfcHelper.h
# End Source File
# Begin Source File

SOURCE=.\Include\oleDispDriverEx.h
# End Source File
# Begin Source File

SOURCE=.\Include\point.h
# End Source File
# Begin Source File

SOURCE=.\Include\PrinterSettings.h
# End Source File
# Begin Source File

SOURCE=.\Include\ProcessHelper.h
# End Source File
# Begin Source File

SOURCE=.\Include\rect.h
# End Source File
# Begin Source File

SOURCE=.\Include\SizeCBar.h
# End Source File
# Begin Source File

SOURCE=.\Include\StdAfx.h
# End Source File
# Begin Source File

SOURCE=.\Include\stringinputdlg.h
# End Source File
# Begin Source File

SOURCE=.\Include\symbbox.h
# End Source File
# Begin Source File

SOURCE=.\Include\textbox.h
# End Source File
# Begin Source File

SOURCE=.\Include\TitleTip.h
# End Source File
# Begin Source File

SOURCE=.\Include\tlist.h
# End Source File
# Begin Source File

SOURCE=.\Include\toolbarex.h
# End Source File
# Begin Source File

SOURCE=.\Include\variant_helper.h
# End Source File
# Begin Source File

SOURCE=.\Include\Version.h
# End Source File
# Begin Source File

SOURCE=.\Include\wspinsertdialog.h
# End Source File
# End Group
# Begin Group "Quellcodedateien"

# PROP Default_Filter "cpp;c"
# Begin Source File

SOURCE=.\Source\BoxPager.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\brushbox.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\colorbox.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\contexthelp.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\CoolDialogBar.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\DDEWnd.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\dib.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\DIBSectionLite.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\executeextern.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\FileDebugContext.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\GridCtrl.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\GridDropTarget.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\grphbox.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Helper.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\images.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\InPlaceEdit.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\InPlaceList.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\lctrlex.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\lctrlex2.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\linebox.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\MapDouble.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\MapUINTToString.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\messagebox2.cpp
# ADD CPP /I "..\Include"
# End Source File
# Begin Source File

SOURCE=.\Source\messagebox5.cpp
# ADD CPP /I ".."
# End Source File
# Begin Source File

SOURCE=.\Source\messageboxwelcher.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\MfcHelper.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\oleDispDriverEx.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\PrinterSettings.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\ProcessHelper.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\SizeCBar.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\StdAfx.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\stringinputdlg.cpp
# ADD CPP /I "..\Include"
# End Source File
# Begin Source File

SOURCE=.\Source\symbbox.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\textbox.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\TitleTip.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\tlist.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\toolbarex.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\variant_helper.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Version.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\wspinsertdialog.cpp
# ADD CPP /I "..\Include"
# End Source File
# End Group
# Begin Group "Resourcen"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\res\abfluss.bmp
# End Source File
# Begin Source File

SOURCE=.\res\bauwerk.bmp
# End Source File
# Begin Source File

SOURCE=.\res\bewuchs.bmp
# End Source File
# Begin Source File

SOURCE=.\res\bmp00001.bmp
# End Source File
# Begin Source File

SOURCE=.\res\bordvoll.bmp
# End Source File
# Begin Source File

SOURCE=.\res\bslinks.bmp
# End Source File
# Begin Source File

SOURCE=.\res\bsrechts.bmp
# End Source File
# Begin Source File

SOURCE=.\res\calc.bmp
# End Source File
# Begin Source File

SOURCE=.\res\csec.bmp
# End Source File
# Begin Source File

SOURCE=.\res\csec_sav.bmp
# End Source File
# Begin Source File

SOURCE=.\res\deich_li.bmp
# End Source File
# Begin Source File

SOURCE=.\res\deich_re.bmp
# End Source File
# Begin Source File

SOURCE=.\res\Druckeigenschaften.bmp
# End Source File
# Begin Source File

SOURCE=.\res\durch.bmp
# End Source File
# Begin Source File

SOURCE=.\res\eiprofil.bmp
# End Source File
# Begin Source File

SOURCE=.\res\filling.bmp
# End Source File
# Begin Source File

SOURCE=.\res\folder.bmp
# End Source File
# Begin Source File

SOURCE=.\res\gauss.bmp
# End Source File
# Begin Source File

SOURCE=.\res\height.bmp
# End Source File
# Begin Source File

SOURCE=.\res\height1.bmp
# End Source File
# Begin Source File

SOURCE=.\res\hoehe.bmp
# End Source File
# Begin Source File

SOURCE=.\res\hwert.bmp
# End Source File
# Begin Source File

SOURCE=.\res\imlayer.bmp
# End Source File
# Begin Source File

SOURCE=.\res\Karte.bmp
# End Source File
# Begin Source File

SOURCE=.\res\kasten.bmp
# End Source File
# Begin Source File

SOURCE=.\res\kreis.bmp
# End Source File
# Begin Source File

SOURCE=.\res\kreisseg.bmp
# End Source File
# Begin Source File

SOURCE=.\res\laenge.bmp
# End Source File
# Begin Source File

SOURCE=.\res\Legende.bmp
# End Source File
# Begin Source File

SOURCE=.\res\loeschen.bmp
# End Source File
# Begin Source File

SOURCE=.\res\Logo.bmp
# End Source File
# Begin Source File

SOURCE=.\res\lsec.bmp
# End Source File
# Begin Source File

SOURCE=.\res\lsec_sav.bmp
# End Source File
# Begin Source File

SOURCE=.\res\map.bmp
# End Source File
# Begin Source File

SOURCE=.\res\maplayer.bmp
# End Source File
# Begin Source File

SOURCE=.\res\maul.bmp
# End Source File
# Begin Source File

SOURCE=.\res\Objekteigenschaften.bmp
# End Source File
# Begin Source File

SOURCE=.\res\ok_bruec.bmp
# End Source File
# Begin Source File

SOURCE=.\res\ok_gelae.bmp
# End Source File
# Begin Source File

SOURCE=.\res\ok_wehr.bmp
# End Source File
# Begin Source File

SOURCE=.\res\print.bmp
# End Source File
# Begin Source File

SOURCE=.\res\project.bmp
# End Source File
# Begin Source File

SOURCE=.\res\project_neu.bmp
# End Source File
# Begin Source File

SOURCE=.\res\punkt_nr.bmp
# End Source File
# Begin Source File

SOURCE=.\res\rahmen.bmp
# End Source File
# Begin Source File

SOURCE=.\res\rauh.bmp
# End Source File
# Begin Source File

SOURCE=.\res\rwert.bmp
# End Source File
# Begin Source File

SOURCE=.\res\scalebar.bmp
# End Source File
# Begin Source File

SOURCE=.\res\schliessen.bmp
# End Source File
# Begin Source File

SOURCE=.\res\sec_dis.bmp
# End Source File
# Begin Source File

SOURCE=.\res\Select.bmp
# End Source File
# Begin Source File

SOURCE=.\res\shoehe.bmp
# End Source File
# Begin Source File

SOURCE=.\res\state.bmp
# End Source File
# Begin Source File

SOURCE=.\res\station.bmp
# End Source File
# Begin Source File

SOURCE=.\res\stempel.bmp
# End Source File
# Begin Source File

SOURCE=.\res\tabelle.bmp
# End Source File
# Begin Source File

SOURCE=.\res\text.bmp
# End Source File
# Begin Source File

SOURCE=.\res\text2.bmp
# End Source File
# Begin Source File

SOURCE=.\res\text3.bmp
# End Source File
# Begin Source File

SOURCE=.\res\titel.bmp
# End Source File
# Begin Source File

SOURCE=.\res\trapez.bmp
# End Source File
# Begin Source File

SOURCE=.\res\uk_bruec.bmp
# End Source File
# Begin Source File

SOURCE=.\res\wassersp.bmp
# End Source File
# Begin Source File

SOURCE=.\res\water.bmp
# End Source File
# Begin Source File

SOURCE=.\res\wsp_brei.bmp
# End Source File
# Begin Source File

SOURCE=.\res\wsp_hoeh.bmp
# End Source File
# Begin Source File

SOURCE=.\res\wsp_mess.bmp
# End Source File
# Begin Source File

SOURCE=.\res\wsphoehe.bmp
# End Source File
# Begin Source File

SOURCE=.\res\WSPMap.ico
# End Source File
# Begin Source File

SOURCE=.\res\zoomin.bmp
# End Source File
# Begin Source File

SOURCE=.\res\zoomout.bmp
# End Source File
# Begin Source File

SOURCE=.\res\zorderback.bmp
# End Source File
# Begin Source File

SOURCE=.\res\zorderfront.bmp
# End Source File
# End Group
# Begin Source File

SOURCE=.\CommonMfc.h
# End Source File
# Begin Source File

SOURCE=.\history.txt
# End Source File
# Begin Source File

SOURCE=.\Readme.txt
# End Source File
# End Target
# End Project
