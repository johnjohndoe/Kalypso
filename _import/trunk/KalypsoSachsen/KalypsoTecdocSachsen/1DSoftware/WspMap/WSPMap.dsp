# Microsoft Developer Studio Project File - Name="WSPMap" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=WSPMap - Win32 Release
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "WSPMap.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "WSPMap.mak" CFG="WSPMap - Win32 Release"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "WSPMap - Win32 Debug" (basierend auf  "Win32 (x86) Application")
!MESSAGE "WSPMap - Win32 Release" (basierend auf  "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\Output\Tmp\Debug\WspMap"
# PROP Intermediate_Dir "..\..\Output\Tmp\Debug\WspMap"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MDd /W3 /Gm /Gi /GX /ZI /Od /I "Include" /I "..\WspPrj\Include" /I "..\Bce" /I ".." /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /FR /YX"stdAfx.h" /FD /c
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
# SUBTRACT LINK32 /pdb:none /incremental:no
# Begin Special Build Tool
TargetPath=\WspHead\Output\Tmp\Debug\WspMap\WSPMap.exe
SOURCE="$(InputPath)"
PostBuild_Desc=Ausgabedatei wird kopiert...
PostBuild_Cmds=copy $(TargetPath) ..\..\Output\Debug
# End Special Build Tool

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "WSPMap__"
# PROP BASE Intermediate_Dir "WSPMap__"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\Output\Tmp\Release\WspMap"
# PROP Intermediate_Dir "..\..\Output\Tmp\Release\WspMap"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /I "..\Common\Include" /I "..\Esri\Include" /I "Include" /I "..\WspPrj\Include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /YX"stdafx.h" /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "Include" /I "..\WspPrj\Include" /I "..\Bce" /I ".." /D "WIN32" /D "_NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /FR /YX"stdAfx.h" /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x407 /i "Include" /d "_DEBUG"
# ADD RSC /l 0x407 /i "Include" /d "_NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 version.lib /nologo /subsystem:windows /machine:I386
# SUBTRACT LINK32 /verbose /pdb:none /incremental:no
# Begin Special Build Tool
TargetPath=\WspHead\Output\Tmp\Release\WspMap\WSPMap.exe
SOURCE="$(InputPath)"
PostBuild_Desc=Ausgabedatei wird kopiert...
PostBuild_Cmds=copy $(TargetPath) ..\..\Output\Release
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "WSPMap - Win32 Debug"
# Name "WSPMap - Win32 Release"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\source\advlabp.cpp
# End Source File
# Begin Source File

SOURCE=.\source\brkssblp.cpp
# End Source File
# Begin Source File

SOURCE=.\source\childfrm.cpp
# End Source File
# Begin Source File

SOURCE=.\source\ClipObjectController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\colorbtnex.cpp
# End Source File
# Begin Source File

SOURCE=.\source\DeleteObjectController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\DeletePointController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\ExtendProfileController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\extendprofiledlg.cpp
# End Source File
# Begin Source File

SOURCE=.\source\FlipProfileController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\FollowCursorControler.cpp
# End Source File
# Begin Source File

SOURCE=.\source\FollowProfileControler.cpp
# End Source File
# Begin Source File

SOURCE=.\source\GenerateProfileController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\genprofiledlg.cpp
# End Source File
# Begin Source File

SOURCE=.\source\GroupController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\imlayer.cpp
# End Source File
# Begin Source File

SOURCE=.\source\improps.cpp
# End Source File
# Begin Source File

SOURCE=.\source\InsertPointController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\IPrintRectFont.cpp
# End Source File
# Begin Source File

SOURCE=.\source\layer.cpp
# End Source File
# Begin Source File

SOURCE=.\source\layerdata.cpp
# End Source File
# Begin Source File

SOURCE=.\source\lgndbar.cpp
# End Source File
# Begin Source File

SOURCE=.\source\LinePointMover.cpp
# End Source File
# Begin Source File

SOURCE=.\source\mainfrm.cpp
# End Source File
# Begin Source File

SOURCE=.\source\mapdoc.cpp
# End Source File
# Begin Source File

SOURCE=.\source\mapdocdata.cpp
# End Source File
# Begin Source File

SOURCE=.\source\maphelper.cpp
# End Source File
# Begin Source File

SOURCE=.\source\maplayer.cpp
# End Source File
# Begin Source File

SOURCE=.\source\MapLayout.cpp
# End Source File
# Begin Source File

SOURCE=.\source\MapObject.cpp
# End Source File
# Begin Source File

SOURCE=.\source\MapperHelpMap.cpp
# End Source File
# Begin Source File

SOURCE=.\source\mappreview.cpp
# End Source File
# Begin Source File

SOURCE=.\source\mappreviewbar.cpp
# End Source File
# Begin Source File

SOURCE=.\source\mapproj.cpp
# End Source File
# Begin Source File

SOURCE=.\source\MapProps.cpp
# End Source File
# Begin Source File

SOURCE=.\source\maprend.cpp
# End Source File
# Begin Source File

SOURCE=.\source\MapStateProfiles.cpp
# End Source File
# Begin Source File

SOURCE=.\source\mapview.cpp
# End Source File
# Begin Source File

SOURCE=.\source\MarkMapObject.cpp
# End Source File
# Begin Source File

SOURCE=.\source\mlpdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\source\mofont.cpp
# End Source File
# Begin Source File

SOURCE=.\source\MoveObjectController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\MovePointController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\MultiMapController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\newldlg.cpp
# End Source File
# Begin Source File

SOURCE=.\source\NewObjectController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\NilController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\nmapdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\source\nutzklass.cpp
# End Source File
# Begin Source File

SOURCE=.\source\obqrydlg.cpp
# End Source File
# Begin Source File

SOURCE=.\source\observerdialog.cpp
# End Source File
# Begin Source File

SOURCE=.\source\openmdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\source\ovrvwbar.cpp
# End Source File
# Begin Source File

SOURCE=.\source\PanController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\printborderpage.cpp
# ADD CPP /I "..\include"
# End Source File
# Begin Source File

SOURCE=.\source\printimagepage.cpp
# ADD CPP /I "..\include"
# End Source File
# Begin Source File

SOURCE=.\source\printlegend2page.cpp
# ADD CPP /I "..\include"
# End Source File
# Begin Source File

SOURCE=.\source\PrintMapPage.cpp
# End Source File
# Begin Source File

SOURCE=.\source\printolepage.cpp
# End Source File
# Begin Source File

SOURCE=.\source\PrintRect.cpp
# End Source File
# Begin Source File

SOURCE=.\source\PrintRectImage.cpp
# End Source File
# Begin Source File

SOURCE=.\source\PrintRectLegend.cpp
# End Source File
# Begin Source File

SOURCE=.\source\PrintRectLegend2.cpp
# End Source File
# Begin Source File

SOURCE=.\source\PrintRectMap.cpp
# End Source File
# Begin Source File

SOURCE=.\source\PrintRectOle.cpp
# End Source File
# Begin Source File

SOURCE=.\source\PrintRectText.cpp
# End Source File
# Begin Source File

SOURCE=.\source\printscaledefdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\source\printtextpage.cpp
# ADD CPP /I "..\include"
# End Source File
# Begin Source File

SOURCE=.\source\profilauswahl.cpp
# End Source File
# Begin Source File

SOURCE=.\source\ProfileDistancer.cpp
# End Source File
# Begin Source File

SOURCE=.\source\profileditor.cpp
# End Source File
# Begin Source File

SOURCE=.\source\profilmodel.cpp
# End Source File
# Begin Source File

SOURCE=.\source\progressdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\source\QueryObjectController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\scalebar.cpp
# End Source File
# Begin Source File

SOURCE=.\source\SelectProfilController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\ShapeMover.cpp
# End Source File
# Begin Source File

SOURCE=.\source\ShowCoordsController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\snglsblp.cpp
# End Source File
# Begin Source File

SOURCE=.\source\splash.cpp
# End Source File
# Begin Source File

SOURCE=.\source\stdlabp.cpp
# End Source File
# Begin Source File

SOURCE=.\source\strangdistancedlg.cpp
# ADD CPP /I "..\include"
# End Source File
# Begin Source File

SOURCE=.\source\tincutdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\source\updatedatablocksdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\source\valsblp.cpp
# End Source File
# Begin Source File

SOURCE=.\source\verschneiddlg.cpp
# End Source File
# Begin Source File

SOURCE=.\source\volumedlg.cpp
# ADD CPP /I "..\include"
# End Source File
# Begin Source File

SOURCE=.\source\WSPMap.cpp
# End Source File
# Begin Source File

SOURCE=.\source\ZoomController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\ZoomOutController.cpp
# End Source File
# Begin Source File

SOURCE=.\source\zuordnung.cpp
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\include\res\bitmap48.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\filebar.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\helpbar.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\idr_mapt.ico
# End Source File
# Begin Source File

SOURCE=.\include\res\inform.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\mapbar.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\objectba.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\pattrn01.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\pattrn2.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\pattrn3.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\profilau.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\theme.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\toolbar.bmp
# End Source File
# Begin Source File

SOURCE=.\include\res\wspmap.ico
# End Source File
# Begin Source File

SOURCE=.\include\WSPMap.rc
# End Source File
# Begin Source File

SOURCE=.\include\res\wspmap.rc2
# End Source File
# End Group
# Begin Group "MapObjects"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\mapobjects\mapobjects2.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mo2defs.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moaddresslocation.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moaddresslocation.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mochartrenderer.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mochartrenderer.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moclassbreaksrenderer.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moclassbreaksrenderer.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\modataconnection.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\modataconnection.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\modatum.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\modatum.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\modotdensityrenderer.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\modotdensityrenderer.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moellipse.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moellipse.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moeventrenderer.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moeventrenderer.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mofield.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mofield.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mofields.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mofields.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mogeocoder.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mogeocoder.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mogeocoordsys.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mogeocoordsys.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mogeodataset.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mogeodataset.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mogeodatasets.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mogeodatasets.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mogeoevent.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mogeoevent.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mogeotransformation.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mogeotransformation.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mogrouprenderer.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mogrouprenderer.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moimagelayer.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moimagelayer.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\molabelplacer.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\molabelplacer.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\molabelrenderer.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\molabelrenderer.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\molayers.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\molayers.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\molegend.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\molegend.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moline.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moline.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\momap.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\momap.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\momaplayer.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\momaplayer.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moparts.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moparts.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mopicture.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mopicture.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moplacelocator.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moplacelocator.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mopoint.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mopoint.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mopoints.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mopoints.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mopolygon.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mopolygon.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moprimemeridian.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moprimemeridian.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moprojcoordsys.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moprojcoordsys.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moprojection.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moprojection.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\morecordset.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\morecordset.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\morectangle.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\morectangle.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mosbextent.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mosbextent.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moscalebar.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\moscalebar.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mospheroid.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mospheroid.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mostandardizer.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mostandardizer.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mostatistics.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mostatistics.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mostrings.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mostrings.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mosymbol.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mosymbol.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\motable.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\motable.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\motabledesc.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\motabledesc.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\motextsymbol.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\motextsymbol.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\motrackinglayer.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\motrackinglayer.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mounit.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mounit.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\movaluemaprenderer.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\movaluemaprenderer.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mozrenderer.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\mozrenderer.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\olefont.cpp
# End Source File
# Begin Source File

SOURCE=.\mapobjects\olefont.h
# End Source File
# Begin Source File

SOURCE=.\mapobjects\pedef.h
# End Source File
# End Group
# Begin Group "Help Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\hlp\AfxCore.rtf

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\AfxPrint.rtf

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\AppExit.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\Bullet.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\CurArw2.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\CurArw4.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\CurHelp.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\EditCopy.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\EditCut.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\EditPast.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\EditUndo.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\FileNew.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\FileOpen.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\FilePrnt.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\FileSave.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\HlpSBar.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\HlpTBar.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\RecFirst.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\RecLast.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\RecNext.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\RecPrev.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\Scmax.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\ScMenu.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\Scmin.bmp

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\hlp\wspmap.hpj

!IF  "$(CFG)" == "WSPMap - Win32 Debug"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Debug\"
# PROP Ignore_Default_Tool 1

!ELSEIF  "$(CFG)" == "WSPMap - Win32 Release"

# PROP Intermediate_Dir "..\..\..\tests\wspwin\Release\"
# PROP Ignore_Default_Tool 1

!ENDIF 

# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\include\advlabp.h
# End Source File
# Begin Source File

SOURCE=.\include\brkssblp.h
# End Source File
# Begin Source File

SOURCE=.\Include\ChildFrm.h
# End Source File
# Begin Source File

SOURCE=.\include\ClipObjectController.h
# End Source File
# Begin Source File

SOURCE=.\include\colorbtnex.h
# End Source File
# Begin Source File

SOURCE=.\include\DeleteObjectController.h
# End Source File
# Begin Source File

SOURCE=.\include\DeletePointController.h
# End Source File
# Begin Source File

SOURCE=.\include\ExtendProfileController.h
# End Source File
# Begin Source File

SOURCE=.\include\extendprofiledlg.h
# End Source File
# Begin Source File

SOURCE=.\include\FlipProfileController.h
# End Source File
# Begin Source File

SOURCE=.\include\FollowCursorControler.h
# End Source File
# Begin Source File

SOURCE=.\include\FollowProfileControler.h
# End Source File
# Begin Source File

SOURCE=.\include\GenerateProfileController.h
# End Source File
# Begin Source File

SOURCE=.\include\genprofiledlg.h
# End Source File
# Begin Source File

SOURCE=.\include\GroupController.h
# End Source File
# Begin Source File

SOURCE=.\include\IMapChanger.h
# End Source File
# Begin Source File

SOURCE=.\include\imapcontroller.h
# End Source File
# Begin Source File

SOURCE=.\Include\ImLayer.h
# End Source File
# Begin Source File

SOURCE=.\include\improps.h
# End Source File
# Begin Source File

SOURCE=.\include\IMultiMapController.h
# End Source File
# Begin Source File

SOURCE=.\include\InsertPointController.h
# End Source File
# Begin Source File

SOURCE=.\include\IPrintRectFont.h
# End Source File
# Begin Source File

SOURCE=.\include\layer.h
# End Source File
# Begin Source File

SOURCE=.\include\layerdata.h
# End Source File
# Begin Source File

SOURCE=.\Include\lgndbar.h
# End Source File
# Begin Source File

SOURCE=.\include\LinePointMover.h
# End Source File
# Begin Source File

SOURCE=.\Include\MainFrm.h
# End Source File
# Begin Source File

SOURCE=.\Include\MapDoc.h
# End Source File
# Begin Source File

SOURCE=.\include\mapdocdata.h
# End Source File
# Begin Source File

SOURCE=.\include\MapDocListener.h
# End Source File
# Begin Source File

SOURCE=.\include\maphelper.h
# End Source File
# Begin Source File

SOURCE=.\Include\MapLayer.h
# End Source File
# Begin Source File

SOURCE=.\include\MapLayout.h
# End Source File
# Begin Source File

SOURCE=.\include\MapObject.h
# End Source File
# Begin Source File

SOURCE=.\include\MapperHelpMap.h
# End Source File
# Begin Source File

SOURCE=.\include\mappreview.h
# End Source File
# Begin Source File

SOURCE=.\include\mappreviewbar.h
# End Source File
# Begin Source File

SOURCE=.\Include\mapproj.h
# End Source File
# Begin Source File

SOURCE=.\include\MapProps.h
# End Source File
# Begin Source File

SOURCE=.\include\maprend.h
# End Source File
# Begin Source File

SOURCE=.\include\MapStateProfiles.h
# End Source File
# Begin Source File

SOURCE=.\Include\MapView.h
# End Source File
# Begin Source File

SOURCE=.\include\MarkMapObject.h
# End Source File
# Begin Source File

SOURCE=.\include\mlpdlg.h
# End Source File
# Begin Source File

SOURCE=.\include\mofont.h
# End Source File
# Begin Source File

SOURCE=.\include\MoveObjectController.h
# End Source File
# Begin Source File

SOURCE=.\include\MovePointController.h
# End Source File
# Begin Source File

SOURCE=.\include\MultiMapController.h
# End Source File
# Begin Source File

SOURCE=.\include\newldlg.h
# End Source File
# Begin Source File

SOURCE=.\include\NewObjectController.h
# End Source File
# Begin Source File

SOURCE=.\include\NilController.h
# End Source File
# Begin Source File

SOURCE=.\include\nmapdlg.h
# End Source File
# Begin Source File

SOURCE=.\include\nutzklass.h
# End Source File
# Begin Source File

SOURCE=.\include\obqrydlg.h
# End Source File
# Begin Source File

SOURCE=.\include\observerdialog.h
# End Source File
# Begin Source File

SOURCE=.\Include\openmdlg.h
# End Source File
# Begin Source File

SOURCE=.\Include\ovrvwbar.h
# End Source File
# Begin Source File

SOURCE=.\include\PanController.h
# End Source File
# Begin Source File

SOURCE=.\include\printborderpage.h
# End Source File
# Begin Source File

SOURCE=.\include\printimagepage.h
# End Source File
# Begin Source File

SOURCE=.\include\printlegend2page.h
# End Source File
# Begin Source File

SOURCE=.\include\PrintMapPage.h
# End Source File
# Begin Source File

SOURCE=.\include\PrintRect.h
# End Source File
# Begin Source File

SOURCE=.\include\PrintRectImage.h
# End Source File
# Begin Source File

SOURCE=.\include\PrintRectLegend.h
# End Source File
# Begin Source File

SOURCE=.\include\PrintRectLegend2.h
# End Source File
# Begin Source File

SOURCE=.\include\PrintRectMap.h
# End Source File
# Begin Source File

SOURCE=.\include\PrintRectOle.h
# End Source File
# Begin Source File

SOURCE=.\include\PrintRectText.h
# End Source File
# Begin Source File

SOURCE=.\include\printtextpage.h
# End Source File
# Begin Source File

SOURCE=.\include\profilauswahl.h
# End Source File
# Begin Source File

SOURCE=.\include\ProfileDistancer.h
# End Source File
# Begin Source File

SOURCE=.\include\profileditor.h
# End Source File
# Begin Source File

SOURCE=.\include\profilmodel.h
# End Source File
# Begin Source File

SOURCE=.\include\progressdlg.h
# End Source File
# Begin Source File

SOURCE=.\include\QueryObjectController.h
# End Source File
# Begin Source File

SOURCE=.\include\resource.h
# End Source File
# Begin Source File

SOURCE=.\include\scalebar.h
# End Source File
# Begin Source File

SOURCE=.\include\SelectProfilController.h
# End Source File
# Begin Source File

SOURCE=.\include\ShapeMover.h
# End Source File
# Begin Source File

SOURCE=.\include\ShowCoordsController.h
# End Source File
# Begin Source File

SOURCE=.\include\snglsblp.h
# End Source File
# Begin Source File

SOURCE=.\include\splash.h
# End Source File
# Begin Source File

SOURCE=.\include\stdafx.h
# End Source File
# Begin Source File

SOURCE=.\include\stdlabp.h
# End Source File
# Begin Source File

SOURCE=.\include\strangdistancedlg.h
# End Source File
# Begin Source File

SOURCE=.\include\tincutdlg.h
# End Source File
# Begin Source File

SOURCE=.\include\updatedatablocksdlg.h
# End Source File
# Begin Source File

SOURCE=.\include\valsblp.h
# End Source File
# Begin Source File

SOURCE=.\include\verschneiddlg.h
# End Source File
# Begin Source File

SOURCE=.\include\VolumeDlg.h
# End Source File
# Begin Source File

SOURCE=.\Include\WSPMap.h
# End Source File
# Begin Source File

SOURCE=.\include\ZoomController.h
# End Source File
# Begin Source File

SOURCE=.\include\ZoomOutController.h
# End Source File
# Begin Source File

SOURCE=.\include\zuordnung.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\History.TxT
# End Source File
# End Target
# End Project
# Section WSPMap : {43FD9771-D8FF-11D1-AF14-006097DA3688}
# 	2:5:Class:CMoProjection
# 	2:10:HeaderFile:moprojection.h
# 	2:8:ImplFile:moprojection.cpp
# End Section
# Section WSPMap : {F9C06032-DA8D-11D1-AF16-006097DA3688}
# 	2:5:Class:CMoPrimeMeridian
# 	2:10:HeaderFile:moprimemeridian.h
# 	2:8:ImplFile:moprimemeridian.cpp
# End Section
# Section WSPMap : {E6E17E86-DF38-11CF-8E74-00A0C90F26F8}
# 	2:5:Class:CSlider
# 	2:10:HeaderFile:slider.h
# 	2:8:ImplFile:slider.cpp
# End Section
# Section WSPMap : {9BD6A676-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoClassBreaksRenderer
# 	2:10:HeaderFile:moclassbreaksrenderer.h
# 	2:8:ImplFile:moclassbreaksrenderer.cpp
# End Section
# Section WSPMap : {9BD6A661-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoPolygon
# 	2:10:HeaderFile:mopolygon.h
# 	2:8:ImplFile:mopolygon.cpp
# End Section
# Section WSPMap : {DBC22C84-03EA-11CF-BABB-444553540000}
# 	2:5:Class:C_Line
# 	2:10:HeaderFile:_line.h
# 	2:8:ImplFile:_line.cpp
# End Section
# Section WSPMap : {9BD6A67D-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoTrackingLayer
# 	2:10:HeaderFile:motrackinglayer.h
# 	2:8:ImplFile:motrackinglayer.cpp
# End Section
# Section WSPMap : {9BD6A696-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoParts
# 	2:10:HeaderFile:moparts.h
# 	2:8:ImplFile:moparts.cpp
# End Section
# Section WSPMap : {9BD6A65D-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoFields
# 	2:10:HeaderFile:mofields.h
# 	2:8:ImplFile:mofields.cpp
# End Section
# Section WSPMap : {83259D42-4A42-11D2-AF7A-006097DA3688}
# 	2:5:Class:CMoChartRenderer
# 	2:10:HeaderFile:mochartrenderer.h
# 	2:8:ImplFile:mochartrenderer.cpp
# End Section
# Section WSPMap : {F9C06026-DA8D-11D1-AF16-006097DA3688}
# 	2:5:Class:CMoGeoCoordSys
# 	2:10:HeaderFile:mogeocoordsys.h
# 	2:8:ImplFile:mogeocoordsys.cpp
# End Section
# Section WSPMap : {AB29FD80-F195-11CE-B3AC-88C801C10000}
# 	2:5:Class:C_Layers
# 	2:10:HeaderFile:_layers.h
# 	2:8:ImplFile:_layers.cpp
# End Section
# Section WSPMap : {9BD6A681-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoEllipse
# 	2:10:HeaderFile:moellipse.h
# 	2:8:ImplFile:moellipse.cpp
# End Section
# Section WSPMap : {79CB8A12-0D8B-11CF-8D83-0800096E6491}
# 	2:5:Class:C_Symbol
# 	2:10:HeaderFile:_symbol.h
# 	2:8:ImplFile:_symbol.cpp
# End Section
# Section WSPMap : {9160E31F-2DED-11D1-86C5-080009EE4E46}
# 	2:21:DefaultSinkHeaderFile:legend.h
# 	2:16:DefaultSinkClass:CLegend
# End Section
# Section WSPMap : {9BD6A685-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoTable
# 	2:10:HeaderFile:motable.h
# 	2:8:ImplFile:motable.cpp
# End Section
# Section WSPMap : {9BD6A64C-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoGeoDatasets
# 	2:10:HeaderFile:mogeodatasets.h
# 	2:8:ImplFile:mogeodatasets.cpp
# End Section
# Section WSPMap : {9BD6A665-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoPoints
# 	2:10:HeaderFile:mopoints.h
# 	2:8:ImplFile:mopoints.cpp
# End Section
# Section WSPMap : {9BD6A655-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoMapLayer
# 	2:10:HeaderFile:momaplayer.h
# 	2:8:ImplFile:momaplayer.cpp
# End Section
# Section WSPMap : {9BD6A670-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoValueMapRenderer
# 	2:10:HeaderFile:movaluemaprenderer.h
# 	2:8:ImplFile:movaluemaprenderer.cpp
# End Section
# Section WSPMap : {7212AB53-049E-11D1-869E-080009EE4E46}
# 	2:21:DefaultSinkHeaderFile:moscalebar.h
# 	2:16:DefaultSinkClass:CMoScaleBar
# End Section
# Section WSPMap : {9BD6A650-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoGeoDataset
# 	2:10:HeaderFile:mogeodataset.h
# 	2:8:ImplFile:mogeodataset.cpp
# End Section
# Section WSPMap : {9BD6A659-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoRecordset
# 	2:10:HeaderFile:morecordset.h
# 	2:8:ImplFile:morecordset.cpp
# End Section
# Section WSPMap : {F9C06035-DA8D-11D1-AF16-006097DA3688}
# 	2:5:Class:CMoGeoTransformation
# 	2:10:HeaderFile:mogeotransformation.h
# 	2:8:ImplFile:mogeotransformation.cpp
# End Section
# Section WSPMap : {9BD6A649-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMap2
# 	2:10:HeaderFile:map2.h
# 	2:8:ImplFile:map2.cpp
# End Section
# Section WSPMap : {77F81760-040C-11CF-BABB-444553540000}
# 	2:5:Class:C_Points
# 	2:10:HeaderFile:_points.h
# 	2:8:ImplFile:_points.cpp
# End Section
# Section WSPMap : {356EF6C6-73E3-11D2-BF0D-0060082D41FB}
# 	2:5:Class:CMoEventRenderer
# 	2:10:HeaderFile:moeventrenderer.h
# 	2:8:ImplFile:moeventrenderer.cpp
# End Section
# Section WSPMap : {96983821-25EB-11D4-9FB9-00C04F8ECE6F}
# 	2:5:Class:CMoSbExtent
# 	2:10:HeaderFile:mosbextent.h
# 	2:8:ImplFile:mosbextent.cpp
# End Section
# Section WSPMap : {F9C0602C-DA8D-11D1-AF16-006097DA3688}
# 	2:5:Class:CMoDatum
# 	2:10:HeaderFile:modatum.h
# 	2:8:ImplFile:modatum.cpp
# End Section
# Section WSPMap : {364E5DC2-4123-11CF-8682-00805F7CED21}
# 	2:5:Class:C_GeoEvent
# 	2:10:HeaderFile:_geoevent.h
# 	2:8:ImplFile:_geoevent.cpp
# End Section
# Section WSPMap : {2637C6F3-0472-11D2-909C-00600826393D}
# 	2:5:Class:CMoStandardizer
# 	2:10:HeaderFile:mostandardizer.h
# 	2:8:ImplFile:mostandardizer.cpp
# End Section
# Section WSPMap : {7212AB52-049E-11D1-869E-080009EE4E46}
# 	2:5:Class:CMoScaleBar
# 	2:10:HeaderFile:moscalebar.h
# 	2:8:ImplFile:moscalebar.cpp
# End Section
# Section WSPMap : {6E9D12DF-A025-11D2-9F40-00C04F8ECE6F}
# 	2:21:DefaultSinkHeaderFile:legend.h
# 	2:16:DefaultSinkClass:CMoLegend
# End Section
# Section WSPMap : {9BD6A694-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoPlaceLocator
# 	2:10:HeaderFile:moplacelocator.h
# 	2:8:ImplFile:moplacelocator.cpp
# End Section
# Section WSPMap : {83259D40-4A42-11D2-AF7A-006097DA3688}
# 	2:5:Class:CMoGroupRenderer
# 	2:10:HeaderFile:mogrouprenderer.h
# 	2:8:ImplFile:mogrouprenderer.cpp
# End Section
# Section WSPMap : {9BD6A65B-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoPoint
# 	2:10:HeaderFile:mopoint.h
# 	2:8:ImplFile:mopoint.cpp
# End Section
# Section WSPMap : {9BD6A64B-CE75-11D1-AF04-204C4F4F5020}
# 	2:21:DefaultSinkHeaderFile:map.h
# 	2:16:DefaultSinkClass:CMoMap
# End Section
# Section WSPMap : {9BD6A674-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoStatistics
# 	2:10:HeaderFile:mostatistics.h
# 	2:8:ImplFile:mostatistics.cpp
# End Section
# Section WSPMap : {F9C06029-DA8D-11D1-AF16-006097DA3688}
# 	2:5:Class:CMoUnit
# 	2:10:HeaderFile:mounit.h
# 	2:8:ImplFile:mounit.cpp
# End Section
# Section WSPMap : {9924A731-E642-11CF-976C-00805F7C9CD7}
# 	2:5:Class:C_Parts
# 	2:10:HeaderFile:_parts.h
# 	2:8:ImplFile:_parts.cpp
# End Section
# Section WSPMap : {540FC6C4-4603-11CF-9243-0080C71A417D}
# 	2:5:Class:C_Ellipse
# 	2:10:HeaderFile:_ellipse.h
# 	2:8:ImplFile:_ellipse.cpp
# End Section
# Section WSPMap : {83259D44-4A42-11D2-AF7A-006097DA3688}
# 	2:5:Class:CMoLabelPlacer
# 	2:10:HeaderFile:molabelplacer.h
# 	2:8:ImplFile:molabelplacer.cpp
# End Section
# Section WSPMap : {9BD6A678-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoDotDensityRenderer
# 	2:10:HeaderFile:modotdensityrenderer.h
# 	2:8:ImplFile:modotdensityrenderer.cpp
# End Section
# Section WSPMap : {9BD6A65F-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoField
# 	2:10:HeaderFile:mofield.h
# 	2:8:ImplFile:mofield.cpp
# End Section
# Section WSPMap : {0C8392E4-3CC4-11D2-8AF6-0060082D41FB}
# 	2:5:Class:CMoZRenderer
# 	2:10:HeaderFile:mozrenderer.h
# 	2:8:ImplFile:mozrenderer.cpp
# End Section
# Section WSPMap : {9BD6A663-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoLine
# 	2:10:HeaderFile:moline.h
# 	2:8:ImplFile:moline.cpp
# End Section
# Section WSPMap : {9BD6A67F-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoGeoEvent
# 	2:10:HeaderFile:mogeoevent.h
# 	2:8:ImplFile:mogeoevent.cpp
# End Section
# Section WSPMap : {A484B95A-E99F-11D1-97BA-080009EE4E46}
# 	2:5:Class:CLegend
# 	2:10:HeaderFile:legend.h
# 	2:8:ImplFile:legend.cpp
# End Section
# Section WSPMap : {8725D862-0274-11CF-BABB-444553540000}
# 	2:5:Class:C_Point
# 	2:10:HeaderFile:_point.h
# 	2:8:ImplFile:_point.cpp
# End Section
# Section WSPMap : {9BD6A683-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoImageLayer
# 	2:10:HeaderFile:moimagelayer.h
# 	2:8:ImplFile:moimagelayer.cpp
# End Section
# Section WSPMap : {9BD6A68A-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoTableDesc
# 	2:10:HeaderFile:motabledesc.h
# 	2:8:ImplFile:motabledesc.cpp
# End Section
# Section WSPMap : {9BD6A67A-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoLabelRenderer
# 	2:10:HeaderFile:molabelrenderer.h
# 	2:8:ImplFile:molabelrenderer.cpp
# End Section
# Section WSPMap : {6E9D12DE-A025-11D2-9F40-00C04F8ECE6F}
# 	2:5:Class:CMoLegend
# 	2:10:HeaderFile:molegend.h
# 	2:8:ImplFile:molegend.cpp
# End Section
# Section WSPMap : {9BD6A657-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoRectangle
# 	2:10:HeaderFile:morectangle.h
# 	2:8:ImplFile:morectangle.cpp
# End Section
# Section WSPMap : {F9C06023-DA8D-11D1-AF16-006097DA3688}
# 	2:5:Class:CMoProjCoordSys
# 	2:10:HeaderFile:moprojcoordsys.h
# 	2:8:ImplFile:moprojcoordsys.cpp
# End Section
# Section WSPMap : {F9C0602F-DA8D-11D1-AF16-006097DA3688}
# 	2:5:Class:CMoSpheroid
# 	2:10:HeaderFile:mospheroid.h
# 	2:8:ImplFile:mospheroid.cpp
# End Section
# Section WSPMap : {BEF6E003-A874-101A-8BBA-00AA00300CAB}
# 	2:5:Class:COleFont
# 	2:10:HeaderFile:font.h
# 	2:8:ImplFile:font.cpp
# End Section
# Section WSPMap : {9BD6A66E-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoSymbol
# 	2:10:HeaderFile:mosymbol.h
# 	2:8:ImplFile:mosymbol.cpp
# End Section
# Section WSPMap : {9BD6A687-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoTextSymbol
# 	2:10:HeaderFile:motextsymbol.h
# 	2:8:ImplFile:motextsymbol.cpp
# End Section
# Section WSPMap : {9BD6A64E-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoDataConnection
# 	2:10:HeaderFile:modataconnection.h
# 	2:8:ImplFile:modataconnection.cpp
# End Section
# Section WSPMap : {9BD6A652-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoLayers
# 	2:10:HeaderFile:molayers.h
# 	2:8:ImplFile:molayers.cpp
# End Section
# Section WSPMap : {7212AB50-049E-11D1-869E-080009EE4E46}
# 	2:5:Class:CMoSbExtent
# 	2:10:HeaderFile:mosbextent.h
# 	2:8:ImplFile:mosbextent.cpp
# End Section
# Section WSPMap : {8CD90DF8-FE97-11CE-8D7B-0800096E6491}
# 	2:5:Class:C_Rectangle
# 	2:10:HeaderFile:_rectangle.h
# 	2:8:ImplFile:_rectangle.cpp
# End Section
# Section WSPMap : {9BD6A672-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoStrings
# 	2:10:HeaderFile:mostrings.h
# 	2:8:ImplFile:mostrings.cpp
# End Section
# Section WSPMap : {9BD6A68E-CE75-11D1-AF04-204C4F4F5020}
# 	2:5:Class:CMoAddressLocation
# 	2:10:HeaderFile:moaddresslocation.h
# 	2:8:ImplFile:moaddresslocation.cpp
# End Section
# Section WSPMap : {364E5DC0-4123-11CF-8682-00805F7CED21}
# 	2:5:Class:C_TrackingLayer
# 	2:10:HeaderFile:_trackinglayer.h
# 	2:8:ImplFile:_trackinglayer.cpp
# End Section
# Section WSPMap : {373FF7F0-EB8B-11CD-8820-08002B2F4F5A}
# 	2:21:DefaultSinkHeaderFile:slider.h
# 	2:16:DefaultSinkClass:CSlider
# End Section
# Section WSPMap : {7BF80981-BF32-101A-8BBB-00AA00300CAB}
# 	2:5:Class:CPicture
# 	2:10:HeaderFile:picture.h
# 	2:8:ImplFile:picture.cpp
# End Section
# Section WSPMap : {DBC22C80-03EA-11CF-BABB-444553540000}
# 	2:5:Class:C_Polygon
# 	2:10:HeaderFile:_polygon.h
# 	2:8:ImplFile:_polygon.cpp
# End Section
# Section WSPMap : {2637C6F5-0472-11D2-909C-00600826393D}
# 	2:5:Class:CMoGeocoder
# 	2:10:HeaderFile:mogeocoder.h
# 	2:8:ImplFile:mogeocoder.cpp
# End Section
