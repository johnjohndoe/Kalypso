# Name
Name Kalypso

# General Symbol Definitions
!define REGKEY "SOFTWARE\$(^Name)"

# MUI Symbol Definitions
!define MUI_ICON images\kalypso_ico_32.ico
!define MUI_FINISHPAGE_NOAUTOCLOSE
!define MUI_STARTMENUPAGE_REGISTRY_ROOT HKLM
!define MUI_STARTMENUPAGE_REGISTRY_KEY ${REGKEY}\${VERSION}
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME StartMenuGroup
!define MUI_STARTMENUPAGE_DEFAULTFOLDER Kalypso
!define MUI_FINISHPAGE_RUN $INSTDIR\${VERSION}\kalypso.exe
!define MUI_FINISHPAGE_SHOWREADME $INSTDIR\${VERSION}\notes.txt
!define MUI_UNICON images\kalypso_ico_32.ico
!define MUI_UNFINISHPAGE_NOAUTOCLOSE
!define MUI_LANGDLL_REGISTRY_ROOT HKLM
!define MUI_LANGDLL_REGISTRY_KEY ${REGKEY}\${VERSION}
!define MUI_LANGDLL_REGISTRY_VALUENAME InstallerLanguage

# Included files
!include Sections.nsh
!include MUI2.nsh
!include includes\modify.nsh

# Reserved Files
!insertmacro MUI_RESERVEFILE_LANGDLL
ReserveFile "${NSISDIR}\Plugins\AdvSplash.dll"

# Variables
Var StartMenuGroup

# Installer pages
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE license\license.txt
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_STARTMENU Application $StartMenuGroup
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

# Installer languages
!insertmacro MUI_LANGUAGE English
!insertmacro MUI_LANGUAGE German

# Installer attributes
OutFile setup_kalypso_${VERSION}_win32_x86.exe
InstallDir $PROGRAMFILES\Kalypso
CRCCheck on
XPStyle on
ShowInstDetails show
VIAddVersionKey /LANG=${LANG_ENGLISH} ProductName Kalypso
VIAddVersionKey /LANG=${LANG_ENGLISH} ProductVersion "${VERSION}"
VIAddVersionKey /LANG=${LANG_ENGLISH} CompanyName "${COMPANY}"
VIAddVersionKey /LANG=${LANG_ENGLISH} CompanyWebsite "${URL}"
VIAddVersionKey /LANG=${LANG_ENGLISH} FileVersion "${VERSION}"
VIAddVersionKey /LANG=${LANG_ENGLISH} FileDescription ""
VIAddVersionKey /LANG=${LANG_ENGLISH} LegalCopyright ""
InstallDirRegKey HKLM "${REGKEY}\${VERSION}" Path
ShowUninstDetails show

# Installer sections
Section -Main SEC0000
    SetOutPath $INSTDIR\${VERSION}
    SetOverwrite on
    File /r /x .svn data\x86\*
    WriteRegStr HKLM "${REGKEY}\${VERSION}\Components" Main 1
SectionEnd

Section -post SEC0001
    WriteRegStr HKLM "${REGKEY}\${VERSION}" Path $INSTDIR
    SetOutPath $INSTDIR\${VERSION}
    WriteUninstaller $INSTDIR\${VERSION}\uninstall.exe
    !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
    SetOutPath $SMPROGRAMS\$StartMenuGroup
    CreateShortcut "$SMPROGRAMS\$StartMenuGroup\${VERSION}\$(^StartLink).lnk" $INSTDIR\${VERSION}\kalypso.exe
    CreateShortcut "$SMPROGRAMS\$StartMenuGroup\${VERSION}\$(^UninstallLink).lnk" $INSTDIR\${VERSION}\uninstall.exe
    !insertmacro MUI_STARTMENU_WRITE_END
    WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)_${VERSION}" DisplayName "$(^Name) ${VERSION}"
    WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)_${VERSION}" DisplayVersion "${VERSION}"
    WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)_${VERSION}" Publisher "${COMPANY}"
    WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)_${VERSION}" URLInfoAbout "${URL}"
    WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)_${VERSION}" DisplayIcon $INSTDIR\${VERSION}\uninstall.exe
    WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)_${VERSION}" UninstallString $INSTDIR\${VERSION}\uninstall.exe
    WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)_${VERSION}" NoModify 1
    WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)_${VERSION}" NoRepair 1
SectionEnd

# Macro for selecting uninstaller sections
!macro SELECT_UNSECTION SECTION_NAME UNSECTION_ID
    Push $R0
    ReadRegStr $R0 HKLM "${REGKEY}\${VERSION}\Components" "${SECTION_NAME}"
    StrCmp $R0 1 0 next${UNSECTION_ID}
    !insertmacro SelectSection "${UNSECTION_ID}"
    GoTo done${UNSECTION_ID}
next${UNSECTION_ID}:
    !insertmacro UnselectSection "${UNSECTION_ID}"
done${UNSECTION_ID}:
    Pop $R0
!macroend

# Uninstaller sections
Section /o -un.Main UNSEC0000
    RmDir /r /REBOOTOK $INSTDIR\${VERSION}
    RmDir $INSTDIR
    DeleteRegValue HKLM "${REGKEY}\${VERSION}\Components" Main
SectionEnd

Section -un.post UNSEC0001
    DeleteRegKey HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)_${VERSION}"
    Delete /REBOOTOK "$SMPROGRAMS\$StartMenuGroup\${VERSION}\$(^StartLink).lnk"
    Delete /REBOOTOK "$SMPROGRAMS\$StartMenuGroup\${VERSION}\$(^UninstallLink).lnk"
    Delete /REBOOTOK $INSTDIR\${VERSION}\uninstall.exe
    DeleteRegValue HKLM "${REGKEY}\${VERSION}" StartMenuGroup
    DeleteRegValue HKLM "${REGKEY}\${VERSION}" Path
    DeleteRegKey /IfEmpty HKLM "${REGKEY}\${VERSION}\Components"
    DeleteRegKey /IfEmpty HKLM "${REGKEY}\${VERSION}"
    DeleteRegKey /IfEmpty HKLM "${REGKEY}"
    RmDir /REBOOTOK "$SMPROGRAMS\$StartMenuGroup\${VERSION}"
    RmDir "$SMPROGRAMS\$StartMenuGroup"
    RmDir /REBOOTOK $INSTDIR\${VERSION}
    RmDir $INSTDIR
    Push $R0
    StrCpy $R0 $StartMenuGroup 1
    StrCmp $R0 ">" no_smgroup
no_smgroup:
    Pop $R0
SectionEnd

# Installer functions
Function .onInit
    InitPluginsDir
    Push $R1
    File /oname=$PLUGINSDIR\spltmp.bmp images\kalypso_logo.bmp
    advsplash::show 1000 600 400 -1 $PLUGINSDIR\spltmp
    Pop $R1
    Pop $R1
    !insertmacro MUI_LANGDLL_DISPLAY
FunctionEnd

# Uninstaller functions
Function un.onInit
    ReadRegStr $INSTDIR HKLM "${REGKEY}\${VERSION}" Path
    !insertmacro MUI_STARTMENU_GETFOLDER Application $StartMenuGroup
    !insertmacro MUI_UNGETLANGUAGE
    !insertmacro SELECT_UNSECTION Main ${UNSEC0000}
FunctionEnd

# Installer Language Strings
# TODO Update the Language Strings with the appropriate translations.

LangString ^StartLink ${LANG_ENGLISH} "$(^Name)"
LangString ^StartLink ${LANG_GERMAN} "$(^Name)"

LangString ^UninstallLink ${LANG_ENGLISH} "Uninstall $(^Name)"
LangString ^UninstallLink ${LANG_GERMAN} "Uninstall $(^Name)"
