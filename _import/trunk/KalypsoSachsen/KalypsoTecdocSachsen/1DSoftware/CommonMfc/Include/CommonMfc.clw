; CLW file contains information for the MFC ClassWizard

[General Info]
Version=1
LastClass=CExecuteExtern
LastTemplate=CDialog
NewFileInclude1=#include "stdafx.h"
NewFileInclude2=#include "commonmfc.h"
LastPage=0

ClassCount=23
Class1=CBrushComboBox
Class2=CColorComboBox
Class3=CCoolDialogBar
Class4=CGridCtrl
Class5=CGridDropTarget
Class6=CGraphicComboBox
Class7=CInPlaceEdit
Class8=CComboEdit
Class9=CInPlaceList
Class10=CListCtrlEx
Class11=CLineComboBox
Class12=CMessageBox5
Class13=CMessageBoxWelcher
Class14=CSizingControlBar
Class15=CSCBMiniDockFrameWnd
Class16=CSymbolComboBox
Class17=CTextComboBox
Class18=CTitleTip
Class19=CToolBarEx

ResourceCount=14
Resource1=IDD_EXECUTE_EXTERN
Resource2=IDD_DATABLOCK_TYPE_CHOOSER_DLG
Resource3=IDD_STRING_INPUT
Resource4=IDD_WSP_INSERT
Resource5=IDD_WELCHER (Neutral)
Resource6=IDD_STATE_CHOOSER
Resource7=IDD_ITEM_CHOOSER
Resource8=IDD_STATE_DIALOG
Class20=CWSPInsertDialog
Resource9=IDD_MESSAGE_BOX_2
Resource10=IDR_PROJECT_MANAGER
Resource11=IDD_PROJECT_MANAGER
Class21=CStringInputDlg
Resource12=IDR_MNG_SIMPLE_MENU
Class22=CMessageBox2
Resource13=IDD_CS_REGION
Class23=CExecuteExtern
Resource14=IDD_MESSAGE_BOX_5 (Neutral)

[CLS:CBrushComboBox]
Type=0
BaseClass=CGraphicComboBox
HeaderFile=brushbox.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\brushbox.cpp
LastObject=CBrushComboBox

[CLS:CColorComboBox]
Type=0
BaseClass=CGraphicComboBox
HeaderFile=colorbox.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\colorbox.cpp

[CLS:CCoolDialogBar]
Type=0
BaseClass=CControlBar
HeaderFile=CoolDialogBar.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\CoolDialogBar.cpp

[CLS:CGridCtrl]
Type=0
BaseClass=CWnd
HeaderFile=gridctrl.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\gridctrl.cpp

[CLS:CGridDropTarget]
Type=0
BaseClass=COleDropTarget
HeaderFile=griddroptarget.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\griddroptarget.cpp

[CLS:CGraphicComboBox]
Type=0
BaseClass=CComboBox
HeaderFile=grphbox.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\grphbox.cpp

[CLS:CInPlaceEdit]
Type=0
BaseClass=CEdit
HeaderFile=inplaceedit.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\inplaceedit.cpp

[CLS:CComboEdit]
Type=0
BaseClass=CEdit
HeaderFile=inplacelist.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\inplacelist.cpp

[CLS:CInPlaceList]
Type=0
BaseClass=CComboBox
HeaderFile=inplacelist.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\inplacelist.cpp

[CLS:CListCtrlEx]
Type=0
BaseClass=CListCtrl
HeaderFile=lctrlex.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\lctrlex.cpp

[CLS:CLineComboBox]
Type=0
BaseClass=CGraphicComboBox
HeaderFile=linebox.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\linebox.cpp

[CLS:CMessageBox5]
Type=0
BaseClass=CDialog
HeaderFile=messagebox5.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\messagebox5.cpp

[CLS:CMessageBoxWelcher]
Type=0
BaseClass=CDialog
HeaderFile=messageboxwelcher.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\messageboxwelcher.cpp

[CLS:CSizingControlBar]
Type=0
BaseClass=baseCSizingControlBar
HeaderFile=SizeCBar.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\SizeCBar.cpp

[CLS:CSCBMiniDockFrameWnd]
Type=0
BaseClass=baseCSCBMiniDockFrameWnd
HeaderFile=SizeCBar.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\SizeCBar.cpp

[CLS:CSymbolComboBox]
Type=0
BaseClass=CGraphicComboBox
HeaderFile=symbbox.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\symbbox.cpp

[CLS:CTextComboBox]
Type=0
BaseClass=CGraphicComboBox
HeaderFile=textbox.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\textbox.cpp

[CLS:CTitleTip]
Type=0
BaseClass=CWnd
HeaderFile=titletip.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\titletip.cpp

[CLS:CToolBarEx]
Type=0
BaseClass=CToolBar
HeaderFile=toolbarex.h
ImplementationFile=\Wsp\Source\CommonMfc\Source\toolbarex.cpp

[DLG:IDD_MESSAGE_BOX_5]
Type=1
Class=CMessageBox5

[DLG:IDD_WELCHER]
Type=1
Class=CMessageBoxWelcher

[DLG:IDD_WSP_INSERT]
Type=1
Class=CWSPInsertDialog
ControlCount=5
Control1=IDOK,button,1342242817
Control2=IDCANCEL,button,1342242816
Control3=IDC_WSP_CONSTRAINT_CHECK,button,1342242819
Control4=IDC_WSP_STATIC_ABFLUSS,static,1342308352
Control5=IDC_WSP_CONSTRAINT_EDIT,edit,1350631552

[DLG:IDD_MESSAGE_BOX_5 (Neutral)]
Type=1
Class=?
ControlCount=6
Control1=IDYES,button,1342242817
Control2=IDALWAYS,button,1342242816
Control3=IDNO,button,1342242816
Control4=IDNEVER,button,1342242816
Control5=IDCANCEL,button,1342242816
Control6=IDC_TEXT,static,1342308352

[DLG:IDD_WELCHER (Neutral)]
Type=1
Class=?
ControlCount=8
Control1=IDCANCEL,button,1342242816
Control2=IDFIRST_YES,button,1342242817
Control3=IDFIRST_ALYWAS,button,1342242816
Control4=IDSECOND_YES,button,1342242816
Control5=IDSECOND_ALWAYS,button,1342242816
Control6=IDNO,button,1342242816
Control7=IDNEVER,button,1342242816
Control8=IDC_TEXT,static,1342308352

[DLG:IDD_STATE_DIALOG]
Type=1
Class=?
ControlCount=8
Control1=IDC_STATIC_WATERNAME,static,1342308352
Control2=IDC_WATER_NAME,edit,1350631552
Control3=IDC_STATIC_DATE,static,1342308352
Control4=IDC_DATE,edit,1350631552
Control5=IDC_STATIC_NAME,static,1342308352
Control6=IDC_STATE_NAME,edit,1350631552
Control7=IDOK,button,1342242817
Control8=IDCANCEL,button,1342242816

[DLG:IDD_PROJECT_MANAGER]
Type=1
Class=?
ControlCount=5
Control1=IDC_PROJECT_LIST,SysListView32,1342245389
Control2=IDC_STATE_TREE,SysTreeView32,1342242855
Control3=IDC_DATA_LIST,SysListView32,1342242825
Control4=IDC_STATIC_PROJEKTE,static,1342177793
Control5=IDC_STATIC_PROFILE,static,1342308865

[DLG:IDD_CS_REGION]
Type=1
Class=?
ControlCount=4
Control1=IDOK,button,1342242817
Control2=IDCANCEL,button,1342242816
Control3=IDC_BEGIN,combobox,1344340227
Control4=IDC_END,combobox,1344340227

[DLG:IDD_STATE_CHOOSER]
Type=1
Class=?
ControlCount=3
Control1=IDOK,button,1342242817
Control2=IDCANCEL,button,1342242816
Control3=IDC_STATE_COMBO,combobox,1342242819

[MNU:IDR_PROJECT_MANAGER]
Type=1
Class=?
Command1=ID_MNG_OPEN_PROJECT
Command2=ID_MNG_ADD_PROJECT
Command3=ID_MNG_ADD_PROJECT_ENTRY
Command4=ID_MNG_DELETE_PROJECT_ENTRY
Command5=ID_MNG_DELETE_PROJECT
Command6=ID_MNG_PRINT
Command7=ID_MNG_CLOSE
Command8=ID_MNG_ADD_STATE
Command9=ID_MNG_COPY_STATE
Command10=ID_MNG_DELETE_STATE
Command11=ID_MNG_OPEN_MAP
Command12=ID_MNG_CREATE_MAP
Command13=ID_MNG_DELETE_MAP
Command14=ID_MNG_MENU_CS_DELETE_DB
Command15=ID_MNG_MENU_CS_REFLECT
Command16=ID_MNG_CREATE_LSECTION
Command17=ID_MNG_INSERT_WSP
Command18=ID_MNG_DELETE_WSP
Command19=ID_MNG_LS_EXPORT_TABLE
Command20=ID_MNG_DATENIMPORT_DA66
Command21=ID_MNG_DATENIMPORT_DA50
Command22=ID_MNG_DATENIMPORT_WSV
Command23=ID_MNG_DATENIMPORT_TRIPPLE
Command24=ID_MNG_DATENIMPORT_WST
Command25=ID_MNG_DATENIMPORT_HYK
Command26=ID_MNG_DATENIMPORT_RELI
Command27=ID_MNG_DATENEXPORT_DA66
Command28=ID_MNG_DATENEXPORT_TRIPPLE
Command29=ID_MNG_DATENEXPORT_CSV
Command30=ID_MNG_DATENEXPORT_HYK
Command31=ID_MNG_DATENEXPORT_RELI
CommandCount=31

[MNU:IDR_MNG_SIMPLE_MENU]
Type=1
Class=?
Command1=ID_MNG_CREATE_MAP
Command2=ID_MNG_OPEN_MAP
Command3=ID_MNG_DELETE_MAP
Command4=ID_MNG_COPY_STATE
Command5=ID_MNG_DELETE_STATE
Command6=ID_MNG_CLOSE
CommandCount=6

[CLS:CWSPInsertDialog]
Type=0
HeaderFile=wspinsertdialog.h
ImplementationFile=..\source\wspinsertdialog.cpp
BaseClass=CDialog
Filter=D
VirtualFilter=dWC
LastObject=CWSPInsertDialog

[DLG:IDD_DATABLOCK_TYPE_CHOOSER_DLG]
Type=1
Class=?
ControlCount=3
Control1=IDOK,button,1342242817
Control2=IDCANCEL,button,1342242816
Control3=IDC_DATABLOCK_TYPE_CHOOSER_DLG_LIST,SysListView32,1350682649

[DLG:IDD_ITEM_CHOOSER]
Type=1
Class=?
ControlCount=4
Control1=IDOK,button,1342242817
Control2=IDCANCEL,button,1342242816
Control3=IDC_ITEM_CHOOSER_TEXT,static,1342308352
Control4=IDC_ITEM_CHOOSER_COMBO,combobox,1344339971

[DLG:IDD_STRING_INPUT]
Type=1
Class=CStringInputDlg
ControlCount=4
Control1=IDOK,button,1342242817
Control2=IDCANCEL,button,1342242816
Control3=IDC_STRING_INPUT_TEXT,static,1342308352
Control4=IDC_STRING_INPUT_EDIT,edit,1350631552

[CLS:CStringInputDlg]
Type=0
HeaderFile=stringinputdlg.h
ImplementationFile=..\source\stringinputdlg.cpp
BaseClass=CDialog
Filter=D
VirtualFilter=dWC
LastObject=CStringInputDlg

[DLG:IDD_MESSAGE_BOX_2]
Type=1
Class=CMessageBox2
ControlCount=3
Control1=IDOK,button,1342242816
Control2=IDCANCEL,button,1342242816
Control3=IDC_MESSAGE_BOX_2_TEXT,static,1342308352

[CLS:CMessageBox2]
Type=0
HeaderFile=messagebox2.h
ImplementationFile=..\source\messagebox2.cpp
BaseClass=CDialog
Filter=D
VirtualFilter=dWC
LastObject=CMessageBox2

[DLG:IDD_EXECUTE_EXTERN]
Type=1
Class=CExecuteExtern
ControlCount=2
Control1=IDC_STATIC,static,1342308865
Control2=IDC_EXECUTE_EXTERN_NAME,static,1342308865

[CLS:CExecuteExtern]
Type=0
HeaderFile=executeextern.h
ImplementationFile=..\source\executeextern.cpp
BaseClass=CDialog
Filter=D
VirtualFilter=dWC
LastObject=CExecuteExtern

