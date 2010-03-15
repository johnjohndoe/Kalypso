// Global.h
//
/////////////////////////////////////////////////////////////////////////////

//#ifndef GLOBAL_H
//#define GLOBAL_H

#define CONVERTERS

#define HORZ_TEXTOFFSET 15
#define VERT_TEXTOFFSET 5

class CDisplayIC : public CDC
{
public:
	CDisplayIC() { CreateIC(_T("DISPLAY"), NULL, NULL, NULL); }
};

struct CCharFormat : public _charformat
{
	CCharFormat() {cbSize = sizeof(_charformat);}
	BOOL operator==(CCharFormat& cf);
};

struct CParaFormat : public _paraformat
{
	CParaFormat() {cbSize = sizeof(_paraformat);}
	BOOL operator==(PARAFORMAT& pf);
};

#include "doctype.h"
#include "chicdial.h"

#include "resource.h"

#include "..\..\commonMfc\commonMfc.h"
#include "..\..\WspPrj\WspPrj.h"

class CEditorDoc;
class CEditorView;

#include "options.h"
#include "pageset.h"
#include "ConvDlg.h"
#include "OpenPDlg.h"
#include "OpenDlg.h"
#include "helpids.h"
#include "strings.h"

#include "multconv.h"

#include "key.h"

#include "listdlg.h"
#include "ddxm.h"
#include "mswd6_32.h"

#include "formatpa.h"
#include "cntritem.h"
#include "childfrm.h"
#include "mainfrm.h"
#include "editdoc.h"
#include "editvw.h"
#include "editor.h"

//#endif	// GLOBAL_H