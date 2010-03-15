// browsetoolbar.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "browsetoolbar.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CBrowseToolBar

CBrowseToolBar::CBrowseToolBar()
{
  m_comboHeight = 100;
  m_comboWidth = 80;
}

CBrowseToolBar::~CBrowseToolBar()
{
}

#define ON_CONTROL_RANGE(wNotifyCode, id, idLast, memberFxn) \
	{ WM_COMMAND, (WORD)wNotifyCode, (WORD)id, (WORD)idLast, AfxSig_vw, \
		(AFX_PMSG)(void (AFX_MSG_CALL CCmdTarget::*)(UINT))&memberFxn },

// ON_CONTROL_EX_RANGE: wie ON_CONTROL_RANGE nur Rückgabewert ist bool, ob
// diese ID behandelt wurde
#define ON_CONTROL_EX_RANGE(wNotifyCode, id, idLast, memberFxn) \
  { WM_COMMAND, (WORD)wNotifyCode, (WORD)id, (WORD)idLast, AfxSig_bw, \
    (AFX_PMSG)(BOOL (AFX_MSG_CALL CCmdTarget::*)(UINT))&memberFxn },

BEGIN_MESSAGE_MAP(CBrowseToolBar, CToolBar)
	//{{AFX_MSG_MAP(CBrowseToolBar)
	//}}AFX_MSG_MAP
  ON_CONTROL_EX_RANGE(CBN_SELCHANGE, 0, 0xffff, OnSelchangeComboBox)  // fängt alle CBN_SELCHANGES ab; in OnSelchangeComboBox wird geschaut, ob die richtige dabei ist, sonst Rückgabewert FALSE
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CBrowseToolBar 

BOOL CBrowseToolBar::OnSelchangeComboBox( UINT nID )
// hierm kommen alle CBN_SELCHANGEs an, ist eine ComboBox dieser toolbar dabei,
// wird TRUE zurückgegeben und die Nachricht als COMMAND and das Hauptfenster
// der Applikation geschickt, sonst FALSE -> die Nachricht wird weiter behandelt
// die Parameter der WM_COMMAND - Message sind : LOWORD(wParam) = m_comboID
//                                               lParam = m_comboBox.GetSafeHwnd()
{
  if ( nID = m_comboID)
  {
    AfxGetMainWnd()->PostMessage( WM_COMMAND, MAKEWPARAM( m_comboID, 0 ),
                                  reinterpret_cast<LPARAM>(m_comboBox.GetSafeHwnd()) );
    return TRUE;
  }
  else
    return FALSE; // andere ComboBox hat gechanged -> Message weiter schicken
};


void CBrowseToolBar::OnBarStyleChange( DWORD dwOldStyle, DWORD dwNewStyle )
{
  if ( dwNewStyle & CBRS_ORIENT_VERT )
    HideComboBox();
  else
    ShowComboBox();

  CToolBar::OnBarStyleChange( dwOldStyle, dwNewStyle );
}

//////////////////////////////////////////////////////////////////////////////
// Überschreibungen

BOOL CBrowseToolBar::Init( CWnd* pParentWnd, UINT replaceID, UINT comboID, 
                           UINT menuID, DWORD dwStyle, UINT toolbarID )
// Created und lädt die Toolbar, Initialisiert die ComboBox
{
  m_comboID = comboID;
  m_replaceID = replaceID;

  CRect rect(0, 0, m_comboWidth, m_comboHeight);
  if ( 
       !Create( pParentWnd, dwStyle, menuID ) || 
       !LoadToolBar( toolbarID ) || 
       !m_comboBox.Create( 
              WS_DISABLED | WS_CHILD | WS_VSCROLL | WS_EX_RIGHT |
              CBS_DROPDOWNLIST | CBS_DISABLENOSCROLL | CBS_AUTOHSCROLL | CBS_HASSTRINGS,
              rect, this, m_comboID ) 
      ) 
    return FALSE;

  HFONT hFont = (HFONT)GetStockObject(DEFAULT_GUI_FONT);
  if (hFont == NULL)
    hFont = (HFONT)GetStockObject(ANSI_VAR_FONT);
  m_comboBox.SendMessage(WM_SETFONT, (WPARAM)hFont);

  ShowComboBox();
  return TRUE;
};

CComboBox* CBrowseToolBar::GetComboBox()
{
  return &m_comboBox;
};

void CBrowseToolBar::ShowComboBox()
{
  // Index des zu ersetzenden Knopfes herausfinden
  int index = CommandToIndex ( m_replaceID );
  if ( index == -1 )
    return;

  CRect rect;
  GetItemRect( index, &rect );

  UINT dummy;

  GetButtonInfo( index, dummy, dummy, m_replaceBitmap );
  SetButtonInfo( index, m_comboID, TBBS_SEPARATOR, m_comboWidth);

  m_comboBox.SetWindowPos(NULL, rect.left, rect.top, 0, 0, SWP_NOZORDER|SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOCOPYBITS);
  m_comboBox.ShowWindow( SW_SHOW );
};

void CBrowseToolBar::HideComboBox()
{
  int index = CommandToIndex( m_comboID );
  if ( index == -1 )
    return;

  SetButtonInfo( index, m_replaceID, TBBS_BUTTON, m_replaceBitmap );
};
