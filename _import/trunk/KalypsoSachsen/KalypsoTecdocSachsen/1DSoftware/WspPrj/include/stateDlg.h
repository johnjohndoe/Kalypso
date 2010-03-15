/////////////////////////////
// Klasse StateDialog      //
/////////////////////////////
// Dialog zum Bearbeiten,  //
// Eingeben der Parameter  //
// eines Zustandes         //
/////////////////////////////

#ifndef _STATE_DLG_H_INCLUDED_
#define _STATE_DLG_H_INCLUDED_

// Typen für den StateDialog

#define STATE_DLG_MASK_NAME   0x01     // = 001b
#define STATE_DLG_MASK_WATER  0x02     // = 010b
#define STATE_DLG_MASK_DATE   0x04     // = 100b
#define STATE_DLG_MASK_ALL    0x07     // = 111b

class StateDialog: public CDialog
{
  // Konstruktion
public:
  StateDialog( WORD maske, CWnd* pParent = NULL );   // Standardkonstruktor
  
  // Dialogfelddaten
  //{{AFX_DATA(StateDialog)
	enum { IDD = IDD_STATE_DIALOG };
	CEdit	m_waterCtrl;
	CEdit	m_nameCtrl;
	CEdit	m_dateCtrl;
	CString	m_date;
	CString	m_name;
	CString	m_staticDate;
	CString	m_staticName;
	CString	m_staticWater;
	CString	m_water;
	//}}AFX_DATA

  COleDateTime m_oleDate;
   
  // Überschreibungen
  // Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
  //{{AFX_VIRTUAL(StateDialog)
protected:
  virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
  //}}AFX_VIRTUAL
  
  // Implementierung
protected:
  WORD m_mask;
  
  // Generierte Nachrichtenzuordnungsfunktionen
  //{{AFX_MSG(StateDialog)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_MSG
  DECLARE_MESSAGE_MAP()
};







#endif _STATE_DLG_H_INCLUDED_
