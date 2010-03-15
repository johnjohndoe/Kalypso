// mappreviewbar.h: Schnittstelle für die Klasse CMapPreviewBar.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MAPPREVIEWBAR_H__CD496C63_4AC1_11D6_B2C1_00104BB3E525__INCLUDED_)
#define AFX_MAPPREVIEWBAR_H__CD496C63_4AC1_11D6_B2C1_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////
// die DialogBar für die MapPreview
// Wird von CDialogBar abgeleitet, weils die CPrieviewView so braucht
// //////////////////////////////////
class CMapPreviewBar : public CDialogBar  
{
public:
  virtual BOOL PreTranslateMessage( MSG* pMsg );

  BOOL InitDialogBar();

  void ClickButton( const int id );
  
protected:
  CToolTipCtrl m_toolTip;
};

#endif // !defined(AFX_MAPPREVIEWBAR_H__CD496C63_4AC1_11D6_B2C1_00104BB3E525__INCLUDED_)
