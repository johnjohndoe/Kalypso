// profoverv.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CProfilOverview Class

#ifndef _PROPOVERV_H_INCLUDED_
#define _PROPOVERV_H_INCLUDED_

class Project;
class CrossSection;

class CProfilOverview : public CMDIChildWnd
{
  DECLARE_DYNCREATE( CProfilOverview );
// Construction
public:
	CProfilOverview();   // standard constructor
  ~CProfilOverview();

protected:
  void DeleteContents();

// Overrides
public:
  virtual BOOL Create( CMDIFrameWnd* pParent = NULL );
  virtual void PostNcDestroy();

// Attribute
public:
  void SetState( State* state );
  State* GetState() { return m_state; };
  void SetSizes( UINT width, UINT height );

  void SetTopLeftSection( Section* topLeftCs );
  Section* GetTopLeftSection() { return m_topLeftCs; };

protected:
  State* m_state;
  CTypedPtrArray<CObArray, ProfilCtrl*> m_profileWindows;
  UINT m_width;
  UINT m_height;

  Section* m_activeCs; // die zur Zeit aktive Cs
  Section* m_topLeftCs; // die CrossSection, welche gerade oben links ist

// Operationen
  BOOL CreateProfileWindows();

// MessageHandling
protected:
  afx_msg void OnClose();
  afx_msg void OnPaint();
  afx_msg BOOL OnBrowse( UINT nID );
  afx_msg void OnXY();
  afx_msg BOOL OnProfilCtrlDblClk( UINT nID, NMHDR* pNMHDR, LRESULT* pResult );
  afx_msg void OnMDIActivate( BOOL bActivate, CWnd* pActivateWnd, CWnd* pDeactivateWnd );

	DECLARE_MESSAGE_MAP()
};


#endif _PROPOVERV_H_INCLUDED_