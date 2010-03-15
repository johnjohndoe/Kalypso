#if !defined(AFX_MOSCALEBAR_H__CC25FE73_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOSCALEBAR_H__CC25FE73_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoPicture;
class CMoSbExtent;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoScaleBar 

class CMoScaleBar : public CWnd
{
protected:
	DECLARE_DYNCREATE(CMoScaleBar)
public:
	CLSID const& GetClsid()
	{
		static CLSID const clsid
			= { 0x7212ab53, 0x49e, 0x11d1, { 0x86, 0x9e, 0x8, 0x0, 0x9, 0xee, 0x4e, 0x46 } };
		return clsid;
	}
	virtual BOOL Create(LPCTSTR lpszClassName,
		LPCTSTR lpszWindowName, DWORD dwStyle,
		const RECT& rect,
		CWnd* pParentWnd, UINT nID,
		CCreateContext* pContext = NULL)
	{ return CreateControl(GetClsid(), lpszWindowName, dwStyle, rect, pParentWnd, nID); }

    BOOL Create(LPCTSTR lpszWindowName, DWORD dwStyle,
		const RECT& rect, CWnd* pParentWnd, UINT nID,
		CFile* pPersist = NULL, BOOL bStorage = FALSE,
		BSTR bstrLicKey = NULL)
	{ return CreateControl(GetClsid(), lpszWindowName, dwStyle, rect, pParentWnd, nID,
		pPersist, bStorage, bstrLicKey); }

// Attribute
public:

// Operationen
public:
	void SetRefFont(LPDISPATCH newValue);
	void SetRefPicture(LPDISPATCH newValue);
	CMoPicture GetPicture();
	void SetScaleBarUnits(long nNewValue);
	void SetScreenUnits(long nNewValue);
	long GetScreenUnits();
	void Refresh();
	unsigned long GetBarColor1();
	void SetBarColor1(unsigned long newValue);
	unsigned long GetBarColor2();
	void SetBarColor2(unsigned long newValue);
	short GetBarWidth();
	void SetBarWidth(short nNewValue);
	long GetMapUnits();
	void SetMapUnits(long nNewValue);
	long GetScaleBarUnits();
	long GetScaleText();
	void SetScaleText(long nNewValue);
	unsigned long GetTextColor();
	void SetTextColor(unsigned long newValue);
	unsigned long GetBackColor();
	void SetBackColor(unsigned long newValue);
	CMoSbExtent GetMapExtent();
	void SetMapExtent(LPDISPATCH newValue);
	CMoSbExtent GetPageExtent();
	void SetPageExtent(LPDISPATCH newValue);
	double GetRFScale();
	LPDISPATCH GetFont();
	long GetHWnd();
	long GetHDC();
	short GetAppearance();
	long GetBorderStyle();
	void SetBorderStyle(long nNewValue);
	CMoPicture GetMouseIcon();
	void SetRefMouseIcon(LPDISPATCH newValue);
	long GetMousePointer();
	void SetMousePointer(long nNewValue);
	void PaintPicture(LPDISPATCH* Picture, float* X1, float* Y1, VARIANT* Width1, VARIANT* Height1, VARIANT* X2, VARIANT* Y2, VARIANT* Width2, VARIANT* Height2, VARIANT* Opcode);
	CString GetToolTipText();
	void SetToolTipText(LPCTSTR lpszNewValue);
	float GetMinTicSpace();
	void SetMinTicSpace(float newValue);
	BOOL GetAdjustForLatitude();
	void SetAdjustForLatitude(BOOL bNewValue);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOSCALEBAR_H__CC25FE73_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
