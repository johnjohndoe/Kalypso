#if !defined(AFX_LEGEND_H__CC25FE70_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_LEGEND_H__CC25FE70_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class COleFont;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoLegend 

class CMoLegend : public CWnd
{
protected:
	DECLARE_DYNCREATE(CMoLegend)
public:
	CLSID const& GetClsid()
	{
		static CLSID const clsid
			= { 0x6e9d12df, 0xa025, 0x11d2, { 0x9f, 0x40, 0x0, 0xc0, 0x4f, 0x8e, 0xce, 0x6f } };
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
	CString GetEntryName(short* Index);
	unsigned long GetBackColor();
	void SetBackColor(unsigned long newValue);
	unsigned long GetForeColor();
	void SetForeColor(unsigned long newValue);
	BOOL GetEnabled();
	void SetEnabled(BOOL bNewValue);
	BOOL GetEnableDragDrop();
	void SetEnableDragDrop(BOOL bNewValue);
	COleFont GetFont();
	void SetRefFont(LPDISPATCH newValue);
	short GetBackStyle();
	void SetBackStyle(short nNewValue);
	short GetBorderStyle();
	void SetBorderStyle(short nNewValue);
	void LoadLegend(BOOL* ShowCheck);
	void RemoveAll();
	BOOL GetActive(short* Index);
	void SetActive(short* Index, BOOL bNewValue);
	short getActiveLayer();
	void setMapSource(LPDISPATCH* map);
	BOOL GetLayerVisible(short* Index);
	void SetLayerVisible(short* Index, BOOL bNewValue);
	BOOL GetShowLegend(short* lyrIndex);
	void SetShowLegend(short* lyrIndex, BOOL bNewValue);
	void ShowAllLegend();
	void HideAllLegend();
	BOOL ExportToBmp(BSTR* FileName, short* LayerIndex);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_LEGEND_H__CC25FE70_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
