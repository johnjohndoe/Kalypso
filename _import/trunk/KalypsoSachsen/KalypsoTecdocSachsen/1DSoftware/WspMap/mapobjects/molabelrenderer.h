#if !defined(AFX_MOLABELRENDERER_H__CC25FE5E_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOLABELRENDERER_H__CC25FE5E_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoTextSymbol;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoLabelRenderer 

class CMoLabelRenderer : public COleDispatchDriver
{
public:
	CMoLabelRenderer() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoLabelRenderer(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoLabelRenderer(const CMoLabelRenderer& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CString GetField();
	void SetField(LPCTSTR);
	BOOL GetDrawBackground();
	void SetDrawBackground(BOOL);
	short GetSymbolCount();
	void SetSymbolCount(short);
	CString GetSymbolField();
	void SetSymbolField(LPCTSTR);
	CString GetLevelField();
	void SetLevelField(LPCTSTR);
	short GetMinLevel();
	void SetMinLevel(short);
	short GetMaxLevel();
	void SetMaxLevel(short);
	CString GetRotationField();
	void SetRotationField(LPCTSTR);
	CString GetHeightField();
	void SetHeightField(LPCTSTR);
	BOOL GetSplinedText();
	void SetSplinedText(BOOL);
	BOOL GetAllowDuplicates();
	void SetAllowDuplicates(BOOL);
	CString GetTag();
	void SetTag(LPCTSTR);
	BOOL GetFlip();
	void SetFlip(BOOL);
	CString GetXOffsetField();
	void SetXOffsetField(LPCTSTR);
	CString GetYOffsetField();
	void SetYOffsetField(LPCTSTR);
	CString GetFittedField();
	void SetFittedField(LPCTSTR);

// Operationen
public:
	CMoTextSymbol GetSymbol(short index);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOLABELRENDERER_H__CC25FE5E_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
