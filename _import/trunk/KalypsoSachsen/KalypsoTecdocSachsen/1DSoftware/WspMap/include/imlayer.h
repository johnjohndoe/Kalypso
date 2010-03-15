// ImLayer.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef IMLAYER_H
#define IMLAYER_H

#include "layer.h"

class CImageLayer : public CLayer
{
	DECLARE_SERIAL(CImageLayer);

protected:
  CImageLayer();
public:
  
	CImageLayer( const CString& strBaseDirectory );
	CImageLayer( const CString& strBaseDirectory, const LPDISPATCH pDispatch );
	CImageLayer( const CString& strBaseDirectory, const CImageLayer& dispatchSrc );
	~CImageLayer();

  CLayer* Copy( const CString& newFileName );

	BOOL CreateDispatch(LPCTSTR lpszProgID, COleException* pError = NULL);

  virtual void SerializeProperties( CArchive& ar, BOOL bOnlyProps );

  virtual BOOL ShowPropertyDialog( CMoMap& pMap, CWnd* pWnd = NULL );

protected:
	CMoImageLayer m_imageLayer;

// Attributes
public:
	BOOL SetGeoDatasetName(const CString& path);

	BOOL GetVisible();
	void SetVisible(BOOL);
	CString GetName();
	void SetName(LPCTSTR);
	CMoRectangle GetExtent();
	void SetExtent(LPDISPATCH);
	CString GetFile();
	void SetFile(LPCTSTR);
	long GetLayerType();
	CString GetTag();
	void SetTag(LPCTSTR);
	BOOL GetValid();
	void SetValid(BOOL);
	BOOL GetUpdateWhileDrawing();
	void SetUpdateWhileDrawing(BOOL);
	BOOL GetTransparent();
	void SetTransparent(BOOL);
	unsigned long GetTransparentColor();
	void SetTransparentColor(unsigned long);
};

#endif // IMLAYER_H