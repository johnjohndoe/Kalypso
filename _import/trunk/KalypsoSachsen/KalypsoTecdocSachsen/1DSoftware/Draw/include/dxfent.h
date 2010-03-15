// DXFEnt.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef DXFENT_H
#define DXFENT_H

class CDXFLayer;

class CDXFEntity : public CObject
{
public:
	CDXFEntity(CDXFLayer* pOwner = NULL);
	~CDXFEntity();
	CDXFLayer* GetOwner() { return m_pOwner; }

	void SetLType(CString& name) { m_LType = name; }
	void SetWidth(int width) { m_nWidth = width; }
	void SetColor(COLORREF color);
	void SetLType(int n);

protected:
	CDXFLayer* m_pOwner;
	CString m_LType;
	int m_nWidth;
	int m_nColor;
};

#endif // DXFENT_H