// DXFText.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef DXFTEXT_H
#define DXFTEXT_H

#include "dxfent.h"

class CDXFText : public CDXFEntity
{
public:
	friend ostream& operator<<(ostream& os, CDXFText &tx);
	friend istream& operator>>(istream& is, CDXFText &tx);
	CDXFText(CDXFLayer* pOwner = NULL);
	~CDXFText();

	void Set( const CString& text, const double posX, const double posY, const double h, const double w );
	void SetJust( const int hpos, const int vpos, const double crdX, const double crdY );

protected:
	double m_dX, m_dY, m_dZ;
	double m_dXAus, m_dYAus, m_dZAus;
	double m_dXMaszstab, m_dNWkl;
	double m_dHoehe, m_dWkl;
	int m_nHPos, m_nVPos;
	CString m_text;
};

#endif // DXFTEXT_H