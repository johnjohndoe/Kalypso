// DXFPLin.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef DXFPLIN_H
#define DXFPLIN_H

#include "..\..\commonMfc\commonMfc.h"

#include "dxfent.h"

class CDXFPolylinie : public CDXFEntity
{
public:
	friend ostream& operator<<(ostream& os, CDXFPolylinie &pl);
	friend istream& operator>>(istream& is, CDXFPolylinie &pl);
	CDXFPolylinie(CDXFLayer* pOwner = NULL);
	~CDXFPolylinie();

	void AddCoord( const double crdX, const double crdY );
	int GetNumCoords() { return m_coords.GetCount(); }

protected:
	double m_dAnfBreite, m_dEndBreite;
	CList<CDoublePoint, CDoublePoint&> m_coords;
};

#endif // DXFPLIN_H