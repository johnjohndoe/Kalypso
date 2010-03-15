// DXFPunkt.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef DXFPUNKT_H
#define DXFPUNKT_H

#include "dxfent.h"

class CDXFPunkt : public CDXFEntity
{
public:
	friend ostream& operator<<(ostream& os, CDXFPunkt &pt);
	friend istream& operator>>(istream& is, CDXFPunkt &pt);
	CDXFPunkt(CDXFLayer* pOwner = NULL);
	~CDXFPunkt();

	void Set(double x, double y, double z = 0) { m_dX = x; m_dY = y; m_dZ = z; }

protected:
	double m_dX, m_dY, m_dZ;
};

#endif // DXFPUNKT_H