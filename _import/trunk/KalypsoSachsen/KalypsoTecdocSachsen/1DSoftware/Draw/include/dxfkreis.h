// DXFKreis.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef DXFKREIS_H
#define DXFKREIS_H

#include "dxfent.h"

class CDXFKreis : public CDXFEntity
{
public:
	friend ostream& operator<<(ostream& os, CDXFKreis &kr);
	friend istream& operator>>(istream& is, CDXFKreis &kr);
	CDXFKreis(CDXFLayer* pOwner = NULL);
	~CDXFKreis();

	void Set( const double posX, const double posY, const double rad );

protected:
	double m_dX, m_dY, m_dZ;
	double m_rad;
};

#endif // DXFKREIS_H