// DXFArc.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef DXFARC_H
#define DXFARC_H

#include "dxfent.h"

class CDXFArc : public CDXFEntity
{
public:
	friend ostream& operator<<(ostream& os, CDXFArc &ar);
	friend istream& operator>>(istream& is, CDXFArc &ar);
	CDXFArc(CDXFLayer* pOwner = NULL);
	~CDXFArc();

	void Set( const double dx, const double dy, double rad, double anfw, double endw);

protected:
	double m_dX, m_dY, m_dZ;
	double m_rad;
	double m_dAnfW, m_dEndW;
};

#endif // DXFARC_H