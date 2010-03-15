// DXFLinie.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef DXFLINIE_H
#define DXFLINIE_H

#include "dxfent.h"

class CDXFLinie : public CDXFEntity
{
public:
	friend ostream& operator<<(ostream& os, CDXFLinie &ln);
	friend istream& operator>>(istream& is, CDXFLinie &ln);
	CDXFLinie(CDXFLayer* pOwner = NULL);
	~CDXFLinie();

	void SetAnf(double x, double y, double z = 0) { m_dXA = x; m_dYA = y; m_dZA = z; }
	void SetEnd(double x, double y, double z = 0) { m_dXE = x; m_dYE = y; m_dZE = z; }

protected:
	double m_dXA, m_dYA, m_dZA;
	double m_dXE, m_dYE, m_dZE;
};

#endif // DXFLINIE_H