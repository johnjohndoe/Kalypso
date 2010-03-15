// DXFBlkIn.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef DXFBLKIN_H
#define DXFBLKIN_H

#include "dxfent.h"

class CDXFBlockInsert : public CDXFEntity
{
public:
	friend ostream& operator<<(ostream& os, CDXFBlockInsert &bi);
	friend istream& operator>>(istream& is, CDXFBlockInsert &bi);
	CDXFBlockInsert(CDXFLayer* pOwner = NULL);
	~CDXFBlockInsert();

	void Set( const double posX, const double posY, const double scale = 1.0, const double wkl = 0.0 );
	void SetBlockName(CString& name) { m_blockname = name; }

protected:
	double m_dX, m_dY, m_dZ;
	CString m_blockname;
	double m_dXMaszstab, m_dYMaszstab, m_dZMaszstab;
	double m_dDrehWkl;
};

#endif // DXFBLKINS_H