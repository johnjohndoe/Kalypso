// LinePointMover.h: Schnittstelle für die Klasse CLinePointMover.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_LINEPOINTMOVER_H__B7CC05B2_2F02_11D8_B4A3_00104BB3E525__INCLUDED_)
#define AFX_LINEPOINTMOVER_H__B7CC05B2_2F02_11D8_B4A3_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "IMapChanger.h"

class CLinePointMover : public IMapChanger  
{
public:
	CLinePointMover( CMoTrackingLayer trackingLayer, LPDISPATCH shapeDisp, const long shapeType, const DWORD dwPointData, CMoPoint& cursor );
	virtual ~CLinePointMover();

protected:
  virtual void MoveToInternal( CMoPoint& cursor );

private:
  DWORD m_dwPointData;
};

#endif // !defined(AFX_LINEPOINTMOVER_H__B7CC05B2_2F02_11D8_B4A3_00104BB3E525__INCLUDED_)
