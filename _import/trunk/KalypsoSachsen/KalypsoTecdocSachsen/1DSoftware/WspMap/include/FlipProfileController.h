// NilController.h: Schnittstelle für die Klasse CNilController.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_FLIPPROFILE_CONTROLLER_H__5E6DDD56_21BE_11D8_B497_00104BB3E525__INCLUDED_)
#define AFX_FLIPPROFILE_CONTROLLER_H__5E6DDD56_21BE_11D8_B497_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "imapcontroller.h"

class CMapView;

class CFlipProfileController : public IMapController  
{
private:
	struct OffsetAndNumber // offset and biggest point number
	{
		double offset;
		long number;
	};

public:
  CFlipProfileController( CMapView* view );
  virtual ~CFlipProfileController();

  virtual long MousePointer() const { return moArrow; };

  virtual void OnMapMouseDown( CMoPoint& cursor, short Button, short Shift );

private:
  CMoPoint FlipLayer( CMapLayer* mapLayer, const long profilID, const OffsetAndNumber offsetAndNumber ) const;
  const OffsetAndNumber CalcOffset( CMapLayer* pointLayer, const long profilID ) const;
  void ChangeFixpoint( CMapLayer* layer, CMoPoint point, const int profilID ) const;
};

class BeepOnDestroy
{
public:
	BeepOnDestroy( const bool beep = true ) 
	{
		m_beep = beep;
	};

	~BeepOnDestroy()
	{
		if( m_beep )
			::MessageBeep( MB_ICONQUESTION );
	}

private:
	bool m_beep;
	
public:
	void setBeep( const bool beep )
	{
		m_beep = beep;
	}

};


#endif // !defined(AFX_FLIPPROFILE_CONTROLLER_H__5E6DDD56_21BE_11D8_B497_00104BB3E525__INCLUDED_)
