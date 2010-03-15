#include "stdafx.h"

#include "dxfent.h"

  ////////////////////////////
  //  Klasse  CDXFEntity
  ///////////////////////////

/* The Default Constructor */
CDXFEntity::CDXFEntity(CDXFLayer* pOwner)
{
	m_pOwner = pOwner;
	m_nColor = 256;
	m_nWidth = 0;
}

CDXFEntity::~CDXFEntity()
{
}

void CDXFEntity::SetColor(COLORREF color)
{
	m_nColor = 256;

	switch (color)
	{
		case RGB(0, 0, 0):			// Black
			m_nColor = 256;
			break;

		case RGB(255, 255, 255):	// White
			m_nColor = 0;
			break;

		case RGB(255, 0, 0):		// Red
			m_nColor = 240;
			break;

		case RGB(0, 255, 0):		// Green
			m_nColor = 80;
			break;

		case RGB(0, 0, 255):		// Blue
			m_nColor = 170;
			break;

		case RGB(255, 255, 0):		// Yellow
			m_nColor = 50;
			break;

		case RGB(255, 0, 255):		// Magenta
			m_nColor = 220;
			break;

		case RGB(0, 255, 255):		// Cyan
			m_nColor = 130;
			break;

		case RGB(128, 128, 128):	// Gray
			m_nColor = 8;
			break;

		case RGB(192, 192, 192):	// Light Gray
			m_nColor = 9;
			break;

		case RGB(128, 0, 0):		// Dark Red
			m_nColor = 244;
			break;

		case RGB(0, 128, 0):		// Dark Green
			m_nColor = 84;
			break;

		case RGB(0, 0, 128):		// Dark Blue
			m_nColor = 174;
			break;

		case RGB(128, 128, 0):		// Light Brown
			m_nColor = 32;
			break;

		case RGB(128, 0, 128):		// Dark Magenta
			m_nColor = 224;
			break;

		case RGB(0, 128, 128):		// Dark Cyan
			m_nColor = 134;
			break;

		default:
			break;
	}
}

void CDXFEntity::SetLType(int n)
{
	m_LType = "CONTINUOUS";

	switch (n)
	{
		case PS_SOLID:
			m_LType = "CONTINUOUS";
			break;

		case PS_DASH:
			m_LType = "GESTRICHELT";
			break;

		case PS_DOT:
			m_LType = "PUNKT";
			break;

		case PS_DASHDOT:
			m_LType = "RAND2";
			break;

		case PS_DASHDOTDOT:
			m_LType = "GETRENNT2";
			break;
	}
}