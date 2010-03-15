// IPrintRectFont.cpp: Implementierung der Klasse IPrintRectFont.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "IPrintRectFont.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

IPrintRectFont::IPrintRectFont()
{
  m_align = DT_CENTER;
  m_color = RGB( 0, 0, 0 ); // Schwarz ist Default
  m_ptSize = 240; // Standard 12pt

  // Set up font as Arial (TRUETYPE)
	m_logfont.lfHeight = -17;
	m_logfont.lfWidth = 0;
	m_logfont.lfEscapement = 0;
	m_logfont.lfOrientation = 0;
	m_logfont.lfWeight = FW_NORMAL;
	m_logfont.lfItalic = FALSE;
	m_logfont.lfUnderline = FALSE;
	m_logfont.lfStrikeOut = FALSE;
	m_logfont.lfCharSet = ANSI_CHARSET;
	m_logfont.lfOutPrecision = OUT_STROKE_PRECIS;
	// set CLIP_LH_ANGLES to ensure the coordinate system for all devices is the same
	m_logfont.lfClipPrecision = CLIP_STROKE_PRECIS | CLIP_LH_ANGLES;
	m_logfont.lfQuality = DRAFT_QUALITY;
	m_logfont.lfPitchAndFamily = 34;
  _tcsncpy( m_logfont.lfFaceName, TEXT( "Arial" ), _tcslen( TEXT( "Arial" ) ) );
}

void IPrintRectFont::ReadFont( CArchive& ar )
// liest die Font-Daten aus einem Archiv
{
  ar >> m_align;
  ar >> m_color;
  ar >> m_ptSize;
  ar.Read( &m_logfont, sizeof( LOGFONT ) );
} // ReadFont

void IPrintRectFont::WriteFont( CArchive& ar )
// schreibt Font-Daten in ein Archiv
{
  ar << m_align;
  ar << m_color;
  ar << m_ptSize;
  ar.Write( &m_logfont, sizeof( LOGFONT ) );
} // WriteFont

