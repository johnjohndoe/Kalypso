#include "stdafx.h"

#include "maplayer.h"

#include "maprend.h"

  ////////////////////////////
  //  Klasse  CMapRenderer
  ///////////////////////////

IMPLEMENT_SERIAL(CMapRenderer, CObject, VERSIONABLE_SCHEMA|1)

CMapRenderer::CMapRenderer()
{
	short i;

	m_nRendType = singleSymbol;
	for (i=0; i<N_FIELDS; i++)
		m_strField[i].Empty();
	for (i=0; i<N_CONSTS; i++)
		m_nConst[i] = 0;
	for (i=0; i<N_BOOLS; i++)
		m_bBool[i] = FALSE;
	for (i=0; i<N_COLORS; i++)
		m_cColor[i] = moBlack;

	// Set up font as MS Sans Serif
	m_logfont.lfHeight = -11;
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
	strcpy(m_logfont.lfFaceName, "MS Sans Serif");
}

CMapRenderer::CMapRenderer(const CMapRenderer& src)
{
	short i;

	m_nRendType = src.m_nRendType;
	for (i=0; i<N_FIELDS; i++)
		m_strField[i] = src.m_strField[i];
	for (i=0; i<N_CONSTS; i++)
		m_nConst[i] = src.m_nConst[i];
	for (i=0; i<N_BOOLS; i++)
		m_bBool[i] = src.m_bBool[i];
	for (i=0; i<N_COLORS; i++)
		m_cColor[i] = src.m_cColor[i];
	m_valCols.RemoveAll();
	for (i=0; i<src.m_valCols.GetSize(); i++)
		m_valCols.Add(src.m_valCols[i]);

	m_logfont = src.m_logfont;
}

CMapRenderer::~CMapRenderer()
{
}

void CMapRenderer::Serialize(CArchive& ar)
{
	short i;
	
	CObject::Serialize(ar);
	if (ar.IsStoring())
	{
		ar << (WORD)m_nRendType;
		for( i = 0; i < N_FIELDS; i++ )
			ar << m_strField[i];
		for( i = 0; i < N_CONSTS; i++ )
			ar << m_nConst[i];
		for( i = 0; i < N_BOOLS; i++ )
			ar << (WORD)m_bBool[i];
		for( i = 0; i < N_COLORS; i++ )
			ar << (DWORD)m_cColor[i];
		ar.Write( &m_logfont, sizeof(LOGFONT) );
		ar.WriteCount( m_valCols.GetSize() );
		for( i = 0; i < m_valCols.GetSize(); i++ )
			ar << (DWORD)m_valCols[i];
	}
	else
	{
    int nVersion = ar.GetObjectSchema();
    ar.SetObjectSchema( nVersion );
    switch( nVersion )
    {
    case 0:
    case 1:
      {
        DWORD dwTemp;
        WORD wTemp;
        
        ar >> wTemp; m_nRendType = (RendererType)wTemp;
        for (i=0; i<N_FIELDS; i++)
          ar >> m_strField[i];
        for (i=0; i<N_CONSTS; i++)
          ar >> m_nConst[i];
        for (i=0; i<N_BOOLS; i++)
        {
          ar >> wTemp;
          m_bBool[i] = (BOOL)wTemp;
        }
        for (i=0; i<N_COLORS; i++)
        {
          ar >> dwTemp;
          m_cColor[i] = (COLORREF)dwTemp;
        }
        ar.Read(&m_logfont,sizeof(LOGFONT));
        dwTemp = ar.ReadCount();
        m_valCols.SetSize(dwTemp);
        for (i=0; i<m_valCols.GetSize(); i++)
        {
          ar >> dwTemp;
          m_valCols[i] = (COLORREF)dwTemp;
        }
      }
      break;
      
    default:
      AfxThrowArchiveException(CArchiveException::badSchema);
      break;
    }
  }
}

void CMapRenderer::SetRendererType(int n)
{
	short i;

	m_nRendType = (RendererType)n;
	
	// Initialise Data
	for (i=0; i<N_FIELDS; i++)
		m_strField[i].Empty();
	for (i=0; i<N_CONSTS; i++)
		m_nConst[i] = 0;
	for (i=0; i<N_BOOLS; i++)
		m_bBool[i] = FALSE;
	for (i=0; i<N_COLORS; i++)
		m_cColor[i] = moBlack;
	m_valCols.RemoveAll();

	// special initialisation?
	switch (m_nRendType)
	{
		case singleSymbol:
			break;

		case valueMap:
			break;

		case classBreaks:
			m_nConst[0] = 5;
			m_nConst[1] = 5;
			m_nConst[2] = 5;
			m_cColor[0] = moBlue;
			m_cColor[1] = moCyan;
			break;

		case standardLabel:
			m_nConst[0] = moAlignCenter;
			m_nConst[1] = moAlignCenter;
			m_bBool[0] = TRUE;
			m_bBool[2] = TRUE;
			break;

		case advancedLabel:
			m_nConst[1] = 75;
			m_nConst[2] = 50;
			m_bBool[0] = TRUE;
			m_bBool[4] = TRUE;
			m_cColor[1] = moGray;
			break;
	}
}

void CMapRenderer::SetField( const short n, const CString& val )
{
	if (n>=0 && n<4)
		m_strField[n] = val;
}

CString CMapRenderer::GetField(short n)
{
	if (n>=0 && n<4)
		return m_strField[n];

	return -1;
}

void CMapRenderer::SetConst(short n, short val)
{
	if (n>=0 && n<3)
		m_nConst[n] = val;
}

short CMapRenderer::GetConst(short n)
{
	if (n>=0 && n<3)
		return m_nConst[n];

	return -1;
}

void CMapRenderer::SetBool(short n, BOOL val)
{
	if (n>=0 && n<7)
		m_bBool[n] = val;
}

BOOL CMapRenderer::GetBool(short n)
{
	if (n>=0 && n<7)
		return m_bBool[n];

	return FALSE;
}

void CMapRenderer::SetColor(short n, COLORREF val)
{
	if (n>=0 && n<2)
		m_cColor[n] = val;
}

COLORREF CMapRenderer::GetColor(short n)
{
	if (n>=0 && n<2)
		return m_cColor[n];

	return moBlack;
}

void CMapRenderer::SetValueColors(CArray<COLORREF, COLORREF>& colors)
{
	m_valCols.RemoveAll();
	for( short i = 0; i < colors.GetSize(); i++ )
		m_valCols.Add( colors[i] );
}

COLORREF CMapRenderer::GetValueColor(short n)
{
	if (n>=0 && n<m_valCols.GetSize())
		return m_valCols[n];

	return moBlack;
}

short CMapRenderer::GetNumValueColors()
{
	return m_valCols.GetSize();
}

void CMapRenderer::ClearValueColors()
{
	m_valCols.RemoveAll();
}