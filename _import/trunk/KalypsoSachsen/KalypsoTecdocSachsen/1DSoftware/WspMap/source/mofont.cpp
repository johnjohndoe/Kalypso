#include "stdafx.h"

#include "mofont.h"

#define DIV(a, b)       (((a)*(b) < 0) ? (((double)(a) / (double)(b)) - 0.5)  \
                                       : (((double)(a) / (double)(b)) + 0.5))


///////////////////////////////////////////////////////////////////////////
// OLE Fonts
//
CMoFont::CMoFont()
:	CFontHolder(0),
	m_pFontDisp(0),
	m_pDispatch(0)
{
	CFontDesc fontDesc;
	InitializeFont(&fontDesc, 0);
}


CMoFont::CMoFont(LPFONTDISP pFontDisp)
:	CFontHolder(0)
{
	InitializeFont(0, pFontDisp);	
		// similar to COleDispatchDriver::CreateDispatch()
}

CMoFont::~CMoFont()
{
	ReleaseDispatch();
}

void CMoFont::AttachDispatch(LPFONTDISP pFontDisp)
{
	ReleaseDispatch();
	InitializeFont(0, pFontDisp);
}

void CMoFont::ReleaseDispatch()
{
	if (m_pFontDisp)
	{
		m_pFontDisp->Release();
		m_pFontDisp = 0;
	}

	if (m_pDispatch)
	{
		m_pDispatch->Release();
		m_pDispatch = 0;
	}
}

CMoFont::operator LPFONTDISP() 
{ 
	if (m_pFontDisp == 0)
		m_pFontDisp = GetFontDispatch();	// must release;

	return m_pFontDisp; 
}

CMoFont::operator LPDISPATCH() 
{ 
	if (m_pDispatch == 0)
	{
		LPFONTDISP pFontDisp = this->operator LPFONTDISP();
		pFontDisp->QueryInterface(IID_IDispatch, (void**)&m_pDispatch);	// must release;
	}

	return m_pDispatch; 
}

LPCTSTR CMoFont::GetName()
{
	USES_CONVERSION;
	ASSERT(m_pFont);
	BSTR pName;	  
	m_pFont->get_Name(&pName);
	return OLE2T(pName);
}

void CMoFont::SetName(LPCTSTR name)
{
	USES_CONVERSION;
	ASSERT(m_pFont);
	m_pFont->put_Name(T2OLE(name));
}

long CMoFont::GetSize()
{
	ASSERT(m_pFont);
	CY size;
	m_pFont->get_Size(&size);
	return (long)DIV(size.int64, 10000);
}

void CMoFont::SetSize(long size)
{
	ASSERT(m_pFont);
	CY _size = { size * 10000, 0 };
	m_pFont->put_Size(_size);
}

short CMoFont::GetWeight()
{
	ASSERT(m_pFont);
	short weight;
	m_pFont->get_Weight(&weight);
	return weight;
}

void CMoFont::SetWeight(short weight)
{
	ASSERT(m_pFont);
	m_pFont->put_Weight(weight);
}

short CMoFont::GetCharSet()
{
	ASSERT(m_pFont);
	short charSet;
	m_pFont->get_Charset(&charSet);
  return charSet;
}

void CMoFont::SetCharSet(short charset)
{
  ASSERT(m_pFont);
  m_pFont->put_Charset(charset);
}

BOOL CMoFont::GetItalic()
{
  ASSERT(m_pFont);
  BOOL flag;
  m_pFont->get_Italic(&flag);
  return flag;
}

void CMoFont::SetItalic(BOOL italic)
{
  ASSERT(m_pFont);
  m_pFont->put_Italic(italic);
}

BOOL CMoFont::GetUnderline()
{
  ASSERT(m_pFont);
  BOOL flag;
  m_pFont->get_Underline(&flag);
  return flag;
}

void CMoFont::SetUnderline(BOOL underline)
{
  ASSERT(m_pFont);
  m_pFont->put_Underline(underline);
}

BOOL CMoFont::GetStrikeThrough()
{
  ASSERT(m_pFont);
  BOOL flag;
  m_pFont->get_Strikethrough(&flag);
  return flag;
}

void CMoFont::SetStrikeThrough(BOOL strikeThrough)
{
  ASSERT(m_pFont);
  m_pFont->put_Strikethrough(strikeThrough);
}
