//
// Wrapper for IFonts.  Similar to COleDispatchDriver except that
// CreateDispatch isn't needed.
//

#ifndef _MO_FONT_H_INCLUDED_
#define _MO_FONT_H_INCLUDED_


///////////////////////////////////////////////////////////////////////////
// OLE Fonts
//
//
// FONTDESC wrapper.  Automatic initialization
//

class CFontDesc : public tagFONTDESC
{
public:
  CFontDesc() 
  {
    cbSizeofstruct = sizeof(FONTDESC);
    lpstrName = OLESTR("Arial");
    cySize.int64 = 120000;
    sWeight = FW_NORMAL;
    sCharset = ANSI_CHARSET;
    fItalic = FALSE;
    fUnderline = FALSE;
    fStrikethrough = FALSE;
  }
};

class CMoFont : public CFontHolder
{
public:
  CMoFont();
  CMoFont(LPFONTDISP pFontDisp);
  virtual ~CMoFont();
  
  void AttachDispatch(LPFONTDISP pFontDisp);
  
  // Attributes
  LPCTSTR GetName();
  void	SetName(LPCTSTR faceName);
  long	GetSize();
  void	SetSize(long size);
  short	GetWeight();
  void	SetWeight(short weight);
  short	GetCharSet();
  void	SetCharSet(short charset);
  BOOL	GetItalic();
  void	SetItalic(BOOL italic);
  BOOL	GetUnderline();
  void	SetUnderline(BOOL underline);
  BOOL	GetStrikeThrough();
  void	SetStrikeThrough(BOOL strikeThrough);
  
  operator LPDISPATCH();
  operator LPFONTDISP();
  
private:
  LPFONTDISP	m_pDispatch;
  LPFONTDISP	m_pFontDisp;
  
  void		ReleaseDispatch();	// Call whenever InitializeFont is called
};



#endif _MO_FONT_H_INCLUDED_