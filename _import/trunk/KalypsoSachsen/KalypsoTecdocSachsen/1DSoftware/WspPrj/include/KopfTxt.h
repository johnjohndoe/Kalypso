// KopfTxt.h: Schnittstelle für die Klasse KopfTxt.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_KOPFTXT_H__AE4520A3_053B_11D4_9DB2_0090270D4773__INCLUDED_)
#define AFX_KOPFTXT_H__AE4520A3_053B_11D4_9DB2_0090270D4773__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

class KopfTxt : public CObject  
{
public:
   KopfTxt(CString name);
   ~KopfTxt();
   int GetSize();
   void GetStr(int i, CString& str);
   void SetStr(int i, CString& str);	
   BOOL Load();
   BOOL Save();
    
protected:
  CString m_Name;
  CStringArray m_KopfStr;

  friend istream& operator>>(istream& is, KopfTxt &dat);
  friend ostream& operator<<(ostream& os, KopfTxt &dat);
};

#endif // !defined(AFX_KOPFTXT_H__AE4520A3_053B_11D4_9DB2_0090270D4773__INCLUDED_)
