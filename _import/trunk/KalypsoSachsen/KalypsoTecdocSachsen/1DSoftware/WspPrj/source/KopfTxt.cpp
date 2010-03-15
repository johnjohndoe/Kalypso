// KopfTxt.cpp: Implementierung der Klasse KopfTxt.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "giostr.h"

#include "kopfTxt.h"


#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

KopfTxt::KopfTxt(CString name)
{
  m_Name = name;
}

KopfTxt::~KopfTxt()
{

}
int KopfTxt::GetSize()
{
	return m_KopfStr.GetSize();
}
void KopfTxt::GetStr(int i, CString& str)
{
	str = m_KopfStr[i];
}

void KopfTxt::SetStr(int i, CString& str)
{
	m_KopfStr[i] = str;
}

BOOL KopfTxt::Load()
{
	CFileStatus rStatus;
	gifstream ifs;
	CString rString;

  if ( CFile::GetStatus(m_Name, rStatus) )
	{
		ifs.open(m_Name, ios::in);
		if (ifs.fail())
		{
			rString.FormatMessage("Konnte Datei %1 nicht zum Lesen öffnen.", m_Name);
			AfxMessageBox(rString, MB_ERROR);
			return FALSE;
		}
		else
		{
			ifs >> *this;
			ifs.close();
		}
		
	}
	return TRUE;
}

BOOL KopfTxt::Save()
{
	CFileStatus rStatus;
	gofstream ofs;
	CString rString;

    ofs.open(m_Name, ios::out | ios::trunc);
    if (ofs.fail())
    {
      rString.FormatMessage("Konnte Datei %1 nicht zum Schreiben öffnen.", m_Name);
      AfxMessageBox(rString, MB_ERROR);
      return FALSE;
    }
    else
    {
      ofs << *this;
      ofs.close();
    }
    
	return TRUE;
}

istream& operator>>(istream& is, KopfTxt &dat)
{
  char buffer[LINE_SIZE];
  CString str;
  while(!is.eof())
  {
    is.getline(buffer, LINE_SIZE, '\n');
    str = buffer;
    if(str.GetLength()!=0 && !is.eof())
      dat.m_KopfStr.Add(str);
  }
	
  return is;
}


ostream& operator<<(ostream& os, KopfTxt &dat)
{
  int i;
  
  for(i=0; i<dat.m_KopfStr.GetSize(); i++)
  {
    os << dat.m_KopfStr[i] << endl;        
  }
  
  return os;	
}