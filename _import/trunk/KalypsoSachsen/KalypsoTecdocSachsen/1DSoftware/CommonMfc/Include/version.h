// Version.h: Schnittstelle für die Klasse CVersion.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_VERSION_H__A76C5F73_7F6D_11D6_B2EA_00104BB3E525__INCLUDED_)
#define AFX_VERSION_H__A76C5F73_7F6D_11D6_B2EA_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


// diese Klasse repräsentiert die Versionsresourcen einer Executable ( .exe / .dll )
class CVersion  
{
  ////////////////////////////////
  // Konstruktion / Destruktion //
  ////////////////////////////////
public:
  CVersion( HMODULE hModule );
  
  ///////////////
  // Attribute //
  ///////////////
public:
  CString GetProductNr() const;
  CString GetFileNr() const;
  CString GetFileName() const;
  const CTime& GetTime() const { return m_time; };
  CString GetTimeString() const;

private:
  VS_FIXEDFILEINFO m_fileInfo;
  CTime m_time;
  CString m_fileName;
};

#endif // !defined(AFX_VERSION_H__A76C5F73_7F6D_11D6_B2EA_00104BB3E525__INCLUDED_)
