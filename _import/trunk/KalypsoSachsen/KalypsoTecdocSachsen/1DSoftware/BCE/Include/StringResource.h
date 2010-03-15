// StringResource.h: Schnittstelle für die Klasse StringResource.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_STRINGRESOURCE_H__9C4EBA83_0A5E_11D6_BED3_00104BB3E525__INCLUDED_)
#define AFX_STRINGRESOURCE_H__9C4EBA83_0A5E_11D6_BED3_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <string>

#define STRINGS_SIZE 100

// Diese Klasse stellt eine Zuordnung von String IDs zu 
namespace BCE
{
  class StringResource
  {
    struct assoc
    {
      unsigned int id;
      const char* text;

      assoc()
      {
        id = 0;
        text = "Kein Text";
      };

      assoc( const unsigned int id, const char* text )
      {
        this->id = id;
        this->text = text;
      };
    }; // struct assoc

  private:
    static const assoc strings[STRINGS_SIZE];

  public:
    StringResource();

    const char* getString( unsigned int id ) const;
  }; // class StringResource
}; // namespace BCE

#endif // !defined(AFX_STRINGRESOURCE_H__9C4EBA83_0A5E_11D6_BED3_00104BB3E525__INCLUDED_)
