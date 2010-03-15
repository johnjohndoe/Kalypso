// StringResource.cpp: Implementierung der Klasse StringResource.
//
//////////////////////////////////////////////////////////////////////

#include <cassert>
#include <string>

#include "Resource.h"

#include "StringResource.h"

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

using namespace BCE;


const StringResource::assoc StringResource::strings[STRINGS_SIZE] = 
{
  assoc( 0, "FEHLENDE STRING-RESOURCE!!!" ),

    // TinCut
  assoc( IDS_TINCUT_ERROR_MEM, "Nicht genug Speicherplatz fuer TinCut." ),
  assoc( IDS_TINCUT_ALLOCATE_POINT, "Reserviere Speicher fuer %d Punkte." ),
  assoc( IDS_TINCUT_ALLOCATE_TRNGL, "Reserviere Speicher fuer %d Dreiecke." ),
  assoc( IDS_TINCUT_ALLOCATE_ARC, "Reserviere Speicher fuer %d Kanten." ),
  assoc( IDS_TINCUT_INTERNAL, "Interner TinCut fehler." ),
  assoc( IDS_TINCUT_ERROR_INPUT, "Fehler beim Lesen der Eingabedateien.")
};

StringResource::StringResource()
{
}

const char* StringResource::getString( unsigned int id ) const
{
  for( int i = 0; i < STRINGS_SIZE; i++ )
  {
    if( strings[i].id == id )
      return strings[i].text;
  }; // for i

  return strings[0].text; // der nullte Text ist die Meldung für die fehlende Resource
}; // getString