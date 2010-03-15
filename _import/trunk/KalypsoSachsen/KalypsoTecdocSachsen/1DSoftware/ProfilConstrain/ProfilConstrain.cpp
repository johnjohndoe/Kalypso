// ProfilConstrain.cpp : Definiert den Einsprungpunkt für die Konsolenanwendung.
//

#include "stdafx.h"

#include "../wspprj/include/profil.h"

int main( int argc, char* argv[] )
{
  // Kommandozeile parsen
  if( argc != 5 )
  {
    cout << "usage:\n" << endl;
    cout << "<infile : file> <outfile : file> <from : double> <to : double>" << endl;
    return -1;
  }

  CString inFile( argv[1] );
  CString outFile( argv[2] );
  double from;
  double to;
  if( sscanf( argv[3], "%lf", &from ) != 1 )
  {
    cout << "<from> is no double value: " << argv[3] << endl;
    return -1;
  }

  if( sscanf(  argv[4], "%lf",&to ) != 1 )
  {
    cout << "<to> is no double value: " << argv[4] << endl;
    return -1;
  }


  cout << "Loading file " << inFile << endl;

  // Profil laden
  Profil profil;
  if( !profil.Load( inFile ) )
  {
    cout << "Error while loading " << inFile << endl;
    return -1;
  }

  cout << inFile << " loaded" << endl;

  // Profil einschränken
  profil.Constrain( from, to );

  // Profil speichern
  cout << "Writing file " << outFile << endl;
  profil.SetModified();
  if( !profil.Save( outFile ) )
  {
    cout << "Error while writing " << outFile;
    return -1;
  }

  cout << outFile << " written" << endl;

	return 0;
}
