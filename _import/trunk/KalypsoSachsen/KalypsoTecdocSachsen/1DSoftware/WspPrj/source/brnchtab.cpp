#include "stdafx.h"

#include "branch.h"
#include "giostr.h"

#include "brnchtab.h"

  ////////////////////////////
  //  Klasse  BranchTable
  ///////////////////////////

/* The Default Constructor */
BranchTable::BranchTable()
{
}

BranchTable::~BranchTable()
{
	for( int i = 0; i < m_branches.GetSize(); i++ )
    delete m_branches[i];
  m_branches.RemoveAll();
};

BranchTable* BranchTable::Clone() const
// gibt Kopie der BranchTable zurück
// Rückgabewert:
//          Zeiger auf eine Kopie der BranchTable
{
  BranchTable* bt = new BranchTable();

  for ( int i = 0; i < m_branches.GetSize(); i++ )
    bt->AddBranch( m_branches[i]->Clone() );

  return bt;
}; // Clone

BOOL BranchTable::Load( CString& filename )
{
	gifstream ifs;
	CString rString;
	CFileStatus rStatus;
	BOOL bOK = FALSE;

	if (CFile::GetStatus(filename, rStatus))
	{
		ifs.open(filename, ios::in);
		if (!ifs.fail())
		{
			ifs >> *this;
			ifs.close();
			bOK = TRUE;
		}
	}

	if (!bOK)
	{
		rString.FormatMessage("Konnte Datei %1 nicht zum lesen öffnen.", filename);
		AfxMessageBox(rString, MB_ERROR);
		return FALSE;
	}

	return TRUE;
}

BOOL BranchTable::Save( CString& filename ) const
{
	gofstream ofs;
	CString rString;

	ofs.open(filename, ios::out);
	if (ofs.fail())
	{
		rString.FormatMessage("Konnte Datei %1 nicht zum Schreiben öffnen.", filename);
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

///////////////
// Attribute //
///////////////

int BranchTable::GetNumBranches() const
{
	return m_branches.GetSize();
}

Branch* BranchTable::GetBranch( const int n ) const
{
	if( n >= 0 && n < m_branches.GetSize() )
		return m_branches[n];

	return NULL;
};

Branch* BranchTable::FindBranch( int vzk )
{
	for( int i = 0; i < m_branches.GetSize(); i++ )
	{
    if( m_branches[i]->GetVZK() == vzk )
      return m_branches[i];
	}

	return NULL;
}

void BranchTable::AddBranch( Branch* br )
{
	BOOL bInserted = FALSE;

	for ( int i = 0; i < m_branches.GetSize(); i++ )
	{
		if( br->GetVZK() < m_branches[i]->GetVZK() )
		{
			m_branches.InsertAt(i, br);
			bInserted = TRUE;
			break;
		}
	}

  if( !bInserted )
    m_branches.Add( br );
}

istream& operator>>( istream& is, BranchTable &bt )
{
	while( !is.eof() )
	{
		Branch* br = new Branch;
		is >> *br;
		if( br->GetVZK() != -1 )
			bt.m_branches.Add( br );
		else
			delete br;
	}; // while !is.Eof

	return is;
}

ostream& operator<<( ostream& os, const BranchTable &bt )
{
	for( int i = 0; i < bt.m_branches.GetSize(); i++ )
		os << *bt.m_branches[i];

	return os;
}


////////////////
// Operatoren //
////////////////

BOOL BranchTable::IsConnected( const int vzk1, const int vzk2 )
// Rekursive Funktion, die rausfindet, ob es möglich ist in Fliessrichtung von einem Strang zum anderen zu kommen
// Parameter:
//        const int vzk1, vzk2: die beiden Strangkennungen
{
  // der Strang selbst fliesst natürlich zu sich und alles fliesst wieder nach 0 ( Hautpstrom )
  if( vzk1 == vzk2 || vzk1 == 0 || vzk2 == 0 )
    return TRUE;

  Branch* b = FindBranch( vzk1 );
  if( b )
  {
    int lAFK = b->GetAFK( 0 ); // linke Abflusskennung
    int rAFK = b->GetAFK( 1 ); // rechte Abflusskennung

    if( vzk2 == lAFK || vzk2 == rAFK )
      return TRUE; // vzk1 fliesst direkt ein vzk2 -> alles verbunden
    else if( lAFK == 0 && rAFK == 0 )
    {
      // falls wir einen Ausfluss in den Haptstrom haben können wir vzk1 und vzk2 direkt vergleichen
      if( vzk1 < vzk2 )
        return TRUE; // ok 
      else 
        return FALSE; // misst
    }
    else if( lAFK != 0 && IsConnected( lAFK, vzk2 ) )
      return TRUE;
    else if( rAFK != 0 && IsConnected( rAFK, vzk2 ) )
      return TRUE;

    // alle anderen Fälle: nicht verbunden
    return FALSE;
  }
  else
    return FALSE;
}; // IsConnected
