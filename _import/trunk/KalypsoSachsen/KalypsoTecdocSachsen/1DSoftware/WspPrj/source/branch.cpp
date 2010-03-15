#include "stdafx.h"

#include "branch.h"

  ////////////////////////////
  //  Klasse  Branch
  ///////////////////////////

/* The Default Constructor */
Branch::Branch()
{
	int i;

	m_nVZK = -1;
	for (i=0; i<2; i++)
	{
		m_nZFK[i] = 0;
		m_nAFK[i] = 0;
	}
	m_nAM = 100;
	m_bAA = FALSE;
}

Branch::~Branch()
{
}

Branch* Branch::Clone()
// kopiert den Zweig
// Rückgabewert:
//        Zeiger auf die Kopie
{
  Branch* branch = new Branch;

  branch->SetAA( m_bAA );
  for ( int i = 0; i < 2; i++ )
  {
    branch->SetAFK( i, m_nAFK[i] );
    branch->SetZFK( i, m_nZFK[i] );
  };

  branch->SetAM( m_nAM );
  branch->SetVZK( m_nVZK );

  return branch;
}; // Clone

///////////////
// Attribute //
///////////////

int Branch::GetVZK() const
{
	return m_nVZK;
};

int Branch::GetZFK( const int n ) const
{
	if (n>=0 && n<2)
		return m_nZFK[n];

	return -1;
};

int Branch::GetAFK( const int n ) const
{
	if (n>=0 && n<2)
		return m_nAFK[n];

	return -1;
};

int Branch::GetAM() const
{
	return m_nAM;
};

BOOL Branch::GetAA() const
{
	return m_bAA;
};

void Branch::SetVZK( const int vzk )
{
	m_nVZK = vzk;
}

void Branch::SetZFK( const int n, const int zfk )
{
	if( n >= 0 && n < 2 )
    m_nZFK[n] = zfk;
};

void Branch::SetAFK( const int n, const int afk )
{
	if( n >= 0 && n < 2 )
		m_nAFK[n] = afk;
}

void Branch::SetAM( const int am )
{
	m_nAM = am;
}

void Branch::SetAA( const BOOL aa )
{
	m_bAA = aa;
}

istream& operator>>(istream& is, Branch &br)
{
	char buffer[LINE_SIZE];
	int i;
	CString str;

	is.getline(buffer, LINE_SIZE);
	str = buffer;
	str.TrimRight();
	str.TrimLeft();
	if (str.IsEmpty())
		return is;
	str += " ";
	br.m_nVZK = atoi(str.Left(str.Find(" ")));
	str = str.Right(str.GetLength()-str.Find(" "));
	str.TrimLeft();

	for (i=0; i<2; i++)
	{
		br.m_nZFK[i] = atoi(str.Left(str.Find(" ")));
		str = str.Right(str.GetLength()-str.Find(" "));
		str.TrimLeft();
	}

	for (i=0; i<2; i++)
	{
		br.m_nAFK[i] = atoi(str.Left(str.Find(" ")));
		str = str.Right(str.GetLength()-str.Find(" "));
		str.TrimLeft();
	}

	br.m_nAM = atoi(str.Left(str.Find(" ")));
	str = str.Right(str.GetLength()-str.Find(" "));
	str.TrimLeft();

	br.m_bAA = (BOOL)atoi(str.Left(str.Find(" ")));

	return is;
}

ostream& operator<<(ostream& os, Branch &br)
{
	int i;

	os << setw(2) << br.m_nVZK << " ";
	for (i=0; i<2; i++)
		os << setw(2) << br.m_nZFK[i] << " ";
	for (i=0; i<2; i++)
		os << setw(2) << br.m_nAFK[i] << " ";
	os << setw(3) << br.m_nAM << " ";
	os << br.m_bAA << endl;

	return os;
}
