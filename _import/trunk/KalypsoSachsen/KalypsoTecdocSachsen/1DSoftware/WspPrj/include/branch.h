// Branch.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef BRANCH_H
#define BRANCH_H

class Branch : public CObject
{
public:
// Konstruktion / Destruktion
   Branch::Branch();
	 ~Branch();

   Branch* Clone();

// Attribute
	 int GetVZK() const;
	 int GetZFK( const int n ) const;
	 int GetAFK( const int n ) const;
	 int GetAM() const;
	 BOOL GetAA() const;

	 void SetVZK( const int vkz );
	 void SetZFK( const int n, const int zfk );
	 void SetAFK( const int n, const int afk );
	 void SetAM( const int am );
	 void SetAA( const BOOL aa );

protected:
  friend istream& operator>>( istream& is, Branch &br );
  friend ostream& operator<<( ostream& os, Branch &br );

protected:
	int m_nVZK;     // Nummer des Strangs
	int m_nZFK[2];  // Verzweigungskennungen der beiden Zuflüsse
	int m_nAFK[2];  // ebenso AbFlussKennung
	int m_nAM;      // Prozentuale Aufteilung des Ab?Flusses
	BOOL m_bAA;     // Flag: ??
};

#endif // BRANCH_H