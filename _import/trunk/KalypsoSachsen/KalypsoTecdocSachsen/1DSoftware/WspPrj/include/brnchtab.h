// BrnchTab.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef BRNCHTAB_H
#define BRNCHTAB_H

class Branch;

class BranchTable : public CObject
{
public:
  // Konstruktion/Destruktion
  BranchTable();
  ~BranchTable();
  
  BranchTable* Clone() const ;
  
  // Attribute
  int GetNumBranches() const;
  Branch* GetBranch( const int n ) const;
  Branch* FindBranch( const int vzk );
  void AddBranch( Branch* br );
  
  // operationen
  BOOL Load( CString& filename );
  BOOL Save( CString& filename ) const;
  
  // Operatoren
  BOOL IsConnected( int vzk1, int vzk2 );
  
protected:
  friend istream& operator>>( istream& is, BranchTable &bt );
  friend ostream& operator<<( ostream& os, const BranchTable &bt );
  
protected:
  CTypedPtrArray<CObArray, Branch*> m_branches;
};

#endif // BRNCHTAB_H