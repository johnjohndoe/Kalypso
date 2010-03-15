// CSection.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef CSECTION_H
#define CSECTION_H

#include "section.h"

class LengthSection;
class DataBlockArray;

class CrossSection : public Section
{
public:
   CrossSection::CrossSection(Project* pProject);
	 ~CrossSection();

   CrossSection* Clone( Project* pProject, BOOL createNewFile );

	 void SetWaterName( const CString& name );
	 void SetStation( const double st );
	 void SetPK( const CString& pk );
	 void SetVZK( const int vzk );
	 void SetStateName( const CString& name );

	 CString GetWaterName() const;
	 double GetStation() const;
	 CString GetPK() const;
   int GetPKNum() const;
	 int GetVZK() const;
	 CString GetStateName() const;

	 void CreateFileName(CStringList* forbiddenFiles = NULL);

   BOOL InsertWsp( const CString& wspKennung, double wspHoehe, BOOL bDurchst, const double abfluss, const CString& strAbflussFormat );
   BOOL RemoveWsp( const CString& wspKennung );

   void CreatePrintSummary( CStringArray& strings );

  //  Operatoren
   BOOL operator==( const CrossSection& otherCs ) const;
   BOOL operator<( const CrossSection& otherCs ) const;


protected:
	CString m_WaterName;
	double m_dStation;
	CString m_PK;
	int m_nVZK;
	CString m_StateName;
};

// die Klasse CrossSectionArray:
// alles was mit einem ganzen Array von CrossSections gemacht werden kann
class CrossSectionArray : public CTypedPtrArray<CPtrArray, CrossSection*>
{
public:
   BOOL ChangeDataBlocks( CTypedPtrMap<CMapWordToPtr, WORD, DataBlockArray*>& dataBlocks, CString& errorString );
   UINT DeleteDatablocks( const CArray<int, int>& dbTypes );
   LengthSection* CrossSectionArray::CreateLSection() const;

   void FlipHorizontal( const bool bSave );
}; // class CrossSectionArray

#endif // CSECTION_H