// LSection.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef LSECTION_H
#define LSECTION_H

#include "section.h"

class Calculation;

class LengthSection : public Section
{
public:
   LengthSection::LengthSection(Project* pProject);
	 ~LengthSection();

   LengthSection* Clone( Project* pProject ); //Gibt eine Kopie des Längsschnitts zurück

	 CString GetVZK(); //VZK ???
	 CString GetName();
	 double GetStartStation();
	 double GetEndStation();

   void CreateFileName( Calculation* calc );
	double GetStation();

	void SetVZK(CString& vzk);
	 void SetName(CString& name);
	void SetStartStation(double value);
	void SetEndStation(double value);

protected:
	CString m_vzk;
	CString m_name;
	double m_dStartStation;
	double m_dEndStation;

	friend istream& operator>>(istream& is, State &st);
	friend ostream& operator<<(ostream& os, State &st);
};


class LengthSectionArray : public CTypedPtrArray<CPtrArray, LengthSection*> {};

#endif // LSECTION_H