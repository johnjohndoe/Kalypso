// Section.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef SECTION_H
#define SECTION_H

#define CLASS_TYPE_LSECTION		0
#define CLASS_TYPE_CSECTION		1

class Project;
class Profil;
class State;

class Section : public CObject
{
public:
   Section::Section( int nClassType, Project* pProject );
	 ~Section();

	 int GetClassType();
	 CString GetFileTitle();
	 CString GetFileExt();
	 CString GetFileName();
	 Profil* GetProfil();
	 int GetProfilNr();
	virtual double GetStation();
	virtual void GetVZK(CString& vzk);

	 void SetFileName( CString& file, BOOL bSetOriginalFile = FALSE );
	 void SetProfilNr(int n);

	 BOOL LoadProfil();
	 BOOL SaveProfil();
   BOOL RemoveFile();
	 void SetProfil(Profil* prof);
	 void FlushProfil();

	void AddState(State* st);
	void RemoveState(State* st);
	 State* GetFirstState();
	 State* GetNextState();
  void SetModified();
	void SetStatesModified();

  // event-Handling
  void RegisterEventHandle( CWnd* window );
  BOOL UnregisterEventHandle( CWnd* window );
  void NotifiyEventHandles( DWORD event );

protected:
	CTypedPtrList<CPtrList, State*> m_States;
	POSITION m_StatePos;
	int m_nClassType;
	Project* m_pProject;
	CString m_fileTitle, m_fileExt;
	Profil* m_profil;
	int m_nProfilNr;
  BOOL m_bModified;

  CTypedPtrArray<CObArray, CWnd*> m_eventHandles;
}; // class Section

class SectionArray : public CArray<Section*, Section*> {};

#endif // SECTION_H