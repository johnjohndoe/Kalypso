#include "stdafx.h"


#include "lsection.h"
#include "calcdata.h"
#include "project.h"
#include "state.h"

#include "calc.h"

  ////////////////////////////
  //  Klasse  Calculation
  ///////////////////////////

/* The Default Constructor */
Calculation::Calculation(Project* pProject, State* pState)
{
	m_pProject = pProject;
	m_pState = pState;
	m_pCalcData = NULL;
	m_pLengthSection = NULL;
}

Calculation::~Calculation()
{
	FlushCalcData();
	if (m_pLengthSection!=NULL)
		delete m_pLengthSection;
}

Calculation* Calculation::Clone( Project* pProject, State* pState, BOOL copyFile )
// erstellt eine Copy dieser Berechungsvariante
// Parameter:
//      BOOL copyFile: falls TRUE; wird eine neue Kopie der Datei erstellt
// Rückgabewert:
//      Zeiger auf die neue Calculation
{
  Calculation* calc = new Calculation( pProject, pState );

  calc->SetEndStation( m_dEndStation );
  calc->SetStartStation( m_dStartStation );
  calc->SetFileName( m_fileTitle + "." + m_fileExt );
  calc->SetName( m_name );
  
  if ( !m_pCalcData )
    LoadCalcData( FALSE ); // TODO: hier wird jetzt stets von BCE-Längsschnitten ausgegeangen
  if ( m_pCalcData )
    calc->SetCalcData( m_pCalcData->Clone( calc, pState ) );
  calc->SetLengthSection( m_pLengthSection->Clone( pProject ) );

  // falls Datei auch kopiert werden soll, einfach unter neuem Namen abspeichern
  if ( copyFile )
  {
    calc->SetFileName( CString("") );
    calc->CreateFileName();
    calc->SaveCalcData();

    LengthSection* ls = calc->GetLengthSection();
    ls->SetFileName( CString("") );
    ls->CreateFileName( calc );
    ls->SaveProfil();
  }; // if copyFile

  return calc;
}; // Clone

BOOL Calculation::LoadCalcData( BOOL bLWA )
{
	CString filename, path;

  filename = GetFileName();
	if (filename.IsEmpty() || m_pCalcData!=NULL)
		return FALSE;
	path = m_pProject->GetDataDir();
	path += filename;
	m_pCalcData = new CalcData(this, m_pState, bLWA);
	if (!m_pCalcData->Load(path))
	{
		delete m_pCalcData;
		m_pCalcData = NULL;
		return FALSE;
	}

	return TRUE;
}

BOOL Calculation::SaveCalcData()
{
	CString filename, path;

	filename = GetFileName();
	if (filename.IsEmpty() || m_pCalcData==NULL)
		return FALSE;
	path = m_pProject->GetDataDir();
	path += filename;
	if (!m_pCalcData->Save(path))
		return FALSE;

	return TRUE;
}

void Calculation::SetCalcData(CalcData* cd)
{
	FlushCalcData();
	m_pCalcData = cd;
	if (m_pState!=NULL)
		m_pState->SetBERModified();
}

void Calculation::FlushCalcData()
{
	if (m_pCalcData!=NULL)
	{
		delete m_pCalcData;
		m_pCalcData = NULL;
	}
}

void Calculation::SetLengthSection(LengthSection* ls)
{
	if (m_pLengthSection!=NULL)
		delete m_pLengthSection;
	m_pLengthSection = ls;
	ls->AddState(m_pState);
}

LengthSection* Calculation::GetLengthSection()
{
	return m_pLengthSection;
}

BOOL Calculation::GetResultFileName(CString& fileName, int nType)
{
	if (nType>=0 && nType<N_RESULT_TYPES)
	{
		fileName = GetFileName();
		fileName.MakeLower();
		if (fileName.GetLength()>4)
		{
			fileName.SetAt(2, szResultTypes[nType][0]);
			fileName.SetAt(3, szResultTypes[nType][1]);
			if (m_pProject!=NULL)
			{
				CFile file;
				CFileStatus rStatus;
				CString path;

				path = m_pProject->GetCalcDir();
				path += fileName;
				if (file.GetStatus(path, rStatus))
					return TRUE;	// file exists
			}
			return FALSE;
		}
	}
	fileName.Empty();
	return FALSE;
}

void Calculation::SetFileName(CString& file)
{
	int i;
	
	i = file.Find('.');
	if (i==-1)
	{
		m_fileTitle = file;
		m_fileExt.Empty();
	}
	else
	{
		m_fileTitle = file.Left(i);
		m_fileExt = file.Right(file.GetLength()-i-1);
	}
	if (m_pState!=NULL)
		m_pState->SetBERModified();
}

void Calculation::SetName(CString& name)
{
	m_name = name;
	if (m_pState!=NULL)
		m_pState->SetBERModified();
}

void Calculation::SetStartStation(double value)
{
	m_dStartStation = value;
	if (m_pState!=NULL)
		m_pState->SetBERModified();
}

void Calculation::SetEndStation(double value)
{
	m_dEndStation = value;
	if (m_pState!=NULL)
		m_pState->SetBERModified();
}

void Calculation::CreateFileName()
{
	CString filename, filepath, str, fileTitle;
	int i;
	CFile file;
	CFileStatus rStatus;

	if (m_pProject==NULL || m_pState==NULL)
		return;
	filename = GetFileName();
	if (filename.IsEmpty())
	{
		i = 1;

		filepath = m_pProject->GetDataDir();
		fileTitle = m_pState->GetFileTitle();
		filename.Format("%s.%03d", fileTitle, i);
		str = filepath + filename;
		while (file.GetStatus(str, rStatus))
		{
			i++;
			filename.Format("%s.%03d", fileTitle, i);
			str = filepath + filename;
		}
		SetFileName(filename);
	}
}

CString Calculation::GetFileName()
{
	if (m_fileExt.IsEmpty())
		return m_fileTitle;
	else
		return m_fileTitle + '.' + m_fileExt;
}

CString Calculation::GetFileTitle()
{
	return m_fileTitle;
}

CString Calculation::GetFileExt()
{
	return m_fileExt;
}

CalcData* Calculation::GetCalcData()
{
	return m_pCalcData;
}

CString Calculation::GetName()
{
	return m_name;
}

double Calculation::GetStartStation()
{
	return m_dStartStation;
}

double Calculation::GetEndStation()
{
	return m_dEndStation;
}

