#include "stdafx.h"

#include "coord.h"
#include "giostr.h"

#include "calcdata.h"

  ////////////////////////////
  //  Klasse  CalcData
  ///////////////////////////

static int min_bce_values[13] =
{
	1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0
};

static int max_bce_values[13] =
{
	3, 3, 2, 1, 2, 2, 3, 3, 1, 1, 1, 1, 1
};

static int default_bce_values[13] =
{
	1, 1, 1, 0, 1, 1, 2, 2, 0, 0, 0, 0, 0
};

/* The Default Constructor */
CalcData::CalcData()
{
	CalcData(NULL, NULL, TRUE);
}

CalcData::CalcData(Calculation* pOwner, State *pState, BOOL bLWA)
{
	int i;
	
	m_pOwner = pOwner;
	m_pState = pState;
	m_bLWA = bLWA;
	m_strQPlot = _T("QPLOT");
	m_strLPlot = _T("LPLOT");
	m_dAnfang = 0;
	m_dEnde = 0;
	m_nEich = 0;
	m_dHe = 0;
	m_nHGralle = 0;
	m_nNormal = 0;
	m_nWTau = 0;
	m_nWasser = 2;
	m_dHoehe = 0;
	m_dGefaelle = 0;
	m_nSelIndex = 0;
	m_nNasall = 0;
	m_nNasabs = 0;
	m_nWQBez = 0;
	m_nQWV = 0;
	m_nKalMin = 0;
	m_nWehrAnf = 0;
	m_dQMin = 0;
	m_dQStep = 0;
	m_dQMax = 0;
	m_nA = 0;
	m_nCar = 1;
	m_nHyd = 1;
	m_nDat = 0;
	m_nFrou = 0;
	m_nAuto = 0;
	m_dEpsh = 0.005;
	m_dEpsv = 0.01;
	m_dRny = 1.31;
	m_dCwr = 1.5;
	m_nSchiess = 0;
	m_nFP = 0;
	m_nDr = 0;
 	m_nPunkt = 0;
	m_nZMax = 67;
	m_nPosey = 0;
	m_nBeta = 0;
	m_nForm = 0;
	m_nNN = 0;
  m_dSM = std::numeric_limits<double>::infinity();
	m_nHMO = 0;
	m_nWSFQ = 0;
	m_nWSFL = 0;
	m_nUSG = 0;
	m_dDHWMax = 2;
	m_dVFMax = 8;
	m_dHZVMax = 1;
	m_dFakLHG = 5;
	m_dFFMax = 5000;

	for (i=0; i<13; i++)
		m_nWerte[i] = default_bce_values[i];
	m_nAbfluss = 0;
	m_dWAnfang = 0;
}

CalcData::~CalcData()
{
	int i;

	for (i=0; i<m_arNasim.GetSize(); i++)
		delete m_arNasim[i];
	m_arNasim.RemoveAll();
}

BOOL CalcData::Load(CString& filename)
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

BOOL CalcData::Save(CString& filename)
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

CalcData* CalcData::Clone( Calculation* pOwner, State* pState )
{
	int i;
	CalcData *newData = new CalcData( pOwner, pState, m_bLWA );

	newData->m_strInfo = m_strInfo;
	newData->m_dAnfang = m_dAnfang;
	newData->m_dEnde = m_dEnde;
	newData->m_nEich = m_nEich;
	newData->m_dHe = m_dHe;
	newData->m_nHGralle = m_nHGralle;
	newData->m_nNormal = m_nNormal;
	newData->m_nWTau = m_nWTau;
	newData->m_nWasser = m_nWasser;
	newData->m_dHoehe = m_dHoehe;
	newData->m_dGefaelle = m_dGefaelle;
	newData->m_nSelIndex = m_nSelIndex;
	newData->m_strQ = m_strQ;
	newData->m_strQPlot = m_strQPlot;
	newData->m_strLPlot = m_strLPlot;
	newData->m_nNasall = m_nNasall;
	newData->m_nNasabs = m_nNasabs;
	newData->m_nWQBez = m_nWQBez;
	newData->m_nQWV = m_nQWV;
	newData->m_nKalMin = m_nKalMin;
	newData->m_nWehrAnf = m_nWehrAnf;
	newData->m_dQMin = m_dQMin;
	newData->m_dQStep = m_dQStep;
	newData->m_dQMax = m_dQMax;
	newData->m_nA = m_nA;
	newData->m_nCar = m_nCar;
	newData->m_nHyd = m_nHyd;
	newData->m_nDat = m_nDat;
	newData->m_nFrou = m_nFrou;
	newData->m_nAuto = m_nAuto;
	newData->m_dEpsh = m_dEpsh;
	newData->m_dEpsv = m_dEpsv;
	newData->m_dRny = m_dRny;
	newData->m_dCwr = m_dCwr;
	newData->m_nSchiess = m_nSchiess;
	for (i=0; i<m_arNasim.GetSize(); i++)
	{
		Coord *pCrd, *pNewCrd;

		pCrd = m_arNasim[i];
		pNewCrd = new Coord(*pCrd);
		newData->m_arNasim.Add(pNewCrd);
	}
	newData->m_nFP = m_nFP;
	newData->m_nDr = m_nDr;
 	newData->m_nPunkt = m_nPunkt;
	newData->m_nZMax = m_nZMax;
	newData->m_nPosey = m_nPosey;
	newData->m_nBeta = m_nBeta;
	newData->m_nForm = m_nForm;
	newData->m_nNN = m_nNN;
	newData->m_dSM = m_dSM;
	newData->m_nHMO = m_nHMO;
	newData->m_nWSFQ = m_nWSFQ;
	newData->m_nWSFL = m_nWSFL;

	for (i=0; i<13; i++)
		newData->m_nWerte[i] = m_nWerte[i];
	newData->m_nAbfluss = m_nAbfluss;
	newData->m_dWAnfang = m_dWAnfang;

	return newData;
}

Calculation* CalcData::GetOwner()
{
	return m_pOwner;
}

State* CalcData::GetState()
{
	return m_pState;
}

void CalcData::GetInfo(CString& text)
{
	text = m_strInfo;
}

void CalcData::SetInfo(CString& text)
{
	m_strInfo = text;
}

istream& operator>>(istream& is, CalcData &cd)
{
	char buffer[256];
	int i, n = 0;
	CString str;

	if (cd.m_bLWA)
	{
		// 1. Zeile
		is.getline(buffer, 256);
		cd.m_strInfo = buffer;
		cd.m_strInfo.TrimLeft();
		cd.m_strInfo.TrimRight();
		// 2. Zeile
		is.getline(buffer, 256);
		cd.m_dAnfang = atof(buffer);
		// 3. Zeile
		is.getline(buffer, 256);
		cd.m_dEnde = atof(buffer);
		// 4. Zeile
		is.getline(buffer, 256);
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		cd.m_nEich = 0;
		if (str.Left(1).CompareNoCase("E")==0 &&
			str.Mid(4, 1).CompareNoCase("V")==0)
			cd.m_nEich = 1;		// EICHVO
		if (str.Left(1).CompareNoCase("E")==0 &&
			str.Mid(4, 1).CompareNoCase("F")==0)
			cd.m_nEich = 2;		// EICHFL
		// 5. Zeile
		is.getline(buffer, 256);
		cd.m_dHe = atof(buffer);
		// 6. Zeile
		is.getline(buffer, 256);
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		if (str.Left(1).CompareNoCase("H")==0)
			cd.m_nHGralle = 1;
		else
			cd.m_nHGralle = 0;
		// 7. Zeile
		is.getline(buffer, 256);
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		if (str.Left(1).CompareNoCase("H")==0)
			cd.m_nNormal = 1;
		else
			cd.m_nNormal = 0;
		// 8. Zeile
		is.getline(buffer, 256);
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		if (str.Left(1).CompareNoCase("W")==0)
			cd.m_nWTau = 1;
		else
			cd.m_nWTau = 0;
		// 9. Zeile
		is.getline(buffer, 256);
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		if (str.Mid(1, 1).CompareNoCase("S")==0)
			cd.m_nWasser = 0;	// WSP
		if (str.Mid(1, 1).CompareNoCase("N")==0)
			cd.m_nWasser = 1;	// HNORM
		if (str.Mid(1, 1).CompareNoCase("G")==0)
			cd.m_nWasser = 2;	// HGRENZ
		// 10. Zeile
		is.getline(buffer, 256);
		cd.m_dHoehe = atof(buffer);
		// 11. Zeile
		is.getline(buffer, 256);
		cd.m_dGefaelle = atof(buffer);
		// 12. Zeile
		is.getline(buffer, 256);
		cd.m_nSelIndex = atoi(buffer);
		// 13. Zeile
		is.getline(buffer, 256);
		cd.m_strQ = buffer;
		cd.m_strQ.TrimLeft();
		cd.m_strQ.TrimRight();
		// 14. Zeile
		is.getline(buffer, 256);
//		cd.m_strQPlot = buffer;
//		cd.m_strQPlot.TrimLeft();
//		cd.m_strQPlot.TrimRight();
		// 15. Zeile
		is.getline(buffer, 256);
//		cd.m_strLPlot = buffer;
//		cd.m_strLPlot.TrimLeft();
//		cd.m_strLPlot.TrimRight();
		// 16. Zeile
		is.getline(buffer, 256);
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		if (str.Left(1).CompareNoCase("N")==0)
			cd.m_nNasall = 1;
		else
			cd.m_nNasall = 0;
		// 17. Zeile
		is.getline(buffer, 256);
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		if (str.Left(1).CompareNoCase("N")==0)
			cd.m_nNasabs = 1;
		else
			cd.m_nNasabs = 0;
		// 18. Zeile
		is.getline(buffer, 256);
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		if (str.Left(1).CompareNoCase("W")==0)
			cd.m_nWQBez = 1;
		else
			cd.m_nWQBez = 0;
		// 19. Zeile
		is.getline(buffer, 256);
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		if (str.Left(1).CompareNoCase("Q")==0)
			cd.m_nQWV = 1;
		else
			cd.m_nQWV = 0;
		// 20. Zeile
		is.getline(buffer, 256);
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		if (str.Left(1).CompareNoCase("K")==0)
			cd.m_nKalMin = 1;
		else
			cd.m_nKalMin = 0;
		// 21. Zeile
		is.getline(buffer, 256);
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		if (str.Left(1).CompareNoCase("W")==0)
			cd.m_nWehrAnf = 1;
		else
			cd.m_nWehrAnf = 0;
		// 22. Zeile
		is.getline(buffer, 256);
		cd.m_dQMin = atof(buffer);
		// 23. Zeile
		is.getline(buffer, 256);
		cd.m_dQStep = atof(buffer);
		// 24. Zeile
		is.getline(buffer, 256);
		cd.m_dQMax = atof(buffer);
		// 25. Zeile
		is.getline(buffer, 256);
		cd.m_nA = atoi(buffer);
		// 26. Zeile
		is.getline(buffer, 256);
		cd.m_nCar = atoi(buffer);
		if (cd.m_nCar>=4)
			cd.m_nCar = 0;
		// 27. Zeile
		is.getline(buffer, 256);
		cd.m_nHyd = atoi(buffer);
		// 28. Zeile
		is.getline(buffer, 256);
		cd.m_nDat = atoi(buffer);
		// 29. Zeile
		is.getline(buffer, 256);
		cd.m_nFrou = atoi(buffer);
		// 30. Zeile
		is.getline(buffer, 256);
		cd.m_nAuto = atoi(buffer);
		// 31. Zeile
		is.getline(buffer, 256);
		cd.m_dEpsh = atof(buffer);
		// 32. Zeile
		is.getline(buffer, 256);
		cd.m_dEpsv = atof(buffer);
		// 33. Zeile
		is.getline(buffer, 256);
		cd.m_dRny = atof(buffer);
		// 34. Zeile
		is.getline(buffer, 256);
		cd.m_dCwr = atof(buffer);
		// 35. Zeile
		is.getline(buffer, 256);
		cd.m_nSchiess = atoi(buffer);
		// folgende Zeile - Nasim
		is.getline(buffer, 256);
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		i = str.Find(" ");
		if (i!=-1)
		{
			n = atoi(str.Right(str.GetLength()-i-1));
			str = str.Left(i);
			cd.m_arNasim.SetSize(n);
			for (i=0; i<n; i++)
			{
				Coord* pCrd = new Coord;
				is >> pCrd->dx >> pCrd->dy;
				cd.m_arNasim.SetAtGrow(i, pCrd);
				is.getline(buffer, 256);	// remove end of line
			}
		}
		if (!is.eof())
		{
			is.getline(buffer, 256);
			cd.m_nFP = atoi(buffer);
		}
		else
			cd.m_nFP = 0;
		if (!is.eof())
		{
			is.getline(buffer, 256);
			cd.m_nDr = atoi(buffer);
		}
		else
			cd.m_nDr = 0;
		if (!is.eof())
		{
			is.getline(buffer, 256);
			cd.m_nPunkt = atoi(buffer);
		}
		else
			cd.m_nPunkt = 0;
		if (!is.eof())
		{
			is.getline(buffer, 256);
			cd.m_nZMax = atoi(buffer);
		}
		else
			cd.m_nZMax = 67;
		if (!is.eof())
		{
			is.getline(buffer, 256);
			cd.m_nPosey = atoi(buffer);
		}
		else
			cd.m_nPosey = 0;
		if (!is.eof())
		{
			is.getline(buffer, 256);
			cd.m_nBeta = atoi(buffer);
		}
		else
			cd.m_nBeta = 0;
		if (!is.eof())
		{
			is.getline(buffer, 256);
			cd.m_nForm = atoi(buffer);
		}
		else
			cd.m_nForm = 0;
		if (!is.eof())
		{
			is.getline(buffer, 256);
			cd.m_nNN = atoi(buffer);
		}
		else
			cd.m_nNN = 0;
		if (!is.eof())
		{
			is.getline(buffer, 256);
			if (buffer[0]=='\0')
        cd.m_dSM = std::numeric_limits<double>::infinity();
			else
				cd.m_dSM = atof(buffer);
		}
		else
			cd.m_dSM = std::numeric_limits<double>::infinity();
		if (!is.eof())
		{
			is.getline(buffer, 256);
			cd.m_nHMO = atoi(buffer);
		}
		else
			cd.m_nHMO = 0;
		if (!is.eof())
		{
			is.getline(buffer, 256);
			cd.m_nWSFQ = atoi(buffer);
		}
		else
			cd.m_nWSFQ = 0;
		if (!is.eof())
		{
			is.getline(buffer, 256);
			cd.m_nWSFL = atoi(buffer);
		}
		else
			cd.m_nWSFL = 0;
		if (!is.eof())
		{
			is.getline(buffer, 256);
			cd.m_nUSG = atoi(buffer);
		}
		else
			cd.m_nUSG = 0;
		if (!is.eof())
		{
			is.getline(buffer, 256);
			cd.m_dDHWMax = atof(buffer);
		}
		else
			cd.m_dDHWMax = 2;
		if (!is.eof())
		{
			is.getline(buffer, 256);
			cd.m_dVFMax = atof(buffer);
		}
		else
			cd.m_dVFMax = 8;
		if (!is.eof())
		{
			is.getline(buffer, 256);
			cd.m_dHZVMax = atof(buffer);
		}
		else
			cd.m_dHZVMax = 1;
		if (!is.eof())
		{
			is.getline(buffer, 256);
			cd.m_dFakLHG = atof(buffer);
		}
		else
			cd.m_dFakLHG = 5;
		if (!is.eof())
		{
			is.getline(buffer, 256);
			cd.m_dFFMax = atof(buffer);
		}
		else
			cd.m_dFFMax = 5000;
	}
	else
	{
		for (i=0; i<13; i++)
		{
			is.getline(buffer, 256);
			cd.m_nWerte[i] = atoi(buffer);
			if (cd.m_nWerte[i]<min_bce_values[i] || cd.m_nWerte[i]>max_bce_values[i])
				cd.m_nWerte[i] = default_bce_values[i];
		}
		is.getline(buffer, 256);
		cd.m_dGefaelle = atof(buffer);
		is.getline(buffer, 256);
		cd.m_dQMin = atof(buffer);
		is.getline(buffer, 256);
		cd.m_dQStep = atof(buffer);
		is.getline(buffer, 256);
		cd.m_dQMax = atof(buffer);
		is.getline(buffer, 256);
		cd.m_nAbfluss = atoi(buffer);
		is.getline(buffer, 256);
		cd.m_dAnfang = atof(buffer);
		is.getline(buffer, 256);
		cd.m_dEnde = atof(buffer);
		is.getline(buffer, 256);
		cd.m_strInfo = buffer;
		cd.m_strInfo.TrimLeft();
		cd.m_strInfo.TrimRight();
		is.getline(buffer, 256);
		cd.m_dHoehe = atof(buffer);
		is.getline(buffer, 256);
		cd.m_nHMO = atoi(buffer);
		is.getline(buffer, 256);
		cd.m_nWSFQ = atoi(buffer);
		is.getline(buffer, 256);
		cd.m_nWSFL = atoi(buffer);
		is.getline(buffer, 256);
		cd.m_strQ = buffer;
		cd.m_strQ.TrimLeft();
		cd.m_strQ.TrimRight();
		is.getline(buffer, 256);
		cd.m_nUSG = atoi(buffer);
		is.getline(buffer, 256);
		cd.m_nKalMin = atoi(buffer);
	}

	return is;
}

ostream& operator<<(ostream& os, CalcData &cd)
{
	CString str;
	int i;
	
	if (cd.m_bLWA)
	{
		// 1. Zeile
		os << cd.m_strInfo << endl;
		// 2. Zeile
		os << cd.m_dAnfang << endl;
		// 3. Zeile
		os << cd.m_dEnde << endl;
		// 4. Zeile
		switch (cd.m_nEich)
		{
		case 1:
			str = "EICHVO";
			break;
			
		case 2:
			str = "EICHFL";
			break;
			
		default:
			str.Empty();
			break;
		}
		os << str << endl;
		// 5. Zeile
		if (cd.m_nEich==0)
			os << endl;
		else
			os << cd.m_dHe << endl;
		// 6. Zeile
		if (cd.m_nHGralle==1)
			str = "HGRALL";
		else
			str.Empty();
		os << str << endl;
		// 7. Zeile
		if (cd.m_nNormal==1)
			str = "HNORMA";
		else
			str.Empty();
		os << str << endl;
		// 8. Zeile
		if (cd.m_nWTau==1)
			str = "WTAU";
		else
			str.Empty();
		os << str << endl;
		// 9. Zeile
		switch (cd.m_nWasser)
		{
		case 0:
			str = "WSP";
			break;
			
		case 1:
			str = "HNORM";
			break;
			
		case 2:
			str = "HGRENZ";
			break;
			
		default:
			str.Empty();
			break;
		}
		os << str << endl;
		// 10. Zeile
		os << cd.m_dHoehe << endl;
		// 11. Zeile
		os << cd.m_dGefaelle << endl;
		// 12. Zeile
		os << cd.m_nSelIndex << endl;
		// 13. Zeile
		os << cd.m_strQ << endl;
		// 14. Zeile
		os << cd.m_strQPlot << endl;
		// 15. Zeile
		os << cd.m_strLPlot << endl;
		// 16. Zeile
		if (cd.m_nNasall==1)
			str = "NASALL";
		else
			str.Empty();
		os << str << endl;
		// 17. Zeile
		if (cd.m_nNasabs==1)
			str = "NASABS";
		else
			str.Empty();
		os << str << endl;
		// 18. Zeile
		if (cd.m_nWQBez==1)
			str = "WQBEZ";
		else
			str.Empty();
		os << str << endl;
		// 19. Zeile
		if (cd.m_nQWV==1)
			str = "QWV";
		else
			str.Empty();
		os << str << endl;
		// 20. Zeile
		if (cd.m_nKalMin==1)
			str = "KALMIN";
		else
			str.Empty();
		os << str << endl;
		// 21. Zeile
		if (cd.m_nWehrAnf==1)
			str = "WEHRAN";
		else
			str.Empty();
		os << str << endl;
		if (cd.m_nWQBez==1)
		{
			// 22. Zeile
			os << cd.m_dQMin << endl;
			// 23. Zeile
			os << cd.m_dQStep << endl;
			// 24. Zeile
			os << cd.m_dQMax << endl;
		}
		else
			os << endl << endl << endl;
		// 25. Zeile
		os << cd.m_nA << endl;
		// 26. Zeile
		os << cd.m_nCar << endl;
		// 27. Zeile
		os << cd.m_nHyd << endl;
		// 28. Zeile
		os << cd.m_nDat << endl;
		// 29. Zeile
		os << cd.m_nFrou << endl;
		// 30. Zeile
		os << cd.m_nAuto << endl;
		// 31. Zeile
		os << cd.m_dEpsh << endl;
		// 32. Zeile
		os << cd.m_dEpsv << endl;
		// 33. Zeile
		os << cd.m_dRny << endl;
		// 34. Zeile
		os << cd.m_dCwr << endl;
		// 35. Zeile
		os << cd.m_nSchiess << endl;
		// folgende Zeile - Nasim
		os << "NASIM " << cd.m_arNasim.GetSize() << endl;
		for (int i=0; i<cd.m_arNasim.GetSize(); i++)
		{
			Coord *pCrd = cd.m_arNasim[i];
			os << setw(10) << pCrd->dx;
			os << setw(10) << pCrd->dy << endl;
		}
		os << cd.m_nFP << endl;
		os << cd.m_nDr << endl;
		os << cd.m_nPunkt << endl;
		os << cd.m_nZMax << endl;
		os << cd.m_nPosey << endl;
		os << cd.m_nBeta << endl;
		os << cd.m_nForm << endl;
		os << cd.m_nNN << endl;
		if( cd.m_dSM == std::numeric_limits<double>::infinity() )
			os << endl;
		else
			os << cd.m_dSM << endl;
		os << cd.m_nHMO << endl;
		os << cd.m_nWSFQ << endl;
		os << cd.m_nWSFL << endl;
		os << cd.m_nUSG << endl;
		os.precision(2);
		os << cd.m_dDHWMax << endl;
		os << cd.m_dVFMax << endl;
		os << cd.m_dHZVMax << endl;
		os << cd.m_dFakLHG << endl;
		os.precision(1);
		os << cd.m_dFFMax << endl;
		os.precision(6);
	}
	else
	{
		for( i = 0; i < 13; i++ )
			os << cd.m_nWerte[i] << endl;

		os << cd.m_dGefaelle << endl;
		os << cd.m_dQMin << endl;
		os << cd.m_dQStep << endl;
		os << cd.m_dQMax << endl;
		os << cd.m_nAbfluss << endl;
		
		CString format;

		format.Format( "%.4lf", cd.m_dAnfang );
		os << format << endl;

		format.Format( "%.4lf", cd.m_dEnde );
		os << format << endl;
		
		os << cd.m_strInfo << endl;

		// die WspHöhe mit zwei Nachkommastellen rauschreiben ( ohne NKS macht wsp.exe Schrott )
		format.Format( "%.2lf", cd.m_dHoehe );
		os << format << endl;

		os << cd.m_nHMO << endl;
		os << cd.m_nWSFQ << endl;
		os << cd.m_nWSFL << endl;
		os << cd.m_strQ << endl;
		os << cd.m_nUSG << endl;
		os << cd.m_nKalMin << endl;
	}

	return os;
}
