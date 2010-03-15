// CalcData.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef CALCDATA_H
#define CALCDATA_H

class Calculation;
class Coord;
class State;

class CalcData : public CObject
{
public:
   CalcData::CalcData();
   CalcData::CalcData(Calculation* pOwner, State* pState, BOOL bLWA);
	 ~CalcData();

   CalcData* Clone( Calculation* pOwner, State* pState );

	 Calculation* GetOwner();
	 State* GetState();

	 void GetInfo(CString& text);

	 void SetInfo(CString& text);

	 BOOL Load(CString& filename);
	 BOOL Save(CString& filename);

  BOOL GetLWA() const { return m_bLWA; };
	

protected:
	friend istream& operator>>(istream& is, CalcData &cd);
	friend ostream& operator<<(ostream& os, CalcData &cd);

protected:
	Calculation* m_pOwner;
	State* m_pState;
	BOOL m_bLWA;
public:
	// 1. Zeile
	CString m_strInfo;
	// 2. Zeile
	double m_dAnfang;
	// 3. Zeile
	double m_dEnde;
	// 4. Zeile
	short m_nEich;
	// 5. Zeile
	double m_dHe;
	// 6. Zeile
	short m_nHGralle;
	// 7. Zeile
	short m_nNormal;
	// 8. Zeile
	short m_nWTau;
	// 9. Zeile
	short m_nWasser;
	// 10. Zeile
	double m_dHoehe;
	// 11. Zeile
	double m_dGefaelle;
	// 12. Zeile
	int m_nSelIndex;
	// 13. Zeile
	CString m_strQ;
	// 14. Zeile
	CString m_strQPlot;
	// 15. Zeile
	CString m_strLPlot;
	// 16. Zeile
	short m_nNasall;
	// 17. Zeile
	short m_nNasabs;
	// 18. Zeile
	short m_nWQBez;
	// 19. Zeile
	short m_nQWV;
	// 20. Zeile
	short m_nKalMin;
	// 21. Zeile
	short m_nWehrAnf;
	// 22. Zeile
	double m_dQMin;
	// 23. Zeile
	double m_dQStep;
	// 24. Zeile
	double m_dQMax;
	// 25. Zeile
	int m_nA;
	// 26. Zeile
	int m_nCar;
	// 27. Zeile
	int m_nHyd;
	// 28. Zeile
	int m_nDat;
	// 29. Zeile
	int m_nFrou;
	// 30. Zeile
	int m_nAuto;
	// 31. Zeile
	double m_dEpsh;
	// 32. Zeile
	double m_dEpsv;
	// 33. Zeile
	double m_dRny;
	// 34. Zeile
	double m_dCwr;
	// 35. Zeile
	int m_nSchiess;
	// folgende Zeile - Nasim
	CTypedPtrArray<CPtrArray, Coord*> m_arNasim;
	int m_nFP;
	int m_nDr;
 	int m_nPunkt;
	int m_nZMax;
	int m_nPosey;
	int m_nBeta;
	int m_nForm;
	int m_nNN;
	double m_dSM;
	int m_nHMO;
	int m_nWSFQ;
	int m_nWSFL;
	int m_nUSG;
	double m_dDHWMax;
	double m_dVFMax;
	double m_dHZVMax;
	double m_dFakLHG;
	double m_dFFMax;

	int m_nWerte[13];
	int m_nAbfluss;
	double m_dWAnfang;
};

#endif // CALCDATA_H