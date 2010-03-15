// options.h : header file
//

class CUnit
{
public:
	int m_nTPU;
	int m_nSmallDiv;	// small divisions - small line displayed
	int m_nMediumDiv;	// medium divisions - large line displayed
	int m_nLargeDiv;	// large divisions - numbers displayed
	int m_nMinMove;		// minimum tracking movements
	UINT m_nAbbrevID;
	BOOL m_bSpaceAbbrev; // put space before abbreviation
	CString m_strAbbrev;// cm, pt, pi, ", in, inch, inches

	CUnit() {}
	CUnit(int nTPU, int nSmallDiv, int nMediumDiv, int nLargeDiv, 
		int nMinMove, UINT nAbbrevID, BOOL bSpaceAbbrev);
	const CUnit& operator=(const CUnit& unit);
};

/////////////////////////////////////////////////////////////////////////////
