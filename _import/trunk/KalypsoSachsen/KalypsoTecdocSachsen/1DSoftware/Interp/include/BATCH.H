// Batch.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef BATCH_H
#define BATCH_H

class Batch : public CObject
{
public:
	Batch();
	~Batch();

	BOOL m_bWholeProfile;
	double m_dDistance;
	BOOL m_bExisting;

	int GetNumEntries();
	double GetXCoord(int n);
	double GetYCoord(int n);
	double GetStation(int n);
	double GetBedHeight(int n);
	CString GetLBankString(int n);
	CString GetRBankString(int n);
	double GetLBankHeight(int n);
	double GetRBankHeight(int n);

	void DeleteContents();

protected:
	friend istream& operator>>(istream& is, Batch &ba);

protected:
	CArray<double, double> m_xCoords;
	CArray<double, double> m_yCoords;
	CArray<double, double> m_stations;
	CArray<double, double> m_bedHeights;
	CStringArray m_lbankStrings;
	CStringArray m_rbankStrings;
	CArray<double, double> m_lbankHeights;
	CArray<double, double> m_rbankHeights;
};

#endif // Batch_H