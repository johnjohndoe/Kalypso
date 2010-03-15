#include "stdAfx.h"

#include "batch.h"

  ////////////////////////////
  //  Klasse  Batch
  ///////////////////////////

/* The Default Constructor */
Batch::Batch()
{
	m_bWholeProfile = FALSE;
	m_dDistance = 0.0;
	m_bExisting = FALSE;
}

Batch::~Batch()
{
}

void Batch::DeleteContents()
{
	m_xCoords.RemoveAll();
	m_yCoords.RemoveAll();
	m_stations.RemoveAll();
	m_bedHeights.RemoveAll();
	m_lbankStrings.RemoveAll();
	m_rbankStrings.RemoveAll();
	m_lbankHeights.RemoveAll();
	m_rbankHeights.RemoveAll();
}

int Batch::GetNumEntries()
{
	return m_stations.GetSize();
}

double Batch::GetXCoord(int n)
{
	if (n>=0 && n<m_xCoords.GetSize())
		return m_xCoords[n];
	else
		return 0;
}

double Batch::GetYCoord(int n)
{
	if (n>=0 && n<m_yCoords.GetSize())
		return m_yCoords[n];
	else
		return 0;
}

double Batch::GetStation(int n)
{
	if (n>=0 && n<m_stations.GetSize())
		return m_stations[n];
	else
		return 0;
}

double Batch::GetBedHeight(int n)
{
	if (n>=0 && n<m_bedHeights.GetSize())
		return m_bedHeights[n];
	else
		return 0;
}

CString Batch::GetLBankString(int n)
{
	CString str;
	
	if (n>=0 && n<m_lbankStrings.GetSize())
		str = m_lbankStrings[n];

	return str;
}

CString Batch::GetRBankString(int n)
{
	CString str;
	
	if (n>=0 && n<m_rbankStrings.GetSize())
		str = m_rbankStrings[n];

	return str;
}

double Batch::GetLBankHeight(int n)
{
	if (n>=0 && n<m_lbankHeights.GetSize())
		return m_lbankHeights[n];
	else
		return 0;
}

double Batch::GetRBankHeight(int n)
{
	if (n>=0 && n<m_rbankHeights.GetSize())
		return m_rbankHeights[n];
	else
		return 0;
}

istream& operator>>(istream& is, Batch &ba)
{
	char buffer[LINE_SIZE];
	CString str;
	double dTemp;

	is.getline(buffer, LINE_SIZE);
	while (!is.eof())
	{
		is.getline(buffer, LINE_SIZE);
		str = buffer;
		str.TrimRight();
		str.TrimLeft();
		if (str.IsEmpty())
			continue;
		str += " ";
		dTemp = atof(str.Left(str.Find(" ")));
		ba.m_xCoords.Add(dTemp);
		str = str.Right(str.GetLength()-str.Find(" "));
		str.TrimLeft();
		
		dTemp = atof(str.Left(str.Find(" ")));
		ba.m_yCoords.Add(dTemp);
		str = str.Right(str.GetLength()-str.Find(" "));
		str.TrimLeft();
		
		dTemp = atof(str.Left(str.Find(" ")));
		ba.m_stations.Add(dTemp);
		str = str.Right(str.GetLength()-str.Find(" "));
		str.TrimLeft();
		
		dTemp = atof(str.Left(str.Find(" ")));
		ba.m_bedHeights.Add(dTemp);
		str = str.Right(str.GetLength()-str.Find(" "));
		str.TrimLeft();
		
		ba.m_lbankStrings.Add(str.Left(str.Find(" ")));
		dTemp = atof(str.Left(str.Find(" ")));
		ba.m_lbankHeights.Add(dTemp);
		str = str.Right(str.GetLength()-str.Find(" "));
		str.TrimLeft();
		
		ba.m_rbankStrings.Add(str.Left(str.Find(" ")));
		dTemp = atof(str.Left(str.Find(" ")));
		ba.m_rbankHeights.Add(dTemp);
		str = str.Right(str.GetLength()-str.Find(" "));
		str.TrimLeft();
	}

	return is;
}