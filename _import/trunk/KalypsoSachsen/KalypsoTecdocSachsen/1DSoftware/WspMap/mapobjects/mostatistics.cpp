// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mostatistics.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoStatistics 

double CMoStatistics::GetMax()
{
	double result;
	GetProperty(0x1, VT_R8, (void*)&result);
	return result;
}

void CMoStatistics::SetMax(double propVal)
{
	SetProperty(0x1, VT_R8, propVal);
}

double CMoStatistics::GetMin()
{
	double result;
	GetProperty(0x2, VT_R8, (void*)&result);
	return result;
}

void CMoStatistics::SetMin(double propVal)
{
	SetProperty(0x2, VT_R8, propVal);
}

double CMoStatistics::GetMean()
{
	double result;
	GetProperty(0x3, VT_R8, (void*)&result);
	return result;
}

void CMoStatistics::SetMean(double propVal)
{
	SetProperty(0x3, VT_R8, propVal);
}

double CMoStatistics::GetStdDev()
{
	double result;
	GetProperty(0x4, VT_R8, (void*)&result);
	return result;
}

void CMoStatistics::SetStdDev(double propVal)
{
	SetProperty(0x4, VT_R8, propVal);
}

double CMoStatistics::GetSum()
{
	double result;
	GetProperty(0x5, VT_R8, (void*)&result);
	return result;
}

void CMoStatistics::SetSum(double propVal)
{
	SetProperty(0x5, VT_R8, propVal);
}

long CMoStatistics::GetCount()
{
	long result;
	GetProperty(0x6, VT_I4, (void*)&result);
	return result;
}

void CMoStatistics::SetCount(long propVal)
{
	SetProperty(0x6, VT_I4, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoStatistics 
