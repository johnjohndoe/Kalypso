// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n).

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


#include "stdafx.h"
#include "mogeodataset.h"


/////////////////////////////////////////////////////////////////////////////
// Eigenschaften CMoGeoDataset 

CString CMoGeoDataset::GetName()
{
	CString result;
	GetProperty(0x1, VT_BSTR, (void*)&result);
	return result;
}

void CMoGeoDataset::SetName(LPCTSTR propVal)
{
	SetProperty(0x1, VT_BSTR, propVal);
}

BOOL CMoGeoDataset::GetAllowSharing()
{
	BOOL result;
	GetProperty(0x4, VT_BOOL, (void*)&result);
	return result;
}

void CMoGeoDataset::SetAllowSharing(BOOL propVal)
{
	SetProperty(0x4, VT_BOOL, propVal);
}

BOOL CMoGeoDataset::GetHasZ()
{
	BOOL result;
	GetProperty(0x2, VT_BOOL, (void*)&result);
	return result;
}

void CMoGeoDataset::SetHasZ(BOOL propVal)
{
	SetProperty(0x2, VT_BOOL, propVal);
}

BOOL CMoGeoDataset::GetHasMeasure()
{
	BOOL result;
	GetProperty(0x3, VT_BOOL, (void*)&result);
	return result;
}

void CMoGeoDataset::SetHasMeasure(BOOL propVal)
{
	SetProperty(0x3, VT_BOOL, propVal);
}

/////////////////////////////////////////////////////////////////////////////
// Operationen CMoGeoDataset 
