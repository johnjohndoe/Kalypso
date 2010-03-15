#include "stdafx.h"
#include "oleDispDriverEx.h"

VARIANT COleDispatchDriverEx::GetProperty( const CString& propertyName )
{
  USES_CONVERSION; // für T2COLE
  VARIANT result;
  LPCOLESTR	lpOleStr = T2COLE( LPCTSTR( propertyName ) );
	DISPID		dispID = -1;

	if ( SUCCEEDED( m_lpDispatch->
                    GetIDsOfNames( IID_NULL, (LPOLESTR*)&lpOleStr, 1, 0, &dispID ) ) )
    COleDispatchDriver::GetProperty(dispID, VT_VARIANT, &result);

	return result;
};
