#if !defined(_OLEDISPDRIVEREX_H_INCLUDED_)
#define _OLEDISPDRIVEREX_H_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


class COleDispatchDriverEx : public COleDispatchDriver
{
public:
	COleDispatchDriverEx() {}		// Calls COleDispatchDriver default constructor
	COleDispatchDriverEx( LPDISPATCH pDispatch, BOOL bAutoRelease = TRUE ) : 
        COleDispatchDriver( pDispatch, bAutoRelease ) {}
	COleDispatchDriverEx( const COleDispatchDriver& dispatchSrc ) : 
        COleDispatchDriver( dispatchSrc ) {}

  VARIANT GetProperty( const CString& propertyName );
};



#endif // _OLEDISPDRIVEREX_H_INCLUDED_