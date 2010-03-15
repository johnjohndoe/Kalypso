#include "stdafx.h"

#include "variant_helper.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

COleVariantEx::operator int()
{
  return changeType( VT_I2 ).iVal;
}

COleVariantEx::operator long()
{
  return changeType( VT_I4 ).lVal;
}

COleVariantEx::operator double()
{
  return changeType( VT_R8 ).dblVal;
}

COleVariantEx::operator CString()
{
  return CString( changeType( VT_BSTR ).bstrVal );
}

VARIANT COleVariantEx::changeType( const VARTYPE vt )
{
  COleVariant newVar( this );
  newVar.ChangeType( vt );
  return newVar;
}