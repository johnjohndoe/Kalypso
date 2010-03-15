#if !defined(_VECTOR_HERLPER_H_INCLUDED_)
#define _VECTOR_HERLPER_H_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class COleVariantEx : public COleVariant
{
public:
  COleVariantEx() : COleVariant() {};
  COleVariantEx( const VARIANT& varSrc ) : COleVariant( varSrc ) {};
  COleVariantEx( const COleVariant& varSrc ) : COleVariant( varSrc ) {};

  operator int();
  operator long();
  operator double();
  operator CString();

private:
  VARIANT changeType( const VARTYPE vt );
};

#endif // !defined(  )