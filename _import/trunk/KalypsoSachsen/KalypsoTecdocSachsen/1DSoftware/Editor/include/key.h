// key.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CKey 

class CKey
{
public:
	CKey() {m_hKey = NULL;}
	~CKey() {Close();}

// Attributes
public:
	HKEY m_hKey;
	BOOL SetStringValue(LPCTSTR lpszValue, LPCTSTR lpszValueName = NULL);
	BOOL GetStringValue(CString& str, LPCTSTR lpszValueName = NULL);

// Operations
public:
	BOOL Create(HKEY hKey, LPCTSTR lpszKeyName);
	BOOL Open(HKEY hKey, LPCTSTR lpszKeyName);
	void Close();

// Overrides

// Implementation
protected:
};

/////////////////////////////////////////////////////////////////////////////
