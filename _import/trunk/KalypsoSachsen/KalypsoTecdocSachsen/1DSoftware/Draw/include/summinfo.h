// summinfo.h : interface of the CSummInfo classes
//

extern const OLECHAR szSummInfo[];

#include "propset.h"

// {34150001-0D07-11d3-A4B8-0080ADAC5D6B}
DEFINE_GUID(FMTID_SummaryInformation, 0x34150001, 0xd07, 0x11d3,
	0xa4, 0xb8, 0x0, 0x80, 0xad, 0xac, 0x5d, 0x6b);

class CSummInfo
{
public:
	CSummInfo();
	BOOL SetTitle(LPCTSTR szTitle);
	CString GetTitle();
	BOOL SetSubject(LPCTSTR szSubject);
	CString GetSubject();
	BOOL SetAuthor(LPCTSTR szAuthor);
	CString GetAuthor();
	BOOL SetKeywords(LPCTSTR szKeywords);
	CString GetKeywords();
	BOOL SetComments(LPCTSTR szComments);
	CString GetComments();
	BOOL SetTemplate(LPCTSTR szTemplate);
	CString GetTemplate();
	BOOL SetLastAuthor(LPCTSTR szLastAuthor);
	CString GetLastAuthor();
	BOOL IncrRevNum();
	CString GetRevNum();
	void StartEditTimeCount();
	BOOL AddCountToEditTime();
	CString GetEditTime();
	BOOL RecordPrintDate();
	CString GetLastPrintDate();
	BOOL RecordCreateDate();
	CString GetCreateDate();
	BOOL RecordSaveDate();
	CString GetLastSaveDate();
	BOOL SetNumPages(ULONG nPages);
	CString GetNumPages();
	BOOL SetNumWords(ULONG nWords);
	CString GetNumWords();
	BOOL SetNumChars(ULONG nChars);
	CString GetNumChars();
	BOOL SetAppname(LPCTSTR szAppname);
	CString GetAppname();
	BOOL SetSecurity(ULONG nLevel);
	CString GetSecurity();
	BOOL WriteToStorage(LPSTORAGE lpRootStg);
	BOOL ReadFromStorage(LPSTORAGE lpRootStg);
protected:
	CPropertySet m_propSet;
	CPropertySection* m_pSection;
	__int64 startEdit;
};
