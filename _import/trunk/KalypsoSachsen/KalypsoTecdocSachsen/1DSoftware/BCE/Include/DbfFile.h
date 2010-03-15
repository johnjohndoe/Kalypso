// DbfFile.h: Schnittstelle für die Klasse DbfFile.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_BCEDBFFILE_H__90C5BE82_F817_11D4_BDD1_00104BB3E525__INCLUDED_)
#define AFX_BCEDBFFILE_H__90C5BE82_F817_11D4_BDD1_00104BB3E525__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include <stdio.h>

namespace BCE
{
  class DbfFile  
  {
    typedef unsigned char uchar;
    
    typedef	struct DBFInfo
    {
      FILE	*fp;
      
      int     nRecords;
      
      int		nRecordLength;
      int		nHeaderLength;
      int		nFields;
      int		*panFieldOffset;
      int		*panFieldSize;
      int		*panFieldDecimals;
      char	*pachFieldType;
      
      char	*pszHeader;
      
      int		nCurrentRecord;
      bool		bCurrentRecordModified;
      char	*pszCurrentRecord;
      
      bool		bNoHeader;
      bool		bUpdated;
    };
    
    typedef DbfFile::DBFInfo* DBFHandle;
    
  public:
    typedef enum 
    {
      FTString,
        FTInteger,
        FTDouble,
        FTInvalid
    } DBFFieldType;
    
  public:
    DbfFile();
    DbfFile(const char *pszFilename);
    virtual ~DbfFile();
    
    void Close();
    int AddField(const char *pszFieldName, DBFFieldType eType, int nWidth, int nDecimals);
    int WriteAttribute( int hEntity, int iField, void *pValue );
    int WriteIntegerAttribute( int iRecord, int iField, int nValue );
    int WriteDoubleAttribute( int iRecord, int iField, double dValue );
    int WriteStringAttribute( int iRecord, int iField, const char * pszValue );

    DBFHandle GetHandle();

    void Open( const char* pszFilename, const char *pszAccess );
    int GetFieldCount();
    int GetRecordCount();
    DBFFieldType GetFieldInfo( int iField, char* pszFieldName, int* pnWidth, int* pnDecimals );
    int	ReadIntegerAttribute( int iRecord, int iField );
    double ReadDoubleAttribute( int iRecord, int iField );
    const char* ReadStringAttribute( int iRecord, int iField );
    
  protected:
    DBFHandle m_handle;
    void *SfRealloc( void * pMem, int nNewSize );
    void DBFWriteHeader(DBFHandle psDBF);
    void DBFFlushRecord( DBFHandle psDBF );
    DBFHandle DBFOpen( const char * pszFilename, const char * pszAccess );
    void	DBFClose(DBFHandle psDBF);
    DBFHandle DBFCreate( const char * pszFilename );
    int	DBFAddField(DBFHandle psDBF, const char * pszFieldName, DBFFieldType eType, int nWidth, int nDecimals );
    void* DBFReadAttribute(DBFHandle psDBF, int hEntity, int iField );
    int	DBFReadIntegerAttribute( DBFHandle psDBF, int iRecord, int iField );
    double DBFReadDoubleAttribute( DBFHandle psDBF, int iRecord, int iField );
    const char *DBFReadStringAttribute( DBFHandle psDBF, int iRecord, int iField );
    int	DBFGetFieldCount( DBFHandle psDBF );
    int	DBFGetRecordCount( DBFHandle psDBF );
    DBFFieldType DBFGetFieldInfo( DBFHandle psDBF, int iField, char * pszFieldName, int * pnWidth, int * pnDecimals );
    int DBFWriteAttribute(DBFHandle psDBF, int hEntity, int iField, void *pValue );
    int DBFWriteDoubleAttribute( DBFHandle psDBF, int iRecord, int iField, double dValue );
    int DBFWriteIntegerAttribute( DBFHandle psDBF, int iRecord, int iField, int nValue );
    int DBFWriteStringAttribute( DBFHandle psDBF, int iRecord, int iField, const char * pszValue );
  }; // class DbfFile
}; // namespace BCE
#endif // !defined(AFX_BCEDBFFILE_H__90C5BE82_F817_11D4_BDD1_00104BB3E525__INCLUDED_)
