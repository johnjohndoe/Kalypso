// DbfFile.cpp: Implementierung der Klasse DbfFile.
//
//////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <string.h>

#include "DbfFile.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

using namespace BCE;

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

DbfFile::DbfFile()
{
  m_handle = NULL;
}

DbfFile::DbfFile(const char *pszFilename)
{
  m_handle = DBFCreate( pszFilename );
}

DbfFile::~DbfFile()
{
  if( m_handle )
    free( m_handle );
}


// Wrapper Funktionen

void DbfFile::Close()
{
  DBFClose(m_handle);
  m_handle = NULL;
};

DbfFile::DBFHandle DbfFile::GetHandle()
{
  DBFHandle handle = m_handle;
  return handle;
 };

int DbfFile::AddField(const char *pszFieldName, DBFFieldType eType, int nWidth, int nDecimals)
{
  return DBFAddField(m_handle, pszFieldName, eType, nWidth, nDecimals);
};

int DbfFile::WriteAttribute(int hEntity, int iField, void *pValue)
{
  return DBFWriteAttribute(m_handle, hEntity, iField, pValue);
};

int DbfFile::WriteIntegerAttribute( int iRecord, int iField, int nValue )
{
  return DBFWriteIntegerAttribute( m_handle, iRecord, iField, nValue );
};

int DbfFile::WriteDoubleAttribute( int iRecord, int iField, double dValue )
{
  return DBFWriteDoubleAttribute( m_handle, iRecord, iField, dValue );
}
    
int DbfFile::WriteStringAttribute( int iRecord, int iField, const char * pszValue )
{
  return DBFWriteStringAttribute( m_handle, iRecord, iField, pszValue );
}


// READING

void DbfFile::Open( const char* pszFilename, const char *pszAccess )
{
  m_handle = DBFOpen( pszFilename, pszAccess );
}

int DbfFile::GetFieldCount()
{
  return DBFGetFieldCount( m_handle );
}

int DbfFile::GetRecordCount()
{
  return DBFGetRecordCount( m_handle );
}

DbfFile::DBFFieldType DbfFile::GetFieldInfo( int iField, char* pszFieldName, int* pnWidth, int* pnDecimals )
{
  return DBFGetFieldInfo( m_handle, iField, pszFieldName, pnWidth, pnDecimals );
}

int	DbfFile::ReadIntegerAttribute( int iRecord, int iField )
{
  return DBFReadIntegerAttribute( m_handle, iRecord, iField );
}

double DbfFile::ReadDoubleAttribute( int iRecord, int iField )
{
  return DBFReadDoubleAttribute( m_handle, iRecord, iField );
}

const char* DbfFile::ReadStringAttribute( int iRecord, int iField )
{
  return DBFReadStringAttribute( m_handle, iRecord, iField );
}

// Original Funktionen (irgendwo aus dem Netz)

/************************************************************************
INHALTSVERZEICHNIS

DBFWriteHeader()
DBFFlushRecord()
DBFOpen()
DBFClose()
DBFCreate()
DBFAddField()
DBFReadAttribute()
DBFReadIntAttribute()
DBFReadDoubleAttribute()
DBFReadStringAttribute()
DBFGetFieldCount()
DBFGetRecordCount()
DBFGetFieldInfo()
DBFWriteAttribute()
DBFWriteDoubleAttribute()
DBFWriteIntegerAttribute()
DBFWriteStringAttribute()
************************************************************************/

/************************************************************************/
/*                             SfRealloc()                              */
/*                                                                      */
/*      A realloc cover function that will access a NULL pointer as     */
/*      a valid input.                                                  */
/************************************************************************/

void *DbfFile::SfRealloc( void * pMem, int nNewSize )
{
    if( pMem == NULL )
        return( (void *) malloc(nNewSize) );
    else
        return( (void *) realloc(pMem,nNewSize) );
}

/************************************************************************/
/*                           DBFWriteHeader()                           */
/*                                                                      */
/*      This is called to write out the file header, and field          */
/*      descriptions before writing any actual data records.  This      */
/*      also computes all the DBFDataSet field offset/size/decimals     */
/*      and so forth values.                                            */
/************************************************************************/

void DbfFile::DBFWriteHeader(DBFHandle psDBF)
{
    uchar	abyHeader[32];
    int		i;

    if( !psDBF->bNoHeader )
        return;

    psDBF->bNoHeader = false;

/* -------------------------------------------------------------------- */
/*	Initialize the file header information.				*/
/* -------------------------------------------------------------------- */
    for( i = 0; i < 32; i++ )
        abyHeader[i] = 0;

    abyHeader[0] = 0x03;		/* memo field? - just copying 	*/

    /* date updated on close, record count preset at zero */

    abyHeader[8] = psDBF->nHeaderLength % 256;
    abyHeader[9] = psDBF->nHeaderLength / 256;
    
    abyHeader[10] = psDBF->nRecordLength % 256;
    abyHeader[11] = psDBF->nRecordLength / 256;

/* -------------------------------------------------------------------- */
/*      Write the initial 32 byte file header, and all the field        */
/*      descriptions.                                     		*/
/* -------------------------------------------------------------------- */
    fseek( psDBF->fp, 0, 0 );
    fwrite( abyHeader, 32, 1, psDBF->fp );
    fwrite( psDBF->pszHeader, 32, psDBF->nFields, psDBF->fp );

/* -------------------------------------------------------------------- */
/*      Write out the newline character if there is room for it.        */
/* -------------------------------------------------------------------- */
    if( psDBF->nHeaderLength > 32*psDBF->nFields + 32 )
    {
        char	cNewline;

        cNewline = 0x0d;
        fwrite( &cNewline, 1, 1, psDBF->fp );
    }
}

/************************************************************************/
/*                           DBFFlushRecord()                           */
/*                                                                      */
/*      Write out the current record if there is one.                   */
/************************************************************************/

void DbfFile::DBFFlushRecord( DBFHandle psDBF )

{
    int		nRecordOffset;

    if( psDBF->bCurrentRecordModified && psDBF->nCurrentRecord > -1 )
    {
	psDBF->bCurrentRecordModified = false;

	nRecordOffset = psDBF->nRecordLength * psDBF->nCurrentRecord 
	                                             + psDBF->nHeaderLength;

	fseek( psDBF->fp, nRecordOffset, 0 );
	fwrite( psDBF->pszCurrentRecord, psDBF->nRecordLength, 1, psDBF->fp );
    }
}

/************************************************************************/
/*                              DBFOpen()                               */
/*                                                                      */
/*      Open a .dbf file.                                               */
/************************************************************************/
   
DbfFile::DBFHandle DbfFile::DBFOpen( const char * pszFilename, const char * pszAccess )
{
    DBFHandle		psDBF;
    uchar		*pabyBuf;
    int			nFields, nRecords, nHeadLen, nRecLen, iField;

/* -------------------------------------------------------------------- */
/*      We only allow the access strings "rb" and "r+".                  */
/* -------------------------------------------------------------------- */
    if( strcmp(pszAccess,"r") != 0 && strcmp(pszAccess,"r+") != 0 
        && strcmp(pszAccess,"rb") != 0 && strcmp(pszAccess,"rb+") != 0 )
        return( NULL );
    
    psDBF = (DBFHandle) calloc( 1, sizeof(DBFInfo) );
    psDBF->fp = fopen( pszFilename, pszAccess );
    if( psDBF->fp == NULL )
        return( NULL );

    psDBF->bNoHeader = false;
    psDBF->nCurrentRecord = -1;
    psDBF->bCurrentRecordModified = false;

/* -------------------------------------------------------------------- */
/*  Read Table Header info                                              */
/* -------------------------------------------------------------------- */
    pabyBuf = (uchar *) malloc(500);
    fread( pabyBuf, 32, 1, psDBF->fp );

    psDBF->nRecords = nRecords = 
     pabyBuf[4] + pabyBuf[5]*256 + pabyBuf[6]*256*256 + pabyBuf[7]*256*256*256;

    psDBF->nHeaderLength = nHeadLen = pabyBuf[8] + pabyBuf[9]*256;
    psDBF->nRecordLength = nRecLen = pabyBuf[10] + pabyBuf[11]*256;
    
    psDBF->nFields = nFields = (nHeadLen - 32) / 32;

    psDBF->pszCurrentRecord = (char *) malloc(nRecLen);

/* -------------------------------------------------------------------- */
/*  Read in Field Definitions                                           */
/* -------------------------------------------------------------------- */
    
    pabyBuf = (uchar *) SfRealloc(pabyBuf,nHeadLen);
    psDBF->pszHeader = (char *) pabyBuf;

    fseek( psDBF->fp, 32, 0 );
    fread( pabyBuf, nHeadLen, 1, psDBF->fp );

    psDBF->panFieldOffset = (int *) malloc(sizeof(int) * nFields);
    psDBF->panFieldSize = (int *) malloc(sizeof(int) * nFields);
    psDBF->panFieldDecimals = (int *) malloc(sizeof(int) * nFields);
    psDBF->pachFieldType = (char *) malloc(sizeof(char) * nFields);

    for( iField = 0; iField < nFields; iField++ )
    {
	uchar		*pabyFInfo;

	pabyFInfo = pabyBuf+iField*32;

	if( pabyFInfo[11] == 'N' )
	{
	    psDBF->panFieldSize[iField] = pabyFInfo[16];
	    psDBF->panFieldDecimals[iField] = pabyFInfo[17];
	}
	else
	{
	    psDBF->panFieldSize[iField] = pabyFInfo[16] + pabyFInfo[17]*256;
	    psDBF->panFieldDecimals[iField] = 0;
	}

	psDBF->pachFieldType[iField] = (char) pabyFInfo[11];
	if( iField == 0 )
	    psDBF->panFieldOffset[iField] = 1;
	else
	    psDBF->panFieldOffset[iField] = 
	      psDBF->panFieldOffset[iField-1] + psDBF->panFieldSize[iField-1];
    }

    return( psDBF );
}

/************************************************************************/
/*                              DBFClose()                              */
/************************************************************************/

void	DbfFile::DBFClose(DBFHandle psDBF)
{
/* -------------------------------------------------------------------- */
/*      Write out header if not already written.                        */
/* -------------------------------------------------------------------- */
    if( psDBF->bNoHeader )
        DBFWriteHeader( psDBF );

    DBFFlushRecord( psDBF );

/* -------------------------------------------------------------------- */
/*      Update last access date, and number of records if we have	*/
/*	write access.                					*/
/* -------------------------------------------------------------------- */
    if( psDBF->bUpdated )
    {
	uchar		abyFileHeader[32];

	fseek( psDBF->fp, 0, 0 );
	fread( abyFileHeader, 32, 1, psDBF->fp );

	abyFileHeader[1] = 95;			/* YY */
	abyFileHeader[2] = 7;			/* MM */
	abyFileHeader[3] = 26;			/* DD */

	abyFileHeader[4] = psDBF->nRecords % 256;
	abyFileHeader[5] = (psDBF->nRecords/256) % 256;
	abyFileHeader[6] = (psDBF->nRecords/(256*256)) % 256;
	abyFileHeader[7] = (psDBF->nRecords/(256*256*256)) % 256;

	fseek( psDBF->fp, 0, 0 );
	fwrite( abyFileHeader, 32, 1, psDBF->fp );
    }

/* -------------------------------------------------------------------- */
/*      Close, and free resources.                                      */
/* -------------------------------------------------------------------- */
    fclose( psDBF->fp );

    if( psDBF->panFieldOffset != NULL )
    {
        free( psDBF->panFieldOffset );
        free( psDBF->panFieldSize );
        free( psDBF->panFieldDecimals );
        free( psDBF->pachFieldType );
    }

    free( psDBF->pszHeader );
    free( psDBF->pszCurrentRecord );

    free( psDBF );
}

/************************************************************************/
/*                             DBFCreate()                              */
/*                                                                      */
/*      Create a new .dbf file.                                         */
/************************************************************************/

DbfFile::DBFHandle DbfFile::DBFCreate( const char * pszFilename )
{
    DBFHandle	psDBF;
    FILE	*fp;

/* -------------------------------------------------------------------- */
/*      Create the file.                                                */
/* -------------------------------------------------------------------- */
    fp = fopen( pszFilename, "wb" );
    if( fp == NULL )
        return( NULL );

    fputc( 0, fp );
    fclose( fp );

    fp = fopen( pszFilename, "rb+" );
    if( fp == NULL )
        return( NULL );

/* -------------------------------------------------------------------- */
/*	Create the info structure.					*/
/* -------------------------------------------------------------------- */
    psDBF = (DBFHandle) malloc(sizeof(DBFInfo));

    psDBF->fp = fp;
    psDBF->nRecords = 0;
    psDBF->nFields = 0;
    psDBF->nRecordLength = 1;
    psDBF->nHeaderLength = 33;
    
    psDBF->panFieldOffset = NULL;
    psDBF->panFieldSize = NULL;
    psDBF->panFieldDecimals = NULL;
    psDBF->pachFieldType = NULL;
    psDBF->pszHeader = NULL;

    psDBF->nCurrentRecord = -1;
    psDBF->bCurrentRecordModified = false;
    psDBF->pszCurrentRecord = NULL;

    psDBF->bNoHeader = true;

    return( psDBF );
}

/************************************************************************/
/*                            DBFAddField()                             */
/*                                                                      */
/*      Add a field to a newly created .dbf file before any records     */
/*      are written.                                                    */
/************************************************************************/

int	DbfFile::DBFAddField(DBFHandle psDBF, const char * pszFieldName, 
                            DBFFieldType eType, int nWidth, int nDecimals )
{
    char	*pszFInfo;
    int		i;

/* -------------------------------------------------------------------- */
/*      Do some checking to ensure we can add records to this file.     */
/* -------------------------------------------------------------------- */
    if( psDBF->nRecords > 0 )
        return( false );

    if( !psDBF->bNoHeader )
        return( false );

    if( eType != FTDouble && nDecimals != 0 )
        return( false );

/* -------------------------------------------------------------------- */
/*      SfRealloc all the arrays larger to hold the additional field      */
/*      information.                                                    */
/* -------------------------------------------------------------------- */
    psDBF->nFields++;

    psDBF->panFieldOffset = (int *) 
      SfRealloc( psDBF->panFieldOffset, sizeof(int) * psDBF->nFields );

    psDBF->panFieldSize = (int *) 
      SfRealloc( psDBF->panFieldSize, sizeof(int) * psDBF->nFields );

    psDBF->panFieldDecimals = (int *) 
      SfRealloc( psDBF->panFieldDecimals, sizeof(int) * psDBF->nFields );

    psDBF->pachFieldType = (char *) 
      SfRealloc( psDBF->pachFieldType, sizeof(char) * psDBF->nFields );

/* -------------------------------------------------------------------- */
/*      Assign the new field information fields.                        */
/* -------------------------------------------------------------------- */
    psDBF->panFieldOffset[psDBF->nFields-1] = psDBF->nRecordLength;
    psDBF->nRecordLength += nWidth;
    psDBF->panFieldSize[psDBF->nFields-1] = nWidth;
    psDBF->panFieldDecimals[psDBF->nFields-1] = nDecimals;

    if( eType == FTString )
        psDBF->pachFieldType[psDBF->nFields-1] = 'C';
    else
        psDBF->pachFieldType[psDBF->nFields-1] = 'N';

/* -------------------------------------------------------------------- */
/*      Extend the required header information.                         */
/* -------------------------------------------------------------------- */
    psDBF->nHeaderLength += 32;
    psDBF->bUpdated = false;

    psDBF->pszHeader = (char *) SfRealloc(psDBF->pszHeader,psDBF->nFields*32);

    pszFInfo = psDBF->pszHeader + 32 * (psDBF->nFields-1);

    for( i = 0; i < 32; i++ )
        pszFInfo[i] = '\0';

    if( strlen(pszFieldName) < 10 )
        strncpy( pszFInfo, pszFieldName, strlen(pszFieldName));
    else
        strncpy( pszFInfo, pszFieldName, 10);

    pszFInfo[11] = psDBF->pachFieldType[psDBF->nFields-1];

    if( eType == FTString )
    {
        pszFInfo[16] = nWidth % 256;
        pszFInfo[17] = nWidth / 256;
    }
    else
    {
        pszFInfo[16] = nWidth;
        pszFInfo[17] = nDecimals;
    }
    
/* -------------------------------------------------------------------- */
/*      Make the current record buffer appropriately larger.            */
/* -------------------------------------------------------------------- */
    psDBF->pszCurrentRecord = (char *) SfRealloc(psDBF->pszCurrentRecord,
					       psDBF->nRecordLength);

    return( true );
}

/************************************************************************/
/*                          DBFReadAttribute()                          */
/*                                                                      */
/*      Read one of the attribute fields of a record.                   */
/************************************************************************/

void *DbfFile::DBFReadAttribute(DBFHandle psDBF, int hEntity, int iField )
{
    int	    nRecordOffset;
    uchar	*pabyRec;
    void	*pReturnField = NULL;

    static double dDoubleField;
    static char * pszStringField = NULL;
    static int	nStringFieldLen = 0;

/* -------------------------------------------------------------------- */
/*	Have we read the record?					*/
/* -------------------------------------------------------------------- */
    if( hEntity < 0 || hEntity >= psDBF->nRecords )
        return( NULL );

    if( psDBF->nCurrentRecord != hEntity )
    {
	DBFFlushRecord( psDBF );

	nRecordOffset = psDBF->nRecordLength * hEntity + psDBF->nHeaderLength;

	fseek( psDBF->fp, nRecordOffset, 0 );
	fread( psDBF->pszCurrentRecord, psDBF->nRecordLength, 1, psDBF->fp );

	psDBF->nCurrentRecord = hEntity;
    }

    pabyRec = (unsigned char *) psDBF->pszCurrentRecord;

/* -------------------------------------------------------------------- */
/*	Ensure our field buffer is large enough to hold this buffer.	    */
/* -------------------------------------------------------------------- */
    if( psDBF->panFieldSize[iField]+1 > nStringFieldLen )
    {
	nStringFieldLen = psDBF->panFieldSize[iField]*2 + 10;
	pszStringField = (char *) SfRealloc(pszStringField,nStringFieldLen);
    }

/* -------------------------------------------------------------------- */
/*	Extract the requested field.					                    */
/* -------------------------------------------------------------------- */
//	char *strncpy( char *strDest, const char *strSource, size_t count );


    strncpy( pszStringField, (const char *)pabyRec+psDBF->panFieldOffset[iField],
	     psDBF->panFieldSize[iField] );
    pszStringField[psDBF->panFieldSize[iField]] = '\0';

    pReturnField = pszStringField;

/* -------------------------------------------------------------------- */
/*      Decode the field.                                               */
/* -------------------------------------------------------------------- */
    if( psDBF->pachFieldType[iField] == 'N'
        || psDBF->pachFieldType[iField] == 'D' )
    {
	if( psDBF->panFieldDecimals[iField] == 0 )
	{
	    dDoubleField = atoi(pszStringField);
	}
	else
	{
	    sscanf( pszStringField, "%lf", &dDoubleField );
	}

	pReturnField = &dDoubleField;
    }
    
    return( pReturnField );
}

/************************************************************************/
/*                        DBFReadIntAttribute()                         */
/*                                                                      */
/*      Read an integer attribute.                                      */
/************************************************************************/

int	DbfFile::DBFReadIntegerAttribute( DBFHandle psDBF, int iRecord, int iField )

{
    double	*pdValue;

    pdValue = (double *) DBFReadAttribute( psDBF, iRecord, iField );

    return( (int) *pdValue );
}

/************************************************************************/
/*                        DBFReadDoubleAttribute()                      */
/*                                                                      */
/*      Read a double attribute.                                        */
/************************************************************************/

double DbfFile::DBFReadDoubleAttribute( DBFHandle psDBF, int iRecord, int iField )

{
    double	*pdValue;

    pdValue = (double *) DBFReadAttribute( psDBF, iRecord, iField );

    return( *pdValue );
}

/************************************************************************/
/*                        DBFReadStringAttribute()                      */
/*                                                                      */
/*      Read a string attribute.                                        */
/************************************************************************/

const char *DbfFile::DBFReadStringAttribute( DBFHandle psDBF, int iRecord, int iField )

{
    return( (const char *) DBFReadAttribute( psDBF, iRecord, iField ) );
}

/************************************************************************/
/*                          DBFGetFieldCount()                          */
/*                                                                      */
/*      Return the number of fields in this table.                      */
/************************************************************************/

int	DbfFile::DBFGetFieldCount( DBFHandle psDBF )
{
    return( psDBF->nFields );
}

/************************************************************************/
/*                         DBFGetRecordCount()                          */
/*                                                                      */
/*      Return the number of records in this table.                     */
/************************************************************************/

int	DbfFile::DBFGetRecordCount( DBFHandle psDBF )
{
    return( psDBF->nRecords );
}

/************************************************************************/
/*                          DBFGetFieldInfo()                           */
/*                                                                      */
/*      Return any requested information about the field.               */
/************************************************************************/

DbfFile::DBFFieldType DbfFile::DBFGetFieldInfo( DBFHandle psDBF, int iField, char * pszFieldName,
                                         int * pnWidth, int * pnDecimals )
{
    if( iField < 0 || iField >= psDBF->nFields )
        return( FTInvalid );

    if( pnWidth != NULL )
        *pnWidth = psDBF->panFieldSize[iField];

    if( pnDecimals != NULL )
        *pnDecimals = psDBF->panFieldDecimals[iField];

    if( pszFieldName != NULL )
    {
	int	i;

	strncpy( pszFieldName, (char *) psDBF->pszHeader+iField*32, 11 );
	pszFieldName[11] = '\0';
	for( i = 10; i > 0 && pszFieldName[i] == ' '; i-- )
	    pszFieldName[i] = '\0';
    }

    if( psDBF->pachFieldType[iField] == 'N' 
        || psDBF->pachFieldType[iField] == 'D' )
    {
	if( psDBF->panFieldDecimals[iField] > 0 )
	    return( FTDouble );
	else
	    return( FTInteger );
    }
    else
    {
	return( FTString );
    }
}

/************************************************************************/
/*                         DBFWriteAttribute()                          */
/*									*/
/*	Write an attribute record to the file.				*/
/************************************************************************/

int DbfFile::DBFWriteAttribute(DBFHandle psDBF, int hEntity, int iField, void * pValue )
{
    int	       	nRecordOffset, i, j;
    uchar	*pabyRec;
    char	szSField[40], szFormat[12];

/* -------------------------------------------------------------------- */
/*	Is this a valid record?						*/
/* -------------------------------------------------------------------- */
    if( hEntity < 0 || hEntity > psDBF->nRecords )
        return( false );

    if( psDBF->bNoHeader )
        DBFWriteHeader(psDBF);

/* -------------------------------------------------------------------- */
/*      Is this a brand new record?                                     */
/* -------------------------------------------------------------------- */
    if( hEntity == psDBF->nRecords )
    {
	DBFFlushRecord( psDBF );

	psDBF->nRecords++;
	for( i = 0; i < psDBF->nRecordLength; i++ )
	    psDBF->pszCurrentRecord[i] = ' ';

	psDBF->nCurrentRecord = hEntity;
    }

/* -------------------------------------------------------------------- */
/*      Is this an existing record, but different than the last one     */
/*      we accessed?                                                    */
/* -------------------------------------------------------------------- */
    if( psDBF->nCurrentRecord != hEntity )
    {
	DBFFlushRecord( psDBF );

	nRecordOffset = psDBF->nRecordLength * hEntity + psDBF->nHeaderLength;

	fseek( psDBF->fp, nRecordOffset, 0 );
	fread( psDBF->pszCurrentRecord, psDBF->nRecordLength, 1, psDBF->fp );

	psDBF->nCurrentRecord = hEntity;
    }

    pabyRec = (uchar *) psDBF->pszCurrentRecord;

/* -------------------------------------------------------------------- */
/*      Assign all the record fields.                                   */
/* -------------------------------------------------------------------- */
    switch( psDBF->pachFieldType[iField] )
    {
      case 'D':
      case 'N':
	if( psDBF->panFieldDecimals[iField] == 0 )
	{
	    sprintf( szFormat, "%%%dd", psDBF->panFieldSize[iField] );
	    sprintf(szSField, szFormat, (int) *((double *) pValue) );
	    if( (int)strlen(szSField) > psDBF->panFieldSize[iField] )
	        szSField[psDBF->panFieldSize[iField]] = '\0';
	    strncpy((char *) (pabyRec+psDBF->panFieldOffset[iField]),
		    szSField, strlen(szSField) );
	}
	else
	{
	    sprintf( szFormat, "%%%d.%df", 
		     psDBF->panFieldSize[iField],
		     psDBF->panFieldDecimals[iField] );
	    sprintf(szSField, szFormat, *((double *) pValue) );
	    if( (int)strlen(szSField) > psDBF->panFieldSize[iField] )
	        szSField[psDBF->panFieldSize[iField]] = '\0';
	    strncpy((char *) (pabyRec+psDBF->panFieldOffset[iField]),
		    szSField, strlen(szSField) );
	}
	break;

      default:
	if( (int)strlen((char *) pValue) > psDBF->panFieldSize[iField] )
	    j = psDBF->panFieldSize[iField];
	else
	    j = strlen((char *) pValue);

	strncpy((char *) (pabyRec+psDBF->panFieldOffset[iField]),
		(char *) pValue, j );
	break;
    }

    psDBF->bCurrentRecordModified = true;
    psDBF->bUpdated = true;

    return( true );
}

/************************************************************************/
/*                      DBFWriteDoubleAttribute()                       */
/*                                                                      */
/*      Write a double attribute.                                       */
/************************************************************************/

int DbfFile::DBFWriteDoubleAttribute( DBFHandle psDBF, int iRecord, int iField, double dValue )
{
    return( DBFWriteAttribute( psDBF, iRecord, iField, (void *) &dValue ) );
}

/************************************************************************/
/*                      DBFWriteIntegerAttribute()                      */
/*                                                                      */
/*      Write a integer attribute.                                      */
/************************************************************************/

int DbfFile::DBFWriteIntegerAttribute( DBFHandle psDBF, int iRecord, int iField,
                                         int nValue )
{
    double	dValue = nValue;

    return( DBFWriteAttribute( psDBF, iRecord, iField, (void *) &dValue ) );
}

/************************************************************************/
/*                      DBFWriteStringAttribute()                       */
/*                                                                      */
/*      Write a string attribute.                                       */
/************************************************************************/


int DbfFile::DBFWriteStringAttribute( DBFHandle psDBF, int iRecord, int iField,
                                        const char * pszValue )
{
    return( DBFWriteAttribute( psDBF, iRecord, iField, (void *) pszValue ) );
}

