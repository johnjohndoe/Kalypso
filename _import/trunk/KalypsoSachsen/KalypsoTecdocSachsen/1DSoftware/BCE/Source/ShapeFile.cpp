// ShapeFile.cpp: Implementierung der Klasse ShapeFile.
//
//////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <malloc.h>
#include <memory.h>
#include <string.h>
#include <minmax.h>

#include "ShapeFile.h"

using namespace BCE;

#if UINT_MAX == 65535
typedef long int32;
#else
typedef int	 int32;
#endif

#define ByteCopy(a, b, c)	memcpy(b, a, c)


//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

ShapeFile::ShapeFile()
{
  m_handle = NULL;
  bBigEndian = false;
}

ShapeFile::ShapeFile( const char* pszLayer, int nShapeType )
{
  m_handle = SHPCreatez(pszLayer, nShapeType);
  bBigEndian = false;
};

ShapeFile::ShapeFile( const char* pszLayer, const char* pszAccess)
{
  m_handle = SHPOpenz(pszLayer, pszAccess);
  bBigEndian = false;
};

ShapeFile::~ShapeFile()
{
  if (m_handle != NULL)
    SHPClosez(m_handle);
}


// Wrapper Funktionen

void ShapeFile::Close()
{
  SHPClosez(m_handle);
  m_handle = NULL;
};

int ShapeFile::WriteVertices(int nVCount, int nPartCount, int *panParts, double *padVertices)
{
  return ShapeFile::SHPWriteVerticesz(m_handle, nVCount, nPartCount, panParts, padVertices);
};

// Originale Funktionen (irgendwoher aus dem Netz)

/**************************** I N H A L T ********************************
SwapWord
SfRealloc
SHPWriteHeaderz
SHPOpenz
SHPClosez
SHPGetInfo
SHPCreatez
_SHPSetBoundsz
SHPWriteVerticesz
SHPReadVerticesz
SHPReadBoundsz
*************************************************************************/

/************************************************************************/
/*                              SwapWord()                              */
/*                                                                      */
/*      Swap a 2, 4 or 8 byte word.                                     */
/************************************************************************/

void	ShapeFile::SwapWord( int length, void * wordP )
{
  int		i;
  uchar	temp;
  
  for( i=0; i < length/2; i++ )
  {
    temp = ((uchar *) wordP)[i];
    ((uchar *)wordP)[i] = ((uchar *) wordP)[length-i-1];
    ((uchar *)wordP)[length-i-1] = temp;
  }
}

/************************************************************************/
/*                             SfRealloc()                              */
/*                                                                      */
/*      A realloc cover function that will access a NULL pointer as     */
/*      a valid input.                                                  */
/************************************************************************/

void *ShapeFile::SfRealloc( void * pMem, int nNewSize )
{
  if( pMem == NULL ) return( (void *) malloc(nNewSize) );
  else return( (void *) realloc(pMem,nNewSize) );
}

/************************************************************************/
/*                          SHPWriteHeaderz()                            */
/*                                                                      */
/*      Write out a header for the .shp and .shx files as well as the	*/
/*	contents of the index (.shx) file.									*/
/************************************************************************/

void ShapeFile::SHPWriteHeaderz(SHPHandle psSHP)
{
  uchar   abyHeader[100];
  int		i, k, dim=psSHP->nShapeType/10+2;//Dimension der Punktkoordinaten
  int32	i32, *panSHX;
  double	dValue;
  
  /*******Prepare header block for .shp file.*******/
  for( i = 0; i < 100; i++ ) abyHeader[i] = 0;
  
  abyHeader[2] = 0x27;				/* magic cookie */
  abyHeader[3] = 0x0a;
  
  i32 = psSHP->nFileSize/2;				/* file size */
  ByteCopy( &i32, abyHeader+24, 4 );
  if( !bBigEndian ) SwapWord( 4, abyHeader+24 );
  
  i32 = 1000;						/* version */
  ByteCopy( &i32, abyHeader+28, 4 );
  if( bBigEndian ) SwapWord( 4, abyHeader+28 );
  
  i32 = psSHP->nShapeType;				/* shape type */
  ByteCopy( &i32, abyHeader+32, 4 );
  if( bBigEndian ) SwapWord( 4, abyHeader+32 );
  
  dValue = psSHP->adBoundsMin[0];			/* set bounds */
  ByteCopy( &dValue, abyHeader+36, 8 );
  dValue = psSHP->adBoundsMin[1];
  ByteCopy( &dValue, abyHeader+44, 8 );
  dValue = psSHP->adBoundsMax[0];
  ByteCopy( &dValue, abyHeader+52, 8 );
  dValue = psSHP->adBoundsMax[1];
  ByteCopy( &dValue, abyHeader+60, 8 );
  if(dim==3)
		{
    dValue = psSHP->adBoundsMin[2];
    ByteCopy( &dValue, abyHeader+68, 8 );
    dValue = psSHP->adBoundsMax[2];
    ByteCopy( &dValue, abyHeader+76, 8 );
		}
  if(bBigEndian)
    for(k=0; k<2*dim; k++) SwapWord(8, abyHeader+36+k*8);
    
    /*******Write .shp file header.*******/
    fseek( psSHP->fpSHP, 0, 0 );
    fwrite( abyHeader, 100, 1, psSHP->fpSHP );
    
    /*******Prepare, and write .shx file header.*******/
    i32 = (psSHP->nRecords * 2 * sizeof(int32) + 100)/2;   /* file size */
    ByteCopy( &i32, abyHeader+24, 4 );
    if( !bBigEndian ) SwapWord( 4, abyHeader+24 );
    
    fseek( psSHP->fpSHX, 0, 0 );
    fwrite( abyHeader, 100, 1, psSHP->fpSHX );
    
    /*******Write out the .shx contents.*******/
    panSHX = (int32 *) malloc(sizeof(int32) * 2 * psSHP->nRecords);
    for( i = 0; i < psSHP->nRecords; i++ )
    {
      panSHX[i*2  ] = psSHP->panRecOffset[i]/2;
      panSHX[i*2+1] = psSHP->panRecSize[i]/2;
      if( !bBigEndian ) SwapWord( 4, panSHX+i*2 );
      if( !bBigEndian ) SwapWord( 4, panSHX+i*2+1 );
    }
    fwrite( panSHX, sizeof(int32) * 2, psSHP->nRecords, psSHP->fpSHX );
    free( panSHX );
}

/************************************************************************/
/*                              SHPOpenz()                               */
/*                                                                      */
/*      Open the .shp and .shx files based on the basename of the       */
/*      files or either file name.                                      */
/************************************************************************/

ShapeFile::SHPHandle ShapeFile::SHPOpenz(const char * pszLayer, const char * pszAccess)
{
  char		*pszFullname, *pszBasename;
  SHPHandle	psSHP;
  uchar		*pabyBuf;
  int			i, k, dim;
  double		dValue;
  
  /*******Ensure the access string is one of the legal ones.*******/
  if(strcmp(pszAccess,"r")!=0 && strcmp(pszAccess,"r+")!=0 &&
    strcmp(pszAccess,"rb")!=0 && strcmp(pszAccess,"rb+")!=0) return(NULL);
  
  /*******Establish the byte order on this machine.*******/
  i = 1;
  if( *((uchar *) &i) == 1) bBigEndian = false;
  else bBigEndian = true;
  
  /*******Initialize the info structure.*******/
  psSHP = (SHPHandle) malloc(sizeof(SHPInfo));
  psSHP->bUpdated = false;
  
  /*******Compute the base (layer) name. If there is any extension
  on the passed in filename we will strip it off.	*******/
  pszBasename = (char *) malloc(strlen(pszLayer)+5);
  strcpy( pszBasename, pszLayer );
  for(i = strlen(pszBasename)-1; i > 0 && pszBasename[i]!='.' && pszBasename[i]!='/' && pszBasename[i]!='\\'; i-- )
  {}
  
  if(pszBasename[i]=='.')
    pszBasename[i]='\0';
  
    /*******Open the .shp and .shx files. Note that files pulled from
  a PC to Unix with upper case filenames won't work!*******/
  pszFullname = (char *) malloc(strlen(pszBasename) + 5);
  sprintf( pszFullname, "%s.shp", pszBasename );
  psSHP->fpSHP = fopen(pszFullname, pszAccess );
  if( psSHP->fpSHP == NULL ) 
  {
    free( pszBasename );
    free( pszFullname );
    return( NULL );
  };
  sprintf( pszFullname, "%s.shx", pszBasename );
  psSHP->fpSHX = fopen(pszFullname, pszAccess );
  if( psSHP->fpSHX == NULL ) 
  {
    free( pszBasename );
    free( pszFullname );
    return( NULL );
  };
  free( pszBasename );
  free( pszFullname );
  
  /*******Read the file size from the SHP file.*******/
  pabyBuf = (uchar *) malloc(100);
  fread( pabyBuf, 100, 1, psSHP->fpSHP );
  psSHP->nFileSize=(pabyBuf[24]*256*256*256+pabyBuf[25]*256*256+pabyBuf[26]*256+pabyBuf[27])*2;
  
  /*******Read SHX file Header info*******/
  fread( pabyBuf, 100, 1, psSHP->fpSHX );
  if(pabyBuf[0]!=0 || pabyBuf[1]!=0 || pabyBuf[2]!=0x27 || (pabyBuf[3]!=0x0a && pabyBuf[3]!=0x0d))
  {
    fclose( psSHP->fpSHP );
    fclose( psSHP->fpSHX );
    free( psSHP );
    return( NULL );
  }
  psSHP->nRecords=pabyBuf[27]+pabyBuf[26]*256+pabyBuf[25]*256*256+pabyBuf[24]*256*256*256;
  psSHP->nRecords=(psSHP->nRecords*2-100)/8;
  psSHP->nShapeType=pabyBuf[32];
  dim=psSHP->nShapeType/10+2;//Dimension der Punktkoordinaten
  if(bBigEndian)
  {
    for(k=0; k<2*dim; k++) SwapWord(8, pabyBuf+36+k*8);
  }
  memcpy( &dValue, pabyBuf+36, 8 );
  psSHP->adBoundsMin[0] = dValue;
  memcpy( &dValue, pabyBuf+44, 8 );
  psSHP->adBoundsMin[1] = dValue;
  memcpy( &dValue, pabyBuf+52, 8 );
  psSHP->adBoundsMax[0] = dValue;
  memcpy( &dValue, pabyBuf+60, 8 );
  psSHP->adBoundsMax[1] = dValue;
  if(dim==3)
  {
    memcpy( &dValue, pabyBuf+68, 8 );
    psSHP->adBoundsMin[2] = dValue;
    memcpy( &dValue, pabyBuf+76, 8 );
    psSHP->adBoundsMax[2] = dValue;
  }
  free( pabyBuf );
  
  /*******Read the .shx file to get the offsets to each record in	the .shp file.*******/
  psSHP->nMaxRecords = psSHP->nRecords;
  psSHP->panRecOffset = (int *) malloc(sizeof(int) * psSHP->nMaxRecords );
  psSHP->panRecSize = (int *) malloc(sizeof(int) * psSHP->nMaxRecords );
  pabyBuf = (uchar *) malloc(8 * psSHP->nRecords );
  fread( pabyBuf, 8, psSHP->nRecords, psSHP->fpSHX );
  for( i = 0; i < psSHP->nRecords; i++ )
  {
    int32		nOffset, nLength;
    
    memcpy( &nOffset, pabyBuf + i * 8, 4 );
    if( !bBigEndian ) SwapWord( 4, &nOffset );
    memcpy( &nLength, pabyBuf + i * 8 + 4, 4 );
    if( !bBigEndian ) SwapWord( 4, &nLength );
    psSHP->panRecOffset[i] = nOffset*2;
    psSHP->panRecSize[i] = nLength*2;
  }
  free( pabyBuf );
  return( psSHP );
}

/************************************************************************/
/*                              SHPClosez()                             */
/*																       	*/
/*	Close the .shp and .shx files.										*/
/************************************************************************/

void	ShapeFile::SHPClosez(SHPHandle psSHP )
{
  /*******Update the header if we have modified anything.*******/
  if( psSHP->bUpdated ) SHPWriteHeaderz( psSHP );
  
  /*******Free all resources, and close files.*******/
  free( psSHP->panRecOffset );
  free( psSHP->panRecSize );
  fclose( psSHP->fpSHX );
  fclose( psSHP->fpSHP );
  free( psSHP );
}

/************************************************************************/
/*                             SHPGetInfo()                             */
/*                                                                      */
/*      Fetch general information about the shape file.                 */
/************************************************************************/

void ShapeFile::SHPGetInfo(SHPHandle psSHP, int * pnEntities, int * pnShapeType )
{
  if( pnEntities != NULL ) *pnEntities = psSHP->nRecords;
  if( pnShapeType != NULL ) *pnShapeType = psSHP->nShapeType;
}

/************************************************************************/
/*                             SHPCreatez()                              */
/*                                                                      */
/*      Create a new shape file and return a handle to the open         */
/*      shape file with read/write access.                              */
/************************************************************************/

ShapeFile::SHPHandle ShapeFile::SHPCreatez( const char * pszLayer, int nShapeType )
{
  char	*pszBasename, *pszFullname;
  int		i, k, dim=nShapeType/10+2;//Dimension der Punktkoordinaten
  FILE	*fpSHP, *fpSHX;
  uchar   abyHeader[100];
  int32	i32;
  double	dValue;
  
  if(dim>3) return NULL;
  
  /*******Establish the byte order on this system.*******/
  i = 1;
  if( *((uchar *) &i) == 1 ) bBigEndian = false;
  else bBigEndian = true;
  
  /*******Compute the base (layer) name.  If there is any extension
  on the passed in filename we will strip it off.*******/
  pszBasename = (char *) malloc(strlen(pszLayer)+5);
  strcpy( pszBasename, pszLayer );
  for(i=strlen(pszBasename)-1;
  i>0 && pszBasename[i]!='.' && pszBasename[i]!='/' && pszBasename[i]!='\\';
  i--) {}
  if( pszBasename[i] == '.' ) pszBasename[i] = '\0';
  
  /*******Open the two files so we can write their headers.*******/
  pszFullname = (char *) malloc(strlen(pszBasename) + 5);
  sprintf( pszFullname, "%s.shp", pszBasename );
  fpSHP = fopen(pszFullname, "wb" );
  if( fpSHP == NULL )
  {
    free(pszBasename);
    free(pszFullname);
    return( NULL );
  };
  sprintf( pszFullname, "%s.shx", pszBasename );
  fpSHX = fopen(pszFullname, "wb" );
  if( fpSHX == NULL ) 
  {
    free(pszBasename);
    free(pszFullname);
    return( NULL );
  };
  free( pszBasename );
  free( pszFullname );
  
  /*******Prepare header block for .shp file.*******/
  for(i=0; i<100; i++) abyHeader[i]=0;
  
  abyHeader[2] = 0x27;				/* magic cookie */
  abyHeader[3] = 0x0a;
  
  i32 = 50;						/* file size */
  ByteCopy( &i32, abyHeader+24, 4 );
  if( !bBigEndian ) SwapWord( 4, abyHeader+24 );
  
  i32 = 1000;						/* version */
  ByteCopy( &i32, abyHeader+28, 4 );
  if( bBigEndian ) SwapWord( 4, abyHeader+28 );
  
  i32 = nShapeType;					/* shape type */
  ByteCopy( &i32, abyHeader+32, 4 );
  if( bBigEndian ) SwapWord( 4, abyHeader+32 );
  
  dValue = 0.0;					/* set bounds */
  for(k=0; k<2*dim; k++) ByteCopy(&dValue, abyHeader+36+8*k, 8);
  
  /*******Write .shp file header.*******/
  fwrite( abyHeader, 100, 1, fpSHP );
  
  /*******Prepare, and write .shx file header.*******/
  i32 = 50;						/* file size */
  ByteCopy( &i32, abyHeader+24, 4 );
  if( !bBigEndian ) SwapWord( 4, abyHeader+24 );
  fwrite( abyHeader, 100, 1, fpSHX );
  
  /*******Close the files, and then open them as regular existing files.*******/
  fclose( fpSHP );
  fclose( fpSHX );
  return( SHPOpenz( pszLayer, "rb+" ) );
}

/************************************************************************/
/*                           _SHPSetBoundsz()                            */
/*                                                                      */
/*      Compute a bounds rectangle for a shape, and set it into the     */
/*      indicated location in the record.                               */
/************************************************************************/

void	ShapeFile::_SHPSetBoundsz(uchar *pabyRec, int nVCount, int nPartCount, double *padVertices, int dim)
{
  double	dMin[3], dMax[3];
  int		i, k;
  
  if(nVCount==0)
    for(k=0; k<dim; k++) dMin[k]=dMax[k]=0.;
    else
    {
      for(k=0; k<dim; k++) dMin[k]=dMax[k]=padVertices[k];
      for(i=1; i<nVCount;i++)
        for(k=0; k<dim; k++)
        {
          dMin[k]=min(dMin[k], padVertices[i*dim+k]);
          dMax[k]=max(dMax[k], padVertices[i*dim+k]);
        }
    }
    if(bBigEndian)
      for(k=0; k<dim; k++) 
      {
        SwapWord(8, dMin+k);
        SwapWord(8, dMax+k);
      }
      ByteCopy(dMin+0, pabyRec+ 4, 8);
      ByteCopy(dMin+1, pabyRec+12, 8);
      ByteCopy(dMax+0, pabyRec+20, 8);
      ByteCopy(dMax+1, pabyRec+28, 8);
      if(dim==3)
      {
        ByteCopy(dMin+2, pabyRec+44+4*nPartCount+16*nVCount, 8);
        ByteCopy(dMax+2, pabyRec+52+4*nPartCount+16*nVCount, 8);
      }
}

/************************************************************************/
/*                          SHPWriteVerticesz()                         */
/*																		*/
/*      Write out the vertices of a new structure.  Note that it is     */
/*      only possible to write vertices at the end of the file.         */
/************************************************************************/

int ShapeFile::SHPWriteVerticesz(SHPHandle psSHP, int nVCount, int nPartCount, int *panParts, double *padVertices)
{
  int	    nRecordOffset, i, k, nRecordSize, dim=psSHP->nShapeType/10+2;//Dimension der Punktkoordinaten
  uchar	*pabyRec, *aktupos, *datsatzanf;
  int32	i32;
  double nix=0.;
  
  psSHP->bUpdated = true;
  
  /*******Add the new entity to the in memory index.*******/
  psSHP->nRecords++;
  if(psSHP->nRecords>psSHP->nMaxRecords)
  {
    psSHP->nMaxRecords =(int) (psSHP->nMaxRecords * 1.3 + 100);
    psSHP->panRecOffset=(int *) SfRealloc(psSHP->panRecOffset,sizeof(int)*psSHP->nMaxRecords);
    psSHP->panRecSize=(int *) SfRealloc(psSHP->panRecSize,sizeof(int)*psSHP->nMaxRecords);
  }
  
  /*******Initialize record.*******/
  psSHP->panRecOffset[psSHP->nRecords-1] = nRecordOffset = psSHP->nFileSize;
  
  pabyRec=(uchar *)malloc(nVCount*dim*sizeof(double)+nPartCount*4+128);/*Position am Anfang des DatensatzKOPFes*/
  datsatzanf=pabyRec+8;/*datsatzanf=Position zu Beginn des DatensatzINHALTs (8=Länge des DatensatzKOPFes)*/
  
  /*******Extract vertices for a Polygon(Z) or PolyLine(Z).*******/
  if(psSHP->nShapeType%10==3 || psSHP->nShapeType%10==5)
  {
    int32		nPoints, nParts;
    int    		i;
    
    nPoints = nVCount;
    nParts = nPartCount;
    _SHPSetBoundsz(datsatzanf, nVCount, nPartCount, padVertices, dim);
    if(bBigEndian) SwapWord(4,&nPoints);
    if(bBigEndian) SwapWord(4,&nParts);
    ByteCopy(&nPoints, datsatzanf+40, 4);
    ByteCopy(&nParts, datsatzanf+36, 4);
    ByteCopy(panParts, datsatzanf+44, 4*nPartCount);
    for(i=0; i<nPartCount; i++)
      if(bBigEndian) SwapWord(4, datsatzanf+44+4*i);
      aktupos=datsatzanf+44+4*nPartCount;/*Position zu Beginn der x-y-Koordinaten der Punkte*/
      for(i=0; i<nVCount; i++)
      {
        ByteCopy(padVertices+i*dim,   aktupos+i*16, 8);				/*x-Koordinaten*/
        ByteCopy(padVertices+i*dim+1, aktupos+i*16+8, 8);			/*y-Koordinaten*/
        if(dim==3) ByteCopy(padVertices+i*3+2, aktupos+16*nVCount+16+i*8, 8);/*z-Koordinaten*/
        if(bBigEndian)
        {
          SwapWord(8, aktupos+i*16);
          SwapWord(8, aktupos+i*16+8);
          if(dim==3) SwapWord(8, aktupos+16*nVCount+16+i*8);
        }
      }
      nRecordSize=60+4*nPartCount+dim*8*nVCount;
  }
  
  /*******Extract vertices for a MultiPoint(Z).*******/
  else if(psSHP->nShapeType%10==8)
  {
    int32		nPoints;
    int    		i;
    
    nPoints = nVCount;
    _SHPSetBoundsz(datsatzanf, nVCount, nPartCount, padVertices, dim);
    if(bBigEndian) SwapWord(4, &nPoints);
    ByteCopy(&nPoints, pabyRec+44, 4);
    aktupos=datsatzanf+40;/*Position zu Beginn der x-y-Koordinaten der Punkte*/
    for(i=0; i<nVCount; i++)
    {
      ByteCopy(padVertices+i*dim,   aktupos+i*16,   8);			/*x-Koordinaten*/
      ByteCopy(padVertices+i*dim+1, aktupos+i*16+8, 8);			/*y-Koordinaten*/
      if(dim==3) ByteCopy(padVertices+i*3+2, aktupos+16*nVCount+16+i*8, 8);/*z-Koordinaten*/
      if(bBigEndian)
      {
        SwapWord(8, aktupos+i*16);
        SwapWord(8, aktupos+i*16+8);
        if(dim==3) SwapWord(8, aktupos+16*nVCount+16+i*8);
      }
    }
    nRecordSize=40+dim*8*nVCount;
  }
  
  /*******Extract vertices for a point(Z).*******/
  else if(psSHP->nShapeType%10==1)
  {
    for(k=0; k<dim; k++)/*Für dim==3 hier trotzdem 4 statt dim, falls M-Koordinate obligatorisch?*/
    {
      ByteCopy(padVertices+k, datsatzanf+4+k*8, 8);
      if(bBigEndian) SwapWord(8, datsatzanf+4+k*8);
    }
    nRecordSize=dim==2?20:36;/*oder 28, falls ohne M-Koordinate???*/
  }
  
  /*******Set the shape type, record number, and record size.*******/
  i32 = psSHP->nRecords-1+1;					/* record # */
  if( !bBigEndian ) SwapWord( 4, &i32 );
  ByteCopy( &i32, pabyRec, 4 );
  
  i32 = nRecordSize/2;				/* record size */
  if( !bBigEndian ) SwapWord( 4, &i32 );
  ByteCopy( &i32, pabyRec + 4, 4 );
  
  i32 = psSHP->nShapeType;				/* shape type */
  if( bBigEndian ) SwapWord( 4, &i32 );
  ByteCopy( &i32, pabyRec + 8, 4 );
  
  /*******Write out record.*******/
  fseek( psSHP->fpSHP, nRecordOffset, 0 );
  fwrite( pabyRec, nRecordSize+8, 1, psSHP->fpSHP );
  free( pabyRec );
  psSHP->panRecSize[psSHP->nRecords-1] = nRecordSize;
  psSHP->nFileSize += nRecordSize + 8;
  
  /*******Expand file wide bounds based on this shape.*******/
  if( psSHP->nRecords == 1 )
  {
    for(k=0; k<dim; k++) 
      psSHP->adBoundsMin[k]=psSHP->adBoundsMax[k]=padVertices[k];
  };
  for(i=0; i<nVCount; i++)
  {
    for(k=0; k<dim; k++)
    {
      psSHP->adBoundsMin[k] = min(psSHP->adBoundsMin[k],padVertices[i*dim+k]);
      psSHP->adBoundsMax[k] = max(psSHP->adBoundsMax[k],padVertices[i*dim+k]);
    }
  };
  return psSHP->nRecords-1;
}

/************************************************************************/
/*                          SHPReadVerticesz()					        */
/*																		*/
/*      Read the vertices for one shape from the shape file.            */
/************************************************************************/

double *ShapeFile::SHPReadVerticesz(SHPHandle psSHP, int hEntity, int *pnVCount, int *pnPartCount, int **ppanParts)
{
  static uchar	*pabyRec = NULL;
  uchar			*datsatzanf, *aktupos;
  static double 	*padVertices = NULL;
  static int		nVertMax = 0, nPartMax = 0, *panParts = NULL, nBufSize = 0;
  int k, dim=psSHP->nShapeType/10+2;//Dimension der Punktkoordinaten
  
  if(dim>3) return NULL;//M-Typen können nicht verarbeitet werden (müßte noch angepaßt werden)
  if(hEntity<0 || hEntity>=psSHP->nRecords) return(NULL);/*Validate the record/entity number.*/
  if(psSHP->panRecSize[hEntity]+8 > nBufSize)/*Ensure our record buffer is large enough.*/
  {
    nBufSize = psSHP->panRecSize[hEntity]+8;
    pabyRec = (uchar *) SfRealloc(pabyRec,nBufSize);
  }
  datsatzanf=pabyRec+8;/*datsatzanf=Position zu Beginn des DatensatzINHALTs (8=Länge des DatensatzKOPFes)*/
  
  /*******Read the record.*******/
  fseek(psSHP->fpSHP, psSHP->panRecOffset[hEntity], 0);
  fread(pabyRec, psSHP->panRecSize[hEntity]+8, 1, psSHP->fpSHP);
  if( pnPartCount != NULL ) *pnPartCount = 0;
  if( ppanParts != NULL ) *ppanParts = NULL;
  *pnVCount = 0;
  
  /*******Extract vertices for a Polygon(Z) or PolyLine(Z).*******/
  if(psSHP->nShapeType%10==3 || psSHP->nShapeType%10==5)
  {
    int32		nPoints, nParts;
    int    		i;
    
    memcpy(&nPoints, datsatzanf+40, 4);
    memcpy(&nParts, datsatzanf+36, 4);
    if(bBigEndian) SwapWord(4, &nPoints);
    if(bBigEndian) SwapWord(4, &nParts);
    *pnVCount = nPoints;
    *pnPartCount = nParts;
    
    /*******Copy out the part array from the record.*******/
    if(nPartMax<nParts)
    {
      nPartMax=nParts;
      panParts=(int *)SfRealloc(panParts, nPartMax*sizeof(int));
    }
    memcpy(panParts, datsatzanf+44, 4*nParts);
    for(i=0; i<nParts; i++)
    {
      if(bBigEndian) SwapWord(4, panParts+i);
    };
    
    /*******Copy out the vertices from the record.*******/
    if(nVertMax<nPoints)
    {
      nVertMax=nPoints;
      padVertices=(double *)SfRealloc(padVertices, nVertMax*dim*sizeof(double));
    }
    aktupos=datsatzanf+44+4*nParts;/*Position zu Beginn der x-y-Koordinaten der Punkte*/
    for(i=0; i<nPoints; i++)
    {
      memcpy(padVertices+i*dim,   aktupos+i*16, 8);				/*x-Koordinaten*/
      memcpy(padVertices+i*dim+1, aktupos+i*16+8, 8);			/*y-Koordinaten*/
      if(dim==3) memcpy(padVertices+i*3+2, aktupos+16*nPoints+16+i*8, 8);/*z-Koordinaten*/
      if(bBigEndian)
        for(k=0; k<dim; k++) SwapWord(8, padVertices+i*dim+k);
    }
  }
  
  /*******Extract vertices for a MultiPoint(Z).*******/
  else if(psSHP->nShapeType%10==8)
  {
    int32		nPoints;
    int    		i;
    
    memcpy(&nPoints, pabyRec+44, 4);
    if(bBigEndian) SwapWord(4, &nPoints);
    *pnVCount = nPoints;
    if(nVertMax<nPoints)
    {
      nVertMax=nPoints;
      padVertices=(double *)SfRealloc(padVertices, nVertMax*dim*sizeof(double));
    }
    aktupos=datsatzanf+40;/*Position zu Beginn der x-y-Koordinaten der Punkte*/
    for(i=0; i<nPoints; i++)
    {
      memcpy(padVertices+i*dim,   aktupos+i*16, 8);				/*x-Koordinaten*/
      memcpy(padVertices+i*dim+1, aktupos+i*16+8, 8);				/*y-Koordinaten*/
      if(dim==3) memcpy(padVertices+i*dim+2, aktupos+16*nPoints+16+i*8, 8);/*z-Koordinaten*/
      if(bBigEndian)
        for(k=0; k<dim; k++) SwapWord(8, padVertices+i*dim+k);
    }
  }
  
  /*******Extract vertices for a point(Z).*******/
  else if(psSHP->nShapeType%10==1)
  {
    *pnVCount=1;
    if(nVertMax<1)
    {
      nVertMax=1;
      padVertices=(double *)SfRealloc(padVertices, nVertMax*dim*sizeof(double));/*4 statt dim, falls mit M*/
    }
    for(k=0; k<dim; k++)/*4 statt dim, falls mit M*/
    {
      memcpy(padVertices+k,   datsatzanf+4+k*8, 8);
      if(bBigEndian) SwapWord(8, padVertices+k);
    }
  }
  
  *ppanParts = panParts;
  return(padVertices);
}
  
/************************************************************************/
/*                           SHPReadBoundsz()							*/
/*                                                                      */
/*      Read the bounds for one shape, or for the whole shapefile.      */
/************************************************************************/

void ShapeFile::SHPReadBoundsz(SHPHandle psSHP, int hEntity, double * padBounds)
{
  int k, dim=psSHP->nShapeType/10+2;//Dimension der Punktkoordinaten
  long offset;
  
  /*******Validate the record/entity number.*******/
  if(hEntity<-1 || hEntity>=psSHP->nRecords)
  {
    for(k=0; k<2*dim; k++) padBounds[k]=0.;
    return;
  }
  
  /*******If the entity is -1 we fetch the bounds for the whole file.*******/
  if(hEntity==-1)
  {
    for(k=0; k<dim; k++)
    {
      padBounds[k]    =psSHP->adBoundsMin[k];
      padBounds[dim+k]=psSHP->adBoundsMax[k];
    }
  } 
  /*******Extract bounds for any record but a point record.*******/
  else if(psSHP->nShapeType%10!=1)
  {
    offset=psSHP->panRecOffset[hEntity]+8;/*Beginn des Datensatzinhalts*/
    fseek(psSHP->fpSHP, offset+4, 0);
    if(dim==2) fread(padBounds, sizeof(double)*4, 1, psSHP->fpSHP);/*xmin, ymin, xmax, ymax*/
    if(dim==3)
    {
      fread(padBounds, sizeof(double)*2, 1, psSHP->fpSHP);/*xmin, ymin*/
      fread(padBounds+3, sizeof(double)*2, 1, psSHP->fpSHP);/*xmax, ymax*/
      if(psSHP->nShapeType==SHPT_POLYLINEZ || psSHP->nShapeType==SHPT_POLYGONZ)
      {
        int32 nPartCount, nVCount;
        fseek(psSHP->fpSHP, offset+36, 0);
        fread(&nPartCount, 4, 1, psSHP->fpSHP);
        fread(&nVCount, 4, 1, psSHP->fpSHP);
        if(bBigEndian) SwapWord(4, &nPartCount);
        if(bBigEndian) SwapWord(4, &nVCount);
        fseek(psSHP->fpSHP, offset+44+4*nPartCount+16*nVCount, 0);
      }
      if(psSHP->nShapeType==SHPT_MULTIPOINTZ)
      {
        int32 nVCount;
        fseek(psSHP->fpSHP, offset+36, 0);
        fread(&nVCount, 4, 1, psSHP->fpSHP);
        if(bBigEndian) SwapWord(4, &nVCount);
        fseek(psSHP->fpSHP, offset+40+16*nVCount, 0);
      }
      fread(padBounds+2, sizeof(double), 1, psSHP->fpSHP);/*zmin*/
      fread(padBounds+5, sizeof(double), 1, psSHP->fpSHP);/*zmax*/
    }
    if(bBigEndian)
      for(k=0; k<2*dim; k++) SwapWord(8, padBounds+k);
  }
  
  /*******For points we fetch the point, and duplicate it as the minimum and maximum bound.*******/
  else
  {
    fseek(psSHP->fpSHP, psSHP->panRecOffset[hEntity]+12, 0);
    fread(padBounds, sizeof(double)*dim, 1, psSHP->fpSHP);
    if(bBigEndian)
      for(k=0; k<dim; k++) SwapWord(8, padBounds+k);
      /*{
      SwapWord(8, padBounds  );
      SwapWord(8, padBounds+1);
  }*/
      memcpy(padBounds+dim, padBounds, dim*sizeof(double));
  }
}
