// ShapeFile.h: Schnittstelle für die Klasse ShapeFile.
//
//////////////////////////////////////////////////////////////////////
// Wrapper Klasse für das Schreiben von ESRI - Shape Dateien
//
///////////////////////////////////////////////////////////////////////

#if !defined(AFX_BCESHAPEFILE_H__9D6F03E0_F792_11D4_BDD0_00104BB3E525__INCLUDED_)
#define AFX_BCESHAPEFILE_H__9D6F03E0_F792_11D4_BDD0_00104BB3E525__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#define SHPT_POINT			1
#define SHPT_POLYLINE		3
#define SHPT_POLYGON		5
#define SHPT_MULTIPOINT		8
#define SHPT_POINTZ			11
#define SHPT_POLYLINEZ		13
#define SHPT_POLYGONZ		15
#define SHPT_MULTIPOINTZ	18
#define SHPT_POINTM			21
#define SHPT_POLYLINEM		23
#define SHPT_POLYGONM		25
#define SHPT_MULTIPOINTM	28
#define SHPT_MULTIPATCH		31

namespace BCE
{
  class ShapeFile  
  {
    typedef unsigned char uchar;
    
    typedef	struct SHPInfo
    {
      FILE    *fpSHP;
      FILE	*fpSHX;
      
      int		nShapeType;				/* SHPT_* */
      int		nFileSize;				/* SHP file */
      
      int     nRecords;
      int		nMaxRecords;
      int		*panRecOffset;
      int		*panRecSize;
      
      double	adBoundsMin[3];			/*xmin, ymin, zmin*/
      double	adBoundsMax[3];			/*xmax, ymax, zmax*/
      
      int		bUpdated;
    };
    
    typedef ShapeFile::SHPInfo* SHPHandle;
    
  public:
    ShapeFile();
    virtual ~ShapeFile();
    ShapeFile(const char *pszLayer, int nShapeType);
    ShapeFile(const char * pszLayer, const char * pszAccess);
    void Close();
    int WriteVertices(int nVCount, int nPartCount, int *panParts, double *padVertices);
    SHPHandle GetHandle() { return m_handle; };
    
  protected:
    SHPHandle m_handle;
    bool bBigEndian;
    
    void	SwapWord( int length, void * wordP );
    void *SfRealloc( void * pMem, int nNewSize );
    void SHPWriteHeaderz(SHPHandle psSHP);
    SHPHandle SHPOpenz(const char * pszLayer, const char * pszAccess);
    void	SHPClosez(SHPHandle psSHP );
    void SHPGetInfo(SHPHandle psSHP, int * pnEntities, int * pnShapeType );
    SHPHandle SHPCreatez( const char * pszLayer, int nShapeType );
    void	_SHPSetBoundsz(uchar *pabyRec, int nVCount, int nPartCount, double *padVertices, int dim);
    int SHPWriteVerticesz(SHPHandle psSHP, int nVCount, int nPartCount, int *panParts, double *padVertices);
    double *SHPReadVerticesz(SHPHandle psSHP, int hEntity, int *pnVCount, int *pnPartCount, int **ppanParts);
    void SHPReadBoundsz(SHPHandle psSHP, int hEntity, double * padBounds);
    
  }; // class ShapeFile
}; // namespace BCE

#endif // !defined(AFX_BCESHAPEFILE_H__9D6F03E0_F792_11D4_BDD0_00104BB3E525__INCLUDED_)
