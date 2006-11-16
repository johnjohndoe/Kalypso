package org.kalypsodeegree_impl.model.cv;

import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.model.geometry.GM_Envelope_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Class which holds the GridDomainData of a RectifiedGridCoverage
 * 
 * @author N. Peiler
 */
public class RectifiedGridDomain
{
  /**
   * lowerleft corner of the RectifiedGridCoverage
   */
  private GM_Point m_origin = null;

  /**
   * Array with offsetValue(size of one gridCell) for each dimension {offSetX,offsetY}
   */
  private double[] m_offset = null;

  private GridRange m_gridRange = null;

  private GM_Surface m_rasterBoundaryAsSurface = null;

  /**
   * constructs a RectifiedGridDomain with the given origin, offset and gridRange
   * 
   * @param origin
   * @param offset
   * @param gridRange
   */
  public RectifiedGridDomain( final GM_Point origin, final double[] offset, final GridRange gridRange )
  {
    m_origin = origin;
    m_offset = offset;
    m_gridRange = gridRange;
  }

  /**
   * @return Returns the origin.
   * @throws Exception
   */
  public GM_Point getOrigin( CS_CoordinateSystem cs ) throws Exception
  {
    if( cs == null || m_origin.getCoordinateSystem().equals( cs ) )
      return m_origin;

    GeoTransformer geoTrans = new GeoTransformer( cs );
    return (GM_Point) geoTrans.transform( m_origin );
  }

  /**
   * @param origin
   *          The origin to set.
   */
  public void setOrigin( GM_Point origin )
  {
    this.m_origin = origin;
  }

  /**
   * @return Returns the offset.
   */
  public double[] getOffset( )
  {
    return m_offset;
  }

  /**
   * @param offset
   *          The offset to set.
   */
  public void setOffset( double[] offset )
  {
    this.m_offset = offset;
  }

  /**
   * @return Returns the gridRange.
   */
  public GridRange getGridRange( )
  {
    return m_gridRange;
  }

  /**
   * @param gridRange
   *          The gridRange to set.
   */
  public void setGridRange( GridRange gridRange )
  {
    this.m_gridRange = gridRange;
  }

  /**
   * @return Returns the numColumns.
   */
  public int getNumColumns( )
  {
    double[] low = m_gridRange.getLow();
    double[] high = m_gridRange.getHigh();
    double numColumns = high[0] - low[0];
    return (new Double( numColumns )).intValue();
  }

  /**
   * @return Returns the numRows.
   */
  public int getNumRows( )
  {
    double[] low = m_gridRange.getLow();
    double[] high = m_gridRange.getHigh();
    double numRows = high[1] - low[1];
    return (new Double( numRows )).intValue();
  }

  /**
   * @return Returns the offset of a gridCell for the x-Axis
   * @throws Exception
   */
  public double getOffsetX( CS_CoordinateSystem cs ) throws Exception
  {
    if( cs == null || m_origin.getCoordinateSystem().equals( cs ) )
      return m_offset[0];

    GM_Envelope destEnv = getGM_Envelope( cs );
    return destEnv.getWidth() / getNumColumns();
  }

  /**
   * @return Returns the offset of a gridCell for the y-Axis
   */
  public double getOffsetY( CS_CoordinateSystem cs ) throws Exception
  {
    if( cs == null || m_origin.getCoordinateSystem().equals( cs ) )
      return m_offset[1];

    GM_Envelope destEnv = getGM_Envelope( cs );
    return destEnv.getHeight() / getNumRows();
  }

  /**
   * @return Returns the GM_Envelope of the RectifiedGridDomain
   * @throws Exception
   */
  public GM_Envelope getGM_Envelope( final CS_CoordinateSystem cs ) throws Exception
  {
    final double minX = m_origin.getX();
    final double minY = m_origin.getY();
    final double maxX = minX + (getNumColumns() * m_offset[0]);
    final double maxY = minY + (getNumRows() * m_offset[1]);

    final double eMinX = Math.min( minX, maxX );
    final double eMinY = Math.min( minY, maxY );
    final double eMaxX = Math.max( minX, maxX );
    final double eMaxY = Math.max( minY, maxY );
    
    final GM_Position min = GeometryFactory.createGM_Position( eMinX, eMinY );
    final GM_Position max = GeometryFactory.createGM_Position( eMaxX, eMaxY );
    
    final GM_Envelope envelope = new GM_Envelope_Impl( min, max );
    
    if( m_origin.getCoordinateSystem().equals( cs ) )
      return envelope;

    final GeoTransformer geoTrans = new GeoTransformer( cs );
    return geoTrans.transformEnvelope( envelope, m_origin.getCoordinateSystem() );
  }

  /**
   * get envelope in real coordinates for the given gridRange
   */
  public GM_Envelope getGM_Envelope( int lowX, int lowY, int highX, int highY, CS_CoordinateSystem cs ) throws Exception
  {
    double minX = m_origin.getX() + (lowX * m_offset[0]);
    double minY = m_origin.getY() + (lowY * m_offset[1]);
    GM_Position min = GeometryFactory.createGM_Position( minX, minY );
    double maxX = m_origin.getX() + (highX * m_offset[0]);
    double maxY = m_origin.getY() + (highY * m_offset[1]);
    GM_Position max = GeometryFactory.createGM_Position( maxX, maxY );
    GM_Envelope envelope = new GM_Envelope_Impl( min, max );

    if( m_origin.getCoordinateSystem().equals( cs ) )
      return envelope;

    GeoTransformer geoTrans = new GeoTransformer( cs );
    return geoTrans.transformEnvelope( envelope, m_origin.getCoordinateSystem() );
  }

  public GM_Surface getGM_Surface( CS_CoordinateSystem cs ) throws Exception
  {
    GM_Envelope orgEnvelope = getGM_Envelope( m_origin.getCoordinateSystem() );
    if( m_rasterBoundaryAsSurface == null )
    {
      m_rasterBoundaryAsSurface = GeometryFactory.createGM_Surface( orgEnvelope, m_origin.getCoordinateSystem() );
    }
    GM_Surface resultSurface = m_rasterBoundaryAsSurface;
    if( cs != null && !cs.equals( m_origin.getCoordinateSystem() ) )
    {
      GeoTransformer geoTrans = new GeoTransformer( cs );
      resultSurface = (GM_Surface) geoTrans.transform( m_rasterBoundaryAsSurface );
    }
    return resultSurface;
  }

  public GM_Surface getGM_Surface( int lowX, int lowY, int highX, int highY, CS_CoordinateSystem cs ) throws Exception
  {
    GM_Envelope orgEnvelope = getGM_Envelope( lowX, lowY, highX, highY, m_origin.getCoordinateSystem() );
    GM_Surface rasterBoundaryAsSurface = GeometryFactory.createGM_Surface( orgEnvelope, m_origin.getCoordinateSystem() );
    GeoTransformer geoTrans = new GeoTransformer( cs );
    return (GM_Surface) geoTrans.transform( rasterBoundaryAsSurface );
  }

  /**
   * get low and high (GridRange) of the RectifiedGridCoverage for the given envelope
   */
  public int[] getGridExtent( GM_Envelope env, CS_CoordinateSystem cs ) throws Exception
  {
    int lowX = (int) getGridRange().getLow()[0];
    int lowY = (int) getGridRange().getLow()[1];

    final GM_Envelope envelope = getGM_Envelope( cs );
    final GM_Position origin = envelope.getMin();
    
    if( (env.getMin().getX() - origin.getX()) > 0 )
    {
      lowX = (int) ((env.getMin().getX() - origin.getX()) / getOffsetX( cs ));
    }

    if( (env.getMin().getY() - origin.getY()) > 0 )
    {
      lowY = (int) ((env.getMin().getY() - origin.getY()) / getOffsetY( cs ));
    }

    int highX = (int) ((env.getMax().getX() - origin.getX()) / getOffsetX( cs ));
    if( highX > (int) getGridRange().getHigh()[0] )
    {
      highX = (int) getGridRange().getHigh()[0];
    }

    int highY = Math.abs(  (int) ((env.getMax().getY() - origin.getY()) / getOffsetY( cs )) );
    if( highY > (int) getGridRange().getHigh()[1] )
    {
      highY = (int) getGridRange().getHigh()[1];
    }

    int[] gridExtent = new int[] { lowX, lowY, highX, highY };

    return gridExtent;
  }
}