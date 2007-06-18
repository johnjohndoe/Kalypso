package org.kalypsodeegree_impl.model.cv;

import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.model.geometry.GM_SurfaceInterpolation_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Class which holds the GridDomainData of a RectifiedGridCoverage
 * 
 * @author N. Peiler
 */
public class RectifiedGridDomain
{
  public static final class OffsetVector
  {
    private final double m_geoX;

    private final double m_geoY;

    public OffsetVector( final double geoX, final double geoY )
    {
      m_geoX = geoX;
      m_geoY = geoY;
    }

    public GM_Position move( final GM_Position pos, final int number )
    {
      if( pos.getAsArray().length > 2 )
        return GeometryFactory.createGM_Position( pos.getX() + number * m_geoX, pos.getY() + number * m_geoY, pos.getZ() );
      else
        return GeometryFactory.createGM_Position( pos.getX() + number * m_geoX, pos.getY() + number * m_geoY );
    }

    public double getGeoX( )
    {
      return m_geoX;
    }

    public double getGeoY( )
    {
      return m_geoY;
    }

    /**
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString( )
    {
      return String.format( "GeoX: %f\tGeoY: %f", m_geoX, m_geoY );
    }
  }

  /**
   * lowerleft corner of the RectifiedGridCoverage
   */
  private final GM_Point m_origin;

  /** Offset vecotr for the raster x-direction. */
  private final OffsetVector m_offsetX;

  /** Offset vecotr for the raster y-direction. */
  private final OffsetVector m_offsetY;

  private final GridRange m_gridRange;

  private final GM_Surface m_rasterBoundaryAsSurface;

  /**
   * constructs a RectifiedGridDomain with the given origin, offset and gridRange
   * 
   * @param origin
   * @param offset
   * @param gridRange
   */
  public RectifiedGridDomain( final GM_Point origin, final OffsetVector offsetX, final OffsetVector offsetY, final GridRange gridRange ) throws Exception
  {
    m_origin = origin;
    m_offsetX = offsetX;
    m_offsetY = offsetY;
    m_gridRange = gridRange;
    m_rasterBoundaryAsSurface = getGM_Surface( origin.getCoordinateSystem() );
  }

  public GM_Surface getGM_Surface( final CS_CoordinateSystem crs ) throws Exception
  {
    return RectifiedGridDomain.calculateSurface( m_origin, m_offsetX, m_offsetY, 0, 0, getNumColumns(), getNumRows(), crs );
  }

  private static GM_Surface calculateSurface( final GM_Point origin, final OffsetVector offsetX, final OffsetVector offsetY, final int minX, final int minY, final int maxX, final int maxY, final CS_CoordinateSystem cs ) throws Exception
  {
    final GM_Position originPos = origin.getPosition();

    final GM_Position pos0 = offsetY.move( offsetX.move( originPos, minX ), minY );
    final GM_Position pos1 = offsetX.move( pos0, maxX - minX );
    final GM_Position pos2 = offsetY.move( pos1, maxY - minY );
    final GM_Position pos3 = offsetY.move( pos0, maxY - minY );
    final GM_Position[] ring = new GM_Position[] { pos0, pos1, pos2, pos3, pos0 };
    final GM_Surface surface = GeometryFactory.createGM_Surface( ring, null, new GM_SurfaceInterpolation_Impl(), origin.getCoordinateSystem() );

    if( origin.getCoordinateSystem().equals( cs ) )
      return surface;

    final GeoTransformer geoTrans = new GeoTransformer( cs );
    return (GM_Surface) geoTrans.transform( surface );
  }

  public CS_CoordinateSystem getCoordinateSystem( )
  {
    return m_origin.getCoordinateSystem();
  }

  public GM_Point getOrigin( final CS_CoordinateSystem cs ) throws Exception
  {
    if( (cs == null) || m_origin.getCoordinateSystem().equals( cs ) )
      return m_origin;

    final GeoTransformer geoTrans = new GeoTransformer( cs );
    return (GM_Point) geoTrans.transform( m_origin );
  }

  /**
   * @return Returns the gridRange.
   */
  public GridRange getGridRange( )
  {
    return m_gridRange;
  }

  /**
   * @return Returns the numColumns.
   */
  public int getNumColumns( )
  {
    final double[] low = m_gridRange.getLow();
    final double[] high = m_gridRange.getHigh();
    final double numColumns = high[0] - low[0];
    return (new Double( numColumns )).intValue();
  }

  /**
   * @return Returns the numRows.
   */
  public int getNumRows( )
  {
    final double[] low = m_gridRange.getLow();
    final double[] high = m_gridRange.getHigh();
    final double numRows = high[1] - low[1];
    return (new Double( numRows )).intValue();
  }

  /**
   * @return Returns the offset of a gridCell for the x-Axis
   * @throws Exception
   */
  private double getOffsetX( final CS_CoordinateSystem cs ) throws Exception
  {
    if( (cs == null) || m_origin.getCoordinateSystem().equals( cs ) )
      return m_offsetX.getGeoX();

    // ???
    final GM_Envelope destEnv = getGM_Envelope( cs );
    return destEnv.getWidth() / getNumColumns();
  }

  /**
   * @return Returns the offset of a gridCell for the y-Axis
   */
  private double getOffsetY( final CS_CoordinateSystem cs ) throws Exception
  {
    if( (cs == null) || m_origin.getCoordinateSystem().equals( cs ) )
      return m_offsetY.getGeoY();

    // ???
    final GM_Envelope destEnv = getGM_Envelope( cs );
    return destEnv.getHeight() / getNumRows();
  }

  /**
   * @return Returns the GM_Envelope of the RectifiedGridDomain
   * @throws Exception
   */
  public GM_Envelope getGM_Envelope( final CS_CoordinateSystem cs ) throws Exception
  {
    if( m_origin.getCoordinateSystem().equals( cs ) )
      return m_rasterBoundaryAsSurface.getEnvelope();

    final GeoTransformer geoTrans = new GeoTransformer( cs );
    return geoTrans.transform( m_rasterBoundaryAsSurface ).getEnvelope();
  }

  public GM_Surface getGM_Surface( final int lowX, final int lowY, final int highX, final int highY, final CS_CoordinateSystem cs ) throws Exception
  {
    return RectifiedGridDomain.calculateSurface( m_origin, m_offsetX, m_offsetY, lowX, lowY, highX, highY, cs );
  }

  /**
   * get low and high (GridRange) of the RectifiedGridCoverage for the given envelope
   */
  public int[] getGridExtent( final GM_Envelope env, final CS_CoordinateSystem cs ) throws Exception
  {
    /* Check precionditions: only cartesian offset vectors supported */
    // TODO: support arbitrary offset vectors
    if( m_offsetX.getGeoY() != 0.0 )
      System.out.println( "OffsetX-Vector is not cartesian!" );
    if( m_offsetY.getGeoX() != 0.0 )
      System.out.println( "OffsetY-Vector is not cartesian!" );

    int lowX = (int) getGridRange().getLow()[0];
    int lowY = (int) getGridRange().getLow()[1];

    final GM_Envelope envelope = getGM_Envelope( cs );
    final GM_Position origin = envelope.getMin();

    if( (env.getMin().getX() - origin.getX()) > 0 )
      lowX = (int) ((env.getMin().getX() - origin.getX()) / getOffsetX( cs ));

    if( (env.getMin().getY() - origin.getY()) > 0 )
      lowY = (int) ((env.getMin().getY() - origin.getY()) / getOffsetY( cs ));

    int highX = (int) ((env.getMax().getX() - origin.getX()) / getOffsetX( cs ));
    if( highX > (int) getGridRange().getHigh()[0] )
      highX = (int) getGridRange().getHigh()[0];

    int highY = Math.abs( (int) ((env.getMax().getY() - origin.getY()) / getOffsetY( cs )) );
    if( highY > (int) getGridRange().getHigh()[1] )
      highY = (int) getGridRange().getHigh()[1];

    final int[] gridExtent = new int[] { lowX, lowY, highX, highY };

    return gridExtent;
  }

  public OffsetVector getOffsetX( )
  {
    return m_offsetX;
  }

  public OffsetVector getOffsetY( )
  {
    return m_offsetY;
  }

  /**
   * Calculates the position of a raster cell.
   */
  public GM_Position getPositionAt( final int x, final int y )
  {
    return m_offsetY.move( m_offsetX.move( m_origin.getPosition(), x ), y );
  }
}