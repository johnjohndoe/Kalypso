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
 *  
 */
public class RectifiedGridDomain //implements GM_Object

{

  /**
   * lowerleft corner of the RectifiedGridCoverage
   */
  private GM_Point origin = null;

  /**
   * Array with offsetValue(size of one gridCell) for each dimension
   * {offSetX,offsetY}
   */
  private double[] offset = null;

  private GridRange gridRange = null;

  private GM_Surface m_rasterBoundaryAsSurface = null;

  /**
   * constructs a RectifiedGridDomain with the given origin, offset and
   * gridRange
   * 
   * @param origin
   * @param offset
   * @param gridRange
   */
  public RectifiedGridDomain( GM_Point origin, double[] offset, GridRange gridRange )
  {
    this.origin = origin;
    this.offset = offset;
    this.gridRange = gridRange;
  }

  /**
   * @return Returns the origin.
   * @throws Exception
   */
  public GM_Point getOrigin( CS_CoordinateSystem cs ) throws Exception
  {
    if( cs == null || origin.getCoordinateSystem().equals( cs ) )
      return origin;

    GeoTransformer geoTrans = new GeoTransformer( cs );
    return (GM_Point)geoTrans.transform( origin );
  }

  /**
   * @param origin
   *          The origin to set.
   */
  public void setOrigin( GM_Point origin )
  {
    this.origin = origin;
  }

  /**
   * @return Returns the offset.
   */
  public double[] getOffset()
  {
    return offset;
  }

  /**
   * @param offset
   *          The offset to set.
   */
  public void setOffset( double[] offset )
  {
    this.offset = offset;
  }

  /**
   * @return Returns the gridRange.
   */
  public GridRange getGridRange()
  {
    return gridRange;
  }

  /**
   * @param gridRange
   *          The gridRange to set.
   */
  public void setGridRange( GridRange gridRange )
  {
    this.gridRange = gridRange;
  }

  /**
   * @return Returns the numColumns.
   */
  public int getNumColumns()
  {
    double[] low = gridRange.getLow();
    double[] high = gridRange.getHigh();
    double numColumns = high[0] - low[0];
    return ( new Double( numColumns ) ).intValue();
  }

  /**
   * @return Returns the numRows.
   */
  public int getNumRows()
  {
    double[] low = gridRange.getLow();
    double[] high = gridRange.getHigh();
    double numRows = high[1] - low[1];
    return ( new Double( numRows ) ).intValue();
  }

  /**
   * 
   * @return Returns the offset of a gridCell for the x-Axis
   * @throws Exception
   */
  public double getOffsetX( CS_CoordinateSystem cs ) throws Exception
  {
    if( origin.getCoordinateSystem().equals( cs ) )
      return offset[0];

    GM_Envelope destEnv = getGM_Envelope( cs );
    return destEnv.getWidth() / getNumColumns();
  }

  /**
   * 
   * @return Returns the offset of a gridCell for the y-Axis
   */
  public double getOffsetY( CS_CoordinateSystem cs ) throws Exception
  {
    if( origin.getCoordinateSystem().equals( cs ) )
      return offset[1];

    GM_Envelope destEnv = getGM_Envelope( cs );
    return destEnv.getHeight() / getNumRows();
  }

  /**
   * 
   * @return Returns the GM_Envelope of the RectifiedGridDomain
   * @throws Exception
   */
  public GM_Envelope getGM_Envelope( CS_CoordinateSystem cs ) throws Exception
  {

    GM_Position min = GeometryFactory.createGM_Position( origin.getX(), origin.getY() );
    double maxX = origin.getX() + ( getNumColumns() * offset[0] );
    double maxY = origin.getY() + ( getNumRows() * offset[1] );
    GM_Position max = GeometryFactory.createGM_Position( maxX, maxY );
    GM_Envelope envelope = new GM_Envelope_Impl( min, max );

    if( origin.getCoordinateSystem().equals( cs ) )
      return envelope;

    GeoTransformer geoTrans = new GeoTransformer( cs );
    return geoTrans.transformEnvelope( envelope, origin.getCoordinateSystem() );
  }

  public GM_Envelope getGM_Envelope( int lowX, int lowY, int highX, int highY,
      CS_CoordinateSystem cs ) throws Exception
  {

    double minX = origin.getX() + ( lowX * offset[0] );
    double minY = origin.getY() + ( lowY * offset[1] );
    GM_Position min = GeometryFactory.createGM_Position( minX, minY );
    double maxX = origin.getX() + ( highX * offset[0] );
    double maxY = origin.getY() + ( highY * offset[1] );
    GM_Position max = GeometryFactory.createGM_Position( maxX, maxY );
    GM_Envelope envelope = new GM_Envelope_Impl( min, max );

    if( origin.getCoordinateSystem().equals( cs ) )
      return envelope;

    GeoTransformer geoTrans = new GeoTransformer( cs );
    return geoTrans.transformEnvelope( envelope, origin.getCoordinateSystem() );
  }

  public GM_Surface getGM_Surface( CS_CoordinateSystem cs ) throws Exception
  {
    GM_Envelope orgEnvelope = getGM_Envelope( origin.getCoordinateSystem() );
    if( m_rasterBoundaryAsSurface == null )
    {
      m_rasterBoundaryAsSurface = GeometryFactory.createGM_Surface( orgEnvelope, origin
          .getCoordinateSystem() );
    }
    GeoTransformer geoTrans = new GeoTransformer( cs );
    return (GM_Surface)geoTrans.transform( m_rasterBoundaryAsSurface );
  }

  public GM_Surface getGM_Surface( int lowX, int lowY, int highX, int highY, CS_CoordinateSystem cs )
      throws Exception
  {
    GM_Envelope orgEnvelope = getGM_Envelope( lowX, lowY, highX, highY, origin
        .getCoordinateSystem() );
    GM_Surface rasterBoundaryAsSurface = GeometryFactory.createGM_Surface( orgEnvelope, origin
          .getCoordinateSystem() );
    GeoTransformer geoTrans = new GeoTransformer( cs );
    return (GM_Surface)geoTrans.transform( rasterBoundaryAsSurface );
  }

  public int[] getGridExtent( GM_Envelope env, CS_CoordinateSystem cs ) throws Exception
  {
    int lowX = (int)getGridRange().getLow()[0];
    int lowY = (int)getGridRange().getLow()[1];

    if( ( env.getMin().getX() - getOrigin( cs ).getX() ) > 0 )
    {
      lowX = (int)( ( env.getMin().getX() - getOrigin( cs ).getX() ) / getOffsetX( cs ) );
    }

    if( ( env.getMin().getY() - getOrigin( cs ).getY() ) > 0 )
    {
      lowY = (int)( ( env.getMin().getY() - getOrigin( cs ).getY() ) / getOffsetY( cs ) );
    }

    int highX = (int)( ( env.getMax().getX() - getOrigin( cs ).getX() ) / getOffsetX( cs ) );
    if( highX > (int)getGridRange().getHigh()[0] )
    {
      highX = (int)getGridRange().getHigh()[0];
    }

    int highY = (int)( ( env.getMax().getY() - getOrigin( cs ).getY() ) / getOffsetY( cs ) );
    if( highY > (int)getGridRange().getHigh()[1] )
    {
      highY = (int)getGridRange().getHigh()[1];
    }
    
    int[] gridExtent = new int[]
    {
        lowX,
        lowY,
        highX,
        highY };
    
    return gridExtent;
  }
}