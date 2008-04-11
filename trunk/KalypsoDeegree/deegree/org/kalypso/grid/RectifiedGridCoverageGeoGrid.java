package org.kalypso.grid;

import java.io.IOException;
import java.math.BigDecimal;
import java.net.URL;

import javax.xml.namespace.QName;

import ogc31.www.opengis.net.gml.FileType;
import ogc31.www.opengis.net.gml.RangeSetType;

import org.kalypso.commons.xml.NS;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * {@link IGeoGrid} implementation based on {@link org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage}s.<br>
 * This implementation analyzes the wrapped coverage and generates a suitable grid, to which all calls are delegated.
 * 
 * @author Gernot Belger
 */
public class RectifiedGridCoverageGeoGrid implements IGeoGrid
{
  private final int m_sizeX;

  private final int m_sizeY;

  private final Coordinate m_origin;

  private final Coordinate m_offsetX;

  private final Coordinate m_offsetY;

  private String m_sourceCRS;

  private final RangeSetType m_rangeSet;

  private final URL m_context;

  private IGeoGrid m_grid;

  public RectifiedGridCoverageGeoGrid( final Feature rgcFeature ) throws Exception
  {
    this( rgcFeature, null );
  }

  public RectifiedGridCoverageGeoGrid( final Feature rgcFeature, final URL context ) throws Exception
  {
    if( context == null )
      m_context = rgcFeature.getWorkspace().getContext();
    else
      m_context = context;

    final RectifiedGridDomain domain = (RectifiedGridDomain) rgcFeature.getProperty( new QName( NS.GML3, "rectifiedGridDomain" ) );
    m_rangeSet = (RangeSetType) rgcFeature.getProperty( new QName( NS.GML3, "rangeSet" ) );
    final GM_Point origin = domain.getOrigin( null );
    m_origin = new Coordinate( origin.getX(), origin.getY() );
    m_offsetX = new Coordinate( domain.getOffsetX().getGeoX(), domain.getOffsetX().getGeoY() );
    m_offsetY = new Coordinate( domain.getOffsetY().getGeoX(), domain.getOffsetY().getGeoY() );
    m_sourceCRS = domain.getCoordinateSystem();

    m_sizeX = domain.getNumColumns();
    m_sizeY = domain.getNumRows();
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getValue(int, int)
   */
  public final double getValue( final int x, final int y ) throws GeoGridException
  {
    final IGeoGrid grid = getGrid();
    if( grid == null )
      return Double.NaN;

    return grid.getValue( x, y );
  }

  private synchronized IGeoGrid getGrid( ) throws GeoGridException
  {
    if( m_grid == null )
    {
      try
      {
        final FileType file = m_rangeSet.getFile();
        if( file != null )
        {
          final URL url = new URL( m_context, file.getFileName() );
          m_grid = GeoGridUtilities.createGrid( file.getMimeType(), url, m_origin, m_offsetX, m_offsetY, m_sourceCRS );
        }
      }
      catch( final IOException e )
      {
        throw new GeoGridException( "Could not access grid-file", e );
      }
    }

    return m_grid;
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getEnvelope()
   */
  public Envelope getEnvelope( ) throws GeoGridException
  {
    return GeoGridUtilities.toEnvelope( this );
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getOrigin()
   */
  public Coordinate getOrigin( )
  {
    return m_origin;
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getSizeX()
   */
  public int getSizeX( )
  {
    return m_sizeX;
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getSizeY()
   */
  public int getSizeY( )
  {
    return m_sizeY;
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getOffsetX()
   */
  public Coordinate getOffsetX( )
  {
    return m_offsetX;
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getOffsetY()
   */
  public Coordinate getOffsetY( )
  {
    return m_offsetY;
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#dispose()
   */
  public void dispose( )
  {
    // do not dispose the grid, we access it via the weak-cache
    if( m_grid != null )
      m_grid.dispose();
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getValueChecked(int, int)
   */
  public double getValueChecked( final int x, final int y ) throws GeoGridException
  {
    if( (x < 0) || (x >= getSizeX()) || (y < 0) || (y >= getSizeY()) )
      return Double.NaN;

    return getValue( x, y );
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getWalkingStrategy()
   */
  public IGeoWalkingStrategy getWalkingStrategy( ) throws GeoGridException
  {
    final IGeoGrid grid = getGrid();
    if( grid == null )
      return null;

    return grid.getWalkingStrategy();
  }

  /**
   * @see org.kalypso.grid.IGeoValueProvider#getValue(com.vividsolutions.jts.geom.Coordinate)
   */
  public double getValue( final Coordinate crd ) throws GeoGridException
  {
    final IGeoGrid grid = getGrid();
    if( grid == null )
      return Double.NaN;

    return grid.getValue( crd );
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getMax()
   */
  public BigDecimal getMax( ) throws GeoGridException
  {
    final IGeoGrid grid = getGrid();
    if( grid == null )
      return null;

    return grid.getMax();
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getMin()
   */
  public BigDecimal getMin( ) throws GeoGridException
  {
    final IGeoGrid grid = getGrid();
    if( grid == null )
      return null;

    return grid.getMin();
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#setMax(java.math.BigDecimal)
   */
  public void setMax( BigDecimal maxValue ) throws GeoGridException
  {
    final IGeoGrid grid = getGrid();
    if( grid == null )
      grid.setMax( maxValue );
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#setMin(java.math.BigDecimal)
   */
  public void setMin( BigDecimal minValue ) throws GeoGridException
  {
    final IGeoGrid grid = getGrid();
    if( grid == null )
      grid.setMin( minValue );
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getCell(int, int, java.lang.String)
   */
  public GM_Surface< ? > getCell( int x, int y, String targetCRS ) throws GeoGridException
  {
    final IGeoGrid grid = getGrid();
    if( grid == null )
      return null;

    return grid.getCell( x, y, targetCRS );
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getSourceCRS()
   */
  public String getSourceCRS( ) throws GeoGridException
  {
    final IGeoGrid grid = getGrid();
    if( grid == null )
      return null;

    return grid.getSourceCRS();
  }

  /**
   * @see org.kalypso.grid.IGeoGrid#getSurface(java.lang.String)
   */
  public GM_Surface< ? > getSurface( String targetCRS ) throws GeoGridException
  {
    final IGeoGrid grid = getGrid();
    if( grid == null )
      return null;

    return grid.getSurface( targetCRS );
  }
}