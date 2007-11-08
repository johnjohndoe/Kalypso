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

    m_sizeX = domain.getNumColumns();
    m_sizeY = domain.getNumRows();
  }

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
          m_grid = GeoGridUtilities.createGrid( file.getMimeType(), url, m_origin, m_offsetX, m_offsetY );
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
   * Calculates the envelope of the whole grid.
   */
  public Envelope getBoundingBox( ) throws GeoGridException
  {
    return GeoGridUtilities.toEnvelope( this );
  }

  public Coordinate getOrigin( )
  {
    return m_origin;
  }

  public int getSizeX( )
  {
    return m_sizeX;
  }

  public int getSizeY( )
  {
    return m_sizeY;
  }

  public Coordinate getOffsetX( )
  {
    return m_offsetX;
  }

  public Coordinate getOffsetY( )
  {
    return m_offsetY;
  }

  /**
   * @see org.kalypso.gis.doubleraster.grid.DoubleGrid#dispose()
   */
  public void dispose( )
  {
    // do not dispose the grid, we access it via the weak-cache
    if( m_grid != null )
      m_grid.dispose();
  }

  /**
   * @see org.kalypso.gis.doubleraster.IDoubleGeoGrid#getValueChecked(int, int)
   */
  public double getValueChecked( final int x, final int y ) throws GeoGridException
  {
    if( (x < 0) || (x >= getSizeX()) || (y < 0) || (y >= getSizeX()) )
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
   * @see org.kalypso.gis.doubleraster.IDoubleProvider#getDouble(com.vividsolutions.jts.geom.Coordinate)
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
}
