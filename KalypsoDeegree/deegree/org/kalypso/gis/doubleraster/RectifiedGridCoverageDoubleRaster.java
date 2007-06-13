package org.kalypso.gis.doubleraster;

import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;

import javax.xml.namespace.QName;

import ogc31.www.opengis.net.gml.FileType;
import ogc31.www.opengis.net.gml.RangeSetType;

import org.kalypso.commons.xml.NS;
import org.kalypso.gis.doubleraster.grid.AsciiGrid;
import org.kalypso.gis.doubleraster.grid.DoubleGrid;
import org.kalypso.gis.doubleraster.grid.ImageGrid;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * <p>
 * Douple Raster wich wraps a RectifiedGridCoverage
 * 
 * @author belger
 */
public class RectifiedGridCoverageDoubleRaster extends AbstractDoubleRaster implements DoubleRaster
{
  private final int m_sizeX;

  private final int m_sizeY;

  private final Coordinate m_origin;

  private final Coordinate m_offsetX;

  private final Coordinate m_offsetY;

  private final RangeSetType m_rangeSet;

  private final URL m_context;

  private DoubleGrid m_grid;

  public RectifiedGridCoverageDoubleRaster( final Feature rgcFeature ) throws Exception
  {
    this( rgcFeature, null );
  }

  public RectifiedGridCoverageDoubleRaster( final Feature rgcFeature, final URL context ) throws Exception
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

  public final double getValue( final int x, final int y )
  {
    final DoubleGrid grid = getTiledImage();
    if( grid == null )
      return Double.NaN;

    return grid.getValue( x, y );
  }

  private DoubleGrid getTiledImage( )
  {
    if( m_grid == null )
    {
      // TODO: Dejan: for now we are using just a File, will be extended to other data types

      final FileType file = m_rangeSet.getFile();
      // TODO: only supported type now is 'File', support other types
      if( file != null )
        try
        {
          // TODO: we assume that mime-type image is tiff
          final String mimeType = file.getMimeType();
          if( mimeType.startsWith( "image" ) )
          {
            final String fileName = file.getFileName();

            final URL url = new URL( m_context, fileName );
            m_grid = new ImageGrid( url );
          }
          else if( mimeType.startsWith( "text/asc" ) )
          {
            final String fileName = file.getFileName();
            final URL fileURL = new URL( m_context, fileName );

            if( fileURL.toURI().isAbsolute() )
              m_grid = new AsciiGrid( fileURL );
            else
            {
              final URL url = new URL( m_context, fileName );
              m_grid = new AsciiGrid( url );
            }
          }
        }
        catch( final MalformedURLException e )
        {
          e.printStackTrace();
        }
        catch( final URISyntaxException e )
        {
          e.printStackTrace();
        }

    }

    return m_grid;
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

  /**
   * Wie getValue, gibt nur Double.NaN zurück für Punkte ausserhalb des Raster
   */
  public double getValueChecked( final int x, final int y )
  {
    if( (x < 0) || (x >= m_sizeX) || (y < 0) || (y >= m_sizeY) )
      return Double.NaN;

    return getValue( x, y );
  }

  public Coordinate getOffsetX( )
  {
    return m_offsetX;
  }

  public Coordinate getOffsetY( )
  {
    return m_offsetY;
  }

}
