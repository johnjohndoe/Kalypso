package org.kalypso.gis.doubleraster;

import com.bce.gis.operation.hmo2fli.DoubleProvider;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * <p>
 * Simple Rasterklasse zum Repräsentieren von gerasterten Daten
 * </p>
 * <p>
 * Die Daten werde nnicht gespeichert, sondern kommen aus einem DoubleProvider
 * </p>
 * 
 * @author belger
 */
public class DoubleFakeRaster extends AbstractDoubleRaster implements DoubleRaster
{
  private final DoubleProvider m_doubleProvider;

  private final int m_sizeX;

  private final int m_sizeY;

  private final Coordinate m_origin;

  private final Coordinate m_rasterOffsetX;

  private final Coordinate m_rasterOffsetY;

  public DoubleFakeRaster( final int sizeX, final int sizeY, final Coordinate origin, final Coordinate rasterOffsetX, final Coordinate rasterOffsetY, final DoubleProvider dp )
  {
    m_sizeX = sizeX;
    m_sizeY = sizeY;
    m_origin = origin;
    m_rasterOffsetX = rasterOffsetX;
    m_rasterOffsetY = rasterOffsetY;
    m_doubleProvider = dp;
  }

  public final double getValue( final int x, final int y )
  {
    final Coordinate c = DoubleRasterUtilities.rasterCellToCoordinate( this, x, y, null );
    return m_doubleProvider.getDouble( c );
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
    if( x < 0 || x >= m_sizeX || y < 0 || y >= m_sizeY )
      return Double.NaN;

    return getValue( x, y );
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRaster#getOffsetX()
   */
  public Coordinate getOffsetX( )
  {
    return m_rasterOffsetX;
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRaster#getOffsetY()
   */
  public Coordinate getOffsetY( )
  {
    return m_rasterOffsetY;
  }
}
