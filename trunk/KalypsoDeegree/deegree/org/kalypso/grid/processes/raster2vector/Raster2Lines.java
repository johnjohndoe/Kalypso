package org.kalypso.grid.processes.raster2vector;

import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.IGeoGridWalker;

import com.vividsolutions.jts.geom.Coordinate;

public class Raster2Lines implements IGeoGridWalker
{
  private double m_volume;

  private static final double VAL_EPS = 0.00001;

  private Coordinate[] m_lastRowCoord = null;

  private Coordinate m_lastSE;

  private Coordinate m_lastNE;

  private LinkedCoordinate[][] m_lastRow;

  private LinkedCoordinate[] m_lastCell;

  private final double[] m_grenzen;

  private final SegmentCollector m_strategy;

  private double m_cellArea;

  private Coordinate m_offsetX;

  private Coordinate m_offsetY;

  private int m_sizeY;

  private double m_xOffset;

  private double m_yOffset;

  private int m_sizeX;

  private IGeoGrid m_grid;

  public Raster2Lines( final SegmentCollector strategy, final double[] isolines )
  {
    m_strategy = strategy;
    m_grenzen = isolines;
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRasterWalker#start(org.kalypso.gis.doubleraster.DoubleRaster)
   */
  public final void start( final IGeoGrid grid ) throws GeoGridException
  {
    m_grid = grid;
    m_lastRowCoord = new Coordinate[grid.getSizeX() + 1];
    m_lastSE = null;
    m_offsetX = grid.getOffsetX();
    m_offsetY = grid.getOffsetY();
    m_lastRow = new LinkedCoordinate[m_grenzen.length][grid.getSizeX() + 1];
    m_lastCell = new LinkedCoordinate[m_grenzen.length];

    m_cellArea = GeoGridUtilities.calcCellArea( m_offsetX, m_offsetY );

    m_xOffset = Math.abs( m_offsetX.x ) + Math.abs( m_offsetY.x );
    m_yOffset = Math.abs( m_offsetX.y ) + Math.abs( m_offsetY.y );
    // reset volume
    m_volume = 0.0;

    m_sizeY = grid.getSizeY();
    m_sizeX = grid.getSizeX();
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRasterWalker#operate(int, int, com.vividsolutions.jts.geom.Coordinate)
   */
  public final void operate( final int x, final int y, final Coordinate crd ) throws GeoGridException
  {
    // Volumen aufaddieren
    final double cellVolume = m_cellArea * crd.z;
    if( crd.z > 0 )
      m_volume += cellVolume;

    // die Zellecken ausrechnen
    final Coordinate cSE = new Coordinate( crd );

    Coordinate cSW = m_lastSE;
    Coordinate cNE = m_lastRowCoord[x];
    Coordinate cNW = m_lastNE;

    if( cSW == null )
    {
      // cSW = new Coordinate( crd.x - m_rasterSize, crd.y, Double.NaN );
      cSW = GeoGridUtilities.calcCoordinate( m_grid, x - 1, y, null );
    }
    if( cNE == null )
    {
      // cNE = new Coordinate( crd.x, crd.y + m_rasterSize, Double.NaN );
      cNE = GeoGridUtilities.calcCoordinate( m_grid, x, y - 1, null );
    }
    if( cNW == null )
    {
      // cNW = new Coordinate( crd.x - m_rasterSize, crd.y + m_rasterSize, Double.NaN );
      cNW = GeoGridUtilities.calcCoordinate( m_grid, x - 1, y - 1, null );
    }

    m_lastSE = cSE;
    m_lastNE = cNE;
    m_lastRowCoord[x] = cSE;

    addSegment( x, y, cNW, cNE, cSW, cSE );

    // TODO: this implementation makes assumptions about the order of visited cells... dangerous!
    if( x == m_sizeX - 1 )
      afterLine( x, y );
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRasterWalker#afterLine(int)
   */
  private final void afterLine( final int x, final int y ) throws GeoGridException
  {
    // die Zellecken ausrechnen

    final Coordinate cSW = m_lastSE;
    final Coordinate cNW = m_lastNE;
    Coordinate cNE = m_lastRowCoord[x + 1];

    if( cNE == null )
      cNE = GeoGridUtilities.calcCoordinate( m_grid, x + 1, y - 1, null );

    final Coordinate cSE = GeoGridUtilities.calcCoordinate( m_grid, x + 1, y, null );

    m_lastSE = null;
    m_lastNE = null;
    m_lastRowCoord[x + 1] = cSE;

    addSegment( x + 1, y, cNW, cNE, cSW, cSE );
  }

  /**
   * Gibt ein Objekt vom Type LineString[] zurück
   * 
   * @see org.kalypso.gis.doubleraster.DoubleRasterWalker#getResult()
   */
  public Object finish( )
  {
    return m_strategy;
  }

  private final void addSegment( final int x, final int y, final Coordinate cNW, final Coordinate cNE, final Coordinate cSW, final Coordinate cSE ) throws GeoGridException
  {
    for( int i = 0; i < m_grenzen.length; i++ )
    {
      try
      {
        addAtIndex( i, x, y, cNW, cNE, cSW, cSE );
      }
      catch( final LinkedCoordinateException lce )
      {
        throw new GeoGridException( "Fehler beim linken der Segmente", lce );
      }
    }
  }

  private final void addAtIndex( final int index, final int x, final int y, final Coordinate cNW, final Coordinate cNE, final Coordinate cSW, final Coordinate cSE ) throws LinkedCoordinateException, GeoGridException
  {
    // check, ob die Koordinaten den Wert genau Treffen
    final double value = m_grenzen[index];

    if( Math.abs( cNW.z - value ) < VAL_EPS )
      cNW.z += VAL_EPS;
    if( Math.abs( cSW.z - value ) < VAL_EPS )
      cSW.z += VAL_EPS;
    if( Math.abs( cNE.z - value ) < VAL_EPS )
      cNE.z += VAL_EPS;
    if( Math.abs( cSE.z - value ) < VAL_EPS )
      cSE.z += VAL_EPS;

    // gibt es einen Schnitt mit der Isolinie?
    final LinkedCoordinate topC = x >= 0 ? m_lastRow[index][x] : null;
    final LinkedCoordinate leftC = x > 0 ? m_lastCell[index] : null;
    final LinkedCoordinate rightC = interpolate( cNE, cSE, index );
    final LinkedCoordinate bottomC = interpolate( cSW, cSE, index );

    m_lastCell[index] = rightC;
    m_lastRow[index][x] = bottomC;

    // Schnittlinien in dieser Zelle ermitteln
    // es gibt 16 Fälle
    if( topC == null && leftC == null && rightC == null && bottomC == null )
    {
      // 0: nix tun
    }
    else if( topC != null && leftC != null && rightC == null && bottomC == null )
    {
      // 2:
      m_strategy.addSegment( index, leftC, topC, cNW, cNE );
    }
    else if( topC != null && leftC == null && rightC != null && bottomC == null )
    {
      // 3:
      m_strategy.addSegment( index, topC, rightC, cNW, cNE );
    }
    else if( topC != null && leftC == null && rightC == null && bottomC != null )
    {
      // 4:
      m_strategy.addSegment( index, topC, bottomC, cNW, cNE );
    }
    else if( topC == null && leftC != null && rightC == null && bottomC != null )
    {
      // 9:
      m_strategy.addSegment( index, leftC, bottomC, cNW, cSW );
    }
    else if( topC == null && leftC != null && rightC != null && bottomC == null )
    {
      // 10:
      m_strategy.addSegment( index, leftC, rightC, cNW, cSW );
    }
    else if( topC == null && leftC == null && rightC != null && bottomC != null )
    {
      // 13:
      m_strategy.addSegment( index, rightC, bottomC, cNE, cSE );
    }
    else if( topC != null && leftC != null && rightC != null && bottomC != null )
    {
      // 15:
      if( topC.crd.x < bottomC.crd.x )
      {
        // erst wie 2
        m_strategy.addSegment( index, leftC, topC, cNW, cNE );

        // dann wie 13
        m_strategy.addSegment( index, rightC, bottomC, cNE, cSE );
      }
      else
      {
        // erst 3
        m_strategy.addSegment( index, topC, rightC, cNW, cNE );

        // dann 9
        m_strategy.addSegment( index, leftC, bottomC, cNW, cSW );
      }
    }
    else
    {
      final String msg = String.format( "Unmöglicher Fall bei x=%d, y=%d, index=%d", x, y, index );
      throw new GeoGridException( msg, null );
    }
  }

  /**
   * Hat die Verbindung c1 - c2 einen Zwischenwert bei value, interpoliere die Zwischencoordinate Voraussetzung ist,
   * dass keine der Koordinaten exakt den Wert annimmt!
   */
  private final LinkedCoordinate interpolate( final Coordinate c1, final Coordinate c2, final int index )
  {
    double zFaktor = -1.0;
    boolean bBorder = false;

    double z1 = c1.z;
    if( Double.isNaN( z1 ) )
    {
      z1 = -1000.0;
      bBorder = true;
    }

    double z2 = c2.z;
    if( Double.isNaN( z2 ) )
    {
      z2 = -1000.0;
      bBorder = true;
    }

    final double value = m_grenzen[index];

    if( (z1 <= value && value < z2) || (z2 <= value && value < z1) )
      zFaktor = (value - z1) / (z2 - z1);

    if( zFaktor < 0 || zFaktor >= 1 )
      return null;

    final double x = zFaktor * (c2.x - c1.x) + c1.x;
    final double y = zFaktor * (c2.y - c1.y) + c1.y;
    return new LinkedCoordinate( new Coordinate( x, y, value ), bBorder );
  }

  public double getSum( )
  {
    return m_volume;
  }
}
