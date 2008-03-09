package org.kalypso.risk.model.actions.dataImport.waterdepth;

import java.io.File;

import org.kalypso.core.preferences.IKalypsoCorePreferences;
import org.kalypso.grid.AscciiGridReader;
import org.kalypso.grid.ConvertAscii2Coverage;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

public class AsciiRasterInfo
{
  private int m_returnPeriod = 0;

  private String m_coordinateSystem;

  private double m_offsetX;

  private double m_offsetY;

  private int m_cellSize;

  private int m_rasterSizeX;

  private int m_rasterSizeY;

  private File m_rasterFile;

  private RectifiedGridDomain m_gridDomain;

  public AsciiRasterInfo( final String rasterFileAbsolutePath ) throws Exception
  {
    m_rasterFile = new File( rasterFileAbsolutePath );
    setCoordinateSystem( IKalypsoCorePreferences.DEFAULT_CRS );
    init();
  }

  private void init( ) throws Exception
  {
    final AscciiGridReader reader = new AscciiGridReader( m_rasterFile );
    m_gridDomain = ConvertAscii2Coverage.importGridArc( reader, m_coordinateSystem );
    m_rasterSizeX = m_gridDomain.getNumColumns();
    m_rasterSizeY = m_gridDomain.getNumRows();
    final GM_Point origin = m_gridDomain.getOrigin( m_coordinateSystem );
    m_offsetX = origin.getX();
    m_offsetY = origin.getY();
    m_cellSize = (int) (m_gridDomain.getOffsetX().getGeoX() - m_gridDomain.getOffsetX().getGeoY());
  }

  public RectifiedGridDomain getGridDomain( )
  {
    return m_gridDomain;
  }

  public double getOffsetX( )
  {
    return m_offsetX;
  }

  public double getOffsetY( )
  {
    return m_offsetY;
  }

  public int getCellSize( )
  {
    return m_cellSize;
  }

  public int getRasterSizeX( )
  {
    return m_rasterSizeX;
  }

  public int getRasterSizeY( )
  {
    return m_rasterSizeY;
  }

  public String[] getDisplayDetails( )
  {
    final String csName = m_coordinateSystem;
    return new String[] { m_rasterFile.getName(), getReturnPeriod() > 0 ? Integer.toString( getReturnPeriod() ) : "", csName }; //$NON-NLS-1$
  }

  public void setReturnPeriod( final int returnPeriod )
  {
    m_returnPeriod = returnPeriod;
  }

  /**
   * Method tries to set a new coordinate system.
   * 
   * @param cs
   *            A coordinate system that sjould be set
   * @return true on success; false otherwise
   */
  public boolean setCoordinateSystem( final String cs )
  {
    final String oldCoordinateSystem = m_coordinateSystem;
    m_coordinateSystem = cs;
    try
    {
      init();
      return true;
    }
    catch( Exception e )
    {
      m_coordinateSystem = oldCoordinateSystem;
      try
      {
        init();
      }
      catch( Exception e1 )
      {
        e1.printStackTrace();
      }
      return false;
    }
  }

  public int getReturnPeriod( )
  {
    return m_returnPeriod;
  }

  public String getCoordinateSystem( )
  {
    return m_coordinateSystem;
  }

  public File getSourceFile( )
  {
    return m_rasterFile;
  }
}