package org.kalypso.risk.model.actions.dataImport.waterdepth;

import java.io.File;

import org.eclipse.core.resources.IFile;
import org.kalypso.grid.AsciiGridReader;
import org.kalypsodeegree.KalypsoDeegreePlugin;
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

  private final File m_rasterFile;

  private RectifiedGridDomain m_gridDomain;

  private final IFile m_iFile;

  /**
   * generates a data collecting for a ascii raster file. <br>
   * At first, the coordinate is set to default. The real crs has to be set from outside!
   */
  public AsciiRasterInfo( final String rasterFileAbsolutePath ) throws Exception
  {
    m_iFile = null;
    m_rasterFile = new File( rasterFileAbsolutePath );
    init();
  }

  public AsciiRasterInfo( final IFile file ) throws Exception
  {
    m_iFile = file;
    m_rasterFile = m_iFile.getLocation().toFile();
    init();
  }

  private void init( ) throws Exception
  {
    // At first the coordinate system is set to default
    if( m_coordinateSystem == null )
      m_coordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    final AsciiGridReader reader = new AsciiGridReader( m_rasterFile.toURI().toURL() );
    m_gridDomain = reader.getGridDomain( m_coordinateSystem );
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
    m_coordinateSystem = cs;
    try
    {
      final GM_Point origin = m_gridDomain.getOrigin( null );
      origin.setCoordinateSystem( cs );

      m_gridDomain = new RectifiedGridDomain( origin, m_gridDomain.getOffsetX(), m_gridDomain.getOffsetY(), m_gridDomain.getGridRange() );
      // TODO
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return true;
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

  public IFile getiSourceFile( )
  {
    return m_iFile;
  }
}