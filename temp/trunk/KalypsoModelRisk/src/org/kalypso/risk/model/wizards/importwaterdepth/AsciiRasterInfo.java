package org.kalypso.risk.model.wizards.importwaterdepth;

import java.io.File;
import java.rmi.RemoteException;

import org.kalypso.core.preferences.IKalypsoCorePreferences;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.cs.Adapters;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.opengis.cs.CS_CoordinateSystem;

public class AsciiRasterInfo
{
  private int m_returnPeriod = 0;

  private CS_CoordinateSystem m_coordinateSystem;

  private double m_offsetX;

  private double m_offsetY;

  private int m_cellSize;

  private int m_rasterSizeX;

  private int m_rasterSizeY;

  private final File m_rasterFile;

  private RectifiedGridCoverage m_gridCoverage;

  public AsciiRasterInfo( final String rasterFileAbsolutePath ) throws Exception
  {
    m_rasterFile = new File( rasterFileAbsolutePath );
    setCoordinateSystem( IKalypsoCorePreferences.DEFAULT_CRS );
    init();
  }

  private void init( ) throws Exception
  {
    // m_gridCoverage = GeoGridUtilities.importGridArc( m_rasterFile, m_coordinateSystem );
    final RectifiedGridDomain gridDomain = m_gridCoverage.getGridDomain();
    m_rasterSizeX = gridDomain.getNumColumns();
    m_rasterSizeY = gridDomain.getNumRows();
    final GM_Point origin = gridDomain.getOrigin( m_coordinateSystem );
    m_offsetX = origin.getX();
    m_offsetY = origin.getY();
    m_cellSize = (int) (gridDomain.getOffsetX().getGeoX() - gridDomain.getOffsetX().getGeoY());
  }

  public Feature getFeature( )
  {
    return m_gridCoverage.getFeature();
  }

  public RectifiedGridDomain getGridDomain( )
  {
    return m_gridCoverage.getGridDomain();
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
    try
    {
      final String csName = m_coordinateSystem.getName();
      return new String[] { m_rasterFile.getName(), getReturnPeriod() > 0 ? Integer.toString( getReturnPeriod() ) : "", csName };
    }
    catch( final RemoteException e )
    {
      return new String[] { m_rasterFile.getName(), getReturnPeriod() > 0 ? Integer.toString( getReturnPeriod() ) : "", "" };
    }
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
    final CS_CoordinateSystem oldCoordinateSystem = m_coordinateSystem;
    final Adapters adapter = org.kalypsodeegree_impl.model.cs.Adapters.getDefault();
    m_coordinateSystem = adapter.export( (new ConvenienceCSFactoryFull()).getCSByName( cs ) );
    try
    {
      init();
      return true;
    }
    catch( final Exception e )
    {
      m_coordinateSystem = oldCoordinateSystem;
      try
      {
        init();
      }
      catch( final Exception e1 )
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
    try
    {
      return m_coordinateSystem.getName();
    }
    catch( final RemoteException e )
    {
      e.printStackTrace();
      return "null";
    }
  }

  public File getSourceFile( )
  {
    return m_rasterFile;
  }

  public RectifiedGridCoverage getGridCoverage( )
  {
    return m_gridCoverage;
  }
}
