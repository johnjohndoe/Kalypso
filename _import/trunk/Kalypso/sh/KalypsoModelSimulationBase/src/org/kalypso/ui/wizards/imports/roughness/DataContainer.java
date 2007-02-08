package org.kalypso.ui.wizards.imports.roughness;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class DataContainer
{

  public final static String GAUS_KRUEGER = "EPSG:31467";

  private String m_inputFile = ""; // absolute path

  private String m_shapeProperty = "";

  private CS_CoordinateSystem m_coordinateSystem;
  
  private GMLWorkspace m_workspace;
  
  private HashMap<String, String> m_roughnessDatabaseMap;

  private static final CS_CoordinateSystem m_defaultCoordinateSystem = ConvenienceCSFactory.getInstance().getOGCCSByName( GAUS_KRUEGER );

  public DataContainer( )
  {
    super();
    m_roughnessDatabaseMap = new HashMap<String, String>();
  }

  public final void setInputFile( String inputFile )
  {
    this.m_inputFile = inputFile;
  }

  public final void setShapeProperty( String shapeProperty )
  {
    this.m_shapeProperty = shapeProperty;
  }

  public final void setCoordinateSystem( String coordinateSystem )
  {
    this.m_coordinateSystem = ConvenienceCSFactory.getInstance().getOGCCSByName( coordinateSystem );
  }

  public String getInputFile( )
  {
    return m_inputFile;
  }

  public URL getInputFileURL( )
  {
    try
    {
      return new URL( "file:" + m_inputFile ); //$NON-NLS-1$
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
    }
    return null;
  }

  public String getShapeProperty( )
  {
    return m_shapeProperty;
  }

  public CS_CoordinateSystem getCoordinateSystem( boolean getDefaultIfNull )
  {
    if( m_coordinateSystem == null && getDefaultIfNull )
      return m_defaultCoordinateSystem;
    else
      return m_coordinateSystem;
  }

  public final GMLWorkspace getWorkspace( )
  {
    return m_workspace;
  }

  public final void setWorkspace( GMLWorkspace workspace )
  {
    m_workspace = workspace;
  }

  public final HashMap<String, String> getRoughnessDatabaseMap( )
  {
    return m_roughnessDatabaseMap;
  }

  public final void setRoughnessDatabaseMap( HashMap<String, String> roughnessDatabaseMap )
  {
    m_roughnessDatabaseMap = roughnessDatabaseMap;
  }

}
