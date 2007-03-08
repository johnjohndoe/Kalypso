package org.kalypso.ui.wizards.imports.roughness;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.LinkedHashMap;

import org.eclipse.core.resources.ResourcesPlugin;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygonCollection;
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
  
  private RoughnessPolygonCollection m_feature;
  
  private LinkedHashMap<String, String> m_roughnessStaticCollectionMap;
  
  private LinkedHashMap<String, String> m_roughnessShapeStaticRelationMap;
  
  private String m_roughnessDatabaseLocation;
  
  private String m_ProjectBaseFolder;
  
  private final String m_AbsolutePath = ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString();

  private static final CS_CoordinateSystem m_defaultCoordinateSystem = ConvenienceCSFactory.getInstance().getOGCCSByName( GAUS_KRUEGER );

  public DataContainer( )
  {
    super();
    m_roughnessStaticCollectionMap = new LinkedHashMap<String, String>();
    m_roughnessShapeStaticRelationMap = new LinkedHashMap<String, String>();
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

  public final LinkedHashMap<String, String> getRoughnessStaticCollectionMap( )
  {
    return m_roughnessStaticCollectionMap;
  }

  public final LinkedHashMap<String, String> getRoughnessShapeStaticRelationMap( )
  {
    return m_roughnessShapeStaticRelationMap;
  }

  public final RoughnessPolygonCollection getRoughnessPolygonCollection( )
  {
    return m_feature;
  }

  public final void setRoughnessPolygonCollection( RoughnessPolygonCollection collection )
  {
    m_feature = collection;
  }

  public final String getRoughnessDatabaseLocation( )
  {
    return m_roughnessDatabaseLocation;
  }
  
  public final URL getRoughnessDatabaseLocationURL() throws MalformedURLException{
    return new URL("file:"+m_AbsolutePath+"/"+m_ProjectBaseFolder+m_roughnessDatabaseLocation);
  }

  public final void setRoughnessDatabaseLocation( String roughnessDatabaseLocation )
  {
    m_roughnessDatabaseLocation = roughnessDatabaseLocation;
  }

  public final String getProjectBaseFolder( )
  {
    return m_ProjectBaseFolder;
  }

  public final void setProjectBaseFolder( String projectBaseFolder )
  {
    m_ProjectBaseFolder = projectBaseFolder;
  }
}
