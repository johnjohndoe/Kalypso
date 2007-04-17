package org.kalypso.ui.wizards.imports.roughness;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.LinkedHashMap;

import org.eclipse.core.resources.ResourcesPlugin;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
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

  private IRoughnessPolygonCollection m_feature;

  private LinkedHashMap<String, String> m_roughnessStaticCollectionMap;

  private LinkedHashMap<String, String> m_roughnessShapeStaticRelationMap;

  private String m_roughnessDatabaseLocation;

  private String m_ProjectBaseFolder;

  private final String m_AbsolutePath = ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString();

  private static final CS_CoordinateSystem m_defaultCoordinateSystem = ConvenienceCSFactory.getInstance().getOGCCSByName( GAUS_KRUEGER );

  private LinkedHashMap<String, String> m_userSelectionMap;

  private String m_userSelectionFile;

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

  public final IRoughnessPolygonCollection getRoughnessPolygonCollection( )
  {
    return m_feature;
  }

  public final void setRoughnessPolygonCollection( IRoughnessPolygonCollection roughnessPolygonCollection )
  {
    m_feature = roughnessPolygonCollection;
  }

  /**
   * @param dbAxAy -
   *          true if user wants dbAxAy database location, false if KS db location is required
   */
  public final String getRoughnessDatabaseLocation( )
  {
    return m_roughnessDatabaseLocation;
  }

  /**
   * @param dbAxAy -
   *          true if user wants dbAxAy database location, false if KS db location is required
   */
  public final URL getRoughnessDatabaseLocationURL( ) throws MalformedURLException
  {
    return new URL( "file:" + m_AbsolutePath + "/" + m_ProjectBaseFolder + getRoughnessDatabaseLocation() );
  }

  public final void setRoughnessDatabaseLocation( String dbLocation ) throws Exception
  {
    m_roughnessDatabaseLocation = dbLocation;

    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( getRoughnessDatabaseLocationURL(), null );
    final IRoughnessClsCollection collection = new RoughnessClsCollection( workspace.getRootFeature() );
    for( int i = 0; i < collection.size(); i++ )
      m_roughnessStaticCollectionMap.put( collection.get( i ).getName(), collection.get( i ).getGmlID() );
  }

  public final String getProjectBaseFolder( )
  {
    return m_ProjectBaseFolder;
  }

  public final void setProjectBaseFolder( String projectBaseFolder )
  {
    m_ProjectBaseFolder = projectBaseFolder;
  }

  public final LinkedHashMap<String, String> getUserSelectionMap( )
  {
    return m_userSelectionMap;
  }

  @SuppressWarnings("unchecked")
  public void loadUserSelection( String userSelectionFile )
  {
    try
    {
      m_userSelectionFile = m_AbsolutePath + "/" + m_ProjectBaseFolder + "/" + userSelectionFile;
      File file = new File( m_userSelectionFile );
      if( file.exists() && file.isFile() && file.length() > 0 )
      {
        FileInputStream fileStream = new FileInputStream( file );
        ObjectInputStream objectStream = new ObjectInputStream( fileStream );
        Object object = objectStream.readObject();
        if( object instanceof LinkedHashMap )
          m_userSelectionMap = (LinkedHashMap<String, String>) object;
      }
      else
      {
        file.createNewFile();
        m_userSelectionMap = new LinkedHashMap<String, String>();
      }
    }
    catch( Exception e )
    {
      m_userSelectionMap = new LinkedHashMap<String, String>();
      e.printStackTrace();
    }
  }

  public void saveUserSelection( )
  {
    try
    {
      FileOutputStream fileStream = new FileOutputStream( m_userSelectionFile );
      ObjectOutputStream objectStream = new ObjectOutputStream( fileStream );
      objectStream.writeObject( m_userSelectionMap );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }
}
