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
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessLayer;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class DataContainer
{
  private String m_inputFile = ""; // absolute path //$NON-NLS-1$

  private String m_shapeProperty = ""; //$NON-NLS-1$

  // private boolean m_isLayerEditable;

  private boolean m_isBasicLayer = true;

  private String m_coordinateSystem;

  private final LinkedHashMap<String, String> m_roughnessStaticCollectionMap;

  private final LinkedHashMap<String, String> m_roughnessShapeStaticRelationMap;

  private String m_roughnessDatabaseLocation;

  private String m_projectBaseFolder;

  private final String m_absolutePath = ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString();

  private static final String m_defaultCoordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  private LinkedHashMap<String, String> m_userSelectionMap;

  private String m_userSelectionFile;

  private ITerrainModel m_model;

  private IRoughnessLayer m_roughnessLayer;

  public DataContainer( )
  {
    super();

    m_roughnessStaticCollectionMap = new LinkedHashMap<>();
    m_roughnessShapeStaticRelationMap = new LinkedHashMap<>();
  }

  public final void setInputFile( final String inputFile )
  {
    m_inputFile = inputFile;
  }

  public final void setLayerName( final String layerName )
  {
    getLayer().setName( layerName );
  }

  private IRoughnessLayer getLayer( )
  {
    if( m_roughnessLayer == null )
      m_roughnessLayer = createNewGMLLayer();
    return m_roughnessLayer;
  }

  public final void setLayerAsBasic( final boolean isBasicLayer )
  {
    m_isBasicLayer = isBasicLayer;
  }

  public String getLayerName( )
  {
    return getLayer().getName();
  }

  public boolean isBasicLayer( )
  {
    return m_isBasicLayer;
  }

  public final void setShapeProperty( final String shapeProperty )
  {
    this.m_shapeProperty = shapeProperty;
  }

  public final void setCoordinateSystem( final String coordinateSystem )
  {
    this.m_coordinateSystem = coordinateSystem;
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
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }
    return null;
  }

  public String getShapeProperty( )
  {
    return m_shapeProperty;
  }

  public String getCoordinateSystem( final boolean getDefaultIfNull )
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
    final IRoughnessPolygonCollection roughnessPolygonCollection = m_model.getRoughnessPolygonCollection( getLayer() );
    return roughnessPolygonCollection;
  }

  /**
   * @param dbAxAy
   *          -
   *          true if user wants dbAxAy database location, false if KS db location is required
   */
  public final String getRoughnessDatabaseLocation( )
  {
    return m_roughnessDatabaseLocation;
  }

  /**
   * @param dbAxAy
   *          -
   *          true if user wants dbAxAy database location, false if KS db location is required
   */
  public final URL getRoughnessDatabaseLocationURL( ) throws MalformedURLException
  {
    return new URL( "file:" + m_absolutePath + "/" + m_projectBaseFolder + getRoughnessDatabaseLocation() ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public final void setRoughnessDatabaseLocation( final String dbLocation, final IRoughnessClsCollection roughnessClsCollection ) throws Exception
  {
    m_roughnessDatabaseLocation = dbLocation;

    // final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( getRoughnessDatabaseLocationURL(), null );
    // final IRoughnessClsCollection collection = new RoughnessClsCollection( workspace.getRootFeature() );
    // final IRoughnessClsCollection collection = Util.getModel( IRoughnessClsCollection.class );
    for( int i = 0; i < roughnessClsCollection.getRoughnessClasses().size(); i++ )
      m_roughnessStaticCollectionMap.put( roughnessClsCollection.getRoughnessClasses().get( i ).getName(), roughnessClsCollection.getRoughnessClasses().get( i ).getId() );
  }

  public final String getProjectBaseFolder( )
  {
    return m_projectBaseFolder;
  }

  public final void setProjectBaseFolder( final String projectBaseFolder )
  {
    m_projectBaseFolder = projectBaseFolder;
  }

  public final LinkedHashMap<String, String> getUserSelectionMap( )
  {
    return m_userSelectionMap;
  }

  @SuppressWarnings( "unchecked" )
  public void loadUserSelection( final String userSelectionFile )
  {
    // TODO: use dialog settings stuff instead!
    m_userSelectionFile = m_absolutePath + "/" + m_projectBaseFolder + "/" + userSelectionFile; //$NON-NLS-1$ //$NON-NLS-2$
    final File file = new File( m_userSelectionFile );

    try
    {
      if( file.exists() && file.isFile() && file.length() > 0 )
      {
        final FileInputStream fileStream = new FileInputStream( file );
        final ObjectInputStream objectStream = new ObjectInputStream( fileStream );
        final Object object = objectStream.readObject();
        if( object instanceof LinkedHashMap )
          m_userSelectionMap = (LinkedHashMap<String, String>)object;
      }
      else
      {
        file.createNewFile();
        m_userSelectionMap = new LinkedHashMap<>();
      }
    }
    catch( final Exception e )
    {
      m_userSelectionMap = new LinkedHashMap<>();
      e.printStackTrace();
    }
  }

  public void saveUserSelection( )
  {
    try
    {
      final FileOutputStream fileStream = new FileOutputStream( m_userSelectionFile );
      final ObjectOutputStream objectStream = new ObjectOutputStream( fileStream );
      objectStream.writeObject( m_userSelectionMap );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  public void setModel( final ITerrainModel model )
  {
    m_model = model;
  }

  public ITerrainModel getModel( )
  {
    return m_model;
  }

  public IFeatureBindingCollection<IRoughnessLayer> getRoughnessLayerCollection( )
  {
    return m_model.getRoughnessLayerCollection();
  }

  public IRoughnessLayer createNewGMLLayer( )
  {
    if( m_roughnessLayer != null )
      return m_roughnessLayer;

    final IFeatureBindingCollection<IRoughnessLayer> roughnessLayerCollection = getRoughnessLayerCollection();
    for( final IRoughnessLayer roughnessLayer : roughnessLayerCollection )
    {
      if( roughnessLayer.isBasicLayer() == m_isBasicLayer )
        return m_roughnessLayer = roughnessLayer;
    }

    return m_roughnessLayer;
  }

  public void deleteCreatedGMLLayer( )
  {
    return;
  }
}
