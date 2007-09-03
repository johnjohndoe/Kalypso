package org.kalypso.ui.wizards.imports.roughness;

import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypso1d2d.pjt.views.SzenarioDataProvider;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessLayer;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ui.views.map.MapView;
import org.kalypso.ui.wizards.imports.Messages;
import org.kalypso.ui.wizards.imports.utils.Util;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * Implements the transformation algorithm from a shape file into a IRoughnessPolygonCollection
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Transformer implements ICoreRunnableWithProgress
{
  private DataContainer m_data;

  private boolean m_isDataPrepared = false;

  private int m_NumberOfEntriesAdded = -1;

  private final MapView m_mapView;

  public Transformer( DataContainer data, MapView mapView )
  {
    m_data = data;
    m_mapView = mapView;
  }

  public IStatus execute( IProgressMonitor monitor )
  {
    boolean hasMonitor = monitor != null;
    try
    {
      if( hasMonitor )
      {
        monitor.beginTask( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.Transformer.0" ), 100 ); //$NON-NLS-1$
        monitor.worked( 10 );
        monitor.subTask( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.Transformer.1" ) ); //$NON-NLS-1$
      }
      try
      {
        final IRoughnessLayer gmlLayer = m_data.createNewGMLLayer();
        createMapLayer( gmlLayer );
        if( !m_isDataPrepared )
          prepare( true );
        setSelectedRoughnessChoice();
        serialize();
        if( hasMonitor && monitor.isCanceled() )
          return Status.CANCEL_STATUS;
      }
      catch( ClassCastException e )
      {
        return new Status( Status.ERROR, KalypsoCorePlugin.getID(), Status.CANCEL, e.getMessage(), e );
        // monitor.setCanceled(true);
        // return Status.CANCEL_STATUS;
      }
      if( hasMonitor )
        monitor.done();
      // m_data.getProject().refreshLocal( IResource.DEPTH_INFINITE, null );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return new Status( Status.ERROR, KalypsoCorePlugin.getID(), Status.CANCEL, e.getMessage(), e );
    }
    return Status.OK_STATUS;
  }

  private void createMapLayer( final IRoughnessLayer layer ) throws ExecutionException
  {
    
    // for the moment, we are working with two fixed layers...
    
    return;
//    final String source = m_data.getModel().getWrappedFeature().getWorkspace().getContext().toString();
//    if( m_mapView != null )
//    {
//      final GisTemplateMapModell mapModell = (GisTemplateMapModell) m_mapView.getMapPanel().getMapModell();
//      final StringBuffer featurePath = new StringBuffer( "#fid#" );
//      featurePath.append( layer.getGmlID() ).append( "/roughnessLayerMember[RoughnessPolygon]" );
//      
//      final AddThemeCommand command = new AddThemeCommand( mapModell, layer.getName(), "gml", featurePath.toString(), source, "sld", "Roughness style", "project:/.metadata/roughness.sld","simple" );
//      m_mapView.postCommand( command, null );
//    }
//    else
//      throw new ExecutionException( "Kartenansicht nicht geöffnet. Es können keine Themen hinzugefügt werden." );
  }

  public void prepare( boolean resetMap ) throws Exception
  {
    if( resetMap )
    {
      m_data.getRoughnessShapeStaticRelationMap().clear();
      // m_data.getRoughnessPolygonCollection().clear();
    }
    final QName shpFeatureName = new QName( "namespace", "featureMember" ); //$NON-NLS-1$ //$NON-NLS-2$
    final QName shpGeomPropertyName = new QName( "namespace", "GEOM" ); //$NON-NLS-1$ //$NON-NLS-2$
    final QName shpCustomPropertyName = new QName( "namespace", m_data.getShapeProperty() ); //$NON-NLS-1$
    final GMLWorkspace shapeWorkSpace = ShapeSerializer.deserialize( FileUtilities.nameWithoutExtension( m_data.getInputFile() ), m_data.getCoordinateSystem( true ) );
    final Feature shapeRootFeature = shapeWorkSpace.getRootFeature();
    final List< ? > shapeFeatureList = (List< ? >) shapeRootFeature.getProperty( shpFeatureName );
    final IRoughnessPolygonCollection roughnessPolygonCollection = m_data.getRoughnessPolygonCollection();

    m_NumberOfEntriesAdded = 0;
    for( int i = 0; i < shapeFeatureList.size(); i++ )
    {
      final Feature shapeFeature = (Feature) shapeFeatureList.get( i );
      final String propertyValue = shapeFeature.getProperty( shpCustomPropertyName ).toString();
      final Object gm_Whatever = shapeFeature.getProperty( shpGeomPropertyName );
      if( gm_Whatever instanceof GM_Object )
      {
        if( gm_Whatever instanceof GM_MultiSurface )
        {
          final GM_MultiSurface multiSurface = (GM_MultiSurface) ((GM_MultiSurface) gm_Whatever).clone();
          final GM_Surface< ? >[] surfaces = multiSurface.getAllSurfaces();
          for( int k = 0; k < surfaces.length; k++ )
          {
            final IRoughnessPolygon roughnessPolygon = roughnessPolygonCollection.addNew( IRoughnessPolygon.QNAME );
            m_NumberOfEntriesAdded++;
            roughnessPolygon.setSurface( surfaces[k] );
            m_data.getRoughnessShapeStaticRelationMap().put( roughnessPolygon.getGmlID(), propertyValue );
          }
        }
        else if( gm_Whatever instanceof GM_Surface )
        {
          final IRoughnessPolygon roughnessPolygon = roughnessPolygonCollection.addNew( IRoughnessPolygon.QNAME );
          m_NumberOfEntriesAdded++;
          roughnessPolygon.setSurface( (GM_Object) gm_Whatever );
          m_data.getRoughnessShapeStaticRelationMap().put( roughnessPolygon.getGmlID(), propertyValue );
        }
        else
          throw new ClassCastException( "Type not supported: " + gm_Whatever.getClass().getName() );
      }
      else
        throw new ClassCastException( "Type not supported: " + gm_Whatever.getClass().getName() );
    }

    m_isDataPrepared = true;
  }

  public void unprepare( )
  {
    if( m_isDataPrepared )
    {
      for( int i = 0; i < m_NumberOfEntriesAdded; i++ )
        m_data.getRoughnessPolygonCollection().remove( m_data.getRoughnessPolygonCollection().size() - 1 );
    }
    m_isDataPrepared = false;
    m_NumberOfEntriesAdded = 0;
    m_data.deleteCreatedGMLLayer();
  }

  private void setSelectedRoughnessChoice( ) throws Exception
  {
    final GMLWorkspace shpWorkspace = GmlSerializer.createGMLWorkspace( m_data.getRoughnessDatabaseLocationURL(), null );
    final GMLWorkspace myWorkspace = m_data.getRoughnessPolygonCollection().getWrappedFeature().getWorkspace();
    for( String key : m_data.getRoughnessShapeStaticRelationMap().keySet() )
    {
      final Feature feature = myWorkspace.getFeature( key );
      final Feature linkedFeature = shpWorkspace.getFeature( m_data.getRoughnessShapeStaticRelationMap().get( key ) );
      if( linkedFeature != null )
      {
        final StringBuffer xlinkBuffer = new StringBuffer(50);
        xlinkBuffer.append( "project:" ).append( m_data.getRoughnessDatabaseLocation() ).append( "#" ).append( linkedFeature.getId() ).trimToSize();
        final XLinkedFeature_Impl linkedFeature_Impl = new XLinkedFeature_Impl( feature, linkedFeature.getParentRelation(), linkedFeature.getFeatureType(), xlinkBuffer.toString(), "", "", "", "", "" );
        feature.setProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_CLASS_MEMBER, linkedFeature_Impl );
      }
    }
    // use (dummy) command to make workspace dirty
    SzenarioDataProvider caseDataProvider = Util.getCaseDataProvider();
    if( caseDataProvider != null )
    {
      caseDataProvider.postCommand( ITerrainModel.class, new AddRoughnessPolygonsCmd() );
    }

  }

  private void serialize( )
  {

    final IRoughnessPolygonCollection roughnessPolygonCollection = m_data.getRoughnessPolygonCollection();
    final FeatureList wrappedList = roughnessPolygonCollection.getWrappedList();
    final Feature parentFeature = wrappedList.getParentFeature();
    final GMLWorkspace workspace = parentFeature.getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, parentFeature, (Feature) null, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    // fire event for roughness changes
    // TODO Patrice check it
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, parentFeature.getParent(), parentFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    // TODO: also post the adds as commands to the dataProvider

    //    
    // GisTemplateMapModell model = GisTemplateHelper.loadGisMapView( new File(absPath) );
    // new AddThemeCommand();
    // m_data.getProjectBaseFolder();
  }
}
