package org.kalypso.ui.wizards.imports.roughness;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygon;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree_impl.gml.binding.shape.AbstractShape;
import org.kalypsodeegree_impl.gml.binding.shape.ShapeCollection;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * Implements the transformation algorithm from a shape file into a IRoughnessPolygonCollection
 *
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Transformer implements ICoreRunnableWithProgress
{
  private final DataContainer m_data;

  private boolean m_isDataPrepared = false;

  private int m_NumberOfEntriesAdded = -1;

  public Transformer( final DataContainer data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final boolean hasMonitor = monitor != null;
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
        m_data.createNewGMLLayer();
        if( !m_isDataPrepared )
          prepare( true );
        setSelectedRoughnessChoice();
        serialize();
        if( hasMonitor && monitor.isCanceled() )
          return Status.CANCEL_STATUS;
      }
      catch( final ClassCastException e )
      {
        // FIXME: show error to user!
        return new Status( Status.ERROR, KalypsoCorePlugin.getID(), Status.CANCEL, e.getMessage(), e );
      }
      if( hasMonitor )
        monitor.done();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return new Status( Status.ERROR, KalypsoCorePlugin.getID(), Status.CANCEL, e.getMessage(), e );
    }
    return Status.OK_STATUS;
  }

  public void prepare( final boolean resetMap ) throws Exception
  {
    if( resetMap )
    {
      m_data.getRoughnessShapeStaticRelationMap().clear();
    }

    final ShapeCollection shapeCollection = ShapeSerializer.deserialize( FileUtilities.nameWithoutExtension( m_data.getInputFile() ), m_data.getCoordinateSystem( true ) );

    final IFeatureBindingCollection<AbstractShape> shapes = shapeCollection.getShapes();

    final IRoughnessPolygonCollection roughnessPolygonCollection = m_data.getRoughnessPolygonCollection();

    m_NumberOfEntriesAdded = 0;
    for( int i = 0; i < shapes.size(); i++ )
    {
      final AbstractShape shapeFeature = shapes.get( i );
      final String propertyValue = shapeFeature.getProperty( m_data.getShapeProperty() ).toString();
      final GM_Object gm_Whatever = shapeFeature.getGeometry();

      if( gm_Whatever instanceof GM_MultiSurface )
      {
        final GM_MultiSurface multiSurface = (GM_MultiSurface) ((GM_MultiSurface) gm_Whatever).clone();
        final GM_Polygon[] surfaces = multiSurface.getAllSurfaces();
        for( final GM_Polygon element : surfaces )
        {
          final IRoughnessPolygon roughnessPolygon = roughnessPolygonCollection.addNew( IRoughnessPolygon.QNAME );
          m_NumberOfEntriesAdded++;
          roughnessPolygon.setSurface( element );
          m_data.getRoughnessShapeStaticRelationMap().put( roughnessPolygon.getId(), propertyValue );
        }
      }
      else
      {
        // FIXME: mega ugly: instead, the wizard should prevent non-polygon shapes to be loaded
        throw new ClassCastException( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.Transformer.2" ) + gm_Whatever.getClass().getName() ); //$NON-NLS-1$
      }
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
    final GMLWorkspace myWorkspace = m_data.getRoughnessPolygonCollection().getParentFeature().getWorkspace();
    for( final String key : m_data.getRoughnessShapeStaticRelationMap().keySet() )
    {
      final Feature feature = myWorkspace.getFeature( key );
      final Feature linkedFeature = shpWorkspace.getFeature( m_data.getRoughnessShapeStaticRelationMap().get( key ) );
      if( linkedFeature != null )
      {
        RoughnessPolygon.createClassLink( feature, RoughnessPolygon.MEMBER_ROUGHNESS_CLASS, linkedFeature );
      }
    }
    // use (dummy) command to make workspace dirty
    final IScenarioDataProvider caseDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    if( caseDataProvider != null )
    {
      caseDataProvider.postCommand( ITerrainModel.class.getName(), new AddRoughnessPolygonsCmd() );
    }
  }

  private void serialize( )
  {
    final IRoughnessPolygonCollection roughnessPolygonCollection = m_data.getRoughnessPolygonCollection();
    final FeatureList wrappedList = roughnessPolygonCollection.getFeatureList();
    final Feature parentFeature = wrappedList.getOwner();
    final GMLWorkspace workspace = parentFeature.getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, parentFeature, (Feature) null, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    // fire event for roughness changes
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, parentFeature.getOwner(), parentFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
  }
}
