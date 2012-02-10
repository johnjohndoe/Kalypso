package org.kalypso.ui.wizards.imports.roughness;

import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygon;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ui.wizards.i18n.Messages;
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
        return new Status( Status.ERROR, KalypsoCorePlugin.getID(), Status.CANCEL, e.getMessage(), e );
        // monitor.setCanceled(true);
        // return Status.CANCEL_STATUS;
      }
      if( hasMonitor )
        monitor.done();
      // m_data.getProject().refreshLocal( IResource.DEPTH_INFINITE, null );
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
      // m_data.getRoughnessPolygonCollection().clear();
    }
    final GMLWorkspace shapeWorkSpace = ShapeSerializer.deserialize( FileUtilities.nameWithoutExtension( m_data.getInputFile() ), m_data.getCoordinateSystem( true ) );
    final String customNamespace = shapeWorkSpace.getGMLSchema().getTargetNamespace();
    final QName shpGeomName = new QName( customNamespace, ShapeSerializer.PROPERTY_GEOM );
    final QName shpCustomPropertyName = new QName( customNamespace, m_data.getShapeProperty() );
    final Feature shapeRootFeature = shapeWorkSpace.getRootFeature();
    final List< ? > shapeFeatureList = (List< ? >) shapeRootFeature.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
    final IRoughnessPolygonCollection roughnessPolygonCollection = m_data.getRoughnessPolygonCollection();

    m_NumberOfEntriesAdded = 0;
    for( int i = 0; i < shapeFeatureList.size(); i++ )
    {
      final Feature shapeFeature = (Feature) shapeFeatureList.get( i );
      final String propertyValue = shapeFeature.getProperty( shpCustomPropertyName ).toString();
      final Object gm_Whatever = shapeFeature.getProperty( shpGeomName );
      if( gm_Whatever instanceof GM_Object )
      {
        if( gm_Whatever instanceof GM_MultiSurface )
        {
          final GM_MultiSurface multiSurface = (GM_MultiSurface) ((GM_MultiSurface) gm_Whatever).clone();
          final GM_Surface< ? >[] surfaces = multiSurface.getAllSurfaces();
          for( final GM_Surface< ? > element : surfaces )
          {
            final IRoughnessPolygon roughnessPolygon = roughnessPolygonCollection.addNew( IRoughnessPolygon.QNAME );
            m_NumberOfEntriesAdded++;
            roughnessPolygon.setSurface( element );
            m_data.getRoughnessShapeStaticRelationMap().put( roughnessPolygon.getGmlID(), propertyValue );
          }
        }
        else if( gm_Whatever instanceof GM_Surface )
        {
          final IRoughnessPolygon roughnessPolygon = roughnessPolygonCollection.addNew( IRoughnessPolygon.QNAME );
          m_NumberOfEntriesAdded++;
          roughnessPolygon.setSurface( (GM_Surface< ? >) gm_Whatever );
          m_data.getRoughnessShapeStaticRelationMap().put( roughnessPolygon.getGmlID(), propertyValue );
        }
        else
          throw new ClassCastException( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.Transformer.2" ) + gm_Whatever.getClass().getName() ); //$NON-NLS-1$
      }
      else
        throw new ClassCastException( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.Transformer.2" ) + gm_Whatever.getClass().getName() ); //$NON-NLS-1$
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
    final GMLWorkspace myWorkspace = m_data.getRoughnessPolygonCollection().getFeature().getWorkspace();
    for( final String key : m_data.getRoughnessShapeStaticRelationMap().keySet() )
    {
      final Feature feature = myWorkspace.getFeature( key );
      final Feature linkedFeature = shpWorkspace.getFeature( m_data.getRoughnessShapeStaticRelationMap().get( key ) );
      if( linkedFeature != null )
      {
        final StringBuffer xlinkBuffer = new StringBuffer( 50 );
        xlinkBuffer.append( "project:" ).append( m_data.getRoughnessDatabaseLocation() ).append( "#" ).append( linkedFeature.getId() ).trimToSize(); //$NON-NLS-1$ //$NON-NLS-2$
        final XLinkedFeature_Impl linkedFeature_Impl = new XLinkedFeature_Impl( feature, linkedFeature.getParentRelation(), linkedFeature.getFeatureType(), xlinkBuffer.toString(), "", "", "", "", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        feature.setProperty( RoughnessPolygon.SIM_BASE_PROP_ROUGHNESS_CLASS_MEMBER, linkedFeature_Impl );
      }
    }
    // use (dummy) command to make workspace dirty
    final SzenarioDataProvider caseDataProvider = Util.getCaseDataProvider();
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
