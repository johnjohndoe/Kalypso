package org.kalypso.ui.wizards.imports.roughness;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ui.wizards.imports.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * Implements the transformation algorithm from a shape file into a IRoughnessPolygonCollection
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Transformer implements ICoreRunnableWithProgress
{
  private DataContainer m_data;

  private static final QName m_GeometryFeatureQName = KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON;

  private boolean isDataPrepared = false;

  public Transformer( DataContainer data )
  {
    m_data = data;
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
        if( !isDataPrepared )
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

  public void prepare( boolean resetMap ) throws Exception
  {
    if( resetMap )
    {
      m_data.getRoughnessShapeStaticRelationMap().clear();
      m_data.getRoughnessPolygonCollection().clear();
    }
    QName shpFeatureName = new QName( "namespace", "featureMember" ); //$NON-NLS-1$ //$NON-NLS-2$
    QName shpGeomPropertyName = new QName( "namespace", "GEOM" ); //$NON-NLS-1$ //$NON-NLS-2$
    QName shpCustomPropertyName = new QName( "namespace", m_data.getShapeProperty() ); //$NON-NLS-1$
    final GMLWorkspace shapeWorkSpace = ShapeSerializer.deserialize( FileUtilities.nameWithoutExtension( m_data.getInputFile() ), m_data.getCoordinateSystem( true ) );
    Feature shapeRootFeature = shapeWorkSpace.getRootFeature();
    List shapeFeatureList = (List) shapeRootFeature.getProperty( shpFeatureName );
    RoughnessPolygonCollection roughnessPolygonCollection = m_data.getRoughnessPolygonCollection();
    
    for( int i = 0; i < shapeFeatureList.size(); i++ )
    {
      final IRoughnessPolygon roughnessPolygon = roughnessPolygonCollection.addNew( m_GeometryFeatureQName );
      final Feature shapeFeature = (Feature) shapeFeatureList.get( i );
      final String propertyValue = shapeFeature.getProperty( shpCustomPropertyName ).toString();
      final Object gm_Whatever = shapeFeature.getProperty( shpGeomPropertyName );
      if( gm_Whatever instanceof GM_Object )
        roughnessPolygon.setSurface( (GM_Object)gm_Whatever );
      else
        throw new ClassCastException( "Type not supported: " + gm_Whatever.getClass().getName() );
      m_data.getRoughnessShapeStaticRelationMap().put( roughnessPolygon.getGmlID(), propertyValue );
    }

    isDataPrepared = true;
  }

  private void setSelectedRoughnessChoice( ) throws Exception
  {
    GMLWorkspace shpWorkspace = GmlSerializer.createGMLWorkspace( m_data.getRoughnessDatabaseLocationURL(), null );
    GMLWorkspace myWorkspace = m_data.getRoughnessPolygonCollection().getWrappedFeature().getWorkspace();
    for( String key : m_data.getRoughnessShapeStaticRelationMap().keySet() )
    {
      Feature f = myWorkspace.getFeature( key );
      Feature linkedFeature = shpWorkspace.getFeature( m_data.getRoughnessShapeStaticRelationMap().get( key ) );
      if( linkedFeature != null )
      {
        XLinkedFeature_Impl linkedFeature_Impl = new XLinkedFeature_Impl( f, linkedFeature.getParentRelation(), linkedFeature.getFeatureType(), "project:" + m_data.getRoughnessDatabaseLocation()
            + "#" + linkedFeature.getId(), "", "", "", "", "" );
        f.setProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_CLASS_MEMBER, linkedFeature_Impl );
      }
    }
  }

  private void serialize( ) throws IOException, GmlSerializeException
  {
    final RoughnessPolygonCollection roughnessPolygonCollection = m_data.getRoughnessPolygonCollection();
    GMLWorkspace myWorkspace = roughnessPolygonCollection.getWrappedFeature().getWorkspace();
    String relPath = File.separator + m_data.getProjectBaseFolder() + File.separator + "Basis" + File.separator + "models" + File.separator + "terrain.gml";
    String absPath = ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString() + relPath;
    FileWriter writer = new FileWriter( absPath );
    // TODO comment this out, not needed
    GmlSerializer.serializeWorkspace( writer, myWorkspace );
    writer.close();
    relPath = File.separator + m_data.getProjectBaseFolder() + File.separator + "Basis" + File.separator + "maps" + File.separator + "base.gmt";
    absPath = ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString() + relPath;

    final FeatureList wrappedList = roughnessPolygonCollection.getWrappedList();
    final Feature parentFeature = wrappedList.getParentFeature();
    final GMLWorkspace workspace = parentFeature.getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, parentFeature, (Feature)null, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    // TODO: also post the adds as commands to the dataProvider

    //    
    // GisTemplateMapModell model = GisTemplateHelper.loadGisMapView( new File(absPath) );
    // new AddThemeCommand();
    // m_data.getProjectBaseFolder();
  }
}
