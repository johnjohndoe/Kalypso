package org.kalypso.ui.wizards.imports.roughness;

import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

import javax.xml.namespace.QName;

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
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Surface;

/**
 * Implements the transformation algorithm from a shape file into a IRoughnessPolygonCollection
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class TransformerShapeToIRoughnessCollection implements ICoreRunnableWithProgress
{
  private DataContainer m_data;

  private static final QName m_RootPropertyMemberQName = KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_LAYER_POLYGON;

  private static final QName m_GeometryFeatureQName = KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON;

  public TransformerShapeToIRoughnessCollection( DataContainer data )
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
        monitor.beginTask( Messages.getString( "ShapeToIRoughnessCollection.0" ), 100 ); //$NON-NLS-1$
        monitor.worked( 10 );
        monitor.subTask( Messages.getString( "ShapeToIRoughnessCollection.1" ) ); //$NON-NLS-1$
      }
      try
      {
        prepare();
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
    }
    return Status.OK_STATUS;
  }

  private void prepare( ) throws GmlSerializeException
  {
    QName shpFeatureName = new QName( "namespace", "featureMember" ); //$NON-NLS-1$ //$NON-NLS-2$
    QName shpGeomPropertyName = new QName( "namespace", "GEOM" ); //$NON-NLS-1$ //$NON-NLS-2$
    QName shpCustomPropertyName = new QName( "namespace", m_data.getShapeProperty() ); //$NON-NLS-1$
    final GMLWorkspace shapeWorkSpace = ShapeSerializer.deserialize( FileUtilities.nameWithoutExtension( m_data.getInputFile() ), m_data.getCoordinateSystem( true ) );
    Feature shapeRootFeature = shapeWorkSpace.getRootFeature();
    List shapeFeatureList = (List) shapeRootFeature.getProperty( shpFeatureName );
    RoughnessPolygonCollection roughnessPolygonCollection = new RoughnessPolygonCollection( m_data.getWorkspace().getRootFeature(), IRoughnessPolygon.class, m_RootPropertyMemberQName );
    IRoughnessPolygon roughnessPolygon = null;
    Feature shapeFeature = null;
    for( int i = 0; i < shapeFeatureList.size(); i++ )
    {
      roughnessPolygon = roughnessPolygonCollection.addNew( m_GeometryFeatureQName );
      shapeFeature = (Feature) shapeFeatureList.get( i );
      final String propertyValue = (String) shapeFeature.getProperty( shpCustomPropertyName );
      final Object gm_Whatever = shapeFeature.getProperty( shpGeomPropertyName );
      GM_Surface gm_Surface = null;
      if( gm_Whatever instanceof GM_Surface )
        gm_Surface = (GM_Surface) gm_Whatever;
      else
        throw new ClassCastException( "Type not supported: " + gm_Whatever.getClass().getName() );
      roughnessPolygon.setSurface( gm_Surface );
      m_data.getRoughnessDatabaseMap().put( propertyValue, null );
    }
  }
  
  private void serialize() throws IOException, GmlSerializeException {
    FileWriter writer = new FileWriter( m_data.getWorkspace().getContext().getPath() );
    GmlSerializer.serializeWorkspace( writer, m_data.getWorkspace() );
    writer.close();
  }
}
