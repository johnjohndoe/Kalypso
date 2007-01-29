package org.kalypso.ui.shapeImportWizards.utils.importRoughness;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
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
import org.kalypso.template.types.ExtentType;
import org.kalypso.ui.shapeImportWizards.utils.MapUtils;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeaturePath;

/**
 * Implements the transformation algorithm from a shape file into a IRoughnessPolygonCollection
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class TransformerShapeToIRoughnessCollection implements ICoreRunnableWithProgress
{

  private DataContainer m_data;

  private GMLWorkspace m_Workspace;

  private FeaturePath m_FeaturePath;

  private ExtentType m_ExtentType;

  private static final QName m_RootFeatureQName = KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON_COLLECTION;

  private static final QName m_RootPropertyMemberQName = KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_LAYER_POLYGON;

  private static final QName m_GeometryFeatureQName = KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON;

  public TransformerShapeToIRoughnessCollection( DataContainer data )
  {
    m_data = data;
  }

  public IStatus execute( IProgressMonitor monitor ) throws CoreException, InvocationTargetException, InterruptedException
  {
    try
    {
      monitor.beginTask( Messages.getString( "ShapeToIRoughnessCollection.0" ), 100 ); //$NON-NLS-1$
      monitor.worked( 10 );
      monitor.subTask( Messages.getString( "ShapeToIRoughnessCollection.1" ) ); //$NON-NLS-1$
      try
      {
        transform( monitor );
        if( monitor.isCanceled() )
          return Status.CANCEL_STATUS;
      }
      catch( ClassCastException e )
      {
        return new Status( Status.ERROR, KalypsoCorePlugin.getDefault().getID(), Status.CANCEL, e.getMessage(), e );
        // monitor.setCanceled(true);
        // return Status.CANCEL_STATUS;
      }
      if( m_data.doCreateMap() )
      {
        monitor.subTask( Messages.getString( "ShapeToIRoughnessCollection.2" ) ); //$NON-NLS-1$
        String baseName = m_data.getOutputFile().substring( 0, m_data.getOutputFile().lastIndexOf( ".gml" ) ); //$NON-NLS-1$
        MapUtils mapUtils = new MapUtils( baseName + ".gmt", //$NON-NLS-1$
        m_data.getOutputFile(), baseName + ".sld", //$NON-NLS-1$
        m_data.getOutputFileRelativePath(), m_FeaturePath.toString(), m_ExtentType, "Roughness", //$NON-NLS-1$
        "Roughness style", //$NON-NLS-1$
        "polygonProperty", //$NON-NLS-1$
        "roughnessID", //$NON-NLS-1$
        m_data.getFilterPropertyColorMap() );
        mapUtils.createMap( true );
      }
      monitor.done();
      m_data.getProject().refreshLocal( IResource.DEPTH_INFINITE, null );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    return Status.OK_STATUS;
  }

  /**
   * Reads (ArcView) roughness shape data and creates corresponding GML output. Output is created based on <CODE>http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase</CODE>
   * namespace
   * 
   * @throws GmlSerializeException -
   *           if input shape file cannot be deserialized
   * @throws InvocationTargetException -
   *           if target workspace cannot be created
   * @throws IOException -
   *           if output file cannot be created/opened for writing
   * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
   */
  public void transform( IProgressMonitor monitor ) throws GmlSerializeException, InvocationTargetException, IOException
  {
    // extent boundaries
    double top = Double.MIN_VALUE, right = Double.MIN_VALUE, bottom = Double.MAX_VALUE, left = Double.MAX_VALUE;

    QName shpFeatureName = new QName( "namespace", "featureMember" ); //$NON-NLS-1$ //$NON-NLS-2$
    QName shpGeomPropertyName = new QName( "namespace", "GEOM" ); //$NON-NLS-1$ //$NON-NLS-2$
    QName shpCustomPropertyName = new QName( "namespace", m_data.getShapeProperty() ); //$NON-NLS-1$

    final GMLWorkspace shapeWorkSpace = ShapeSerializer.deserialize( FileUtilities.nameWithoutExtension( m_data.getInputFile() ), m_data.getCoordinateSystem( true ) );
    monitor.worked( 30 );
    if( monitor.isCanceled() )
      return;
    Feature shapeRootFeature = shapeWorkSpace.getRootFeature();
    List shapeFeatureList = (List) shapeRootFeature.getProperty( shpFeatureName );

    m_Workspace = FeatureFactory.createGMLWorkspace( m_RootFeatureQName, m_data.getOutputFileURL(), GmlSerializer.DEFAULT_FACTORY );
    RoughnessPolygonCollection roughnessPolygonCollection = new RoughnessPolygonCollection( m_Workspace.getRootFeature(), IRoughnessPolygon.class, m_RootPropertyMemberQName );

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
      roughnessPolygon.setRoughnessID( propertyValue );
      if( right < gm_Surface.getEnvelope().getMax().getX() )
        right = gm_Surface.getEnvelope().getMax().getX();
      if( left > gm_Surface.getEnvelope().getMin().getX() )
        left = gm_Surface.getEnvelope().getMin().getX();
      if( top < gm_Surface.getEnvelope().getMax().getY() )
        top = gm_Surface.getEnvelope().getMax().getY();
      if( bottom > gm_Surface.getEnvelope().getMin().getY() )
        bottom = gm_Surface.getEnvelope().getMin().getY();
    }
    monitor.worked( 30 );
    if( monitor.isCanceled() )
      return;

    // Setting class values used by createMap function
    final Feature feature = roughnessPolygon.getWrappedFeature();
    final String typeName = "[" + feature.getFeatureType().getQName().getLocalPart() + "]"; //$NON-NLS-1$ //$NON-NLS-2$
    final String memberName = feature.getParentRelation().getQName().getLocalPart() + typeName;
    final FeaturePath featurePathToParent = new FeaturePath( feature.getParent() );
    m_FeaturePath = new FeaturePath( featurePathToParent, memberName );
    m_ExtentType = new ExtentType();
    m_ExtentType.setBottom( bottom );
    m_ExtentType.setTop( top );
    m_ExtentType.setLeft( left );
    m_ExtentType.setRight( right );
    m_ExtentType.setSrs( m_data.getCoordinateSystem( true ).getName() );
    // //

    // monitor.subTask("Serializing workspace...");
    FileWriter writer = new FileWriter( m_data.getOutputFile() );
    GmlSerializer.serializeWorkspace( writer, m_Workspace );
    writer.close();
    if( monitor.isCanceled() )
    {
      File file = new File( m_data.getOutputFile() );
      file.delete();
    }
  }

}
