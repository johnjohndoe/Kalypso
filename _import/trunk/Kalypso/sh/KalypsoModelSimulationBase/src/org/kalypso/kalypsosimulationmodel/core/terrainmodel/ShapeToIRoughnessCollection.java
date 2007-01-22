package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.namespace.QName;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.kalypsosimulationmodel.wizard.shapeImport.DataContainer;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.Gismapview.Layers;
import org.kalypso.template.types.ExtentType;
import org.kalypso.template.types.StyledLayerType;
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
public class ShapeToIRoughnessCollection extends Job
{	
	DataContainer		m_data;
	GMLWorkspace		m_Workspace;
	FeaturePath			m_FeaturePath;
	IFeatureType		m_FeatureType;
	ExtentType			m_ExtentType;
	
	public ShapeToIRoughnessCollection( String name, DataContainer data )
	{
		super(name);
		m_data = data;
	}
	
	@Override
	protected IStatus run(IProgressMonitor monitor) {
		try {
			transform(monitor);
			if(m_data.doCreateMap()) createMap();
			m_data.getProject().refreshLocal(IResource.DEPTH_INFINITE, null );
		} catch (Exception e) {
			e.printStackTrace();
		}
		return Status.OK_STATUS;
	}
	
	/**
	 * Reads (ArcView) roughness shape data and creates corresponding GML output. Output is created based on <CODE>http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase</CODE> namespace
	 * 
	 * @param monitor - progress monitor
	 * @throws GmlSerializeException - if input shape file cannot be deserialized 
	 * @throws InvocationTargetException - if target workspace cannot be created
	 * @throws IOException - if output file cannot be created/opened for writing
	 *  
	 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
	 */
	public void transform(IProgressMonitor monitor) throws GmlSerializeException, InvocationTargetException, IOException
	{
	    // extent boundaries
		double 	top = Double.MIN_VALUE, 
				right = Double.MIN_VALUE, 
				bottom = Double.MAX_VALUE, 
				left = Double.MAX_VALUE;
		
		monitor.beginTask( "Converting...", 100 ); //$NON-NLS-1$
		QName shpFeatureName = new QName( "namespace", "featureMember" ); //$NON-NLS-1$ //$NON-NLS-2$
		QName shpGeomPropertyName = new QName( "namespace", "GEOM" ); //$NON-NLS-1$ //$NON-NLS-2$
		QName shpCustomPropertyName = new QName( "namespace", m_data.getShapeProperty() ); //$NON-NLS-1$
		
		monitor.subTask("Deserializing shape data...");
		monitor.worked( 20 );
		final GMLWorkspace shapeWorkSpace = ShapeSerializer.deserialize(FileUtilities.nameWithoutExtension(m_data.getInputFile()), m_data.getCoordinateSystem(true));
		monitor.worked( 20 );
		Feature shapeRootFeature = shapeWorkSpace.getRootFeature();
		List shapeFeatureList = (List) shapeRootFeature.getProperty( shpFeatureName );

		monitor.subTask("Creating workspace...");
		m_Workspace = FeatureFactory.createGMLWorkspace(KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON_COLLECTION, m_data.getOutputFileURL(), GmlSerializer.DEFAULT_FACTORY);
		RoughnessPolygonCollection roughnessPolygonCollection = new RoughnessPolygonCollection(m_Workspace.getRootFeature(), IRoughnessPolygon.class, KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_LAYER_POLYGON);

		IRoughnessPolygon roughnessPolygon = null;
		Feature shapeFeature = null;
		
		monitor.subTask("Converting...");
		monitor.worked( 30 );
		for( int i = 0; i < shapeFeatureList.size(); i++ )
		{
			roughnessPolygon = roughnessPolygonCollection.addNew(KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON);
			shapeFeature = (Feature) shapeFeatureList.get( i );
			final String propertyValue = (String) shapeFeature.getProperty( shpCustomPropertyName );
			final GM_Surface gm_Surface = (GM_Surface) shapeFeature.getProperty( shpGeomPropertyName );
			roughnessPolygon.setSurface(gm_Surface);
			roughnessPolygon.setRoughnessID(propertyValue);
			if(right < gm_Surface.getEnvelope().getMax().getX())
				right = gm_Surface.getEnvelope().getMax().getX();
			if(left > gm_Surface.getEnvelope().getMin().getX())
				left = gm_Surface.getEnvelope().getMin().getX();
			if(top < gm_Surface.getEnvelope().getMax().getY())
				top = gm_Surface.getEnvelope().getMax().getY();
			if(bottom > gm_Surface.getEnvelope().getMin().getY())
				bottom = gm_Surface.getEnvelope().getMin().getY();
		}
		
		// Setting class values used by createMap function
		final Feature feature = roughnessPolygon.getWrappedFeature();
        final String typeName = "[" + feature.getFeatureType().getQName().getLocalPart() + "]";
        final String memberName = feature.getParentRelation().getQName().getLocalPart() + typeName;
        final FeaturePath featurePathToParent = new FeaturePath( feature.getParent() );
        m_FeatureType = feature.getFeatureType();
        m_FeaturePath = new FeaturePath( featurePathToParent, memberName );
        m_ExtentType = new ExtentType();
        m_ExtentType.setBottom(bottom);
        m_ExtentType.setTop(top);
        m_ExtentType.setLeft(left);
        m_ExtentType.setRight(right);
        m_ExtentType.setSrs(m_data.getCoordinateSystem(true).getName());
        ////
        
//		monitor.subTask("Serializing workspace...");
		monitor.worked( 60 );
		FileWriter writer = new FileWriter(m_data.getOutputFile());
		GmlSerializer.serializeWorkspace(writer, m_Workspace);
		writer.close();
	    monitor.done();
	}
	
	public void createMap() throws IOException, JAXBException {
		FileWriter writer = new FileWriter(m_data.getMapFileURL().getPath());
	    final Gismapview gismapview = GisTemplateHelper.emptyGisView();
	    gismapview.setExtent(m_ExtentType);
	    Layers layers = gismapview.getLayers();
	    StyledLayerType element = new StyledLayerType();
	    element.setId("layer_1");
	    element.setLinktype("gml");
	    element.setType("simple");
	    element.setName("Roughness");
	    element.setActuate("onRequest");
	    element.setFeaturePath( m_FeaturePath.toString() );
	    element.setHref( m_data.getOutputFileRelativePath() );
	    element.setVisible(true);
	    
//	    final List<Style> styleList = element.getStyle();
//	    final Style style = KalypsoGisPlugin.getDefaultStyleFactory().createDefaultStyle("Stil", null);
//	    //.createUserStyle( m_FeatureType, " - stil -"); //$NON-NLS-1$;
//	    styleList.add( style );
//	    
	    layers.getLayer().add(0, element);
	    GisTemplateHelper.saveGisMapView( gismapview, writer, "UTF8" );
	    writer.close();
	}
}
