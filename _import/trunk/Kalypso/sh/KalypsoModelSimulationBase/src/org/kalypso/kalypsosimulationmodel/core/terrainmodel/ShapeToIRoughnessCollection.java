package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.awt.Color;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.StringReader;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import javax.xml.bind.JAXBException;
import javax.xml.namespace.QName;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.IOUtils;
import org.apache.log4j.Logger;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
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
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.Geometry;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.Literal;
import org.kalypsodeegree_impl.filterencoding.PropertyIsLikeOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.graphics.sld.FeatureTypeStyle_Impl;
import org.kalypsodeegree_impl.graphics.sld.Geometry_Impl;
import org.kalypsodeegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.graphics.sld.StyledLayerDescriptor_Impl;
import org.kalypsodeegree_impl.graphics.sld.UserStyle_Impl;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeaturePath;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

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
	private final HashSet<String> m_RoughnessNameSet = new HashSet<String>();

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


		final List<Style> styleList = element.getStyle();
		final Style style = new Style();

		// set attributes for the style
		String sldFileName = m_data.getOutputFile().substring(0, m_data.getOutputFile().lastIndexOf(".gml")) + ".sld";
		style.setLinktype( "sld" ); //$NON-NLS-1$
		style.setStyle( "Roughness style" ); //$NON-NLS-1$
		style.setActuate( "onRequest" ); //$NON-NLS-1$
		style.setHref( FileUtilities.nameFromPath(sldFileName) ); //$NON-NLS-1$
		style.setType( "simple" ); //$NON-NLS-1$    
		styleList.add( style );

		layers.getLayer().add(0, element);
		GisTemplateHelper.saveGisMapView( gismapview, writer, "UTF8" );
		writer.close();
		try
		{
			createCustomStyle(m_data.getOutputFile(), sldFileName, "Roughness style", null, "polygonProperty", "roughnessID");
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
	}

	/**
	 * Creates custom SLD file based on input GML file
	 * 
	 * @param inputGMLFile
	 * @param resultSLDFile
	 * @param styleLayerName
	 * @param knownPropertyColorSet
	 * @param geometryProperty - if null, default geometry property will be used
	 * @param filterProperty - if null, no filter will be created
	 * @throws TransformerFactoryConfigurationError
	 * @throws Exception
	 * 
	 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
	 */
	public static void createCustomStyle(String inputGMLFile, String resultSLDFile, String styleLayerName, HashMap<String, Color> knownPropertyColorSet, String geometryProperty, String filterProperty) throws TransformerFactoryConfigurationError, Exception {
		GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( new URL("file:"+inputGMLFile), null );
		String ns = workspace.getGMLSchema().getTargetNamespace();
		String geometryPropertyName = geometryProperty;
		boolean geometryPropertySet = (geometryPropertyName != null);
		IFeatureType rootFeatureType = workspace.getRootFeature().getFeatureType();
		IPropertyType[] propertyTypes = rootFeatureType.getProperties();
		HashMap<String, Color> filterPropertyColorMap = new HashMap<String, Color>();
		try
		{
			for(int i=0; i<propertyTypes.length; i++)
			{
				Object object = workspace.getRootFeature().getProperty(propertyTypes[i]);
				if(object instanceof FeatureList)
				{
					ListIterator<Feature> iterator = ((FeatureList) object).listIterator();
					while(iterator.hasNext())
					{
						Feature feature = iterator.next();
						try
						{
							if(!geometryPropertySet)
							{
								geometryPropertyName = feature.getFeatureType().getDefaultGeometryProperty().getQName().getLocalPart();
								geometryPropertySet = true;
							}
							String styledOne = feature.getProperty(new QName(ns, filterProperty)).toString();
							if(!filterPropertyColorMap.containsKey(styledOne))
							{
								// don't want to put the same color twice...
								Color color = getRandomColor();
								while(filterPropertyColorMap.containsValue(color))
									color = getRandomColor();
								//
								filterPropertyColorMap.put(styledOne, color);
							}
						}
						catch(Exception e){
							System.out.println(e.getLocalizedMessage());
							continue;
						}
					}

				}
			}
			createStyles(styleLayerName, geometryPropertyName, filterProperty, filterPropertyColorMap, resultSLDFile);
		}
		catch(Exception e) {
			e.printStackTrace();
		}
	}

	private static final void createStyles(String styleLayerName, String geometryProperty, String filterProperty, HashMap<String, Color> filterPropertyColorMap, String resultSLDFile) throws IOException, SAXException, TransformerFactoryConfigurationError, TransformerException
	{
		final Logger logger = Logger.getLogger(ShapeToIRoughnessCollection.class);
		
		final double minScaleDenominator = 0;
		final double maxScaleDenominator = 1000000000000000.0; 
		Stroke stroke = StyleFactory.createStroke(new Color(0,0,0), 1.0, 0.5, null, "mitre", "butt");
		FeatureTypeStyle featureTypeStyle = new FeatureTypeStyle_Impl();
		Fill fill = null;
		Iterator iterator = filterPropertyColorMap.entrySet().iterator();
		while(iterator.hasNext())
		{
			Map.Entry<String, Color> mapEntry = (Map.Entry<String, Color>) iterator.next();
			Geometry geometry = new Geometry_Impl( geometryProperty );
			fill = StyleFactory.createFill( mapEntry.getValue(), 0.8 );
			PolygonSymbolizer polygonSymbolizer = new PolygonSymbolizer_Impl(fill, stroke, geometry, minScaleDenominator, maxScaleDenominator);
			Symbolizer[] symbolizers = new Symbolizer[] { polygonSymbolizer };
			Operation operation = new PropertyIsLikeOperation(new PropertyName(filterProperty), new Literal(mapEntry.getKey()), '*', '$', '/');
			Filter filter = new ComplexFilter(operation);
			Rule rule = StyleFactory.createRule( symbolizers, "default", "default", "default", minScaleDenominator, maxScaleDenominator ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			rule.setFilter( filter );
			featureTypeStyle.addRule( rule );
		}

		FeatureTypeStyle[] featureTypeStyles = new FeatureTypeStyle[] { featureTypeStyle };
		org.kalypsodeegree.graphics.sld.Style[] styles = new org.kalypsodeegree.graphics.sld.Style[] { new UserStyle_Impl( styleLayerName, styleLayerName, null, false, featureTypeStyles ) };
		org.kalypsodeegree.graphics.sld.Layer[] layers = new org.kalypsodeegree.graphics.sld.Layer[] { SLDFactory.createNamedLayer( "deegree style definition", null, styles ) }; //$NON-NLS-1$
		StyledLayerDescriptor sld = SLDFactory.createStyledLayerDescriptor( layers, "1.0" ); //$NON-NLS-1$
		Document doc = XMLTools.parse( new StringReader( ((StyledLayerDescriptor_Impl) sld).exportAsXML() ) );
		final Source source = new DOMSource( doc );
		OutputStream os = null;
		try
		{
			os = new FileOutputStream( resultSLDFile );
			StreamResult result = new StreamResult( os );
			Transformer t = TransformerFactory.newInstance().newTransformer();
			t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" ); //$NON-NLS-1$ //$NON-NLS-2$
			t.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$
			t.transform( source, result );
		}
		finally
		{
			IOUtils.closeQuietly( os );
		}
	}

	private static final Color getRandomColor() {
		int r = (int)(255.0 * Math.random());
		int g = (int)(255.0 * Math.random());
		int b = (int)(255.0 * Math.random());
		return new Color(r,g,b);
	}

}
