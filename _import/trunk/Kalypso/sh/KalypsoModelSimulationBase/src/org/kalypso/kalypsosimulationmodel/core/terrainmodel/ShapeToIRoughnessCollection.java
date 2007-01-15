package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Implements the transformation algorithm from a shape file into a IRoughnessPolygonCollection
 * 
 * @author Dejan Antanaskovic, Patrice Congo
 */
public class ShapeToIRoughnessCollection extends Job
{	
	URL 				m_inputFileURL;
	URL 				m_outputFileURL;
	CS_CoordinateSystem m_shapeCS;
	String 				m_shapeProperty;
	
	public ShapeToIRoughnessCollection( String name, URL inputFileURL, URL outputFileURL, CS_CoordinateSystem shapeCS, String shapeProperty )
	{
		super(name);
		m_inputFileURL = inputFileURL;
		m_outputFileURL = outputFileURL;
		m_shapeCS = shapeCS;
		m_shapeProperty = shapeProperty;
	}
	

	@Override
	protected IStatus run(IProgressMonitor monitor) {
		try {
			transform(monitor, m_inputFileURL, m_shapeCS, m_shapeProperty, m_outputFileURL);
		} catch (GmlSerializeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return Status.OK_STATUS;
	}
	
//	/**
//	 * Utility for fetching GML schema from global schema catalog
//	 * 
//	 * @param schemaNamespace - schema to be fetched
//	 * @param version - version of GML schema; if this parameter is <code>null</code>, default value "3.1" is used 
//	 * @return GMLschema object, or <code>null</code> in case of exception
//	 * 
//	 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
//	 */
//	private static GMLSchema getGMLSchema( String schemaNamespace, String version )
//	{
//		final String ver = (version!=null)?version:"3.1";
//		try {
//			return KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog().getSchema( schemaNamespace, ver );
//		} catch (InvocationTargetException e) {
//			//e.printStackTrace();
//			return null;
//		} catch (Exception e) {
//			e.printStackTrace();
//			return null;
//		}
//	}
//	
//	/**
//	 * <b><u>DO NOT USE THIS FUNCTION, CREATED FOR TEST PURPOSE ONLY !!!</u></b><br><br>
//	 * 
//	 * Utility function for creating IRoughnessPolygonCollection from ArcView shape data
//	 * 
//	 * @param inputFileURL - URL of input SHP file
//	 * @param sourceCrs - coordinate sistem used by input SHP file
//	 * @param shpCustomProperty - (roughness) property to read from SHP data
//	 * @throws GmlSerializeException - if input file cannot be deserialized
//	 *  
//	 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
//	 */
//	@Deprecated
//	private static List<IRoughnessPolygon> transform2List( URL inputFileURL, CS_CoordinateSystem sourceCrs, String shpCustomProperty) throws GmlSerializeException
//	{
//		final GMLWorkspace 	workSpace 	= ShapeSerializer.deserialize(FileUtilities.nameWithoutExtension(inputFileURL.getPath()), sourceCrs);
//		GMLSchema schema = null;
//		schema = getGMLSchema(UrlCatalogModelSimulationBase.SIM_MODEL_NS, null);
//
//		QName shpFeatureName = new QName( "namespace", "featureMember" ); //$NON-NLS-1$ //$NON-NLS-2$
//		QName shpGeomPropertyName = new QName( "namespace", "GEOM" ); //$NON-NLS-1$ //$NON-NLS-2$
//		QName shpCustomPropertyName = new QName( "namespace", shpCustomProperty ); //$NON-NLS-1$
//		
//		final IFeatureType rootFT = schema.getFeatureType(new QName(UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RoughnessLayerPolygonCollection"));
//		final IFeatureType polygonFT = schema.getFeatureType(new QName(UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RoughnessPolygon"));
//		
//		Feature rootFeature = FeatureFactory.createFeature( null, rootFT.getQName() + "0", rootFT, true ); //$NON-NLS-1$
//		//rootFeature.setProperty(featureMemberProp, FeatureFactory.createFeatureList(rootFeature, ((IRelationType)polygonFT)));
//		
//		List<IRoughnessPolygon> list = new LinkedList<IRoughnessPolygon>();
//		
//		Feature shapeRootFeature = workSpace.getRootFeature();
//		List featureList = (List) shapeRootFeature.getProperty( shpFeatureName );
//		for( int i = 0; i < featureList.size(); i++ )
//		{
//			final Feature polygonFeature = FeatureFactory.createFeature( rootFeature, polygonFT.getQName() + String.valueOf(i), polygonFT, true ); //$NON-NLS-1$
//			RoughnessPolygon roughnessPolygon = new RoughnessPolygon(polygonFeature);
//			final Feature feat = (Feature) featureList.get( i );
//			final String propertyValue = (String) feat.getProperty( shpCustomPropertyName );
//			final GM_Surface gm_Surface = (GM_Surface) feat.getProperty( shpGeomPropertyName );
//
//			roughnessPolygon.setSurface(gm_Surface);
//			roughnessPolygon.setRoughnessID(propertyValue);
//			
//			list.add(roughnessPolygon);
//		}
//		return list;
//	}

	/**
	 * Reads (ArcView) roughness shape data and creates corresponding GML output. Output is created based on <CODE>http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase</CODE> namespace
	 * 
	 * @param inputFileURL - URL of input SHP file
	 * @param sourceCrs - coordinate sistem used by input SHP file; if omitted, default value is Gauss-Krueger (EPSG:31467)
	 * @param shpCustomProperty - (roughness) property to read from SHP data
	 * @param outputFileURL - URL of output GML file
	 * @throws GmlSerializeException - if input shape file cannot be deserialized 
	 * @throws InvocationTargetException - if target workspace cannot be created
	 * @throws IOException - if output file cannot be created/opened for writing
	 *  
	 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
	 */
	public static void transform(URL inputFileURL, CS_CoordinateSystem sourceCrs, String shpCustomProperty, URL outputFileURL) throws GmlSerializeException, InvocationTargetException, IOException
	{
		QName shpFeatureName = new QName( "namespace", "featureMember" ); //$NON-NLS-1$ //$NON-NLS-2$
		QName shpGeomPropertyName = new QName( "namespace", "GEOM" ); //$NON-NLS-1$ //$NON-NLS-2$
		QName shpCustomPropertyName = new QName( "namespace", shpCustomProperty ); //$NON-NLS-1$
		
		if(sourceCrs == null) sourceCrs = KalypsoModelSimulationBaseConsts.CS_GAUSS_KRUEGER;
//		sourceCrs = ConvenienceCSFactory.getInstance().getOGCCSByName("EPSG:31469");
		
		final GMLWorkspace shapeWorkSpace = ShapeSerializer.deserialize(FileUtilities.nameWithoutExtension(inputFileURL.getPath()), sourceCrs);
		Feature shapeRootFeature = shapeWorkSpace.getRootFeature();
		List shapeFeatureList = (List) shapeRootFeature.getProperty( shpFeatureName );

		final GMLWorkspace rpColWorkspace = FeatureFactory.createGMLWorkspace(KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON_COLLECTION, outputFileURL, GmlSerializer.DEFAULT_FACTORY);
		RoughnessPolygonCollection roughnessPolygonCollection = new RoughnessPolygonCollection(rpColWorkspace.getRootFeature(), IRoughnessPolygon.class, KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_LAYER_POLYGON);

		IRoughnessPolygon roughnessPolygon = null;
		Feature shapeFeature = null;
		
		for( int i = 0; i < shapeFeatureList.size(); i++ )
		{
			roughnessPolygon = roughnessPolygonCollection.addNew(KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON);
			shapeFeature = (Feature) shapeFeatureList.get( i );
			final String propertyValue = (String) shapeFeature.getProperty( shpCustomPropertyName );
			final GM_Surface gm_Surface = (GM_Surface) shapeFeature.getProperty( shpGeomPropertyName );
			roughnessPolygon.setSurface(gm_Surface);
			roughnessPolygon.setRoughnessID(propertyValue);
		}
		FileWriter writer = new FileWriter(outputFileURL.getPath());
		GmlSerializer.serializeWorkspace(writer, rpColWorkspace);
		writer.close();
	}

	/**
	 * Reads (ArcView) roughness shape data and creates corresponding GML output. Output is created based on <CODE>http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase</CODE> namespace
	 * 
	 * @param inputFileURL - URL of input SHP file
	 * @param sourceCrs - coordinate sistem used by input SHP file; if omitted, default value is Gauss-Krueger (EPSG:31467)
	 * @param shpCustomProperty - (roughness) property to read from SHP data
	 * @param outputFileURL - URL of output GML file
	 * @throws GmlSerializeException - if input shape file cannot be deserialized 
	 * @throws InvocationTargetException - if target workspace cannot be created
	 * @throws IOException - if output file cannot be created/opened for writing
	 *  
	 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
	 */
	public void transform(IProgressMonitor monitor, URL inputFileURL, CS_CoordinateSystem sourceCrs, String shpCustomProperty, URL outputFileURL) throws GmlSerializeException, InvocationTargetException, IOException
	{
	    int totalWork = 100;
	    monitor.beginTask( "Rintam...", totalWork ); //$NON-NLS-1$
		QName shpFeatureName = new QName( "namespace", "featureMember" ); //$NON-NLS-1$ //$NON-NLS-2$
		QName shpGeomPropertyName = new QName( "namespace", "GEOM" ); //$NON-NLS-1$ //$NON-NLS-2$
		QName shpCustomPropertyName = new QName( "namespace", shpCustomProperty ); //$NON-NLS-1$
		
		if(sourceCrs == null) sourceCrs = KalypsoModelSimulationBaseConsts.CS_GAUSS_KRUEGER;
//		sourceCrs = ConvenienceCSFactory.getInstance().getOGCCSByName("EPSG:31469");
		
		final GMLWorkspace shapeWorkSpace = ShapeSerializer.deserialize(FileUtilities.nameWithoutExtension(inputFileURL.getPath()), sourceCrs);
	      monitor.subTask( "subtask" ); //$NON-NLS-1$ //$NON-NLS-2$
	      monitor.worked( 10 );
		Feature shapeRootFeature = shapeWorkSpace.getRootFeature();
		List shapeFeatureList = (List) shapeRootFeature.getProperty( shpFeatureName );

		final GMLWorkspace rpColWorkspace = FeatureFactory.createGMLWorkspace(KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON_COLLECTION, outputFileURL, GmlSerializer.DEFAULT_FACTORY);
		RoughnessPolygonCollection roughnessPolygonCollection = new RoughnessPolygonCollection(rpColWorkspace.getRootFeature(), IRoughnessPolygon.class, KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_LAYER_POLYGON);

		IRoughnessPolygon roughnessPolygon = null;
		Feature shapeFeature = null;
		
		for( int i = 0; i < shapeFeatureList.size(); i++ )
		{
			roughnessPolygon = roughnessPolygonCollection.addNew(KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON);
			shapeFeature = (Feature) shapeFeatureList.get( i );
			final String propertyValue = (String) shapeFeature.getProperty( shpCustomPropertyName );
			final GM_Surface gm_Surface = (GM_Surface) shapeFeature.getProperty( shpGeomPropertyName );
			roughnessPolygon.setSurface(gm_Surface);
			roughnessPolygon.setRoughnessID(propertyValue);
		}
		FileWriter writer = new FileWriter(outputFileURL.getPath());
		GmlSerializer.serializeWorkspace(writer, rpColWorkspace);
		writer.close();
	    monitor.done();
	}
}
