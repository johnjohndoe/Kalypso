package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.LinkedList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
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
public class ShapeToIRoughnessCollection 
{	
	/**
	 * Utility for fetching GML schema from global schema catalog
	 * 
	 * @param schemaNamespace - schema to be fetched
	 * @param version - version of GML schema; if this parameter is <code>null</code>, default value "3.1" is used 
	 * @return GMLschema object, or <code>null</code> in case of exception
	 * 
	 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
	 */
	private static GMLSchema getGMLSchema( String schemaNamespace, String version )
	{
		final String ver = (version!=null)?version:"3.1";
		try {
			return KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog().getSchema( schemaNamespace, ver );
		} catch (InvocationTargetException e) {
			//e.printStackTrace();
			return null;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
	
	/**
	 * Utility function for creating IRoughnessPolygonCollection from ArcView shape data
	 * 
	 * @param inputFileURL - URL of input SHP file
	 * @param sourceCrs - coordinate sistem used by input SHP file
	 * @param shpCustomProperty - (roughness) property to read from SHP data
	 * @throws GmlSerializeException - if input file cannot be deserialized
	 *  
	 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
	 */
	public static List<IRoughnessPolygon> transform2List( URL inputFileURL, CS_CoordinateSystem sourceCrs, String shpCustomProperty) throws GmlSerializeException
	{
		final GMLWorkspace 	workSpace 	= ShapeSerializer.deserialize(FileUtilities.nameWithoutExtension(inputFileURL.getPath()), sourceCrs);
		GMLSchema schema = null;
		schema = getGMLSchema(UrlCatalogModelSimulationBase.SIM_MODEL_NS, null);

		QName shpFeatureName = new QName( "namespace", "featureMember" ); //$NON-NLS-1$ //$NON-NLS-2$
		QName shpGeomPropertyName = new QName( "namespace", "GEOM" ); //$NON-NLS-1$ //$NON-NLS-2$
		QName shpCustomPropertyName = new QName( "namespace", shpCustomProperty ); //$NON-NLS-1$
		
		final IFeatureType rootFT = schema.getFeatureType(new QName(UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RoughnessLayerPolygonCollection"));
		final IFeatureType polygonFT = schema.getFeatureType(new QName(UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RoughnessPolygon"));
		
		Feature rootFeature = FeatureFactory.createFeature( null, rootFT.getQName() + "0", rootFT, true ); //$NON-NLS-1$
		//rootFeature.setProperty(featureMemberProp, FeatureFactory.createFeatureList(rootFeature, ((IRelationType)polygonFT)));
		
		List<IRoughnessPolygon> list = new LinkedList<IRoughnessPolygon>();
		
		Feature shapeRootFeature = workSpace.getRootFeature();
		List featureList = (List) shapeRootFeature.getProperty( shpFeatureName );
		for( int i = 0; i < featureList.size(); i++ )
		{
			final Feature polygonFeature = FeatureFactory.createFeature( rootFeature, polygonFT.getQName() + String.valueOf(i), polygonFT, true ); //$NON-NLS-1$
			RoughnessPolygon roughnessPolygon = new RoughnessPolygon(polygonFeature);
			final Feature feat = (Feature) featureList.get( i );
			final String propertyValue = (String) feat.getProperty( shpCustomPropertyName );
			final GM_Surface gm_Surface = (GM_Surface) feat.getProperty( shpGeomPropertyName );

			roughnessPolygon.setPolygon(gm_Surface);
			roughnessPolygon.setRougthnessID(propertyValue);
			
			list.add(roughnessPolygon);
		}
		return list;
	}

	/**
	 * Utility function for creating IRoughnessPolygonCollection from ArcView shape data
	 * 
	 * @param inputFileURL - URL of input SHP file
	 * @param sourceCrs - coordinate sistem used by input SHP file
	 * @param shpCustomProperty - (roughness) property to read from SHP data
	 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
	 * @throws Exception 
	 */
	public static IRoughnessPolygonCollection transform( URL inputFileURL, CS_CoordinateSystem sourceCrs, String shpCustomProperty) throws Exception
	{
//		List list = transform2List(inputFileURL, sourceCrs, shpCustomProperty);
		
		final GMLWorkspace 	workSpace 	= ShapeSerializer.deserialize(FileUtilities.nameWithoutExtension(inputFileURL.getPath()), sourceCrs);
		GMLSchema schema = null;
		schema = getGMLSchema(UrlCatalogModelSimulationBase.SIM_MODEL_NS, null);

		QName shpFeatureName = new QName( "namespace", "featureMember" ); //$NON-NLS-1$ //$NON-NLS-2$
		QName shpGeomPropertyName = new QName( "namespace", "GEOM" ); //$NON-NLS-1$ //$NON-NLS-2$
		QName shpCustomPropertyName = new QName( "namespace", shpCustomProperty ); //$NON-NLS-1$
		
		QName featureMemberProp = new QName(UrlCatalogModelSimulationBase.SIM_MODEL_NS, "roughnessLayerMember");

		final IFeatureType rootFT = schema.getFeatureType(new QName(UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RoughnessLayerPolygonCollection"));
		final IFeatureType polygonFT = schema.getFeatureType(new QName(UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RoughnessPolygon"));
		
		Feature rootFeature = FeatureFactory.createFeature( null, rootFT.getQName() + "0", rootFT, true ); //$NON-NLS-1$
//		rootFeature.setProperty(featureMemberProp, FeatureFactory.createFeatureList(rootFeature, ((IRelationType)polygonFT)));
		
		
		
		
		RoughnessPolygonCollection roughnessPolygonCollection = new RoughnessPolygonCollection(rootFeature, IRoughnessPolygon.class, featureMemberProp);

		Feature shapeRootFeature = workSpace.getRootFeature();
		List featureList = (List) shapeRootFeature.getProperty( shpFeatureName );
		for( int i = 0; i < featureList.size(); i++ )
		{
			final Feature polygonFeature = FeatureFactory.createFeature( rootFeature, polygonFT.getQName() + String.valueOf(i), polygonFT, true ); //$NON-NLS-1$
			RoughnessPolygon roughnessPolygon = new RoughnessPolygon(polygonFeature);
			final Feature feat = (Feature) featureList.get( i );
			final String propertyValue = (String) feat.getProperty( shpCustomPropertyName );
			final GM_Surface gm_Surface = (GM_Surface) feat.getProperty( shpGeomPropertyName );

			roughnessPolygon.setPolygon(gm_Surface);
			roughnessPolygon.setRougthnessID(propertyValue);
			
			roughnessPolygonCollection.add(roughnessPolygon);
		}
		return roughnessPolygonCollection;
	}
}
