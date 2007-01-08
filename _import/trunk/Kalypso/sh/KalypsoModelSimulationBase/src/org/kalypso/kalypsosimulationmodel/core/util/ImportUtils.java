package org.kalypso.kalypsosimulationmodel.core.util;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygon;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Utility class for importing SHP data
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 *
 */
public class ImportUtils
{
	//private static final String m_shapeSchemaNamespace = "http://www.tuhh.de/Kalypso1D2D/shapeData";
	private static final String m_shapeSchemaNamespace = "http://www.tu-harburg.de/wb/kalypso/schemata/simulationbase";

	/**
	 * Utility for fetching GML schema from global schema catalog
	 * 
	 * @param schemaNamespace - schema to be fetched
	 * @param version - version of GML schema; if this parameter is <code>null</code>, default value "3.1" is used 
	 * @return GMLschema object, or <code>null</code> in case of InvocationTargetException
	 * 
	 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
	 */
	public static GMLSchema getGMLSchema( String schemaNamespace, String version )
	{
		final String ver = (version!=null)?version:"3.1";
		try {
			return KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog().getSchema( schemaNamespace, ver );
		} catch (InvocationTargetException e) {
			//e.printStackTrace();
			return null;
		}
	}
	/**
	 * Utility function for creating GML document based on ArcView shape data
	 * 
	 * @param inputFileURL - URL of input SHP file
	 * @param sourceCrs - coordinate sistem used by input SHP file
	 * @param outputFileURL - URL of output GML file
	 * @param shpCustomProperty - property to extract from SHP data
	 * @throws GmlSerializeException - if input SHP file cannot be deserialized, or output GML cannot be serialized (for any reason)
	 * @throws IOException - if output file cannot be created/opened for writing
	 * 
	 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
	 */
	public static void convertShp2Gml( URL inputFileURL, CS_CoordinateSystem sourceCrs, URL outputFileURL, String shpCustomProperty) throws IOException, GmlSerializeException
	{
	    File outputFile = new File(outputFileURL.getPath());
	    final GMLWorkspace 	workSpace 	= ShapeSerializer.deserialize(FileUtilities.nameWithoutExtension(inputFileURL.getPath()), sourceCrs);
		GMLSchema schema = null;
		schema = getGMLSchema(m_shapeSchemaNamespace, null);
		
		QName shpFeatureName = new QName( "namespace", "featureMember" ); //$NON-NLS-1$ //$NON-NLS-2$
		QName shpGeomPropertyName = new QName( "namespace", "GEOM" ); //$NON-NLS-1$ //$NON-NLS-2$
		QName shpCustomPropertyName = new QName( "namespace", shpCustomProperty ); //$NON-NLS-1$

	    final IFeatureType rootFT = schema.getFeatureType(new QName(m_shapeSchemaNamespace, "RoughnessLayerPolygonCollection", ""));
	    final IFeatureType polygonFT = schema.getFeatureType(new QName(m_shapeSchemaNamespace, "RoughnessPolygonType", ""));
	    
	    Feature rootFeature = FeatureFactory.createFeature( null, rootFT.getQName() + "0", rootFT, true ); //$NON-NLS-1$
	    Feature polygonFeature = FeatureFactory.createFeature( rootFeature, polygonFT.getQName() + "0", polygonFT, true ); //$NON-NLS-1$
	    int defaultGeometryPropertyPosition = polygonFT.getDefaultGeometryPropertyPosition();
	    IPropertyType ftp_feature = polygonFT.getProperties(defaultGeometryPropertyPosition);
	    
	    RoughnessPolygon roughnessPolygon = new RoughnessPolygon(polygonFeature);
	    
	    
	    Feature shapeRootFeature = workSpace.getRootFeature();
	    List featureList = (List) shapeRootFeature.getProperty( shpFeatureName );
	    for( int i = 0; i < featureList.size(); i++ )
	    {
	      final Feature feat = (Feature) featureList.get( i );
	      final String propertyValue = (String) feat.getProperty( shpCustomPropertyName );
	      Object[] properties = new Object[] { null, null, null, null, null, (GM_Object) feat.getProperty( shpGeomPropertyName ), propertyValue };
	      //IRelationType aaa = (IRelationType) ftp_feature;
	      //IFeatureType  bbb = aaa.getTargetFeatureType();
	      //((PropertyType)ftp_feature).getTypeHandler().getTypeName()
	      
	      
	      //RelationType
	      // TODO nekako napravi da procitas IFeatureType iz ftp_feature (Jebe ga CustomPropertyType)
	      
	      final Feature feature = FeatureFactory.createFeature( rootFeature, "ShapeData" + i, ((IRelationType) ftp_feature).getTargetFeatureType(), properties ); //$NON-NLS-1$
	      //final Feature feature = FeatureFactory.createFeature( rootFeature, "ShapeData" + i, ((IRelationType) ftp_feature).getTargetFeatureType(), properties ); //$NON-NLS-1$
	      FeatureHelper.addProperty( rootFeature, ftp_feature, feature );
	    }
	    final GMLWorkspace workspace = new GMLWorkspace_Impl( schema, schema.getAllFeatureTypes(), rootFeature, outputFileURL, "", null ); //$NON-NLS-1$
	    FileWriter fw = new FileWriter( outputFile );
	    GmlSerializer.serializeWorkspace( fw, workspace );
	    fw.close();
	}
	
	
}
