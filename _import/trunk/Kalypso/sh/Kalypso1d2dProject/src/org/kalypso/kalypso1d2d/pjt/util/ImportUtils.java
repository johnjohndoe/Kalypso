package org.kalypso.kalypso1d2d.pjt.util;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.log4j.Logger;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.wizards.datatransfer.FileSystemImportWizard;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
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
	private static final Logger logger= Logger.getLogger(ImportUtils.class);

	public void importGML(Shell parentShell)
	{
		FileSystemImportWizard fsiWizard= new FileSystemImportWizard();

		WizardDialog wd= new WizardDialog(parentShell,fsiWizard);
		wd.setTitle("Import GML");
		int decision=wd.open();
		if(decision==WizardDialog.OK)
		{

		}
		else
		{
			logger.info("Wizard canceled:"+decision);
		}
	}
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
	 * Utility function for creating GML document based on ArcView shape data and certain GML schema
	 * 
	 * @param inputFileURL - URL of input SHP file
	 * @param sourceCrs - coordinate sistem used by input SHP file
	 * @param schemaNamespace - GML schema namespace used for creating GML data
	 * @param outputFileURL - URL of output GML file
	 * @param shpPropertyName - 
	 * @throws GmlSerializeException - if input SHP file cannot be deserialized, or output GML cannot be serialized (for any reason)
	 * @throws IOException - if output file cannot be created/opened for writing
	 * 
	 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
	 */
	public static void convertShp2Gml( URL inputFileURL, CS_CoordinateSystem sourceCrs, String schemaNamespace, URL outputFileURL, String shpPropertyName) throws IOException, GmlSerializeException
	{
	    File outputFile = new File(outputFileURL.getPath());
	    final GMLWorkspace 	workSpace 	= ShapeSerializer.deserialize(FileUtilities.nameWithoutExtension(inputFileURL.getPath()), sourceCrs);
		final GMLSchema 	schema 		= getGMLSchema(schemaNamespace, null);
		
		QName shpFeatureName = new QName( "namespace", "featureMember" ); //$NON-NLS-1$ //$NON-NLS-2$
		QName shpGeomPropertyName = new QName( "namespace", "GEOM" ); //$NON-NLS-1$ //$NON-NLS-2$
		QName shpCustomPropertyName = new QName( "namespace", shpPropertyName ); //$NON-NLS-1$

	    final IFeatureType[] types = schema.getAllFeatureTypes();
	    Feature rootFeature = FeatureFactory.createFeature( null, types[0].getQName() + "0", types[0], true ); //$NON-NLS-1$
	    IPropertyType ftp_feature = types[0].getProperties(0);
	    Feature shapeRootFeature = workSpace.getRootFeature();
	    List featureList = (List) shapeRootFeature.getProperty( shpFeatureName );
	    for( int i = 0; i < featureList.size(); i++ )
	    {
	      final Feature feat = (Feature) featureList.get( i );
	      final String propertyValue = (String) feat.getProperty( shpCustomPropertyName );
	      Object[] properties = new Object[] { null, null, null, null, null, (GM_Object) feat.getProperty( shpGeomPropertyName ), propertyValue };
	      final Feature feature = FeatureFactory.createFeature( rootFeature, "Feature" + i, ((IRelationType) ftp_feature).getTargetFeatureType(), properties ); //$NON-NLS-1$
	      FeatureHelper.addProperty( rootFeature, ftp_feature, feature );
	    }
	    final GMLWorkspace workspace = new GMLWorkspace_Impl( schema, types, rootFeature, outputFileURL, "", null ); //$NON-NLS-1$
	    FileWriter fw = new FileWriter( outputFile );
	    GmlSerializer.serializeWorkspace( fw, workspace );
	    fw.close();
	}
	
	
}
