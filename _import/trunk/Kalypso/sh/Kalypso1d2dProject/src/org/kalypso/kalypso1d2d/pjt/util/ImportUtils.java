package org.kalypso.kalypso1d2d.pjt.util;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.apache.log4j.Logger;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.wizards.datatransfer.FileSystemImportWizard;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.opengis.cs.CS_CoordinateSystem;

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
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}
	}
	/**
	 * Utility function for converting ArcView shape data to GML
	 * 
	 * @param fileURL - URL of input SHP file
	 * @param schemaNamespace - schema for creating GML data
	 * @return StringBuffer containing GML formatted data
	 * @throws IOException
	 * @throws GmlSerializeException
	 * 
	 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
	 */
	public static void convertShp2Gml( String filePath, CS_CoordinateSystem sourceCrs, String schemaNamespace) throws IOException, GmlSerializeException
	{
	    File landuseDataGML = new File("./test.gml" ); //$NON-NLS-1$
		final GMLWorkspace 	workSpace 	= ShapeSerializer.deserialize(FileUtilities.nameWithoutExtension(filePath), sourceCrs);
		final GMLSchema 	schema 		= getGMLSchema(schemaNamespace, null);
		
		//QName featureTypePropertyName = new QName( UrlCatalogFloodRisk.NS_VECTORDATAMODEL, "FeatureMember" ); //$NON-NLS-1$
		//QName shapeFeatureTypePropertyName = new QName( "namespace", "featureMember" ); //$NON-NLS-1$ //$NON-NLS-2$
		//QName shapeGeomPropertyName = new QName( "namespace", "GEOM" ); //$NON-NLS-1$ //$NON-NLS-2$
		//QName propertyName = new QName( "namespace", "LANDUSE" ); //$NON-NLS-1$

		final IFeatureType[] types = schema.getAllFeatureTypes();
		Feature shapeRootFeature = workSpace.getRootFeature();
		IFeatureType rootFeatureType = shapeRootFeature.getFeatureType();
		Feature rootFeature = FeatureFactory.createFeature( null, rootFeatureType.getName() + "0", rootFeatureType, true ); //$NON-NLS-1$
		/*
		IPropertyType ftp_feature = rootFeatureType.getProperty( featureTypePropertyName );
		List featureList = (List) shapeRootFeature.getProperty( shapeFeatureTypePropertyName );
		for( int i = 0; i < featureList.size(); i++ )
		{
			final Feature feat = (Feature) featureList.get( i );
			final String propertyValue = (String) feat.getProperty( propertyName );
			if( !m_landuseTypeSet.contains( propertyValue ) )
			{
				m_landuseTypeSet.add( propertyValue );
			}
			Object[] properties = new Object[] { null, null, null, null, null, (GM_Object) feat.getProperty( shapeGeomPropertyName ), propertyValue };
			final Feature feature = FeatureFactory.createFeature( rootFeature, "Feature" + i, ((IRelationType) ftp_feature).getTargetFeatureType(), properties ); //$NON-NLS-1$
			FeatureHelper.addProperty( rootFeature, ftp_feature, feature );
		}
		*/
		final GMLWorkspace workspace = new GMLWorkspace_Impl( schema, types, rootFeature, landuseDataGML.toURL(), "", null ); //$NON-NLS-1$
		FileWriter fw = new FileWriter( landuseDataGML );
		GmlSerializer.serializeWorkspace( fw, workspace );
		fw.close();
	}

}
