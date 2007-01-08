/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package test.org.kalypso.kalypsomodel1d2d;

import java.net.URL;
import java.util.Hashtable;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.log4j.Logger;
import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;

/**
 * 
 * @author Patrice Congo
 *
 */
public class TestWorkspaces
{
	private static final Logger logger= 
			Logger.getLogger(TestWorkspaces.class);
	
    
	public static final String CS_KEY_GAUSS_KRUEGER="EPSG:31467";
	
    public static final URL URL_EMPTY_GML;
    public static final String REL_RES_EMPTY_GML="data/empty_gml.xml";
    
    
	public static final URL URL_FE1D2DNODE;
	public static final String REL_RES_FE1D2DNODE="data/test_fe1d2dnode.xml";
	
//	public static final URL URL_POLYNOMIAL2D;
//	public static final String REL_RES_POLYNOMIAL2D="data/polynomial2d.xml";
//	
//	
//	public static final URL URL_MPCOV_ROUGHNESS_CORRECTION;
//	public static final String REL_RES_MPCOV_ROUGHNESS_CORRECTION="data/mpcov_pol1d.xml";
//	
//	public static final URL URL_MULTIPOINT;
//	public static final String REL_RES_MULTIPOINT="data/multipoint.xml";
//	
//	
//	public static final URL URL_FEATURERANGESET;
//	public static final String REL_RES_FEATURERANGESET="data/feature_range_set.xml";
//	
//	public static final URL URL_ROUGHNESS_CLS;
//	public static final String REL_RES_ROUGHNESS_CLS=
//							"data/roughness_cls.xml";
//	
//	public static final URL URL_ROUGHNESS_CLS_COR;
//	public static final String REL_RES_ROUGHNESS_CLS_COR=
//							"data/roughness_cls_correction.xml";
//	
//	public static final URL URL_ROUGHNESS_CLS_COLLECTION;
//	public static final String REL_RES_ROUGHNESS_CLS_COLLECTION=
//							"data/roughness_cls_collection.xml";
//	
//	public static final URL URL_ROUGHNESS_POLYGON;
//	public static final String REL_RES_ROUGHNESS_POLYGON=
//							"data/roughness_polygon.xml";
//	
//	public static final URL URL_COL_ROUGHNESS_CLS_COR;
//	public static final String REL_RES_COL_ROUGHNESS_CLS_COR=
//							"data/collection_of_roughness_cls_correction.xml";
//	
	public static final QName GML_PROP_FEATURE_MEMBER= 
						new QName(NS.GML3,"featureMember");
	
	public static final Throwable EXCEPTION;
	
	static 
	{
		Map<String ,URL> urlMap=new Hashtable<String, URL>();
		Throwable  th1=null;
		try
		{
			
			
			urlMap.put(
					REL_RES_EMPTY_GML,
					TestWorkspaces.class.getResource(REL_RES_EMPTY_GML));
            
            urlMap.put(
                REL_RES_FE1D2DNODE,
                TestWorkspaces.class.getResource(REL_RES_FE1D2DNODE));
			
		}
		catch(Throwable th)
		{
			th.printStackTrace();
			th1=th;
			urlMap.clear();
			
		}
		finally
		{
			EXCEPTION=th1;
			URL_EMPTY_GML=urlMap.get(REL_RES_EMPTY_GML);
            URL_FE1D2DNODE=urlMap.get(REL_RES_FE1D2DNODE);
			
		}
	}
	
	
	
	public static GMLWorkspace loadGMLWorkspace(
							URL gmlURL,
							String schemaLocation) throws Exception
	{
		GMLSchemaCatalog schemaCatalog = 
			KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    
		GMLSchema modelGmlSchema = 
			schemaCatalog.getSchema( 
					UrlCatalogModelSimulationBase.SIM_MODEL_NS, 
					(String)null );
    
		IFeatureType[] featureTypes = 
    				modelGmlSchema.getAllFeatureTypes();
    
		GMLWorkspace modelWorkspace = 
			new GMLWorkspace_Impl( 
					modelGmlSchema, 
					featureTypes, 
					(null), 
					gmlURL, 
					schemaLocation, 
					null );
    
		return modelWorkspace;
	}
}
