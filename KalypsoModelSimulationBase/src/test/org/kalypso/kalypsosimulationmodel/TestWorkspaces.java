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
package test.org.kalypso.kalypsosimulationmodel;

import java.net.URL;
import java.util.Hashtable;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
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
  // private static final Logger logger=
  // Logger.getLogger(TestWorkspaces.class);

  public static final String CS_KEY_GAUSS_KRUEGER = "EPSG:31467"; //$NON-NLS-1$

  public static final double[][] NO_INTERIOR = {};

  // public static final URL URL_POLYNOMIAL1D;
  // public static final String REL_RES_POLYNOMIAL1D="data/polynomial1d.xml";

  // public static final URL URL_POLYNOMIAL2D;
  // public static final String REL_RES_POLYNOMIAL2D="data/polynomial2d.xml";

  public static final URL URL_EMPTY_GML;

  public static final String REL_RES_EMPTY_GML = "data/empty_gml.xml"; //$NON-NLS-1$

  public static final URL URL_MPCOV_ROUGHNESS_CORRECTION;

  public static final String REL_RES_MPCOV_ROUGHNESS_CORRECTION = "data/mpcov_pol1d.xml"; //$NON-NLS-1$

  public static final URL URL_MULTIPOINT;

  public static final String REL_RES_MULTIPOINT = "data/multipoint.xml"; //$NON-NLS-1$

  public static final URL URL_FEATURERANGESET;

  public static final String REL_RES_FEATURERANGESET = "data/feature_range_set.xml"; //$NON-NLS-1$

  public static final URL URL_ROUGHNESS_CLS;

  public static final String REL_RES_ROUGHNESS_CLS = "data/roughness_cls.xml"; //$NON-NLS-1$

  public static final URL URL_ROUGHNESS_CLS_COR;

  public static final String REL_RES_ROUGHNESS_CLS_COR = "data/roughness_cls_correction.xml"; //$NON-NLS-1$

  public static final URL URL_ROUGHNESS_CLS_COLLECTION;

  public static final String REL_RES_ROUGHNESS_CLS_COLLECTION = "data/roughness_cls_collection.xml"; //$NON-NLS-1$

  public static final URL URL_ROUGHNESS_CLS_COLLECTION_VIEW_TEST;

  public static final String REL_RES_ROUGHNESS_CLS_COLLECTION_VIEW_TEST = "data/roughness_cls_collection.xml"; //$NON-NLS-1$

  public static final URL URL_ROUGHNESS_DB;

  public static final String REL_RES_ROUGHNESS_DB = "data/roughness_db.xml"; //$NON-NLS-1$

  public static final URL URL_ROUGHNESS_POLYGON;

  public static final String REL_RES_ROUGHNESS_POLYGON = "data/roughness_polygon.xml"; //$NON-NLS-1$

  public static final URL URL_ROUGHNESS_POLYGON_COLLECTION;

  public static final String REL_RES_ROUGHNESS_POLYGON_COLLECTION = "data/terrain.gml"; //$NON-NLS-1$

  public static final URL URL_ROUGHNESS_POLYGON_COLLECTION_ESTIMATE;

  public static final String REL_RES_ROUGHNESS_POLYGON_COLLECTION_ESTIMATE = "data/roughness_polygon_collection_estimate.xml"; //$NON-NLS-1$

  public static final URL URL_COL_ROUGHNESS_CLS_COR;

  public static final String REL_RES_COL_ROUGHNESS_CLS_COR = "data/collection_of_roughness_cls_correction.xml"; //$NON-NLS-1$

  public static final URL URL_SIMPLE_OPERATIONAL_MODEL;

  public static final String REL_RES_SIMPLE_OPERATIONAL_MODEL = "data/simulation_model_operational.xml"; //$NON-NLS-1$

  public static final QName GML_PROP_FEATURE_MEMBER = new QName( NS.GML3, "featureMember" ); //$NON-NLS-1$

  public static final Throwable EXCEPTION;

  public static final URL URL_SHAPE_2_ROUGHNESS_POLYGON;

  public static final String REL_RES_SHAPE_2_ROUGHNESS_POLYGON = "data/shapeConverter.gml"; //$NON-NLS-1$

  public static final URL URL_SMALL_ASC;

  public static final String REL_RES_SMALL_ASC = "data/test_file_small_asc.asc"; //$NON-NLS-1$

  public static final URL URL_GC_SMALL_ASC;

  public static final String REL_RES_GC_SMALL_ASC = "data/gc_elevation_model.xml"; //$NON-NLS-1$

  public static final URL URL_NATIVE_TEM_WRAPPER;

  public static final String REL_RES_NATIVE_TEM_WRAPPER = "data/native_terrain_elevation_model_wrapper.xml"; //$NON-NLS-1$

  public static final URL URL_TEM_SYSREM;

  public static final String REL_RES_TEM_SYSTEM = "data/native_terrain_elevation_model_system.xml"; //$NON-NLS-1$

  public static final URL URL_TEST_HMO_3_TRI;

  public static final String REL_RES_TEST_HMO_3_TRI = "data/test_hmo_3_tri.xml"; //$NON-NLS-1$

  public static final URL URL_TEM_EMPTY_TER_MODEL;

  public static final String REL_RES_TEM_EMPTY_TER_MODEL = "data/test_terrain_model_without_tem_system.xml"; //$NON-NLS-1$

  static
  {
    final Map<String, URL> urlMap = new Hashtable<>();
    Throwable th1 = null;
    try
    {
      // urlMap.put(
      // REL_RES_POLYNOMIAL1D,
      // TestWorkspaces.class.getResource(REL_RES_POLYNOMIAL1D));
      // urlMap.put(
      // REL_RES_POLYNOMIAL2D,
      // TestWorkspaces.class.getResource(REL_RES_POLYNOMIAL2D));
      urlMap.put( REL_RES_EMPTY_GML, TestWorkspaces.class.getResource( REL_RES_EMPTY_GML ) );
      urlMap.put( REL_RES_MPCOV_ROUGHNESS_CORRECTION, TestWorkspaces.class.getResource( REL_RES_MPCOV_ROUGHNESS_CORRECTION ) );
      urlMap.put( REL_RES_MULTIPOINT, TestWorkspaces.class.getResource( REL_RES_MULTIPOINT ) );
      urlMap.put( REL_RES_FEATURERANGESET, TestWorkspaces.class.getResource( REL_RES_FEATURERANGESET ) );
      urlMap.put( REL_RES_ROUGHNESS_CLS, TestWorkspaces.class.getResource( REL_RES_ROUGHNESS_CLS ) );
      urlMap.put( REL_RES_ROUGHNESS_DB, TestWorkspaces.class.getResource( REL_RES_ROUGHNESS_DB ) );

      urlMap.put( REL_RES_ROUGHNESS_CLS_COR, TestWorkspaces.class.getResource( REL_RES_ROUGHNESS_CLS_COR ) );
      urlMap.put( REL_RES_ROUGHNESS_CLS_COLLECTION, TestWorkspaces.class.getResource( REL_RES_ROUGHNESS_CLS_COLLECTION ) );
      urlMap.put( REL_RES_COL_ROUGHNESS_CLS_COR, TestWorkspaces.class.getResource( REL_RES_COL_ROUGHNESS_CLS_COR ) );
      urlMap.put( REL_RES_ROUGHNESS_POLYGON, TestWorkspaces.class.getResource( REL_RES_ROUGHNESS_POLYGON ) );
      urlMap.put( REL_RES_ROUGHNESS_POLYGON_COLLECTION, TestWorkspaces.class.getResource( REL_RES_ROUGHNESS_POLYGON_COLLECTION ) );
      urlMap.put( REL_RES_ROUGHNESS_POLYGON_COLLECTION_ESTIMATE, TestWorkspaces.class.getResource( REL_RES_ROUGHNESS_POLYGON_COLLECTION_ESTIMATE ) );

      urlMap.put( REL_RES_SHAPE_2_ROUGHNESS_POLYGON, TestWorkspaces.class.getResource( REL_RES_SHAPE_2_ROUGHNESS_POLYGON ) );
      urlMap.put( REL_RES_ROUGHNESS_CLS_COLLECTION_VIEW_TEST, TestWorkspaces.class.getResource( REL_RES_ROUGHNESS_CLS_COLLECTION_VIEW_TEST ) );
      urlMap.put( REL_RES_SMALL_ASC, TestWorkspaces.class.getResource( REL_RES_SMALL_ASC ) );
      urlMap.put( REL_RES_GC_SMALL_ASC, TestWorkspaces.class.getResource( REL_RES_GC_SMALL_ASC ) );
      urlMap.put( REL_RES_NATIVE_TEM_WRAPPER, TestWorkspaces.class.getResource( REL_RES_NATIVE_TEM_WRAPPER ) );
      urlMap.put( REL_RES_TEM_SYSTEM, TestWorkspaces.class.getResource( REL_RES_TEM_SYSTEM ) );
      urlMap.put( REL_RES_TEM_EMPTY_TER_MODEL, TestWorkspaces.class.getResource( REL_RES_TEM_EMPTY_TER_MODEL ) );
      urlMap.put( REL_RES_SIMPLE_OPERATIONAL_MODEL, TestWorkspaces.class.getResource( REL_RES_SIMPLE_OPERATIONAL_MODEL ) );
      urlMap.put( REL_RES_TEST_HMO_3_TRI, TestWorkspaces.class.getResource( REL_RES_TEST_HMO_3_TRI ) );
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      th1 = th;
      urlMap.clear();

    }
    finally
    {
      EXCEPTION = th1;
      // URL_POLYNOMIAL1D=urlMap.get(REL_RES_POLYNOMIAL1D);
      // URL_POLYNOMIAL2D=urlMap.get(REL_RES_POLYNOMIAL2D);
      URL_EMPTY_GML = urlMap.get( REL_RES_EMPTY_GML );
      URL_MPCOV_ROUGHNESS_CORRECTION = urlMap.get( REL_RES_MPCOV_ROUGHNESS_CORRECTION );
      URL_MULTIPOINT = urlMap.get( REL_RES_MULTIPOINT );
      URL_FEATURERANGESET = urlMap.get( REL_RES_FEATURERANGESET );
      URL_ROUGHNESS_CLS = urlMap.get( REL_RES_ROUGHNESS_CLS );
      URL_ROUGHNESS_CLS_COR = urlMap.get( REL_RES_ROUGHNESS_CLS_COR );

      URL_ROUGHNESS_CLS_COLLECTION = urlMap.get( REL_RES_ROUGHNESS_CLS_COLLECTION );

      URL_ROUGHNESS_CLS_COLLECTION_VIEW_TEST = urlMap.get( REL_RES_ROUGHNESS_CLS_COLLECTION_VIEW_TEST );

      URL_COL_ROUGHNESS_CLS_COR = urlMap.get( REL_RES_COL_ROUGHNESS_CLS_COR );

      URL_ROUGHNESS_DB = urlMap.get( REL_RES_ROUGHNESS_DB );

      URL_ROUGHNESS_POLYGON = urlMap.get( REL_RES_ROUGHNESS_POLYGON );
      URL_ROUGHNESS_POLYGON_COLLECTION = urlMap.get( REL_RES_ROUGHNESS_POLYGON_COLLECTION );

      URL_ROUGHNESS_POLYGON_COLLECTION_ESTIMATE = urlMap.get( REL_RES_ROUGHNESS_POLYGON_COLLECTION_ESTIMATE );

      URL_SHAPE_2_ROUGHNESS_POLYGON = urlMap.get( REL_RES_SHAPE_2_ROUGHNESS_POLYGON );

      URL_SMALL_ASC = urlMap.get( REL_RES_SMALL_ASC );

      URL_GC_SMALL_ASC = urlMap.get( REL_RES_GC_SMALL_ASC );

      URL_NATIVE_TEM_WRAPPER = urlMap.get( REL_RES_NATIVE_TEM_WRAPPER );

      URL_TEM_SYSREM = urlMap.get( REL_RES_TEM_SYSTEM );

      URL_TEM_EMPTY_TER_MODEL = urlMap.get( REL_RES_TEM_EMPTY_TER_MODEL );

      URL_SIMPLE_OPERATIONAL_MODEL = urlMap.get( REL_RES_SIMPLE_OPERATIONAL_MODEL );

      URL_TEST_HMO_3_TRI = urlMap.get( REL_RES_TEST_HMO_3_TRI );
    }
  }

  public static GMLWorkspace loadGMLWorkspace( final URL gmlURL, final String schemaLocation ) throws Exception
  {
    final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();

    final GMLSchema modelGmlSchema = schemaCatalog.getSchema( UrlCatalogModelSimulationBase.SIM_MODEL_NS, (String) null );

    return new GMLWorkspace_Impl( modelGmlSchema, null, gmlURL, null, schemaLocation, null );
  }

  public static final String getGaussKrueger( )
  {
    return TestWorkspaces.CS_KEY_GAUSS_KRUEGER;
  }
}