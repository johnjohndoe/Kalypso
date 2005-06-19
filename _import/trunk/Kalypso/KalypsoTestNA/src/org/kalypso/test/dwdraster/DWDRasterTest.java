/*
 * --------------- Kalypso-Header
 * --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal
 * engineering Denickestr. 22 21073 Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany
 * http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.test.dwdraster;

import java.io.File;

import junit.framework.TestCase;

import org.kalypso.convert.namodel.schema.UrlCatalogNA;
import org.kalypso.dwd.DWDRaster;
import org.kalypso.dwd.DWDRasterGeoLayer;
import org.kalypso.dwd.RasterPart;
import org.kalypso.dwd.RasterStorage;
import org.kalypso.dwd.schema.UrlCatalogDWD;
import org.kalypso.java.net.IUrlCatalog;
import org.kalypso.java.net.MultiUrlCatalog;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.TypeRegistrySingleton;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.kalypsodeegree_impl.gml.schema.schemata.DeegreeUrlCatalog;
import org.kalypsodeegree_impl.gml.schema.schemata.UrlCatalogUpdateObservationMapping;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * DWDRasterTest
 * <p>
 * 
 * created by
 * 
 * @author doemming (16.06.2005)
 */
public class DWDRasterTest extends TestCase
{
  public void testDWDRaster() throws Exception
  {
    final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();
    registry.registerTypeHandler( new ObservationLinkHandler() );

    // initalize urlcatalogs
    final MultiUrlCatalog multiCatalog = new MultiUrlCatalog( new IUrlCatalog[]
    {
        new UrlCatalogNA(),
        new UrlCatalogDWD(),
        new UrlCatalogUpdateObservationMapping(),
        new DeegreeUrlCatalog() } );
    final File cache = new File( "C:\\TMP\\schemaCatalog" );
    if( !cache.exists() )
      cache.mkdirs();
    GMLSchemaCatalog.init( multiCatalog, cache );

    RasterStorage storage = new RasterStorage();
    storage.loadRaster( getClass().getResource( "resources/lm_inv_slug" ) );
    storage.loadRaster( getClass().getResource( "resources/lm_2004_11_10_00" ) );
    DWDRaster xRaster = (DWDRaster)storage.get( DWDRaster.KEY_100000_LON );
    DWDRaster yRaster = (DWDRaster)storage.get( DWDRaster.KEY_100000_LAT );
    DWDRasterGeoLayer geoRaster = new DWDRasterGeoLayer( "EPSG:31469", xRaster, yRaster );
    geoRaster.saveAsGML( new File( "C:\\TMP\\raster.gml" ) );
    GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( getClass().getResource( "resources/modell_epsg31469.gml" ) );
    FeatureType featureType = workspace.getFeatureType( "Catchment" );
    Feature[] features = workspace.getFeatures( featureType );
    for( int i = 0; i < features.length; i++ )
    {
      Feature feature = features[i];
      GM_Surface surface = (GM_Surface)feature.getProperty( "Ort" );
      double modellArea = GeometryUtilities.calcArea( surface );
      RasterPart[] positions = geoRaster.getPositions( surface );
      System.out.println( "Feature: " + feature.getId() + " : " + positions.length );
      System.out.println( "A(modell)=" + modellArea );
      double cellArea = 0;
      for( int j = 0; j < positions.length; j++ )
      {
        cellArea += positions[j].getPortion();
        //        System.out.println( " " + positions[j].getPosition() + ". " +
        // positions[j].getPortion() );
      }
      System.out.println( "A(cell)=  " + cellArea + " deltaA=" + ( modellArea - cellArea ) + " faktor=" + modellArea / cellArea );
    }
  }
}
