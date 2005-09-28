/*
 * --------------- Kalypso-Header
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ----------------------------------------------------------------------
 */
package org.kalypso.test.dwdraster;

import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.net.URL;
import java.util.List;

import javax.xml.bind.Marshaller;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.net.IUrlCatalog;
import org.kalypso.contribs.java.net.MultiUrlCatalog;
import org.kalypso.convert.namodel.schema.UrlCatalogNA;
import org.kalypso.dwd.DWDRaster;
import org.kalypso.dwd.DWDRasterGeoLayer;
import org.kalypso.dwd.DWDRasterHelper;
import org.kalypso.dwd.DWDTask;
import org.kalypso.dwd.RasterPart;
import org.kalypso.dwd.dwdzml.DwdzmlConf;
import org.kalypso.dwd.dwdzml.ObjectFactory;
import org.kalypso.dwd.dwdzml.DwdzmlConfType.TargetType;
import org.kalypso.dwd.dwdzml.DwdzmlConfType.TargetType.MapType;
import org.kalypso.dwd.schema.UrlCatalogDWD;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.kalypsodeegree_impl.gml.schema.schemata.DeegreeUrlCatalog;
import org.kalypsodeegree_impl.gml.schema.schemata.UrlCatalogUpdateObservationMapping;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * DWDRasterTest
 * <p>
 * It is a test, but also
 * <li>a tool to generate a GML-File that represents <br>
 * the DWD-LM-BaseRaster as polygon theme</li>
 * <li>a tool that generates the dwd to zml configuration</li>
 * 
 * 
 * created by
 * 
 * @author doemming (16.06.2005)
 */
public class WeisseElster_DWDConfigureTest extends TestCase
{

  public void testDWDRaster() throws Exception
  {
    final ITypeRegistry registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
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

    //    LM1:
    //    final URL lmBaseURL = getClass().getResource( "resources/LM1/lm_inv_slug" );
    //    final URL lmObsURL = getClass().getResource( "resources/LM1/lm_2004_11_09_00" );

    //    LM2:
    final URL lmBaseURL = getClass().getResource( "resources/LM2/lm2_inv_slug" );
    final URL lmObsURL = getClass().getResource( "resources/LM2/lm2_2005_06_23_00.txt" );

    final File fileDwdZmlN = new File( "C:\\TMP\\dwdZmlConfN.xml" );
    final File fileDwdZmlT = new File( "C:\\TMP\\dwdZmlConfT.xml" );

    createBaseRasterDWDRaster( lmBaseURL, fileDwdZmlN, fileDwdZmlT );
    final File targetDir = new File( "C:\\TMP\\zmltarget" );
    targetDir.mkdirs();
    createZMLFormDWD( lmObsURL, fileDwdZmlN.toURL(), targetDir );
    createZMLFormDWD( lmObsURL, fileDwdZmlT.toURL(), targetDir );
  }

  private void createZMLFormDWD( final URL lmObsURL, final URL dwd2zmlURL, final File targetContext ) throws Exception
  {
    final DWDTask task = new DWDTask();
    task.setObsRasterURL( lmObsURL );
    task.setTargetContext( targetContext );
    task.setDwd2zmlConfUrl( dwd2zmlURL );
    
    task.execute();
  }

  /**
   * generate raster as polygon-gml and create raster-mapping configuration
   * 
   * @param fileDwdZmlT
   * @param fileDwdZmlN
   * 
   * @throws Exception
   */
  public void createBaseRasterDWDRaster( final URL lmbaseURL, final File fileDwdZmlN, final File fileDwdZmlT )
      throws Exception
  {
    try
    {

      DWDRasterGeoLayer geoRaster = DWDRasterHelper.loadGeoRaster( lmbaseURL, "EPSG:31469" );
      geoRaster.saveAsGML( new File( "C:\\TMP\\raster.gml" ) );
      // raster gml generated
      // start part two and generate mapping configuration
      // N
      generateDwdZmlConf( geoRaster, DWDRaster.KEY_RAIN, KalypsoStati.BIT_OK, "Niederschlag/Niederschlag_",
          new FileWriter( fileDwdZmlN ) );
      // T
      generateDwdZmlConf( geoRaster, DWDRaster.KEY_TEMP, KalypsoStati.BIT_OK, "Temperatur/Temperatur_", new FileWriter(
          fileDwdZmlT ) );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }
  }

  private void generateDwdZmlConf( final DWDRasterGeoLayer geoRaster, final int dwdKey, final int defaultStatusValue,
      final String targetZmlPrefix, final Writer writer ) throws Exception
  {
    try
    {
      final ObjectFactory dwdFac = new ObjectFactory();
      final DwdzmlConf conf = dwdFac.createDwdzmlConf();
      conf.setDefaultStatusValue( defaultStatusValue );
      conf.setDwdKey( dwdKey );
      conf.setNumberOfCells( geoRaster.getNumberOfCells() );
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( getClass().getResource(
          "resources/modell_epsg31469.gml" ) );
      final FeatureType featureType = workspace.getFeatureType( "Catchment" );
      final Feature[] features = workspace.getFeatures( featureType );
      final List targetList = conf.getTarget();
      for( int i = 0; i < features.length; i++ )
      {
        final Feature feature = features[i];
        final TargetType target = dwdFac.createDwdzmlConfTypeTargetType();
        target.setTargetZR( targetZmlPrefix + feature.getId() + ".zml" );
        final List mapList = target.getMap();
        GM_Surface surface = (GM_Surface)feature.getProperty( "Ort" );
        double modellArea = GeometryUtilities.calcArea( surface );
        RasterPart[] positions = geoRaster.getPositions( surface );
        System.out.println( "Feature: " + feature.getId() + " : " + positions.length );
        System.out.println( "A(modell)=" + modellArea );
        // calc full cell area
        double fullCellArea = 0;
        for( int j = 0; j < positions.length; j++ )
          fullCellArea += positions[j].getPortion();
        // set mapping
        if( positions.length == 0 )
          throw new Exception( "für Gebiet " + feature.getId() + " wurden keine Rasterzellen zugeordnet !" );
        for( int j = 0; j < positions.length; j++ )
        {
          final MapType map = dwdFac.createDwdzmlConfTypeTargetTypeMapType();
          map.setCellPos( positions[j].getPosition() );
          map.setFactor( positions[j].getPortion() / fullCellArea );
          mapList.add( map );
        }
        targetList.add( target );
        System.out.println( "A(cell)=  " + fullCellArea + " deltaA=" + ( modellArea - fullCellArea ) + " faktor="
            + modellArea / fullCellArea );
      }
      final Marshaller marshaller = dwdFac.createMarshaller();
      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      marshaller.marshal( conf, writer );
    }
    catch( Exception e )
    {
      throw e;
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }
}
