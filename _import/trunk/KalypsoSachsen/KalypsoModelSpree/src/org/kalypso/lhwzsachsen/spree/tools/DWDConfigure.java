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
package org.kalypso.lhwzsachsen.spree.tools;

import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.net.URL;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.dwd.DWDRaster;
import org.kalypso.dwd.DWDRasterGeoLayer;
import org.kalypso.dwd.DWDRasterHelper;
import org.kalypso.dwd.RasterPart;
import org.kalypso.dwd.dwdzml.DwdzmlConf;
import org.kalypso.dwd.dwdzml.ObjectFactory;
import org.kalypso.dwd.dwdzml.DwdzmlConf.Target;
import org.kalypso.dwd.dwdzml.DwdzmlConf.Target.Map;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * Diese Klasse ist ein Werkzeug, um die Zuordnung zwischen Rasterzellen des DWD-Rasters und den Niederschlagsgebieten
 * zu generiernen.
 * <p>
 * Optional kann das Raster auch als GML (Polygon-Themen) ausgegeben werden.
 * </p>
 * <p>
 * Liest resources/dwd/lm2_inv_slug und resources/dwd/modell.gml
 * </p>
 * <p>
 * Schreibt C:/tmp/dwdConf.xml
 * </p>
 * 
 * @author doemming (16.06.2005)
 * @author belger Adaptiert f�r die Spree Februar 2006
 */
public class DWDConfigure
{
  private final static ObjectFactory OF = new ObjectFactory();

  private final static JAXBContext JC = JaxbUtilities.createQuiet( OF.getClass() );

  /**
   * <p>
   * Synopsis: DWDConfigureTest -rasteroutput rasterfilename.gml
   * </p>
   * <p>
   * If -rasteroutput is not specified, no raster will be written (faster!)
   * </p>
   * 
   * @throws Exception
   */
  public static void main( final String[] args ) throws Exception
  {
    // LM2:
    final URL lmBaseURL = DWDConfigure.class.getResource( "../resources/dwd/lm2_inv_slug" );

    final File fileDwdZmlN = new File( "C:\\TMP\\dwdZmlConf.xml" );

    createBaseRasterDWDRaster( args, lmBaseURL, fileDwdZmlN );
  }

  /**
   * generate raster as polygon-gml and create raster-mapping configuration
   * 
   * @param fileDwdZmlN
   * @throws Exception
   */
  public static void createBaseRasterDWDRaster( final String args[], final URL lmbaseURL, final File fileDwdZmlN ) throws Exception
  {
    System.out.println( "Start reading raster..." );
    final DWDRasterGeoLayer geoRaster = DWDRasterHelper.loadGeoRaster( lmbaseURL, "EPSG:31468" );
    System.out.println( "Raster read" );

    if( args.length == 2 && args[0].equals( "-rasteroutput" ) )
    {
      final File file = new File( args[1] );
      System.out.println( "Writing raster to file: " + file.getAbsolutePath() );
      geoRaster.saveAsGML( file );
    }

    // raster gml generated
    // start part two and generate mapping configuration
    // N
    FileWriter fileWriter = null;
    try
    {
      fileWriter = new FileWriter( fileDwdZmlN );
      generateDwdZmlConf( geoRaster, DWDRaster.KEY_RAIN, KalypsoStati.BIT_OK, fileWriter );
    }
    finally
    {
      IOUtils.closeQuietly( fileWriter );
    }
  }

  private static void generateDwdZmlConf( final DWDRasterGeoLayer geoRaster, final int dwdKey, final int defaultStatusValue, final Writer writer ) throws Exception
  {
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( DWDConfigure.class.getResource( "../resources/dwd/modell.gml" ), null );

    final IFeatureType featureType = workspace.getFeatureType( "Einzugsgebiet" );
    final Feature[] features = workspace.getFeatures( featureType );

    final DwdzmlConf conf = OF.createDwdzmlConf();
    conf.setDefaultStatusValue( defaultStatusValue );
    conf.setDwdKey( dwdKey );
    conf.setNumberOfCells( geoRaster.getNumberOfCells() );
    final List<Target> targetList = conf.getTarget();
    for( final Feature feature : features )
    {
      final Target target = OF.createDwdzmlConfTarget();

      final TimeseriesLinkType targetZmlLink = (TimeseriesLinkType) feature.getProperty( "Niederschlag_rechnung" );
      target.setTargetZR( targetZmlLink.getHref() );

      final List<Map> mapList = target.getMap();
      final GM_Surface<GM_SurfacePatch> surface = (GM_Surface<GM_SurfacePatch>) feature.getProperty( "GEOM_EZG" );
      final double modellArea = GeometryUtilities.calcArea( surface );
      final RasterPart[] positions = geoRaster.getPositions( surface );
      System.out.println( "Feature: " + feature.getId() + " : " + positions.length );
      System.out.println( "A(modell)=" + modellArea );

      // calc full cell area
      double fullCellArea = 0;
      for( final RasterPart position : positions )
        fullCellArea += position.getPortion();

      // set mapping
      if( positions.length == 0 )
        throw new Exception( "f�r Gebiet " + feature.getId() + " wurden keine Rasterzellen zugeordnet !" );

      for( final  RasterPart rasterPart : positions )
      {
        // percentage of intersection
        final double percentage = rasterPart.getPortion() / fullCellArea;

        // round percentage to 2 digits
        final int percent = Math.round( (float) (percentage * 100) );
        if( percent > 0 )
        {
          final Map map = OF.createDwdzmlConfTargetMap();
          map.setCellPos( rasterPart.getPosition() );
          map.setFactor( (double) percent / 100 );
          mapList.add( map );
        }
      }

      targetList.add( target );
      System.out.println( "A(cell)=  " + fullCellArea + " deltaA=" + (modellArea - fullCellArea) + " faktor=" + modellArea / fullCellArea );
    }

    final Marshaller marshaller = JC.createMarshaller();
    marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
    marshaller.marshal( conf, writer );
  }
}
