/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.dwd.raster;

import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.xml.bind.Marshaller;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.TypeRegistrySingleton;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.sort.SplitSort;
import org.kalypso.convert.namodel.schema.KalypsoNADefaultSchema;
import org.kalypso.dwd.DWDRaster;
import org.kalypso.dwd.RasterStorage;
import org.kalypso.dwd.dwdzml.DwdzmlConf;
import org.kalypso.dwd.dwdzml.ObjectFactory;
import org.kalypso.dwd.dwdzml.DwdzmlConfType.CatchmentType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * generates an initial configuration file for generating catchment based
 * timeseries files from dwd-raster based weather forecast files. the resulting
 * configuration file will include the mapping from catchments to the
 * rastercells. input and output directories must be added to the resulting
 * configurationfile manually. Use the configurationFile for the automatic
 * timeseries generating process (see KalypsoDWD)
 * 
 * @author doemming
 */
public class ForecastGeneratorConfigurator
{
  private static final int KEY_GEO_BBOX_4326 = -4326;

  /**
   * size of a raster cell DX (EPSG:4326)
   */
  private final double rasterDX = 0.09688948027718162;

  /**
   * size of a raster cell DY (EPSG:4326)
   */
  private final double rasterDY = 0.282090923076925;

  private static final int KEY_HIGH_RESOTUTION = -9971;

  private static final int KEY_GEO_Point_4326 = -9970;

  private static final int FAKE_DENSITY = 10;

  private final RasterStorage m_storage;

  private final File m_baseRasterFile;

  private final File m_modelFile;

  private Feature[] m_features;

  public ForecastGeneratorConfigurator( final File baseRasterFile, final File modelFile )
  {
    m_storage = new RasterStorage();
    m_baseRasterFile = baseRasterFile;
    m_modelFile = modelFile;
  }

  /**
   * @param args:
   *          modelFile, baseRaster
   */
  public static void main( String[] args )
  {
    if( args.length != 2 )
    {
      System.out.println( "2 arguments needed: [modelfile] [baseRaster] " );
      System.exit( 0 );
    }
    final File modelFile = new File( args[0] );
    final File baseRasterFile = new File( args[1] );
    System.out.println( "Konvertierung von Rasterdaten zu Zeitreihen" );

    try
    {
      ForecastGeneratorConfigurator raster2ZML = new ForecastGeneratorConfigurator( baseRasterFile,
          modelFile );
      raster2ZML.init();
      raster2ZML.createConf();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  public void init() throws Exception
  {
    // load baseraster
    m_storage.loadRaster( m_baseRasterFile );
    // load model
    final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();
    registry.registerTypeHandler( new ObservationLinkHandler() );

    System.out.println( "loading model: " + m_modelFile.getName() );
    final URL modellURL = m_modelFile.toURL();
    final URL schemaURL = KalypsoNADefaultSchema.getDefaultNaModellSchemaURL();
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modellURL, schemaURL );
    m_features = workspace.getFeatures( workspace.getFeatureType( "Catchment" ) );
  }

  public void createConf() throws Exception
  {
    createGeoRaster();
    final SplitSort featureSort = new SplitSort();
    final HashMap f2rasterPos = new HashMap();
    for( int i = 0; i < m_features.length; i++ )
    {
      f2rasterPos.put( m_features[i], new ArrayList() );
      featureSort.add( m_features[i] );
    }
    final DWDRaster bbox_4326_Raster = (DWDRaster)m_storage.get( KEY_GEO_BBOX_4326 );
    Object o = m_storage.get( KEY_GEO_Point_4326 );
    final DWDRaster point_4326_Raster = (DWDRaster)o;
    final GeoTransformer transformer = new GeoTransformer( m_features[0]
        .getDefaultGeometryProperty().getCoordinateSystem() );
    final DWDRaster bbox_GK_Raster = transformRaster( bbox_4326_Raster, transformer );
    final DWDRaster fake_GK_Raster = createFakeRaster( point_4326_Raster, FAKE_DENSITY, transformer );

    System.out.println( "calculate raster to catchment mapping" );
    for( int i = 0; i < bbox_GK_Raster.size(); i++ )
    {
      final GM_Surface bbox = (GM_Surface)bbox_GK_Raster.getElementAt( i );
      final List featureList = featureSort.query( bbox.getEnvelope(), new ArrayList() );
      if( featureList.size() > 0 )
        System.out.println( featureList.size() + " catchmets intersect raster" );
      for( Iterator iter = featureList.iterator(); iter.hasNext(); )
      {
        final Feature feature = (Feature)iter.next();
        // for those catchments that will not be catched by the
        // rasterpoints
        ( (List)f2rasterPos.get( feature ) ).add( new Integer( i ) );
        final List pointList = (List)fake_GK_Raster.getElementAt( i );
        for( Iterator iterator = pointList.iterator(); iterator.hasNext(); )
        {
          final GM_Point testPoint = (GM_Point)iterator.next();
          if( feature.getDefaultGeometryProperty().contains( testPoint ) )
            ( (List)f2rasterPos.get( feature ) ).add( new Integer( i ) );
        }
      }
    }
    Set set = f2rasterPos.keySet();

    final ObjectFactory dwdFac = new ObjectFactory();
    final DwdzmlConf conf = dwdFac.createDwdzmlConf();

    conf.setCreateZMLFile( true );
    conf.setInputFolder( "in" );
    conf.setOutputFolder( "out" );
    conf.setProcessedRasterDir( "" );
    conf.setDefaultStatusValue( Integer.toString( KalypsoStati.BIT_OK ) );
    final List catchmentList = conf.getCatchment();

    for( Iterator iter = set.iterator(); iter.hasNext(); )
    {
      final Feature feature = (Feature)iter.next();
      final List posList = (List)f2rasterPos.get( feature );

      final CatchmentType catchment = dwdFac.createDwdzmlConfTypeCatchmentType();
      catchmentList.add( catchment );
      catchment.setFid( feature.getId() );
      final List rasterPos = catchment.getRasterPos();
      for( Iterator iterator = posList.iterator(); iterator.hasNext(); )
      {
        final Integer pos = (Integer)iterator.next();
        rasterPos.add( pos );
      }
    }
    final Marshaller marshaller = dwdFac.createMarshaller();
    marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
    final File confFile = File.createTempFile( "raster", ".conf" );
    final Writer writer = new FileWriter( confFile );
    marshaller.marshal( conf, writer );
    writer.close();
    System.out.println( "wrote configuration to " + confFile.getCanonicalPath() );
  }

  private DWDRaster createFakeRaster( DWDRaster pointGeoRaster, int max, GeoTransformer transformer )
      throws Exception
  {
    System.out.println( "increase resolution of raster" );
    double dx = rasterDX / ( 2d * max );
    double dy = rasterDY / ( 2d * max );
    final DWDRaster result = new DWDRaster( pointGeoRaster.getDate(), KEY_HIGH_RESOTUTION );
    for( int i = 0; i < pointGeoRaster.size(); i++ )
    {
      final GM_Point point = (GM_Point)pointGeoRaster.getElementAt( i );
      final double px = point.getX();
      final double py = point.getY();
      if( i % 100 == 0 )
        System.out.print( i + "/" + pointGeoRaster.size() + "\n" );
      final List col = new ArrayList();
      for( int n = 0; n < max; n++ )
      {
        col.add( transformer.transform( GeometryFactory.createGM_Point( px + dx * n, py, point
            .getCoordinateSystem() ) ) );
        col.add( transformer.transform( GeometryFactory.createGM_Point( px - dx * n, py, point
            .getCoordinateSystem() ) ) );
        col.add( transformer.transform( GeometryFactory.createGM_Point( px, py + dy * n, point
            .getCoordinateSystem() ) ) );
        col.add( transformer.transform( GeometryFactory.createGM_Point( px, py - dy * n, point
            .getCoordinateSystem() ) ) );
      }
      result.addValue( col );
    }
    return result;
  }

  private DWDRaster transformRaster( DWDRaster geoRaster, GeoTransformer transformer )
      throws Exception
  {
    System.out.println( "transforming coordinates" );
    final DWDRaster result = new DWDRaster( geoRaster.getDate(), 0 );
    for( int i = 0; i < geoRaster.size(); i++ )
    {
      GM_Object geom = (GM_Object)geoRaster.getElementAt( i );
      result.addValue( transformer.transform( geom ) );
    }
    return result;
  }

  private void createGeoRaster() throws GM_Exception
  {
    System.out.println( "create geometries from raster data" );
    DWDRaster rLat = (DWDRaster)m_storage.get( DWDRaster.KEY_100000_LAT );
    DWDRaster rLon = (DWDRaster)m_storage.get( DWDRaster.KEY_100000_LON );
    int size = rLat.size();

    final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    final CS_CoordinateSystem csLatLon = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().export(
        csFac.getCSByName( "EPSG:4326" ) );
    final DWDRaster geoRaster = new DWDRaster( rLat.getDate(), KEY_GEO_BBOX_4326 );
    final DWDRaster pointRaster = new DWDRaster( rLat.getDate(), KEY_GEO_Point_4326 );
    m_storage.storeRaster( geoRaster );
    m_storage.storeRaster( pointRaster );
    for( int i = 0; i < size; i++ )
    {
      double x = ( Integer.parseInt( rLon.getElementAt( i ).toString() ) ) / 100000d;
      double y = ( Integer.parseInt( rLat.getElementAt( i ).toString() ) ) / 100000d;
      final GM_Point point = GeometryFactory.createGM_Point( x, y, csLatLon );
      pointRaster.addValue( point );
      double minx = x - rasterDX / 2d;
      double maxx = x + rasterDX / 2d;
      double miny = y - rasterDY / 2d;
      double maxy = y + rasterDY / 2d;
      GM_Surface box = GeometryFactory.createGM_Surface( GeometryFactory.createGM_Envelope( minx,
          miny, maxx, maxy ), csLatLon );
      geoRaster.addValue( box );
    }
  }
}