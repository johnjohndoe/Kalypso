package org.kalypso.lhwzsachsen.spree;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.GM_Point;
import org.deegree_impl.io.shpapi.ShapeFile;
import org.deegree_impl.model.cs.Adapters;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.kalypso.java.io.StreamUtilities;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Diese Klasse sammelt alles, was mit dem Erzeugen der Nativen Daten aus den
 * Eingabedaten zu tun hat.
 * 
 * @author Belger
 */
public class SpreeInputWorker
{
  private SpreeInputWorker()
  {
  // wird nicht instantiiert
  }

  /** Erzeugt aus den InputBeans eine HashMap der Form [id |-> bean] */
  public static Map hashInput( final CalcJobDataBean[] input )
  {
    final Map map = new HashMap( input.length );
    for( int i = 0; i < input.length; i++ )
    {
      final CalcJobDataBean bean = input[i];
      map.put( bean.getId(), bean );
    }

    return map;
  }

  public static Map createNativeInput( final File inputdir, final File nativedir,
      final CalcJobDataBean[] input ) throws CalcJobServiceException
  {
    nativedir.mkdirs();

    final Map inputMap = hashInput( input );

    final File propsFile = checkInput( "PROPS", inputMap, inputdir );
    final Map props = parseCalculationFile( propsFile, nativedir );

    final KalypsoFeatureLayer[] layers = loadGML( inputdir, inputMap );

    try
    {
      final File vhsFile = (File)props.get( SpreeCalcJob.DATA_VHSFILE );
      final String flpFilename = (String)props.get( SpreeCalcJob.DATA_FLPFILENAME );
      final String napFilename = (String)props.get( SpreeCalcJob.DATA_NAPFILENAME );
      final String tsFilename = (String)props.get( SpreeCalcJob.DATA_TSFILENAME );

      StreamUtilities.streamCopy( SpreeInputWorker.class.getResourceAsStream( "resources/"
          + SpreeCalcJob.VHS_FILE ), new FileOutputStream( vhsFile ) );

      findAndWriteLayer( layers, SpreeCalcJob.FLP_NAME, SpreeCalcJob.FLP_MAP,
          SpreeCalcJob.FLP_GEOM, flpFilename );
      findAndWriteLayer( layers, SpreeCalcJob.NAP_NAME, SpreeCalcJob.NAP_MAP,
          SpreeCalcJob.NAP_GEOM, napFilename );

      final Map valuesMap = createTsData( inputdir, inputMap );
      createTimeseriesFile( tsFilename, valuesMap );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Fehler beim Erzeugen der Inputdateien", e );
    }
    
    return props;
  }

  public static void createTimeseriesFile( final String tsFilename, final Map valuesMap )
      throws Exception
  {
    final List ftpList = new LinkedList();

    // Layer erzeugen!
    ftpList.add( FeatureFactory.createFeatureTypeProperty( "GEOM", GM_Point.class.getName(), false ) );
    ftpList.add( FeatureFactory.createFeatureTypeProperty( "DZAHL", "java.lang.Double", false ) );
    ftpList.add( FeatureFactory.createFeatureTypeProperty( "STUNDE", "java.lang.Double", false ) );
    ftpList.add( FeatureFactory.createFeatureTypeProperty( "DATUM", "java.lang.String", false ) );
    ftpList.add( FeatureFactory.createFeatureTypeProperty( "VON", "java.lang.Double", false ) );
    ftpList.add( FeatureFactory.createFeatureTypeProperty( "AB", "java.lang.Double", false ) );

    for( int i = 0; i < SpreeCalcJob.TS_DESCRIPTOR.length; i++ )
    {
      final TSDesc desc = SpreeCalcJob.TS_DESCRIPTOR[i];
      ftpList.add( FeatureFactory.createFeatureTypeProperty( desc.id, "java.lang.Double", true ) );
    }

    final FeatureTypeProperty[] ftps = (FeatureTypeProperty[])ftpList
        .toArray( new FeatureTypeProperty[ftpList.size()] );
    final FeatureType type = FeatureFactory.createFeatureType( null, null, "TS_TYPE", ftps );
    final FeatureCollection fc = FeatureFactory
        .createFeatureCollection( tsFilename, type, null, 10 );

    // Werte schreiben
    final DateFormat specialDateFormat = new SimpleDateFormat( "yMM.dd" );
    final DateFormat dateFormat = new SimpleDateFormat( "dd.MM.yyyy" );
    final Calendar calendar = Calendar.getInstance();
    final Date[] dateArray = (Date[])valuesMap.get( "DATE" );
    for( int i = 0; i < dateArray.length; i++ )
    {
      final Date date = dateArray[i];

      final Object[] data = new Object[6 + SpreeCalcJob.TS_DESCRIPTOR.length];

      calendar.setTime( date );
      data[0] = GeometryFactory.createGM_Point( 0.0, 0.0, null );
      data[1] = new Double( specialDateFormat.format( date ) );
      data[2] = new Double( calendar.get( Calendar.HOUR_OF_DAY ) );
      data[3] = new String( dateFormat.format( date ) );
      calendar.add( Calendar.HOUR_OF_DAY, -3 );
      data[4] = new Double( calendar.get( Calendar.HOUR_OF_DAY ) );
      calendar.add( Calendar.HOUR_OF_DAY, -3 );
      data[5] = new Double( calendar.get( Calendar.HOUR_OF_DAY ) );

      for( int j = 0; j < SpreeCalcJob.TS_DESCRIPTOR.length; j++ )
      {
        final String id = SpreeCalcJob.TS_DESCRIPTOR[j].id;
        final Map datesToValuesMap = (Map)valuesMap.get( id );
        
        final Double value = (Double)datesToValuesMap.get( date );

        Double outVal = null;
        
        if( value == null )
        {
          if( id.startsWith( "PA_" ) || id.startsWith( "PG" ) )
            outVal = new Double( -99.9 ); 
          else
            outVal = new Double( 0 );
        }
        else
          outVal = value;
        
        data[6 + j] = outVal;
      }

      for( int j = 0; j < data.length; j++ )
        System.out.print( data[j] + "\t" );
      System.out.println( );

      final Feature feature = FeatureFactory.createFeature( "" + ( i + 1 ), type, data );
      fc.appendFeature( feature );
    }

    final ShapeFile shapeFile = new ShapeFile( tsFilename, "rw" );
    shapeFile.writeShape( fc );
    shapeFile.close();

    //    // Hack: festes Zeitreihen File
    //    FileUtilities.makeFileFromStream( false, new File( tsFilename + ".dbf" ),
    // getClass()
    //        .getResourceAsStream( "resources/HW040427.dbf" ) );
    //    FileUtilities.makeFileFromStream( false, new File( tsFilename + ".shp" ),
    // getClass()
    //        .getResourceAsStream( "test/HW040427.shp" ) );
    //    FileUtilities.makeFileFromStream( false, new File( tsFilename + ".shx" ),
    // getClass()
    //        .getResourceAsStream( "test/HW040427.shx" ) );
  }

  public static Map createTsData( final File inputdir, final Map inputMap ) throws IOException, SensorException
  {
    final Map map = new HashMap();

    // sortiert die Daten nach der Zeit
    final Set dateSet = new TreeSet();

    // alle Zeitreihen lesen
    for( int i = 0; i < SpreeCalcJob.TS_DESCRIPTOR.length; i++ )
    {
      final TSDesc tsDesc = SpreeCalcJob.TS_DESCRIPTOR[i];

      File obsFile = null;
      try
      {
        obsFile = checkInput( tsDesc.id, inputMap, inputdir );
      }
      catch( final CalcJobServiceException cse )
      {
        // ignore, file is not present
        // TODO: better: check if required?
        continue;
      }

      final IObservation obs = ZmlFactory.parseXML( obsFile.toURL(), "" );

      final IAxis[] axisList = obs.getAxisList();

      try
      {
        final IAxis dateAxis = ObservationUtilities.findAxis( axisList, Date.class )[0];
        final IAxis valueAxis = ObservationUtilities.findAxis( axisList, Double.class )[0];
      
        final Map dateToValueMap = new HashMap();

        final ITuppleModel model = obs.getValues( null );

        for( int j = 0; j < model.getCount(); j++ )
        {
          final Date date = (Date)model.getElement( j, dateAxis );
          final Number val = (Number)model.getElement( j, valueAxis );
          final Double value = val == null ? null : new Double( val.doubleValue() );

          dateSet.add( date );

          dateToValueMap.put( date, value );
        }

        map.put( tsDesc.id, dateToValueMap );
      }
      catch( final NoSuchElementException nse )
      {
        // passiert, wenn es keine entsprechende Axen giebt
        nse.printStackTrace();
        
        throw new CalcJobServiceException( "Fehlerhafte Eingabedateien", nse );
      }
      catch( final SensorException se )
      {
        se.printStackTrace();

        throw new CalcJobServiceException( "Fehler beim Einlesen der Zeitreihen: ", se );
      }
    }

    map.put( "DATE", dateSet.toArray( new Date[dateSet.size()] ) );

    return map;
  }

  public static void findAndWriteLayer( final KalypsoFeatureLayer[] layers, final String layerName,
      final Map mapping, final String geoName, final String filenameBase )
      throws CalcJobServiceException
  {
    try
    {
      for( int i = 0; i < layers.length; i++ )
      {
        if( layers[i].getFeatureType().getName().equals( layerName ) )
        {
          ShapeSerializer.serialize( layers[i], mapping, geoName, filenameBase );
          return;
        }
      }

      throw new CalcJobServiceException(
          "EIngabedatei für Rechenmodell konnte nicht erzeugt werden. Layer nicht gefunden: "
              + layerName, null );
    }
    catch( final GmlSerializeException e )
    {
      throw new CalcJobServiceException( "Fehler beim Schreiben der Eingabedateien", e );
    }
  }

  public static KalypsoFeatureLayer[] loadGML( final File inputdir, final Map map )
      throws CalcJobServiceException
  {
    // GML lesen
    try
    {
      ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
      final CS_CoordinateSystem crs = Adapters.getDefault()
          .export( csFac.getCSByName( "EPSG:4326" ) );

      final File xsdFile = checkInput( "XSD", map, inputdir );
      final File gmlFile = checkInput( "GML", map, inputdir );

      return GmlSerializer.deserialize( xsdFile.toURL(), gmlFile.toURL(), crs, null );
    }
    catch( final Exception ioe )
    {
      ioe.printStackTrace();

      throw new CalcJobServiceException( "Fehler beim Lesen der Basisdaten", ioe );
    }
  }

  public static Map parseCalculationFile( final File propsfile, final File nativedir )
      throws CalcJobServiceException
  {
    try
    {
      final Properties props = new Properties();
      props.load( new FileInputStream( propsfile ) );

      final String date = props.getProperty( SpreeCalcJob.CALC_PROP_STARTTIME );
      final Date startTime = new SimpleDateFormat( "dd.MM.yyyy HH:mm" ).parse( date );

      final String startTimeString = new SimpleDateFormat( "yyMMdd" ).format( startTime );
      final String baseFileName = "HW" + startTimeString;

      final String tsFilename = new File( nativedir, baseFileName ).getAbsolutePath();
      final File tsFile = new File( tsFilename + ".dbf" );
      final String napFilename = tsFilename + SpreeCalcJob.NAP_FILE;
      final File napFile = new File( napFilename + ".dbf" );
      final File vhsFile = new File( tsFilename + SpreeCalcJob.VHS_FILE );
      final String flpFilename = tsFilename + SpreeCalcJob.FLP_FILE;
      final File flpFile = new File( flpFilename + ".dbf" );

      final Map dataMap = new HashMap();
      dataMap.put( SpreeCalcJob.DATA_STARTDATE, startTime );
      dataMap.put( SpreeCalcJob.DATA_STARTDATESTRING, startTimeString );
      dataMap.put( SpreeCalcJob.DATA_BASEFILENAME, baseFileName );
      dataMap.put( SpreeCalcJob.DATA_FLPFILE, flpFile );
      dataMap.put( SpreeCalcJob.DATA_VHSFILE, vhsFile );
      dataMap.put( SpreeCalcJob.DATA_NAPFILE, napFile );
      dataMap.put( SpreeCalcJob.DATA_FLPFILENAME, flpFilename );
      dataMap.put( SpreeCalcJob.DATA_NAPFILENAME, napFilename );
      dataMap.put( SpreeCalcJob.DATA_TSFILENAME, tsFilename );
      dataMap.put( SpreeCalcJob.DATA_TSFILE, tsFile );

      return dataMap;

    }
    catch( final Exception e )
    {
      throw new CalcJobServiceException( "Fehler beim Einlesen der Berechnungsparameter", e );
    }
  }

  /**
   * Gibt die Datei zum entsprechenden index zurück
   * 
   * @throws CalcJobServiceException
   */
  public static File checkInput( final String id, final Map input, final File basedir )
      throws CalcJobServiceException
  {
    final CalcJobDataBean bean = (CalcJobDataBean)input.get( id );
    if( bean == null )
      throw new CalcJobServiceException( "Eingabedatei für Index <" + id + "> fehlt", null );

    final File file = new File( basedir, bean.getPath() );
    if( !file.exists() || !file.isFile() )
      throw new CalcJobServiceException( "Eingabedatei für Index <" + id + "> fehlt: "
          + file.getAbsolutePath(), null );

    return file;
  }
}