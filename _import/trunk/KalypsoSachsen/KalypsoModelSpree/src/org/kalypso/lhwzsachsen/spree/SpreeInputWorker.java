package org.kalypso.lhwzsachsen.spree;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Properties;
import java.util.logging.Logger;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.io.shpapi.DBaseFile;
import org.deegree_impl.io.shpapi.FieldDescriptor;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.io.StreamUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.wq.WQObservationFilter;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * Diese Klasse sammelt alles, was mit dem Erzeugen der Nativen Daten aus den
 * Eingabedaten zu tun hat.
 * 
 * @author Belger
 */
public class SpreeInputWorker
{
  private final static Logger LOGGER = Logger.getLogger( SpreeInputWorker.class.getName() );

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

  /**
   * <p>
   * Converts inputfiles to nativefiles and reads control parameters
   * </p>
   * 
   * @return Location of native files
   * @throws IOException
   * @throws IOException
   */
  public static File createNativeInput( final File tmpdir, final CalcJobDataBean[] input,
      final Properties props, final PrintWriter logwriter, final TSMap tsmap ) throws IOException
  {
    try
    {
      final File nativedir = new File( tmpdir, "native" );
      nativedir.mkdirs();

      final Map inputMap = hashInput( input );

      final File controlGML = checkInput( "CONTROL_GML", inputMap, tmpdir );
      final File controlXSD = checkInput( "CONTROL_XSD", inputMap, tmpdir );

      logwriter.println( "Lese Steuerparameter: " + controlGML.getName() );

      final Map map = parseControlFile( controlGML, controlXSD, nativedir );
      props.putAll( map );

      logwriter.println( "Lese Modelldaten: " + controlGML );

      final GMLWorkspace workspace = loadGML( tmpdir, inputMap );

      final String tsFilename = writeNonTs( props, logwriter, workspace );

      final Date startDate = (Date)props.get( SpreeCalcJob.DATA_STARTSIM_DATE );
      setAnfangsstauvolumen( "V_TSQUITZ", startDate, "TS_QUITZDORF", tsmap, workspace, logwriter );
      setAnfangsstauvolumen( "V_TSBAUTZ", startDate, "TS_BAUTZEN", tsmap, workspace, logwriter );

      logwriter.println( "Erzeuge Zeitreihen-Datei: " + tsFilename );
      readZML( tmpdir, inputMap, tsmap );
      createTimeseriesFile( tsFilename, tsmap );

      return nativedir;
    }
    catch( final CalcJobServiceException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Fehler beim Erzeugen der Inputdateien", e );
    }
  }

  private static void setAnfangsstauvolumen( final String name, final Date startDate,
      final String fid, final TSMap tsmap, final GMLWorkspace workspace, final PrintWriter logwriter )
  {
    // das Anfangsstauvolumen aus der GMl raussuchen und als Wert in den
    // Zeitreihen setzen
    final Feature feature = workspace.getFeature( fid );
    final Object property = feature.getProperty( "Anfangsstauvolumen" );
    if( property != null && property instanceof Double )
      tsmap.putValue( name, startDate, (Double)property );
    else
      logwriter.println( "Kein Anfangsstauvolumen angegeben für: " + fid );
  }

  private static String writeNonTs( final Properties props, final PrintWriter logwriter,
      final GMLWorkspace workspace ) throws IOException, FileNotFoundException,
      CalcJobServiceException
  {
    final File vhsFile = (File)props.get( SpreeCalcJob.DATA_VHSFILE );
    final String flpFilename = (String)props.get( SpreeCalcJob.DATA_FLPFILENAME );
    final String napFilename = (String)props.get( SpreeCalcJob.DATA_NAPFILENAME );
    final String tsFilename = (String)props.get( SpreeCalcJob.DATA_TSFILENAME );

    logwriter.println( "Erzeuge _vhs Datei: " + vhsFile.getName() );
    StreamUtilities.streamCopy( SpreeInputWorker.class.getResourceAsStream( "resources/"
        + SpreeCalcJob.VHS_FILE ), new FileOutputStream( vhsFile ) );

    logwriter.println( "Erzeuge _flp Datei: " + flpFilename );
    findAndWriteLayer( workspace, SpreeCalcJob.FLP_NAME, SpreeCalcJob.FLP_MAP,
        SpreeCalcJob.FLP_GEOM, flpFilename );

    logwriter.println( "Erzeuge _nap Datei: " + napFilename );
    findAndWriteLayer( workspace, SpreeCalcJob.NAP_NAME, SpreeCalcJob.NAP_MAP,
        SpreeCalcJob.NAP_GEOM, napFilename );
    
    final File shpfile = new File( tsFilename + ".shp" );
    final InputStream shpresource = SpreeInputWorker.class.getResourceAsStream( "resources/HW.shp" );
    FileUtilities.makeFileFromStream( false, shpfile, shpresource );

    final File shxfile = new File( tsFilename + ".shx" );
    final InputStream shxresource = SpreeInputWorker.class.getResourceAsStream( "resources/HW.shx" );
    FileUtilities.makeFileFromStream( false, shxfile, shxresource );
    
    return tsFilename;
  }

  //    public static void createTimeseriesFile( final String tsFilename, final
  //   TSMap valuesMap, final PrintWriter logwriter ) throws
  //   CalcJobServiceException
  //    {
  //      final List ftpList = new LinkedList();
  //  
  //      // Layer erzeugen!
  //      ftpList
  //          .add( FeatureFactory.createFeatureTypeProperty( "GEOM",
  //   GM_Point.class.getName(), false ) );
  //      ftpList.add( FeatureFactory.createFeatureTypeProperty( "DZAHL",
  //   "java.lang.Double", false ) );
  //      ftpList.add( FeatureFactory.createFeatureTypeProperty( "STUNDE",
  //   "java.lang.Double", false ) );
  //      ftpList.add( FeatureFactory.createFeatureTypeProperty( "DATUM",
  //   "java.lang.String", false ) );
  //      ftpList.add( FeatureFactory.createFeatureTypeProperty( "VON",
  //   "java.lang.Double", false ) );
  //      ftpList.add( FeatureFactory.createFeatureTypeProperty( "AB",
  //   "java.lang.Double", false ) );
  //  
  //      for( int i = 0; i < SpreeCalcJob.TS_DESCRIPTOR.length; i++ )
  //      {
  //        final TSDesc desc = SpreeCalcJob.TS_DESCRIPTOR[i];
  //        ftpList.add( FeatureFactory.createFeatureTypeProperty( desc.id,
  //   "java.lang.Double", true ) );
  //      }
  //  
  //      final FeatureTypeProperty[] ftps = (FeatureTypeProperty[])ftpList
  //          .toArray( new FeatureTypeProperty[ftpList.size()] );
  //  
  //      final FeatureType type = FeatureFactory.createFeatureType( "shapetype",
  //   null, ftps,
  //          new int[ftps.length], new int[ftps.length], null, null );
  //  
  //      // Werte schreiben
  //      final DateFormat specialDateFormat = new SimpleDateFormat( "yMM.dd" );
  //      final DateFormat dateFormat = new SimpleDateFormat( "dd.MM.yyyy" );
  //      final Calendar calendar = Calendar.getInstance();
  //      
  //      final Date[] dateArray = valuesMap.getDates();
  //  
  //      final Collection shapeFeatures = new ArrayList( dateArray.length );
  //  
  //      for( int i = 0; i < dateArray.length; i++ )
  //      {
  //        final Date date = dateArray[i];
  //  
  //        final Object[] data = new Object[6 + SpreeCalcJob.TS_DESCRIPTOR.length];
  //  
  //        calendar.setTime( date );
  //        data[0] = GeometryFactory.createGM_Point( 0.0, 0.0, null );
  //        data[1] = new Double( specialDateFormat.format( date ) );
  //        data[2] = new Double( calendar.get( Calendar.HOUR_OF_DAY ) );
  //        data[3] = new String( dateFormat.format( date ) );
  //        calendar.add( Calendar.HOUR_OF_DAY, -3 );
  //        data[4] = new Double( calendar.get( Calendar.HOUR_OF_DAY ) );
  //        calendar.add( Calendar.HOUR_OF_DAY, -3 );
  //        data[5] = new Double( calendar.get( Calendar.HOUR_OF_DAY ) );
  //  
  //        for( int j = 0; j < SpreeCalcJob.TS_DESCRIPTOR.length; j++ )
  //        {
  //          final String id = SpreeCalcJob.TS_DESCRIPTOR[j].id;
  //          final Map datesToValuesMap = valuesMap.getTimeserie( id );
  //  
  //          if( datesToValuesMap == null )
  //            continue;
  //  
  //          final Double value = (Double)datesToValuesMap.get( date );
  //  
  //          Double outVal = null;
  //  
  //          if( value == null )
  //          {
  //            if( id.startsWith( "PA_" ) || id.startsWith( "PG" ) )
  //              outVal = new Double( -99.9 );
  //            else
  //              outVal = null;
  //          }
  //          else
  //            outVal = value;
  //  
  //          data[6 + j] = outVal;
  //        }
  //  
  //        for( int j = 0; j < data.length; j++ )
  //          logwriter.print( data[j] + "\t" );
  //        logwriter.println();
  //  
  //        final Feature feature = FeatureFactory.createFeature( "" + ( i + 1 ), type,
  //   data );
  //        shapeFeatures.add( feature );
  //      }
  //  
  //      try
  //      {
  //        final ShapeFile shapeFile = new ShapeFile( tsFilename, "rw" );
  //        shapeFile.writeShape( (Feature[])shapeFeatures.toArray( new
  //   Feature[shapeFeatures.size()] ) );
  //        shapeFile.close();
  //      }
  //      catch( final Exception e )
  //      {
  //        e.printStackTrace();
  //        
  //        throw new CalcJobServiceException( "Fehler beim Schreiben der Datei " +
  //   tsFilename + "\n" + e.getLocalizedMessage(), e );
  //      }
  //    }

  public static void createTimeseriesFile( final String tsFilename, final TSMap valuesMap ) throws CalcJobServiceException
  {
    try
    {
      final FieldDescriptor[] fds = new FieldDescriptor[SpreeCalcJob.TS_DESCRIPTOR.length + 5];
      fds[0] = new FieldDescriptor( "DZAHL", "N", (byte)6, (byte)2 );
      fds[1] = new FieldDescriptor( "STUNDE", "N", (byte)2, (byte)0 );
      fds[2] = new FieldDescriptor( "DATUM", "C", (byte)10, (byte)0 );
      fds[3] = new FieldDescriptor( "VON", "N", (byte)2, (byte)0 );
      fds[4] = new FieldDescriptor( "AB", "N", (byte)2, (byte)0 );

      for( int j = 0; j < SpreeCalcJob.TS_DESCRIPTOR.length; j++ )
      {
        final TSDesc desc = SpreeCalcJob.TS_DESCRIPTOR[j];
        final String name = desc.id;

        final int i = j + 5;

        if( name.startsWith( "S_" ) )
          fds[i] = new FieldDescriptor( name, "C", (byte)5, (byte)0 );
        else if( name.startsWith( "W_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte)5, (byte)0 );
        else if( name.startsWith( "Q_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte)7, (byte)2 );
        else if( name.startsWith( "QX_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte)7, (byte)2 );
        else if( name.startsWith( "WV_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte)5, (byte)0 );
        else if( name.startsWith( "QV_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte)7, (byte)2 );
        else if( name.startsWith( "QP_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte)8, (byte)3 );
        else if( name.startsWith( "PG_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte)6, (byte)1 );
        else if( name.startsWith( "PP_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte)3, (byte)0 );
        else if( name.startsWith( "PA_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte)7, (byte)2 );
        else if( name.startsWith( "V_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte)7, (byte)2 );
        else if( name.startsWith( "ZG_" ) )
          fds[i] = new FieldDescriptor( name, "N", (byte)7, (byte)2 );
      }

      final DBaseFile dbf = new DBaseFile( tsFilename, fds );

      // Werte schreiben
      final DateFormat specialDateFormat = new SimpleDateFormat( "yMM.dd" );
      final DateFormat dateFormat = new SimpleDateFormat( "dd.MM.yyyy" );
      final Calendar calendar = Calendar.getInstance();

      final Date[] dateArray = valuesMap.getDates();

      for( int i = 0; i < dateArray.length; i++ )
      {
        final Date date = dateArray[i];

        final ArrayList record = new ArrayList();

        calendar.setTime( date );
        record.add( new Double( specialDateFormat.format( date ) ) );
        record.add( new Integer( calendar.get( Calendar.HOUR_OF_DAY ) ) );
        record.add( dateFormat.format( date ) );
        calendar.add( Calendar.HOUR_OF_DAY, -3 );
        record.add( new Integer( calendar.get( Calendar.HOUR_OF_DAY ) ) );
        calendar.add( Calendar.HOUR_OF_DAY, -3 );
        record.add( new Integer( calendar.get( Calendar.HOUR_OF_DAY ) ) );

        for( int j = 0; j < SpreeCalcJob.TS_DESCRIPTOR.length; j++ )
        {
          final String id = SpreeCalcJob.TS_DESCRIPTOR[j].id;
          final Map datesToValuesMap = valuesMap.getTimeserie( id );

          Double outVal = null;

          if( datesToValuesMap != null )
          {
            final Double value = (Double)datesToValuesMap.get( date );

            if( value == null )
            {
              if( id.startsWith( "PA_" ) || id.startsWith( "PG" ) )
                outVal = new Double( -99.9 );
              else
                outVal = null;
            }
            else
              outVal = value;
          }

          record.add( outVal );
        }

        dbf.setRecord( record );
      }

      dbf.writeAllToFile();
      dbf.close();
    }
    catch( final Exception e1 )
    {
      e1.printStackTrace();

      throw new CalcJobServiceException( "Fehler beim Scheiben der Zeitreihen", e1 );
    }
  }

  /** Liest die Zeitreihen und erzeugt daraus eine Tabelle (Map) */
  public static TSMap readZML( final File inputdir, final Map inputMap, final TSMap tsmap )
      throws IOException
  {
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
        // todo: better: check if required?
        continue;
      }

      try
      {
        final IObservation obs = ZmlFactory.parseXML( obsFile.toURL(), "" );

        tsmap.addObservation( obs, tsDesc.id );

        if( tsDesc.id.startsWith( "W_" ) )
        {
          // neuen Namen generieren
          final String qName = "Q_" + tsDesc.id.substring( 2 );

          final WQObservationFilter filter = new WQObservationFilter();
          filter.initFilter( TimeserieConstants.TYPE_WATERLEVEL, obs );

          tsmap.addObservation( filter, qName );
        }
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

    return tsmap;
  }

  public static void findAndWriteLayer( final GMLWorkspace workspace, final String layerName,
      final Map mapping, final String geoName, final String filenameBase )
      throws CalcJobServiceException
  {
    try
    {
      final FeatureType featureType = workspace.getFeatureType( layerName );
      if( featureType == null )
        throw new CalcJobServiceException(
            "Eingabedatei für Rechenmodell konnte nicht erzeugt werden. Layer nicht gefunden: "
                + layerName, null );

      final Feature[] features = workspace.getFeatures( featureType );

      ShapeSerializer.serializeFeatures( features, mapping, geoName, filenameBase );
    }
    catch( final GmlSerializeException e )
    {
      e.printStackTrace();

      throw new CalcJobServiceException( "Fehler beim Schreiben der Eingabedateien", e );
    }
  }

  public static GMLWorkspace loadGML( final File inputdir, final Map map )
      throws CalcJobServiceException
  {
    // GML lesen
    try
    {
      final File xsdFile = checkInput( "MODELL_XSD", map, inputdir );
      final File gmlFile = checkInput( "GML", map, inputdir );

      return GmlSerializer.createGMLWorkspace( gmlFile.toURL(), xsdFile.toURL() );
    }
    catch( final Exception ioe )
    {
      ioe.printStackTrace();

      throw new CalcJobServiceException( "Fehler beim Lesen der Basisdaten", ioe );
    }
  }

  public static Map parseControlFile( final File controlGML, final File controlXSD,
      final File nativedir ) throws CalcJobServiceException
  {
    try
    {
      final URL gmlURL = controlGML.toURL();
      LOGGER.info( "GML-URL: " + gmlURL.toString() );

      final URL schemaURL = controlXSD.toURL();
      LOGGER.info( "Schema-URL: " + schemaURL.toString() );

      final Feature controlFeature = GmlSerializer.createGMLWorkspace( gmlURL, schemaURL )
          .getRootFeature();

      final Date startSimTime = (Date)controlFeature.getProperty( "startsimulation" );
      final Date startForecastTime = (Date)controlFeature.getProperty( "startforecast" );

      final String startTimeString = new SimpleDateFormat( "yyMMdd" ).format( startForecastTime );
      final String baseFileName = "HW" + startTimeString;

      final String tsFilename = new File( nativedir, baseFileName ).getAbsolutePath();
      final File tsFile = new File( tsFilename + ".dbf" );
      final String napFilename = tsFilename + SpreeCalcJob.NAP_FILE;
      final File napFile = new File( napFilename + ".dbf" );
      final File vhsFile = new File( tsFilename + SpreeCalcJob.VHS_FILE );
      final String flpFilename = tsFilename + SpreeCalcJob.FLP_FILE;
      final File flpFile = new File( flpFilename + ".dbf" );

      final Map dataMap = new HashMap();
      dataMap.put( SpreeCalcJob.DATA_STARTSIM_DATE, startSimTime );
      dataMap.put( SpreeCalcJob.DATA_STARTFORECAST_DATE, startForecastTime );
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