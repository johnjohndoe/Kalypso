package org.kalypso.services.calcjob.spree;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.bind.JAXBException;

import org.deegree_impl.model.cs.Adapters;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.io.ReaderUtilities;
import org.kalypso.java.io.StreamUtilities;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlObservation;
import org.kalypso.services.calcjob.CalcJobException;
import org.kalypso.services.calcjob.CalcJobStatus;
import org.kalypso.services.calcjob.impl.jobs.AbstractCalcJob;
import org.kalypso.services.calcjob.impl.jobs.CalcJobProgressMonitor;
import org.kalypso.zml.ObservationType;
import org.opengis.cs.CS_CoordinateSystem;
import org.xml.sax.InputSource;

/**
 * <p>
 * Der Rechenservice für das SpreeModell
 * </p>
 * <p>
 * Erwartet als Argumente ein Schema und ein GML
 * </p>.
 * 
 * @author gernot
 */
public class SpreeCalcJob extends AbstractCalcJob
{
  private static final String CALC_PROP_STARTTIME = "startTime";

  private static final String VHS_FILE = "_vhs.dbf";

  private static final String FLP_FILE = "_flp";

  private static final String FLP_NAME = "Flusslaufmodell";

  private static final String FLP_GEOM = "Ort";

  private static final Map FLP_MAP = new LinkedHashMap();
  static
  {
    FLP_MAP.put( "PEGEL", "Name" );
    FLP_MAP.put( "KORRFAKTOR", "Korrektur_Faktor" );
    FLP_MAP.put( "NIVEAUKORR", "Korrektur_Niveau" );
    FLP_MAP.put( "LAUFZEITK", "Korrektur_Laufzeit" );
    FLP_MAP.put( "LZK_EMPF", "KorrekturEmpfehlungLaufzeit" );
  }

  private static final String NAP_FILE = "_nap";

  private static final String NAP_NAME = "Einzugsgebiet";

  private static final String NAP_GEOM = "Ort";

  private static final Map NAP_MAP = new LinkedHashMap();

  static
  {
    NAP_MAP.put( "PEGEL", "Name" );
    NAP_MAP.put( "MIN", "BodenfeuchteMin" );
    NAP_MAP.put( "VORFEUCHTE", "Bodenfeuchte" );
    NAP_MAP.put( "MAX", "BodenfeuchteMax" );
  }

  private final CS_CoordinateSystem m_targetCrs;

  public static final Object DATA_STARTDATE = "startDate";

  public static final Object DATA_STARTDATESTRING = "startDateString";

  public static final Object DATA_BASEFILENAME = "baseFileName";

  public static final Object DATA_DATADIR = "dataDir";

  public static final Object DATA_FLPFILE = "flpFile";

  public static final Object DATA_VHSFILE = "vhsFile";

  public static final Object DATA_NAPFILE = "napFile";

  public static final Object DATA_NAPFILENAME = "napFilename";

  public static final Object DATA_FLPFILENAME = "flpFilename";

  public static final Object DATA_TSFILENAME = "tsFilename";

  public static final Object DATA_TSFILE = "tsFile";

  public static final Object DATA_LABEL = "label";

  /**
   * Zwischenspeichern für diverse Variablen. Die Key sind alle Konstanten der
   * Art DATA_
   */
  private final Map m_data = new HashMap();

  private TSDesc[] m_tsDescriptor = new TSDesc[]
  {
      new TSDesc( "S_SCHIRG", 0, "", "" ),
      new TSDesc( "W_SCHIRG", 0, "", "" ),
      new TSDesc( "Q_SCHIRG", 0, "", "" ),
      new TSDesc( "QX_SCHIRG", 0, "", "" ),
      new TSDesc( "WV_SCHIRG", 0, "", "" ),
      new TSDesc( "QV_SCHIRG", 0, "", "" ),
      new TSDesc( "QP_SCHIRG", 0, "", "" ),
      new TSDesc( "PG_SCHIRG", 3, "", "" ),
      new TSDesc( "PP_SCHIRG", 0, "", "" ),
      new TSDesc( "PA_SCHIRG", 0, "", "" ),
      new TSDesc( "S_BAUTZWB", 0, "", "" ),
      new TSDesc( "W_BAUTZWB", 0, "", "" ),
      new TSDesc( "Q_BAUTZWB", 0, "", "" ),
      new TSDesc( "QX_BAUTZWB", 0, "", "" ),
      new TSDesc( "WV_BAUTZWB", 0, "", "" ),
      new TSDesc( "QV_BAUTZWB", 0, "", "" ),
      new TSDesc( "QP_BAUTZWB", 0, "", "" ),
      new TSDesc( "PG_BAUTZWB", 4, "", "" ),
      new TSDesc( "PP_BAUTZWB", 0, "", "" ),
      new TSDesc( "PA_BAUTZWB", 0, "", "" ),
      new TSDesc( "S_TSBAUTZ", 0, "", "" ),
      new TSDesc( "Q_TSBAUTZ", 0, "", "" ),
      new TSDesc( "QV_TSBAUTZ", 0, "", "" ),
      new TSDesc( "QP_TSBAUTZ", 0, "", "" ),
      new TSDesc( "V_TSBAUTZ", 0, "", "" ),
      new TSDesc( "S_GROEDI", 0, "", "" ),
      new TSDesc( "W_GROEDI", 0, "", "" ),
      new TSDesc( "Q_GROEDI", 0, "", "" ),
      new TSDesc( "QX_GROEDI", 0, "", "" ),
      new TSDesc( "WV_GROEDI", 0, "", "" ),
      new TSDesc( "QV_GROEDI", 0, "", "" ),
      new TSDesc( "QP_GROEDI", 0, "", "" ),
      new TSDesc( "ZG_GROEDI", 0, "", "" ),
      new TSDesc( "PG_GROEDI", 5, "", "" ),
      new TSDesc( "PP_GROEDI", 0, "", "" ),
      new TSDesc( "PA_GROEDI", 0, "", "" ),
      new TSDesc( "S_SPWIESE", 0, "", "" ),
      new TSDesc( "QV_SPWIESE", 0, "", "" ),
      new TSDesc( "QP_SPWIESE", 0, "", "" ),
      new TSDesc( "S_LIESKE", 0, "", "" ),
      new TSDesc( "W_LIESKE", 0, "", "" ),
      new TSDesc( "Q_LIESKE", 0, "", "" ),
      new TSDesc( "QX_LIESKE", 0, "", "" ),
      new TSDesc( "WV_LIESKE", 0, "", "" ),
      new TSDesc( "QV_LIESKE", 0, "", "" ),
      new TSDesc( "QP_LIESKE", 0, "", "" ),
      new TSDesc( "S_JAENKD", 0, "", "" ),
      new TSDesc( "W_JAENKD", 0, "", "" ),
      new TSDesc( "Q_JAENKD", 0, "", "" ),
      new TSDesc( "QX_JAENKD", 0, "", "" ),
      new TSDesc( "WV_JAENKD", 0, "", "" ),
      new TSDesc( "QV_JAENKD", 0, "", "" ),
      new TSDesc( "QP_JAENKD", 0, "", "" ),
      new TSDesc( "PG_JAENKD", 6, "", "" ),
      new TSDesc( "PP_JAENKD", 0, "", "" ),
      new TSDesc( "PA_JAENKD", 0, "", "" ),
      new TSDesc( "S_TSQUITZ", 0, "", "" ),
      new TSDesc( "Q_TSQUITZ", 0, "", "" ),
      new TSDesc( "QV_TSQUITZ", 0, "", "" ),
      new TSDesc( "QP_TSQUITZ", 0, "", "" ),
      new TSDesc( "V_TSQUITZ", 0, "", "" ),
      new TSDesc( "S_SAERI", 0, "", "" ),
      new TSDesc( "W_SAERI", 0, "", "" ),
      new TSDesc( "Q_SAERI", 0, "", "" ),
      new TSDesc( "QX_SAERI", 0, "", "" ),
      new TSDesc( "WV_SAERI", 0, "", "" ),
      new TSDesc( "QV_SAERI", 0, "", "" ),
      new TSDesc( "QP_SAERI", 0, "", "" ),
      new TSDesc( "ZG_SAERI", 0, "", "" ),
      new TSDesc( "PG_SAERI", 7, "", "" ),
      new TSDesc( "PP_SAERI", 0, "", "" ),
      new TSDesc( "PA_SAERI", 0, "", "" ),
      new TSDesc( "S_BOXBRG", 0, "", "" ),
      new TSDesc( "W_BOXBRG", 0, "", "" ),
      new TSDesc( "Q_BOXBRG", 0, "", "" ),
      new TSDesc( "QX_BOXBRG", 0, "", "" ),
      new TSDesc( "WV_BOXBRG", 0, "", "" ),
      new TSDesc( "QV_BOXBRG", 0, "", "" ),
      new TSDesc( "QP_BOXBRG", 0, "", "" ),
      new TSDesc( "S_BWALDE", 0, "", "" ),
      new TSDesc( "QV_BWALDE", 0, "", "" ),
      new TSDesc( "QP_BWALDE", 0, "", "" ),
      new TSDesc( "S_LOHSA", 0, "", "" ),
      new TSDesc( "QV_LOHSA", 0, "", "" ),
      new TSDesc( "QP_LOHSA", 0, "", "" ),
      new TSDesc( "S_SPREY", 0, "", "" ),
      new TSDesc( "W_SPREY", 0, "", "" ),
      new TSDesc( "Q_SPREY", 0, "", "" ),
      new TSDesc( "QX_SPREY", 0, "", "" ),
      new TSDesc( "WV_SPREY", 0, "", "" ),
      new TSDesc( "QV_SPREY", 0, "", "" ),
      new TSDesc( "QP_SPREY", 0, "", "" ),
      new TSDesc( "S_BURGNEU", 0, "", "" ),
      new TSDesc( "QP_BURGNEU", 0, "", "" ),
      new TSDesc( "S_SPWITZ", 0, "", "" ),
      new TSDesc( "W_SPWITZ", 0, "", "" ),
      new TSDesc( "Q_SPWITZ", 0, "", "" ),
      new TSDesc( "QX_SPWITZ", 0, "", "" ),
      new TSDesc( "WV_SPWITZ", 0, "", "" ),
      new TSDesc( "QV_SPWITZ", 0, "", "" ),
      new TSDesc( "QP_SPWITZ", 0, "", "" ),
      new TSDesc( "S_RLKETTE", 0, "", "" ),
      new TSDesc( "QV_RLKETTE", 0, "", "" ),
      new TSDesc( "QP_RLKETTE", 0, "", "" ),
      new TSDesc( "S_SPREMB", 0, "", "" ),
      new TSDesc( "W_SPREMB", 0, "", "" ),
      new TSDesc( "Q_SPREMB", 0, "", "" ),
      new TSDesc( "QX_SPREMB", 0, "", "" ),
      new TSDesc( "WV_SPREMB", 0, "", "" ),
      new TSDesc( "QV_SPREMB", 0, "", "" ),
      new TSDesc( "QP_SPREMB", 0, "", "" ) };

  public SpreeCalcJob()
  {
    ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    m_targetCrs = Adapters.getDefault().export( csFac.getCSByName( "EPSG:4326" ) );
  }

  /**
   * @see org.kalypso.services.calcjob.impl.jobs.AbstractCalcJob#runIntern(java.net.URL[], org.kalypso.services.calcjob.impl.jobs.CalcJobProgressMonitor)
   */
  protected URL[] runIntern( final URL[] arguments, final CalcJobProgressMonitor monitor )
      throws CalcJobException
  {
    checkArguments( arguments );
    parseProperties( arguments[0], monitor );
    final KalypsoFeatureLayer[] layers = parseInput( arguments );
    writeInput( layers, monitor, arguments );

    startCalculation();

    return loadOutput( monitor );
  }

  private void parseProperties( final URL properties, final CalcJobProgressMonitor monitor )
      throws CalcJobException
  {
    final Properties props = new Properties();
    try
    {
      props.load( properties.openStream() );

      final String date = props.getProperty( CALC_PROP_STARTTIME );
      final Date startTime = new SimpleDateFormat( "dd.MM.yyyy HH:mm" ).parse( date );

      final String startTimeString = new SimpleDateFormat( "yyMMdd" ).format( startTime );
      final String baseFileName = "HW" + startTimeString;

      final File dataDir = FileUtilities.createRandomTmpDir( "SpreeCalculation" );
      if( dataDir == null )
        throw new CalcJobException( "Konnte Temporäres Verzeichnis nicht erzeugen" );

      final String tsFilename = new File( dataDir, baseFileName ).getAbsolutePath();
      final File tsFile = new File( tsFilename + ".dbf" );
      final String napFilename = tsFilename + NAP_FILE;
      final File napFile = new File( napFilename + ".dbf" );
      final File vhsFile = new File( tsFilename + VHS_FILE );
      final String flpFilename = tsFilename + FLP_FILE;
      final File flpFile = new File( flpFilename + ".dbf" );

      m_data.put( DATA_STARTDATE, startTime );
      m_data.put( DATA_STARTDATESTRING, startTimeString );
      m_data.put( DATA_BASEFILENAME, baseFileName );
      m_data.put( DATA_DATADIR, dataDir );
      m_data.put( DATA_FLPFILE, flpFile );
      m_data.put( DATA_VHSFILE, vhsFile );
      m_data.put( DATA_NAPFILE, napFile );
      m_data.put( DATA_FLPFILENAME, flpFilename );
      m_data.put( DATA_NAPFILENAME, napFilename );
      m_data.put( DATA_TSFILENAME, tsFilename );
      m_data.put( DATA_TSFILE, tsFile );
      m_data.put( DATA_LABEL, getDescription().getDescription() );

    }
    catch( final Exception e )
    {
      throw new CalcJobException( "Fehler beim Einlesen der Berechnungsparameter", e );
    }
    finally
    {
      monitor.worked( 100 );
    }
  }

  private void checkArguments( final URL[] arguments ) throws CalcJobException
  {
    if( arguments.length < 3 || arguments[0] == null || arguments[1] == null
        || arguments[2] == null )
      throw new CalcJobException( "Argumente falsch" );
  }

  private URL[] loadOutput( final CalcJobProgressMonitor monitor ) throws CalcJobException
  {
    try
    {
      final File napFile = (File)m_data.get( DATA_NAPFILE );
      final File vhsFile = (File)m_data.get( DATA_VHSFILE );
      final File flpFile = (File)m_data.get( DATA_FLPFILE );
      final File tsFile = (File)m_data.get( DATA_TSFILE );
      final String tsFilename = (String)m_data.get( DATA_TSFILENAME );

      monitor.beginTask( "Ergebnisdaten erzeugen" );

      final File dataDir = (File)m_data.get( DATA_DATADIR );
      final File resultDir = new File( dataDir, "resultTS" );
      resultDir.mkdir();

      final Collection tsUrls = writeResultsToFolder( tsFilename, resultDir, m_data );

      monitor.done();

      final Collection urls = new ArrayList();
      urls.add( napFile.toURL() );
      urls.add( vhsFile.toURL() );
      urls.add( flpFile.toURL() );
      urls.add( tsFile.toURL() );
      urls.addAll( tsUrls );

      return (URL[])urls.toArray( new URL[urls.size()] );
    }
    catch( final Exception e )
    {
      throw new CalcJobException( e );
    }
  }

  private void startCalculation() throws CalcJobException
  {
    InputStreamReader inStream = null;
    InputStreamReader errStream = null;

    try
    {
      final File batFile = new File( "D:\\VSpree\\Debug\\trick.bat" );
      batFile.delete();
      final File tsFile = (File)m_data.get( DATA_TSFILE );
      final String commandString = "D:/VSpree/Debug/hw.exe" + " " + tsFile.getAbsolutePath();
      //      final String commandString = "D:/VSpree/Debug/hw.exe"; // + " " +
      // tsFile.getAbsolutePath();
      final Date startTime = (Date)m_data.get( DATA_STARTDATE );

      // create crackfile
      final PrintWriter crackWriter = new PrintWriter( new OutputStreamWriter(
          new FileOutputStream( batFile ) ) );
      crackWriter.println( "date " + new SimpleDateFormat( "dd-MM-yyyy" ).format( startTime ) );
      crackWriter.println( "time " + new SimpleDateFormat( "hh:mm" ).format( startTime ) );
      crackWriter.println( commandString );
      crackWriter.close();

      final File outputDir = (File)m_data.get( DATA_DATADIR );

      final Process process = Runtime.getRuntime().exec( "cmd.exe /C " + batFile.getAbsolutePath(),
          null, outputDir );

      inStream = new InputStreamReader( process.getInputStream() );
      errStream = new InputStreamReader( process.getErrorStream() );
      while( true )
      {
        ReaderUtilities.dumpAllAvailable( inStream );
        ReaderUtilities.dumpAllAvailable( errStream );

        try
        {
          process.exitValue();
          return;
        }
        catch( IllegalThreadStateException e )
        {
          // noch nicht fertig
        }

        if( getDescription().getState() == CalcJobStatus.CANCELED )
        {
          process.destroy();
          throw new CalcJobException( "Benutzerabbruch" );
        }

        Thread.sleep( 100 );
      }
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new CalcJobException( "Fehler beim Ausführen der hw.exe", e );
    }
    catch( InterruptedException e )
    {
      e.printStackTrace();
      throw new CalcJobException( "Fehler beim Ausführen der hw.exe", e );
    }
    finally
    {
      try
      {
        if( inStream != null )
          inStream.close();

        if( errStream != null )
          errStream.close();
      }
      catch( IOException e1 )
      {
        e1.printStackTrace();
      }
    }
  }

  private void writeInput( final KalypsoFeatureLayer[] layers, final CalcJobProgressMonitor monitor,
      final URL[] arguments ) throws CalcJobException
  {
    // Basisdateinamen ermitteln
    try
    {
      final File vhsFile = (File)m_data.get( DATA_VHSFILE );
      final String flpFilename = (String)m_data.get( DATA_FLPFILENAME );
      final String napFilename = (String)m_data.get( DATA_NAPFILENAME );
      final String tsFilename = (String)m_data.get( DATA_TSFILENAME );

      StreamUtilities.streamCopy( getClass().getResourceAsStream( "resources/" + VHS_FILE ),
          new FileOutputStream( vhsFile ) );

      findAndWriteLayer( layers, FLP_NAME, FLP_MAP, FLP_GEOM, flpFilename );
      findAndWriteLayer( layers, NAP_NAME, NAP_MAP, NAP_GEOM, napFilename );

      final Map valuesMap = createTsData( arguments );
      createTimeseriesFile( tsFilename, valuesMap );

      monitor.worked( 25 );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CalcJobException( "Fehler beim Erzeugen der Inputdateien", e );
    }
  }

  private Map createTsData( final URL[] arguments ) throws JAXBException, IOException,
      SensorException
  {
    final Map map = new HashMap();

    final Set dateSet = new TreeSet();

    // alle Zeitreihen lesen
    for( int i = 0; i < m_tsDescriptor.length; i++ )
    {
      final TSDesc tsDesc = m_tsDescriptor[i];

      final int urlIndex = tsDesc.inputIndex;
      if( urlIndex < 3 )
        continue;

      final URL u = arguments[urlIndex];
      final ZmlObservation obs = new ZmlObservation( u );

      final IAxis[] axisList = obs.getAxisList();
      int dateIndex = -1;
      int valueIndex = -1;
      for( int j = 0; j < axisList.length; j++ )
      {
        if( dateIndex == -1 && axisList[j].getDataClass() == Date.class )
          dateIndex = j;
        if( valueIndex == -1 && axisList[j].getDataClass() == Double.class )
          valueIndex = j;

        if( dateIndex != -1 && valueIndex != -1 )
          break;
      }

      try
      {
        final ArrayList values = new ArrayList();

        final ITuppleModel model = obs.getValues( null );

        for( int j = 0; j < model.getCount(); j++ )
        {
          final Date date = (Date)model.getElement( j, dateIndex );
          final Number val = (Number)model.getElement( j, valueIndex );
          final Double value = val == null ? null : new Double( val.doubleValue() );

          dateSet.add( date );

          values.add( value );
        }

        map.put( tsDesc.column, values );
      }
      catch( SensorException se )
      {
        se.printStackTrace();
      }
    }

    map.put( "DATE", dateSet.toArray( new Date[dateSet.size()] ) );

    return map;
  }

  private void findAndWriteLayer( final KalypsoFeatureLayer[] layers, final String layerName,
      final Map mapping, final String geoName, final String filenameBase ) throws CalcJobException
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

      throw new CalcJobException(
          "EIngabedatei für Rechenmodell konnte nicht erzeugt werden. Layer nicht gefunden: "
              + layerName );
    }
    catch( final GmlSerializeException e )
    {
      throw new CalcJobException( "Fehler beim Schreiben der Eingabedateien", e );
    }
  }

  private KalypsoFeatureLayer[] parseInput( final URL[] arguments )
  {
    try
    {
      final InputSource schemaSource = new InputSource( arguments[1].openStream() );
      final InputSource gmlSource = new InputSource( arguments[2].openStream() );

      return GmlSerializer.deserialize( schemaSource, gmlSource, m_targetCrs, null );
    }
    catch( final Exception ioe )
    {
      getDescription().setMessage( ioe.getLocalizedMessage() );
      return null;
    }
  }

  /**
   * @see org.kalypso.services.calcjob.CalcJob#disposeJob()
   */
  public void disposeJob()
  {
    final File folder = (File)m_data.get( DATA_DATADIR );
    FileUtilities.deleteRecursive( folder );
  }

  private void createTimeseriesFile( final String tsFilename, final Map valuesMap )
      throws Exception
  {
    valuesMap.getClass();
    //    final List ftpList = new LinkedList();
    //
    //    // Layer erzeugen!
    //    ftpList.add( FeatureFactory.createFeatureTypeProperty( "DZAHL",
    // "java.lang.Double", false ) );
    //    ftpList.add( FeatureFactory.createFeatureTypeProperty( "STUNDE",
    // "java.lang.Double", false ) );
    //    ftpList.add( FeatureFactory.createFeatureTypeProperty( "DATUM",
    // "java.lang.String", false ) );
    //    ftpList.add( FeatureFactory.createFeatureTypeProperty( "VON",
    // "java.lang.Double", false ) );
    //    ftpList.add( FeatureFactory.createFeatureTypeProperty( "AB",
    // "java.lang.Double", false ) );
    //
    //    for( int i = 0; i < m_tsDescriptor.length; i++ )
    //    {
    //      final TSDesc desc = m_tsDescriptor[i];
    //      ftpList
    //          .add( FeatureFactory.createFeatureTypeProperty( desc.column,
    // "java.lang.Double", true ) );
    //    }
    //
    //    final FeatureTypeProperty[] ftps = (FeatureTypeProperty[])ftpList
    //        .toArray( new FeatureTypeProperty[ftpList.size()] );
    //    final FeatureType type = FeatureFactory.createFeatureType( null, null,
    // "TS_TYPE", ftps );
    //    final FeatureCollection fc = FeatureFactory
    //        .createFeatureCollection( tsFilename, type, null, 10 );
    //
    //    // Werte schreiben
    //    final DateFormat specialDateFormat = new SimpleDateFormat( "yMM.dd" );
    //    final DateFormat dateFormat = new SimpleDateFormat( "dd.MM.yyyy" );
    //    final Calendar calendar = Calendar.getInstance();
    //    final Date[] dateArray = (Date[])valuesMap.get( "DATE" );
    //    for( int i = 0; i < dateArray.length; i++ )
    //    {
    //      final Date date = dateArray[i];
    //
    //      final Object[] data = new Object[5 + m_tsDescriptor.length];
    //
    //      calendar.setTime( date );
    //      data[0] = new Double( specialDateFormat.format( date ) );
    //      data[1] = new Double( calendar.get( Calendar.HOUR_OF_DAY ) );
    //      data[2] = new String( dateFormat.format( date ) );
    //      calendar.add( Calendar.HOUR_OF_DAY, -3 );
    //      data[3] = new Double( calendar.get( Calendar.HOUR_OF_DAY ) );
    //      data[4] = new Double( calendar.get( Calendar.HOUR_OF_DAY ) );
    //
    //      for( int j = 0; j < m_tsDescriptor.length; j++ )
    //        data[5 + j] = new Double( 0 );
    //
    //      final Feature feature = FeatureFactory.createFeature( "" + ( i + 1 ),
    // type, data );
    //      fc.appendFeature( feature );
    //    }
    //
    //    final ShapeFile shapeFile = new ShapeFile( tsFilename, "rw" );
    //    shapeFile.writeShape( fc );
    //    shapeFile.close();

    // TODO: Hack: festes Zeitreihen File
    FileUtilities.makeFileFromStream( false, new File( tsFilename + ".dbf" ), getClass()
        .getResourceAsStream( "resources/HW040427.dbf" ) );
    FileUtilities.makeFileFromStream( false, new File( tsFilename + ".shp" ), getClass()
        .getResourceAsStream( "test/HW040427.shp" ) );
    FileUtilities.makeFileFromStream( false, new File( tsFilename + ".shx" ), getClass()
        .getResourceAsStream( "test/HW040427.shx" ) );
  }

  public Collection writeResultsToFolder( final String tsFilename, final File resultDir, final Map dataMap ) throws Exception
  {
    final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    final CS_CoordinateSystem crs = org.deegree_impl.model.cs.Adapters.getDefault().export(
        csFac.getCSByName( "EPSG:4326" ) );

    final KalypsoFeatureLayer layer = ShapeSerializer.deserialize( tsFilename, crs, crs, null );

    final DateFormat dateFormat = new SimpleDateFormat( "dd.MM.yyyy" );
    final Calendar calendar = new GregorianCalendar();

    // für jede Spalte der Liste eine Zeitreihe erzeugen!
    final Map valuesMap = new HashMap();
    final Collection dates = new ArrayList();

    final KalypsoFeature[] features = layer.getAllFeatures();
    for( int i = 0; i < features.length; i++ )
    {
      final KalypsoFeature feature = features[i];

      final String dateString = (String)feature.getProperty( "DATUM" );
      final Date date = dateFormat.parse( dateString );
      final int hour = ( (Integer)feature.getProperty( "STUNDE" ) ).intValue();
      calendar.setTime( date );
      calendar.add( Calendar.HOUR_OF_DAY, hour );
      dates.add( calendar.getTime() );

      for( int j = 0; j < m_tsDescriptor.length; j++ )
      {
        final TSDesc desc = m_tsDescriptor[j];
        final String column = desc.column;

        Collection values = (Collection)valuesMap.get( column );
        if( values == null )
        {
          values = new ArrayList();
          valuesMap.put( column, values );
        }

        final Object value = feature.getProperty( column );
        double dblVal = Double.NaN;
        if( value instanceof Number )
        {
          dblVal = ( (Number)value ).doubleValue();
        }
        else
          dblVal = Double.NaN;

        if( Double.isNaN( dblVal ) || Math.abs( dblVal + 99.9 ) < 0.01 )
          values.add( null );
        else
          values.add( new Double( dblVal ) );
      }
    }

    final DefaultAxis dateAxis = new DefaultAxis( "Datum", "datum", "", Date.class, false, 0 );
    final IAxis valueAxis = new DefaultAxis( "Wert", "wert", "", Double.class, false, 1 );
    final IAxis[] achsen = new IAxis[]
    {
        dateAxis,
        valueAxis };

    final Date[] dateArray = (Date[])dates.toArray( new Date[dates.size()] );

    // create ZML for each timeserie
    final Collection urls = new ArrayList();
    for( int i = 0; i < m_tsDescriptor.length; i++ )
    {
      final TSDesc desc = m_tsDescriptor[i];
      final String column = desc.column;

      // TODO: save tuple to result dir
      //      final String outdirname = desc.outFiledir;
      final String outdirname = "Zeitreihen";
      //      final String outfilename = desc.outFilename + ".zml";
      final String outfilename = column + ".zml";
      final File outputDir = new File( resultDir, outdirname );
      outputDir.mkdirs();
      final File outFile = new File( outputDir, outfilename );

      final Collection values = (Collection)valuesMap.get( column );
      if( values == null )
      {
        FileUtilities.makeFileFromStream( false, outFile, getClass().getResourceAsStream(
            "resources/empty.zml" ) );
        urls.add( outFile.toURL() );
        continue;
      }

      final int size = values.size();
      final Double[] valueArray = (Double[])values.toArray( new Double[size] );

      final Collection tuples = new ArrayList( dateArray.length );

      for( int j = 0; j < size; j++ )
      {
        final Date date = dateArray[j];
        final Double value = valueArray[j];

        if( date != null && value != null )
          tuples.add( new Object[]
          {
              date,
              value } );
      }

      if( tuples.size() > 0 )
      {
        final Object[][] tupleArray = (Object[][])tuples.toArray( new Object[tuples.size()][] );
        final SimpleTuppleModel model = new SimpleTuppleModel( achsen, tupleArray );

        final MetadataList metadata = new MetadataList();
        metadata.setProperty( "Berechnung", (String)dataMap.get( DATA_LABEL ) );
        metadata.setProperty( "StartZeit", (String)dataMap.get( DATA_STARTDATESTRING ) );

        final IObservation observation = new SimpleObservation( column, false, null, metadata,
            achsen );
        observation.setValues( model );

        final ObservationType observationType = ZmlFactory.createXML( observation );
        final FileOutputStream outStream = new FileOutputStream( outFile );
        ZmlFactory.getMarshaller().marshal( observationType, outStream );
        outStream.close();

      }
      else
        FileUtilities.makeFileFromStream( false, outFile, getClass().getResourceAsStream(
            "resources/empty.zml" ) );

      urls.add( outFile.toURL() );
    }

    return urls;
  }

  private final static class TSDesc
  {
    public final String column;

    public final int inputIndex;

    public final String outFiledir;

    public final String outFilename;

    public TSDesc( final String name, final int inputindex, final String outfiledir,
        final String outfilename )
    {
      this.column = name;
      this.inputIndex = inputindex;
      this.outFiledir = outfiledir;
      this.outFilename = outfilename;
    }
  }

}