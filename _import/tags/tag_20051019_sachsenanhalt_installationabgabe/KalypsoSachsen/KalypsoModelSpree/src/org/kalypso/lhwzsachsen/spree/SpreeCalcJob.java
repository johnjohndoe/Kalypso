package org.kalypso.lhwzsachsen.spree;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.URL;
import java.net.URLConnection;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.io.CopyUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.io.output.NullOutputStream;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.envelope.TranProLinFilterUtilities;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.xml.sax.InputSource;

/**
 * <p>
 * Der Rechenservice für das SpreeModell
 * </p>
 * 
 * @author Belger
 */
public class SpreeCalcJob implements ICalcJob
{
  public static final String VHS_FILE = "_vhs.dbf";

  public static final String FLP_FILE = "_flp";

  public static final String FLP_NAME = "FlusslaufModell";

  public static final String FLP_GEOM = "Ort";

  public static final Map FLP_MAP = new LinkedHashMap();
  static
  {
    FLP_MAP.put( "PEGEL", "Name" );
    FLP_MAP.put( "KORRFAKTOR", "Korrektur_Faktor" );
    FLP_MAP.put( "NIVEAUKORR", "Korrektur_Niveau" );
    FLP_MAP.put( "LAUFZEITK", "Korrektur_Laufzeit" );
    FLP_MAP.put( "LZK_EMPF", "KorrekturEmpfehlungLaufzeit" );
  }

  public static final String NAP_FILE = "_nap";

  public static final String NAP_NAME = "Einzugsgebiet";

  public static final String NAP_GEOM = "Ort";

  public static final Map NAP_MAP = new LinkedHashMap();

  static
  {
    NAP_MAP.put( "PEGEL", "Name" );
    NAP_MAP.put( "MIN", "BodenfeuchteMin" );
    NAP_MAP.put( "VORFEUCHTE", "Bodenfeuchte" );
    NAP_MAP.put( "MAX", "BodenfeuchteMax" );
  }

  public static final Object DATA_STARTFORECAST_STRING = "startDate";

  public static final Object DATA_STARTSIM_DATE = "startSimulation";

  public static final Object DATA_STARTFORECAST_DATE = "startForecast";

  public static final Object DATA_STARTDATESTRING = "startDateString";

  public static final Object DATA_BASEFILENAME = "baseFileName";

  public static final Object DATA_GML = "gmlWorkspace";

  public static final Object DATA_FLPFILE = "flpFile";

  public static final Object DATA_VHSFILE = "vhsFile";

  public static final Object DATA_NAPFILE = "napFile";

  public static final Object DATA_NAPFILENAME = "napFilename";

  public static final Object DATA_FLPFILENAME = "flpFilename";

  public static final Object DATA_TSFILENAME = "tsFilename";

  public static final Object DATA_TSFILE = "tsFile";

  public static final Object DATA_LABEL = "label";

  public final static TSDesc[] TS_DESCRIPTOR = new TSDesc[]
  {
      new TSDesc( "S_SCHIRG" ),
      new TSDesc( "W_SCHIRG", false, null, true ),
      new TSDesc( "Q_SCHIRG" ),
      new TSDesc( "QX_SCHIRG", true, "W_SCHIRG", false ),
      new TSDesc( "WV_SCHIRG" ),
      new TSDesc( "QV_SCHIRG", true, "W_SCHIRG", false ),
      new TSDesc( "QP_SCHIRG", true, "W_SCHIRG", false ),
      new TSDesc( "PG_SCHIRG" ),
      new TSDesc( "PP_SCHIRG" ),
      new TSDesc( "PA_SCHIRG", false, null, true ),
      new TSDesc( "S_BAUTZWB" ),
      new TSDesc( "W_BAUTZWB", false, null, true ),
      new TSDesc( "Q_BAUTZWB" ),
      new TSDesc( "QX_BAUTZWB", true, "W_BAUTZWB", false ),
      new TSDesc( "WV_BAUTZWB" ),
      new TSDesc( "QV_BAUTZWB", true, "W_BAUTZWB", false ),
      new TSDesc( "QP_BAUTZWB", true, "W_BAUTZWB", false ),
      new TSDesc( "PG_BAUTZWB" ),
      new TSDesc( "PP_BAUTZWB" ),
      new TSDesc( "PA_BAUTZWB", false, null, true ),
      new TSDesc( "S_TSBAUTZ" ),
      new TSDesc( "Q_TSBAUTZ", true, "QV_TSBAUTZ", false ),
      new TSDesc( "QV_TSBAUTZ", false, null, true ),
      new TSDesc( "QP_TSBAUTZ", true, "QV_TSBAUTZ", false ),
      new TSDesc( "V_TSBAUTZ", true, "QV_TSBAUTZ", false ),
      new TSDesc( "S_GROEDI" ),
      new TSDesc( "W_GROEDI", false, null, true ),
      new TSDesc( "Q_GROEDI" ),
      new TSDesc( "QX_GROEDI", true, "W_GROEDI", false ),
      new TSDesc( "WV_GROEDI" ),
      new TSDesc( "QV_GROEDI", true, "W_GROEDI", false ),
      new TSDesc( "QP_GROEDI", true, "W_GROEDI", false ),
      new TSDesc( "ZG_GROEDI" ),
      new TSDesc( "PG_GROEDI" ),
      new TSDesc( "PP_GROEDI" ),
      new TSDesc( "PA_GROEDI", false, null, true ),
      new TSDesc( "S_SPWIESE" ),
      new TSDesc( "QV_SPWIESE", false, null, true ),
      new TSDesc( "QP_SPWIESE" ),
      new TSDesc( "S_LIESKE" ),
      new TSDesc( "W_LIESKE", false, null, true ),
      new TSDesc( "Q_LIESKE" ),
      new TSDesc( "QX_LIESKE", true, "W_LIESKE", false ),
      new TSDesc( "WV_LIESKE" ),
      new TSDesc( "QV_LIESKE", true, "W_LIESKE", false ),
      new TSDesc( "QP_LIESKE", true, "W_LIESKE", false ),
      new TSDesc( "S_JAENKD" ),
      new TSDesc( "W_JAENKD", false, null, true ),
      new TSDesc( "Q_JAENKD" ),
      new TSDesc( "QX_JAENKD", true, "W_JAENKD", false ),
      new TSDesc( "WV_JAENKD" ),
      new TSDesc( "QV_JAENKD", true, "W_JAENKD", false ),
      new TSDesc( "QP_JAENKD", true, "W_JAENKD", false ),
      new TSDesc( "PG_JAENKD" ),
      new TSDesc( "PP_JAENKD" ),
      new TSDesc( "PA_JAENKD", false, null, true ),
      new TSDesc( "S_TSQUITZ" ),
      new TSDesc( "Q_TSQUITZ", true, "QV_TSQUITZ", false ),
      new TSDesc( "QV_TSQUITZ", false, null, true ),
      new TSDesc( "QP_TSQUITZ", true, "QV_TSQUITZ", false ),
      new TSDesc( "V_TSQUITZ", true, "QV_TSQUITZ", false ),
      new TSDesc( "S_SAERI" ),
      new TSDesc( "W_SAERI", false, null, true ),
      new TSDesc( "Q_SAERI" ),
      new TSDesc( "QX_SAERI", true, "W_SAERI", false ),
      new TSDesc( "WV_SAERI" ),
      new TSDesc( "QV_SAERI", true, "W_SAERI", false ),
      new TSDesc( "QP_SAERI", true, "W_SAERI", false ),
      new TSDesc( "ZG_SAERI" ),
      new TSDesc( "PG_SAERI" ),
      new TSDesc( "PP_SAERI" ),
      new TSDesc( "PA_SAERI", false, null, true ),
      new TSDesc( "S_BOXBRG" ),
      new TSDesc( "W_BOXBRG", false, null, true ),
      new TSDesc( "Q_BOXBRG" ),
      new TSDesc( "QX_BOXBRG", true, "W_BOXBRG", false ),
      new TSDesc( "WV_BOXBRG" ),
      new TSDesc( "QV_BOXBRG", true, "W_BOXBRG", false ),
      new TSDesc( "QP_BOXBRG", true, "W_BOXBRG", false ),
      new TSDesc( "S_BWALDE" ),
      new TSDesc( "QV_BWALDE", false, null, true ),
      new TSDesc( "QP_BWALDE" ),
      new TSDesc( "S_LOHSA" ),
      new TSDesc( "QV_LOHSA", false, null, true ),
      new TSDesc( "QP_LOHSA" ),
      new TSDesc( "S_SPREY" ),
      new TSDesc( "W_SPREY", false, null, true ),
      new TSDesc( "Q_SPREY" ),
      new TSDesc( "QX_SPREY", true, "W_SPREY", false ),
      new TSDesc( "WV_SPREY" ),
      new TSDesc( "QV_SPREY", true, "W_SPREY", false ),
      new TSDesc( "QP_SPREY", true, "W_SPREY", false ),
      new TSDesc( "S_BURGNEU" ),
      new TSDesc( "QP_BURGNEU", false, null, true ),
      new TSDesc( "S_SPWITZ" ),
      new TSDesc( "W_SPWITZ", false, null, true ),
      new TSDesc( "Q_SPWITZ" ),
      new TSDesc( "QX_SPWITZ", true, "W_SPWITZ", false ),
      new TSDesc( "WV_SPWITZ" ),
      new TSDesc( "QV_SPWITZ", true, "W_SPWITZ", false ),
      new TSDesc( "QP_SPWITZ", true, "W_SPWITZ", false ),
      new TSDesc( "S_RLKETTE" ),
      new TSDesc( "QV_RLKETTE", false, null, true ),
      new TSDesc( "QP_RLKETTE" ),
      new TSDesc( "S_SPREMB" ),
      new TSDesc( "W_SPREMB", false, null, true ),
      new TSDesc( "Q_SPREMB" ),
      new TSDesc( "QX_SPREMB", true, "W_SPREMB", false ),
      new TSDesc( "WV_SPREMB" ),
      new TSDesc( "QV_SPREMB", true, "W_SPREMB", false ),
      new TSDesc( "QP_SPREMB", true, "W_SPREMB", false ) };

  private static final String EXE_FILE = "spree.exe";

  private static final String[] OTHER_FILES = new String[]
  {
      "Flusspar.dbf",
      "Flutung.dbf",
      "Hw_wq.dbf",
      "Na_para.dbf",
      "Ts_bautz.dbf",
      "Ts_para.dbf",
      "Ts_quitz.dbf",
      "Zwipar.dbf",
      "xHWKERNEL.DLL" };

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( final File tmpdir, final ICalcDataProvider inputProvider, final ICalcResultEater resultEater,
      final ICalcMonitor monitor ) throws CalcJobServiceException
  {
    final File outputdir = new File( tmpdir, ICalcServiceConstants.OUTPUT_DIR_NAME );

    outputdir.mkdirs();
    final File logfile = new File( outputdir, "spree.log" );

    PrintWriter pw = null;

    try
    {
      pw = new PrintWriter( new FileWriter( logfile ) );
      pw.println( "Spree - Modell Berechnung wird gestartet" );
      pw.println();

      if( monitor.isCanceled() )
        return;

      final Properties props = new Properties();
      monitor.setMessage( "Dateien für Rechenkern werden erzeugt" );
      pw.println( "Dateien für Rechenkern werden erzeugt" );
      final TSMap tsmap = new TSMap();
      final File exedir = SpreeInputWorker.createNativeInput( tmpdir, inputProvider, props, pw, tsmap );

      final File nativedir = new File( tmpdir, ".native" );
      final File nativeindir = new File( nativedir, "in" );
      final File nativeoutdir = new File( nativedir, "out" );

      final File napFile = (File)props.get( DATA_NAPFILE );
      final File vhsFile = (File)props.get( DATA_VHSFILE );
      final File flpFile = (File)props.get( DATA_FLPFILE );
      final File tsFile = (File)props.get( DATA_TSFILE );
      FileUtils.copyFileToDirectory( napFile, nativeindir );
      FileUtils.copyFileToDirectory( vhsFile, nativeindir );
      FileUtils.copyFileToDirectory( flpFile, nativeindir );
      FileUtils.copyFileToDirectory( tsFile, nativeindir );
      resultEater.addResult( "NATIVE_IN_DIR", nativeindir );

      monitor.setProgress( 33 );
      if( monitor.isCanceled() )
        return;

      monitor.setMessage( "Rechenkern wird aufgerufen" );
      pw.println( "Rechenkern wird aufgerufen" );
      prepareExe( exedir, pw );
      startCalculation( exedir, props, pw, monitor );
      FileUtils.copyFileToDirectory( napFile, nativeoutdir );
      FileUtils.copyFileToDirectory( vhsFile, nativeoutdir );
      FileUtils.copyFileToDirectory( flpFile, nativeoutdir );
      FileUtils.copyFileToDirectory( tsFile, nativeoutdir );
      resultEater.addResult( "NATIVE_OUT_DIR", nativeindir );

      monitor.setProgress( 33 );
      if( monitor.isCanceled() )
        return;

      monitor.setMessage( "Ergebnisse werden zurückgelesen" );
      pw.println( "Ergebnisse werden zurückgelesen" );
      try
      {
        final String tsFilename = (String)props.get( DATA_TSFILENAME );
        writeResultsToFolder( tsFilename, outputdir, tsmap );
        fetchOptimalValues( props, outputdir );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        throw new CalcJobServiceException( "Fehler beim Schreiben der Ergebnis-Zeitreihen", e );
      }
      monitor.setProgress( 34 );
      if( monitor.isCanceled() )
        return;

      pw.println( "Berechnung beendet" );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new CalcJobServiceException( "Fehler bei der Berechnung:\n" + e.getLocalizedMessage(), e );
    }
    finally
    {
      if( pw != null )
        pw.close();

      resultEater.addResult( "ERGEBNISSE", outputdir );
    }
  }

  private void fetchOptimalValues( final Properties props, final File outdir )
  {
    final GMLWorkspace workspace = (GMLWorkspace)props.get( DATA_GML );
    if( workspace == null )
      return;

    //////////////////////////////////////////////////
    // NAP Werte (Bodenfeuchten) in GML Übertragen) //
    //////////////////////////////////////////////////
    final FeatureType napFT = workspace.getFeatureType( NAP_NAME );
    if( napFT != null )
    {
      final Feature[] napFeatures = workspace.getFeatures( napFT );
      fetchNativeIntoGml( napFeatures, (String)props.get( DATA_NAPFILENAME ), "Bodenfeuchte", "VORFEUCHTE" );
    }

    /////////////////////////////////////////////////////////////////
    // FLP Werte (Empfehlung KorrekturLaufzeit) in GML Übertragen) //
    /////////////////////////////////////////////////////////////////
    final FeatureType flpFT = workspace.getFeatureType( FLP_NAME );
    if( flpFT != null )
    {
      final Feature[] flpFeatures = workspace.getFeatures( flpFT );
      fetchNativeIntoGml( flpFeatures, (String)props.get( DATA_FLPFILENAME ), "KorrekturEmpfehlungLaufzeit", "LZK_EMPF" );
    }

    /////////////////////////////////
    // gml in Ergebnisse schreiben //
    /////////////////////////////////
    final String outfilename = "calcCase.gml";
    final File outFile = new File( outdir, outfilename );

    FileOutputStream fos = null;
    try
    {
      fos = new FileOutputStream( outFile );
      final OutputStreamWriter writer = new OutputStreamWriter( fos, "UTF-8" );
      GmlSerializer.serializeWorkspace( writer, workspace );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    if( fos != null )
    {
      try
      {
        fos.close();
      }
      catch( final IOException e1 )
      {
        e1.printStackTrace();
      }
    }
  }

  /**
   * Überträgt Werte aus einer DBf in das GML
   */
  private void fetchNativeIntoGml( final Feature[] gmlFeatures, final String dbfFileName, final String gmlProperty,
      final String dbfProperty )
  {
    final Collection dbfFeatures = ShapeSerializer.readFeaturesFromDbf( dbfFileName );

    if( gmlFeatures.length != dbfFeatures.size() )
      return;

    final Iterator iter = dbfFeatures.iterator();
    for( int i = 0; i < gmlFeatures.length; i++ )
    {
      final Feature dbfFeature = (Feature)iter.next();
      final Feature gmlFeature = gmlFeatures[i];

      final double optimalValue = ( (Double)dbfFeature.getProperty( dbfProperty ) ).doubleValue();

      final FeatureProperty newValue = FeatureFactory.createFeatureProperty( gmlProperty, new Double( optimalValue ) );
      gmlFeature.setProperty( newValue );
    }
  }

  private void startCalculation( final File exedir, final Map m_data, final PrintWriter logwriter,
      final ICalcMonitor monitor ) throws CalcJobServiceException
  {
    InputStreamReader inStream = null;
    InputStreamReader errStream = null;

    try
    {
      final Date startTime = (Date)m_data.get( DATA_STARTFORECAST_DATE );
      final String timeString = new SimpleDateFormat( "yyyy,MM,dd,HH,mm,ss" ).format( startTime );

      final File tsFile = (File)m_data.get( DATA_TSFILE );

      final File exefile = new File( exedir, EXE_FILE );

      final String commandString = exefile + " " + timeString + " " + tsFile.getName();

      logwriter.println( commandString );
      logwriter.println( "Ausgabe des Rechenkerns" );
      logwriter.println( "#######################" );
      logwriter.println( "#########START#########" );
      logwriter.println( "#######################" );
      logwriter.println();

      final Process process = Runtime.getRuntime().exec( commandString, null, exedir );

      inStream = new InputStreamReader( process.getInputStream() );
      errStream = new InputStreamReader( process.getErrorStream() );
      final OutputStream nul_dev = new NullOutputStream();
      while( true )
      {
        CopyUtils.copy( inStream, logwriter );
        CopyUtils.copy( errStream, nul_dev );

        try
        {
          process.exitValue();
          return;
        }
        catch( final IllegalThreadStateException e )
        {
          // noch nicht fertig
        }

        if( monitor.isCanceled() )
        {
          process.destroy();
          return;
        }

        Thread.sleep( 100 );
      }
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Fehler beim Ausführen der hw.exe", e );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Fehler beim Ausführen der hw.exe", e );
    }
    finally
    {
      IOUtils.closeQuietly( inStream );
      IOUtils.closeQuietly( errStream );

      logwriter.println( "#######################" );
      logwriter.println( "#########ENDE#########" );
      logwriter.println( "#######################" );
      logwriter.println();
    }
  }

  /**
   * schreibt die exe aus den Resourcen in ein temproäres Verzeichnis
   * 
   * @param exedir
   * @param logwriter
   * 
   * @throws CalcJobServiceException
   */
  private void prepareExe( final File exedir, final PrintWriter logwriter ) throws CalcJobServiceException
  {
    try
    {
      logwriter.println( "Rechenkern wird vorbereitet" );
      copyFileToTmp( exedir, EXE_FILE );

      for( int i = 0; i < OTHER_FILES.length; i++ )
        copyFileToTmp( exedir, OTHER_FILES[i] );
    }
    catch( final IOException e )
    {
      e.printStackTrace();

      throw new CalcJobServiceException( "Ausführbares Programm konnte nicht gestartet werden", e );
    }
  }

  /**
   * Kopiert die entsrpechende Dateien aus den resourcen in das tmp-dir, aber nur, wenn sie neuer ist als die bereits
   * vorhandene
   * 
   * @param exedir
   * @param filename
   * @throws IOException
   */
  private void copyFileToTmp( final File exedir, final String filename ) throws IOException
  {
    final File file = new File( exedir, filename );

    final URL resource = getClass().getResource( "resources/exe/" + filename );
    if( resource == null )
    {
      System.out.println( "Für Rechnung benötigte Resource nicht gefunden: " + resource.toString() );
      return;
    }

    final URLConnection connection = resource.openConnection();

    FileUtilities.makeFileFromStream( false, file, connection.getInputStream() );
    // die Zeit auf 1.1.1970 setzen, weil sonst der Rechenkern meckert
    file.setLastModified( 0 );
  }

  public void writeResultsToFolder( final String tsFilename, final File outdir, final TSMap tsmap ) throws Exception
  {
    ///////////////////
    // TS-File lesen //
    ///////////////////
    final Collection features = ShapeSerializer.readFeaturesFromDbf( tsFilename );

    final DateFormat dateFormat = new SimpleDateFormat( "dd.MM.yyyy" );
    final Calendar calendar = new GregorianCalendar();

    // Die erzeugten Daten sammeln
    final Map valuesMap = new HashMap();
    final Collection dates = new ArrayList();

    for( final Iterator iter = features.iterator(); iter.hasNext(); )
    {
      final Feature feature = (Feature)iter.next();

      final String dateString = (String)feature.getProperty( "DATUM" );
      final Date date = dateFormat.parse( dateString );
      final int hour = ( (Number)feature.getProperty( "STUNDE" ) ).intValue();
      calendar.setTime( date );
      calendar.add( Calendar.HOUR_OF_DAY, hour );
      dates.add( calendar.getTime() );

      for( int j = 0; j < TS_DESCRIPTOR.length; j++ )
      {
        final TSDesc desc = TS_DESCRIPTOR[j];
        final String column = desc.id;
        if( !desc.output )
          continue;

        Collection values = (Collection)valuesMap.get( column );
        if( values == null )
        {
          values = new ArrayList();
          valuesMap.put( column, values );
        }

        final Object value = feature.getProperty( column );
        double dblVal = Double.NaN;
        if( value instanceof Number )
          dblVal = ( (Number)value ).doubleValue();
        else
          dblVal = Double.NaN;

        if( Double.isNaN( dblVal ) || Math.abs( dblVal + 99.9 ) < 0.01
            || ( column.startsWith( "Q" ) && Double.compare( dblVal, 0.0 ) == 0 ) )
          values.add( null );
        else
          values.add( new Double( dblVal ) );
      }
    }

    final String dateType = TimeserieConstants.TYPE_DATE;
    final DefaultAxis dateAxis = new DefaultAxis( "Datum", dateType, TimeserieUtils.getUnit( dateType ), Date.class,
        true );

    final Date[] dateArray = (Date[])dates.toArray( new Date[dates.size()] );

    ///////////////////////////////////
    // create ZML for each timeserie //
    ///////////////////////////////////
    for( int i = 0; i < TS_DESCRIPTOR.length; i++ )
    {
      final TSDesc desc = TS_DESCRIPTOR[i];
      final String column = desc.id;

      // entscheiden, ob es ein Ergebnis ist: falls ja weitermachen
      if( !desc.output )
        continue;

      final String outdirname = "Zeitreihen";
      final String outfilename = column + ".zml";
      final File outputDir = new File( outdir, outdirname );
      outputDir.mkdirs();
      final File outFile = new File( outputDir, outfilename );
      final File outFileRelative = FileUtilities.getRelativeFileTo( outdir, outFile );

      final Collection values = (Collection)valuesMap.get( column );
      if( values == null )
      {
        FileUtilities.makeFileFromStream( false, outFile, getClass().getResourceAsStream( "resources/empty.zml" ) );
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
        final String valueType = TSMap.getTypeForName( column );
        final String unit = TimeserieUtils.getUnit( valueType );
        final String name = TimeserieUtils.getName( valueType );

        final IAxis valueAxis = new DefaultAxis( name, valueType, unit, Double.class, false );
        final IAxis[] achsen = new IAxis[]
        {
            dateAxis,
            valueAxis };

        final Object[][] tupleArray = (Object[][])tuples.toArray( new Object[tuples.size()][] );
        final SimpleTuppleModel model = new SimpleTuppleModel( achsen, tupleArray );

        // jetzt die Metadaten entsprechend der Kennung aus den Eingangsdaten
        // übertragen!
        MetadataList metadata;
        if( tsmap == null )
          metadata = new MetadataList();
        else
          metadata = tsmap.getMetadataFor( desc.useMetadataFrom );

        if( metadata == null )
          metadata = new MetadataList();

        final IObservation observation = new SimpleObservation( outFileRelative.getPath(), column, column, false, null,
            metadata, achsen );
        observation.setValues( model );

        // umhüllenden parameter vom modell holen
        // original und die zwei geänderten schreiben
        writeVorhersageZml( observation, outFile, tsmap.getAccuracy( column ), column.startsWith( "QV_" ) );
      }
      else
        FileUtilities.makeFileFromStream( false, outFile, getClass().getResourceAsStream( "resources/empty.zml" ) );
    }
  }

  /**
   * Schreibt eine Vorhersagezeitreihe und ihre umhüllenden
   */
  private void writeVorhersageZml( final IObservation obs, final File outFile, final double accuracy, final boolean writeUmhuellende ) throws Exception
  {
    ZmlFactory.writeToFile( obs, outFile );

    final InputSource is = new InputSource( outFile.getAbsolutePath() );
    final IObservation observation = ZmlFactory.parseXML( is, "", null );
    
    if( !writeUmhuellende )
      return;

    // get first and last date of observation
    final IAxis dateAxis = ObservationUtilities.findAxisByType( observation.getAxisList(), TimeserieConstants.TYPE_DATE );
    final ITuppleModel values = observation.getValues(null);
    final int valueCount = values.getCount();
    if( valueCount < 2 )
      return;
    
    final Date startPrediction = (Date)values.getElement( 0, dateAxis );
    final Date endPrediction = (Date)values.getElement( valueCount - 1, dateAxis );
    
    final Calendar calBegin = Calendar.getInstance();
    calBegin.setTime( startPrediction );

    final Calendar calEnd = Calendar.getInstance();
    calEnd.setTime( endPrediction );

    final long dayOfMillis = 1000 * 60 * 60 * 24;

    final double endOffest = accuracy
        * ( ( (double)( endPrediction.getTime() - startPrediction.getTime() ) ) / ( (double)dayOfMillis ) );

    final String baseName = FileUtilities.nameWithoutExtension( outFile.getName() );
    
    TranProLinFilterUtilities.transformAndWrite( observation, calBegin, calEnd, 0, endOffest, "-", TimeserieConstants.TYPE_WATERLEVEL,
        KalypsoStati.BIT_DERIVATED, new File( outFile.getParentFile(), baseName + "_unten.zml" ), "- Spur Unten" );
    TranProLinFilterUtilities.transformAndWrite( observation, calBegin, calEnd, 0, endOffest, "+", TimeserieConstants.TYPE_WATERLEVEL,
        KalypsoStati.BIT_DERIVATED, new File( outFile.getParentFile(), baseName + "_oben.zml" ), "- Spur Oben" );
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation()
  {
    return getClass().getResource( "spreecalcjob_spec.xml" );
  }
}