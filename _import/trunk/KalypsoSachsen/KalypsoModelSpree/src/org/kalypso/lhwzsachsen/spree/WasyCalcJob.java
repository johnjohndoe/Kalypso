package org.kalypso.lhwzsachsen.spree;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
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

import org.apache.commons.io.FileUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.commons.java.lang.ProcessHelper.ProcessTimeoutException;
import org.kalypso.gmlschema.feature.IFeatureType;
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
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.envelope.TranProLinFilterUtilities;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationConstants;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.xml.sax.InputSource;

/**
 * <p>
 * Der gemeinsame Rechenservice für Spree- und Schwarze-Elster Modell
 * </p>
 * 
 * @author Belger
 */
public abstract class WasyCalcJob implements ISimulation
{
  public static final class WQInfo
  {
    private final int m_totzeit;

    private final String m_zmlId;

    private final String m_name;

    public WQInfo( final String name, final String zmlId, final int totzeit )
    {
      m_name = name;
      m_zmlId = zmlId;
      m_totzeit = totzeit;
    }

    public String getName( )
    {
      return m_name;
    }

    public int getTotzeit( )
    {
      return m_totzeit;
    }

    public String getZmlId( )
    {
      return m_zmlId;
    }
  }

  /** Datumsformat für spalte 'DZAHL' in dbf file */
  public static final DateFormat DF_DZAHL = new SimpleDateFormat( "yyMM.dd" );

  /** Datumsformat für spalte 'DATUM' in dbf file */
  public static final DateFormat DF_DATUM = new SimpleDateFormat( "dd.MM.yyyy" );

  public static final String VHS_FILE = "_vhs.dbf";

  public static final String FLP_NAME = "FlusslaufModell";

  public static final String FLP_GEOM = "Ort";

  public static final Map<String, String> FLP_MAP = new LinkedHashMap<String, String>();
  static
  {
    FLP_MAP.put( "PEGEL", "Name" );
    FLP_MAP.put( "KORRFAKTOR", "Korrektur_Faktor" );
    FLP_MAP.put( "NIVEAUKORR", "Korrektur_Niveau" );
    FLP_MAP.put( "LAUFZEITK", "Korrektur_Laufzeit" );
    FLP_MAP.put( "LZK_EMPF", "KorrekturEmpfehlungLaufzeit" );
  }

  public static final String NAP_NAME = "Einzugsgebiet";

  public static final String DATA_STARTFORECAST_STRING = "startDate";

  public static final String DATA_STARTSIM_DATE = "startSimulation";

  public static final String DATA_STARTFORECAST_DATE = "startForecast";

  public static final String DATA_STARTDATESTRING = "startDateString";

  public static final String DATA_BASEFILENAME = "baseFileName";

  public static final String DATA_GML = "gmlWorkspace";

  public static final String DATA_WQMAP = "wqMap";

  public static final String DATA_WQPARAMCOUNT = "wqParamCount";

  public static final String DATA_FLPFILE = "flpFile";

  public static final String DATA_VHSFILE = "vhsFile";

  public static final String DATA_NAPFILE = "napFile";

  public static final String DATA_NAPFILENAME = "napFilename";

  public static final String DATA_FLPFILENAME = "flpFilename";

  public static final String DATA_TSFILENAME = "tsFilename";

  public static final String DATA_TSFILE = "tsFile";

  public static final String DATA_WQFILE = "wqFile";

  public static final String DATA_LABEL = "label";

  public static final String DATA_STARTVOLUMEMAP = "anfangsstauvolumen";

  /**
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final File outputdir = new File( tmpdir, ISimulationConstants.OUTPUT_DIR_NAME );

    outputdir.mkdirs();
    final File logfile = new File( outputdir, "wasy.log" );

    PrintWriter pw = null;

    try
    {
      pw = new PrintWriter( new FileWriter( logfile ) );
      pw.println( "Modell Berechnung wird gestartet" );
      pw.println();

      if( monitor.isCanceled() )
        return;

      final Properties props = new Properties();
      monitor.setMessage( "Dateien für Rechenkern werden erzeugt" );
      pw.println( "Dateien für Rechenkern werden erzeugt" );

      final TSMap tsmap = new TSMap();
      props.put( DATA_WQMAP, getWQMap() );
      props.put( DATA_WQPARAMCOUNT, getWQParamCount() );
      props.put( DATA_STARTVOLUMEMAP, getStartVolumeMap() );

      final File exedir = WasyInputWorker.createNativeInput( tmpdir, inputProvider, props, pw, tsmap, TS_DESCRIPTOR(), this );

      final File nativedir = new File( tmpdir, ".native" );
      final File nativeindir = new File( nativedir, "in" );
      final File nativeoutdir = new File( nativedir, "out" );

      final File napFile = (File) props.get( DATA_NAPFILE );
      final File vhsFile = (File) props.get( DATA_VHSFILE );
      final File flpFile = (File) props.get( DATA_FLPFILE );
      final File tsFile = (File) props.get( DATA_TSFILE );

      FileUtils.copyFileToDirectory( napFile, nativeindir );
      if( isSpreeFormat() )
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

      if( isSpreeFormat() )
        FileUtils.copyFileToDirectory( vhsFile, nativeoutdir );

      FileUtils.copyFileToDirectory( flpFile, nativeoutdir );
      FileUtils.copyFileToDirectory( tsFile, nativeoutdir );
      resultEater.addResult( "NATIVE_OUT_DIR", nativeoutdir );

      monitor.setProgress( 33 );
      if( monitor.isCanceled() )
        return;

      monitor.setMessage( "Ergebnisse werden zurückgelesen" );
      pw.println( "Ergebnisse werden zurückgelesen" );
      try
      {
        final String tsFilename = (String) props.get( DATA_TSFILENAME );
        writeResultsToFolder( tsFilename, outputdir, tsmap );
        fetchOptimalValues( props, outputdir );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        throw new SimulationException( "Fehler beim Schreiben der Ergebnis-Zeitreihen", e );
      }

      monitor.setProgress( 34 );
      if( monitor.isCanceled() )
        return;

      pw.println( "Berechnung beendet" );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new SimulationException( "Fehler bei der Berechnung:\n" + e.getLocalizedMessage(), e );
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
    final GMLWorkspace workspace = (GMLWorkspace) props.get( DATA_GML );
    if( workspace == null )
      return;

    // ////////////////////////////////////////////////
    // NAP Werte (Bodenfeuchten) in GML Übertragen) //
    // ////////////////////////////////////////////////
    final IFeatureType napFT = workspace.getFeatureType( NAP_NAME );
    if( napFT != null )
    {
      final Feature[] napFeatures = workspace.getFeatures( napFT );
      fetchNativeIntoGml( napFeatures, (String) props.get( DATA_NAPFILENAME ), "Bodenfeuchte", "VORFEUCHTE" );
    }

    // ///////////////////////////////////////////////////////////////
    // FLP Werte (Empfehlung KorrekturLaufzeit) in GML Übertragen) //
    // ///////////////////////////////////////////////////////////////
    final IFeatureType flpFT = workspace.getFeatureType( FLP_NAME );
    if( flpFT != null )
    {
      final Feature[] flpFeatures = workspace.getFeatures( flpFT );
      fetchNativeIntoGml( flpFeatures, (String) props.get( DATA_FLPFILENAME ), "KorrekturEmpfehlungLaufzeit", "LZK_EMPF" );
    }

    // ///////////////////////////////
    // gml in Ergebnisse schreiben //
    // ///////////////////////////////
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
  private void fetchNativeIntoGml( final Feature[] gmlFeatures, final String dbfFileName, final String gmlProperty, final String dbfProperty )
  {
    final Collection<Feature> dbfFeatures = ShapeSerializer.readFeaturesFromDbf( dbfFileName );

    if( gmlFeatures.length != dbfFeatures.size() )
      return;

    final Iterator<Feature> iter = dbfFeatures.iterator();
    for( final Feature gmlFeature : gmlFeatures )
    {
      final Feature dbfFeature = iter.next();
      final double optimalValue = ((Double) dbfFeature.getProperty( dbfProperty )).doubleValue();
      gmlFeature.setProperty( gmlProperty, new Double( optimalValue ) );
    }
  }

  private void startCalculation( final File exedir, final Map<Object,Object> m_data, final PrintWriter logwriter, final ISimulationMonitor monitor ) throws SimulationException
  {
    final Date startTime = (Date) m_data.get( DATA_STARTFORECAST_DATE );
    final String timeString = new SimpleDateFormat( "yyyy,MM,dd,HH,mm,ss" ).format( startTime );

    final File tsFile = (File) m_data.get( DATA_TSFILE );

    final File exefile = new File( exedir, getExeFilename() );

    final String commandString = exefile + " " + timeString + " " + tsFile.getName();

    logwriter.println( commandString );

    final ByteArrayOutputStream processOutS = new ByteArrayOutputStream();
    final ByteArrayOutputStream processErrS = new ByteArrayOutputStream();
    final ByteArrayInputStream processInS = new ByteArrayInputStream( new byte[0] );

    try
    {
      // timeout after 10 sec
      ProcessHelper.startProcess( commandString, null, exedir, monitor, 10000, processOutS, processErrS, processInS );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler beim Ausführen der hw.exe", e );
    }
    catch( final ProcessTimeoutException e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler beim Ausführen der hw.exe", e );
    }
    finally
    {
      final String processOut = processOutS.toString();
      logwriter.println( "Ausgaben des Rechenkerns" );
      logwriter.println( "========================" );
      logwriter.println( "=   Standard-Ausgabe   =" );
      logwriter.println( "========================" );
      logwriter.println( processOut );

      // spree.exe erzeugt keinen error stream
      // final String processErr = errWriter.toString();
      // logwriter.println( "========================" );
      // logwriter.println( "= Fehler-Ausgabe =" );
      // logwriter.println( "========================" );
      // logwriter.println( processErr );

      logwriter.println( "========================" );
      logwriter.println();

      if( processOut.endsWith( "Berechnung erfolgreich beendet\r\n" ) )
        logwriter.println( "Rechnung erfolgreich beendet." );
      else
        logwriter.println( "Rechnung nicht erfolgreich beendet." );
    }
  }

  /**
   * schreibt die exe aus den Resourcen in ein temproäres Verzeichnis
   * 
   * @param exedir
   * @param logwriter
   * @throws CalcJobServiceException
   */
  private void prepareExe( final File exedir, final PrintWriter logwriter ) throws SimulationException
  {
    try
    {
      logwriter.println( "Rechenkern wird vorbereitet" );

      final String[] otherFiles = getOtherFiles();
      final String resourceBase = getResourceBase();
      for( final String otherFile : otherFiles )
        copyFileToTmp( exedir, otherFile, resourceBase );
    }
    catch( final IOException e )
    {
      e.printStackTrace();

      throw new SimulationException( "Ausführbares Programm konnte nicht gestartet werden", e );
    }
  }

  /**
   * Kopiert die entsprechenden Dateien aus den resourcen in das tmp-dir, aber nur, wenn sie noch nicht existiert.
   */
  private void copyFileToTmp( final File exedir, final String filename, final String resourceBase ) throws IOException
  {
    final File file = new File( exedir, filename );
    if( file.exists() )
      return;

    final URL resource = getClass().getResource( resourceBase + filename );
    if( resource == null )
    {
      System.out.println( "Für Rechnung benötigte Resource nicht gefunden: " + resourceBase + filename );
      return;
    }

    final URLConnection connection = resource.openConnection();

    FileUtilities.makeFileFromStream( false, file, connection.getInputStream() );
    // die Zeit auf 1.1.1970 setzen, weil sonst der Rechenkern meckert
    file.setLastModified( 0 );
  }

  public void writeResultsToFolder( final String tsFilename, final File outdir, final TSMap tsmap ) throws Exception
  {
    // /////////////////
    // TS-File lesen //
    // /////////////////
    final Collection<Feature> features = ShapeSerializer.readFeaturesFromDbf( tsFilename );

    final Calendar calendar = new GregorianCalendar();

    // Die erzeugten Daten sammeln
    final Map<String, Collection<Double>> valuesMap = new HashMap<String, Collection<Double>>();
    final Collection<Date> dates = new ArrayList<Date>();

    final boolean isSpreeFormat = isSpreeFormat();

    final TSDesc[] TS_DESCRIPTOR = TS_DESCRIPTOR();
    for( final Feature feature : features )
    {
      final Date date;
      if( isSpreeFormat )
      {
        final String dateString = (String) feature.getProperty( "DATUM" );
        date = DF_DATUM.parse( dateString );
      }
      else
      {
        final Number dzahl = (Number) feature.getProperty( "DATUM" );
        // IMPORTENT: add leading zeros to avoid bug for the years 2000-2009
        final String dzahlStr = String.format( "%07.2f", dzahl );

        date = DF_DZAHL.parse( dzahlStr );
      }

      final int hour = ((Number) feature.getProperty( "STUNDE" )).intValue();
      calendar.setTime( date );
      calendar.add( Calendar.HOUR_OF_DAY, hour );

      dates.add( calendar.getTime() );

      for( final TSDesc desc : TS_DESCRIPTOR )
      {
        final String column = desc.id;
        if( !desc.output )
          continue;

        Collection<Double> values = valuesMap.get( column );
        if( values == null )
        {
          values = new ArrayList<Double>();
          valuesMap.put( column, values );
        }

        final Object value = feature.getProperty( column );
        double dblVal = Double.NaN;
        if( value instanceof Number )
          dblVal = ((Number) value).doubleValue();
        else
          dblVal = Double.NaN;

        if( Double.isNaN( dblVal ) || Math.abs( dblVal + 99.9 ) < 0.01 || (column.startsWith( "Q" ) && Double.compare( dblVal, 0.0 ) == 0) )
          values.add( null );
        else
          values.add( new Double( dblVal ) );
      }
    }

    final String dateType = TimeserieConstants.TYPE_DATE;
    final DefaultAxis dateAxis = new DefaultAxis( "Datum", dateType, TimeserieUtils.getUnit( dateType ), Date.class, true );

    final Date[] dateArray = dates.toArray( new Date[dates.size()] );

    // /////////////////////////////////
    // create ZML for each timeserie //
    // /////////////////////////////////

    for( final TSDesc desc : TS_DESCRIPTOR )
    {
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

      final Collection<Double> values = valuesMap.get( column );
      if( values == null )
      {
        FileUtilities.makeFileFromStream( false, outFile, getClass().getResourceAsStream( "resources/empty.zml" ) );
        continue;
      }

      final int size = values.size();
      final Double[] valueArray = values.toArray( new Double[size] );

      final Collection<Object[]> tuples = new ArrayList<Object[]>( dateArray.length );

      for( int j = 0; j < size; j++ )
      {
        final Date date = dateArray[j];
        final Double value = valueArray[j];

        if( date != null && value != null )
          tuples.add( new Object[] { date, value } );
      }

      if( tuples.size() > 0 )
      {
        final String valueType = TSMap.getTypeForName( column );
        final String unit = TimeserieUtils.getUnit( valueType );
        final String name = TimeserieUtils.getName( valueType );

        final IAxis valueAxis = new DefaultAxis( name, valueType, unit, Double.class, false );
        final IAxis[] achsen = new IAxis[] { dateAxis, valueAxis };

        final Object[][] tupleArray = tuples.toArray( new Object[tuples.size()][] );
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

        final IObservation observation = new SimpleObservation( outFileRelative.getPath(), column, column, false, null, metadata, achsen );
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
   * 
   * @param accuracy
   *          In Prozent per 60h
   */
  private void writeVorhersageZml( final IObservation obs, final File outFile, final double accuracy, final boolean writeUmhuellende ) throws Exception
  {
    ZmlFactory.writeToFile( obs, outFile );

    final InputSource is = new InputSource( outFile.getAbsolutePath() );
    final IObservation observation = ZmlFactory.parseXML( is, "", null );

    if( !writeUmhuellende )
      return;

    /* The axis type for which to create umhüllende */
    final IAxis[] axisList = observation.getAxisList();
    final String axisType;
    if( ObservationUtilities.hasAxisOfType( axisList, TimeserieConstants.TYPE_RUNOFF ) )
      axisType = TimeserieConstants.TYPE_RUNOFF;
    else if( ObservationUtilities.hasAxisOfType( axisList, TimeserieConstants.TYPE_WATERLEVEL ) )
      axisType = TimeserieConstants.TYPE_WATERLEVEL;
    else
      throw new SimulationException( "Ergebniszeitreihe enthält weder Abfluss noch Waserstand, Umhüllendenberechnung nicht möglich.", null );

    // get first and last date of observation
    final IAxis dateAxis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_DATE );
    final IAxis valueAxis = ObservationUtilities.findAxisByType( axisList, axisType );
    final ITuppleModel values = observation.getValues( null );
    final int valueCount = values.getCount();
    if( valueCount < 2 )
      return;

    final Date startPrediction = (Date) values.getElement( 0, dateAxis );
    final Date endPrediction = (Date) values.getElement( valueCount - 1, dateAxis );
    final Double endValue = (Double) values.getElement( valueCount - 1, valueAxis );

    final Calendar calBegin = Calendar.getInstance();
    calBegin.setTime( startPrediction );

    final Calendar calEnd = Calendar.getInstance();
    calEnd.setTime( endPrediction );

    final long millisOf60hours = 1000 * 60 * 60 * 60;

    final double endAccuracy = accuracy * (((double) (endPrediction.getTime() - startPrediction.getTime())) / ((double) millisOf60hours));

    final double endOffset = Math.abs( endValue.doubleValue() * endAccuracy / 100 );

    final String baseName = org.kalypso.contribs.java.io.FileUtilities.nameWithoutExtension( outFile.getName() );

    final IRequest request = new ObservationRequest( calBegin.getTime(), calEnd.getTime() );

    TranProLinFilterUtilities.transformAndWrite( observation, calBegin, calEnd, 0, endOffset, "-", axisType, KalypsoStati.BIT_DERIVATED, new File( outFile.getParentFile(), baseName + "_unten.zml" ), "- Spur Unten", request );
    TranProLinFilterUtilities.transformAndWrite( observation, calBegin, calEnd, 0, endOffset, "+", axisType, KalypsoStati.BIT_DERIVATED, new File( outFile.getParentFile(), baseName + "_oben.zml" ), "- Spur Oben", request );
  }

  protected abstract TSDesc[] TS_DESCRIPTOR( );

  protected abstract String getExeFilename( );

  /** All files which gets copied from the resources to the native dir. Must be relative pathes to resources/exe */
  protected abstract String[] getOtherFiles( );

  /** Base resource path from which to copy the 'other' files into the native dir. */
  protected abstract String getResourceBase( );

  /** Info, what to write into the WQ-file */
  protected abstract WQInfo[] getWQMap( );

  /** Number of wq-wechmann parameters allowed in the wq-dbase file. */
  protected abstract Integer getWQParamCount( );

  /** Map zml-name -> feature id, in order to set the start volumes */
  protected abstract Map<String,String> getStartVolumeMap( );

  public abstract String makeNapFilename( final File nativedir, final String tsFilename );

  public abstract String makeFlpFilename( final File nativeDir, final String tsFilename );

  public abstract boolean isSpreeFormat( );
}