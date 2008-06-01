package org.kalypso.lhwzsachsen.spree;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringReader;
import java.net.URL;
import java.nio.charset.Charset;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.java.io.StreamUtilities;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.lhwzsachsen.spree.WasyCalcJob.WQInfo;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.wq.WQTimeserieProxy;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannGroup;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannParams;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannSet;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationDataPath;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.io.shpapi.DBaseException;
import org.kalypsodeegree_impl.io.shpapi.DBaseFile;
import org.kalypsodeegree_impl.io.shpapi.FieldDescriptor;
import org.xml.sax.InputSource;

/**
 * Diese Klasse sammelt alles, was mit dem Erzeugen der Nativen Daten aus den Eingabedaten zu tun hat.
 * 
 * @author Belger
 */
public class WasyInputWorker
{
  protected final static Logger LOGGER = Logger.getLogger( WasyInputWorker.class.getName() );

  private static String WECHMANN_EMPTY;

  static
  {
    try
    {
      // wird nicht instantiiert
      WECHMANN_EMPTY = toString( WasyInputWorker.class.getResource( "resources/wechmannEmpty.xml" ), "UTF8" );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
  }

  private WasyInputWorker( )
  {
    // wird nicht instantiiert
  }

  /**
   * <p>
   * Converts inputfiles to nativefiles and reads control parameters
   * </p>
   * 
   * @return Location of native files
   * @throws IOException
   */
  public static File createNativeInput( final File tmpdir, final ISimulationDataProvider inputProvider, final Properties props, final PrintWriter logwriter, final TSMap tsmap, final TSDesc[] TS_DESCRIPTOR, final WasyCalcJob wasyJob ) throws Exception
  {
    try
    {
      final File nativedir = new File( tmpdir, "native" );
      nativedir.mkdirs();

      final URL controlGmlURL = (URL) inputProvider.getInputForID( "CONTROL_GML" );

      logwriter.println( "Lese Steuerparameter: " + controlGmlURL.toString() );

      final Map map = parseControlFile( controlGmlURL, nativedir, wasyJob );
      props.putAll( map );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( "GML" ), null );

      props.put( WasyCalcJob.DATA_GML, workspace );

      final String tsFilename = writeNonTs( props, logwriter, workspace, wasyJob.isSpreeFormat() );

      final Date startDate = (Date) props.get( WasyCalcJob.DATA_STARTSIM_DATE );
      final Calendar calendar = Calendar.getInstance();
      calendar.setTime( startDate );
      calendar.add( Calendar.HOUR_OF_DAY, -3 );
      final Date volDate = calendar.getTime();

      final Map startVolumeMap = (Map) props.get( WasyCalcJob.DATA_STARTVOLUMEMAP );
      for( final Iterator iter = startVolumeMap.entrySet().iterator(); iter.hasNext(); )
      {
        final Map.Entry entry = (Entry) iter.next();
        final String name = (String) entry.getKey();
        final String fid = (String) entry.getValue();
        setAnfangsstauvolumen( name, volDate, fid, tsmap, workspace, logwriter );
      }

      logwriter.println( "Erzeuge Zeitreihen-Datei: " + tsFilename );
      readZML( inputProvider, tsmap, TS_DESCRIPTOR );

      writeWQ( props, tsmap, startDate );

      calcNiederschlagsummen( tsmap, TS_DESCRIPTOR );
      applyAccuracyPrediction( workspace, tsmap );
      createTimeseriesFile( tsFilename, tsmap, TS_DESCRIPTOR, wasyJob.isSpreeFormat() );

      return nativedir;
    }
    catch( final SimulationException e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler beim Erzeugen der Inputdateien", e );
    }
  }

  /**
   * Create the WQ-File
   * 
   * @throws CalcJobServiceException
   */
  private static void writeWQ( final Properties props, final TSMap tsmap, final Date startSim ) throws SimulationException
  {
    final WasyCalcJob.WQInfo[] wqinfos = (WasyCalcJob.WQInfo[]) props.get( WasyCalcJob.DATA_WQMAP );
    final int wechmanParamCount = ((Integer) props.get( WasyCalcJob.DATA_WQPARAMCOUNT )).intValue();

    final Map paramMap = new LinkedHashMap();
    for( final WQInfo info : wqinfos )
    {
      try
      {
        final MetadataList metadataList = tsmap.getMetadataFor( info.getZmlId() );
        final String xml = metadataList == null ? null : metadataList.getProperty( TimeserieConstants.MD_WQWECHMANN );
        if( xml == null )
          System.out.println( "Keine W-Q Wechmann-Parameter vorhanden für: " + info.getZmlId() );

        /*
         * Falls die Zeitreihe nicht existiert oder keine Wechmann-Metadaten hat, werden Standardwerte rausgeschrieben.
         * (Vermutlich Fall Schwarze-Elster: nicht berücksichtigte Unterwasserpegel)
         */
        final String wechmannXml = xml == null ? WECHMANN_EMPTY : xml;

        final WechmannGroup group = WechmannFactory.parse( new InputSource( new StringReader( wechmannXml ) ) );
        final WechmannSet wechmannSet = group.getFor( startSim );
        final WechmannParams[] paramArray = new WechmannParams[wechmanParamCount];
        int count = 0;
        for( final Iterator wechIt = wechmannSet.iterator(); wechIt.hasNext(); )
        {
          final WechmannParams params = (WechmannParams) wechIt.next();
          paramArray[count++] = params;

          if( count == wechmanParamCount )
            break;
        }

        paramMap.put( info, paramArray );
      }
      catch( final Throwable e )
      {
        // on any error return, because we still have a default wq file in the resources.
        System.out.println( "Fehler beim Schreiben der W-Q Beziehung. Standardwerte werden benutzt." );
        e.printStackTrace();
        System.out.println();
        return;
      }
    }

    /* Now write the params */
    try
    {
      final FieldDescriptor[] fds = new FieldDescriptor[8 + (wechmanParamCount - 1) * 5 + 3];
      int fieldCounter = 0;
      fds[fieldCounter++] = new FieldDescriptor( "PEGEL", "C", (byte) 11, (byte) 0 );

      for( int i = 0; i < wechmanParamCount; i++ )
      {
        fds[fieldCounter++] = new FieldDescriptor( "W0_" + (i + 1), "N", (byte) 10, (byte) 2 );
        fds[fieldCounter++] = new FieldDescriptor( "LNK1_" + (i + 1), "N", (byte) 15, (byte) 5 );
        fds[fieldCounter++] = new FieldDescriptor( "K2_" + (i + 1), "N", (byte) 15, (byte) 5 );
        if( i < wechmanParamCount - 1 )
        {
          fds[fieldCounter++] = new FieldDescriptor( "WGR_" + +(i + 1) + "_" + (i + 2), "N", (byte) 10, (byte) 1 );
          fds[fieldCounter++] = new FieldDescriptor( "DUMMY" + (i + 1), "N", (byte) 10, (byte) 0 );
        }
      }

      fds[fieldCounter++] = new FieldDescriptor( "WQ_OK", "L", (byte) 1, (byte) 0 );
      fds[fieldCounter++] = new FieldDescriptor( "AST1", "N", (byte) 5, (byte) 0 );
      fds[fieldCounter++] = new FieldDescriptor( "AST2", "N", (byte) 5, (byte) 0 );
      fds[fieldCounter++] = new FieldDescriptor( "AST3", "N", (byte) 5, (byte) 0 );
      fds[fieldCounter++] = new FieldDescriptor( "AST4", "N", (byte) 5, (byte) 0 );
      fds[fieldCounter++] = new FieldDescriptor( "AST_OK", "L", (byte) 1, (byte) 0 );
      fds[fieldCounter++] = new FieldDescriptor( "TOTZEIT", "N", (byte) 2, (byte) 0 );

      final File wqFile = (File) props.get( WasyCalcJob.DATA_WQFILE );
      final String wqFilename = FileUtilities.nameWithoutExtension( wqFile.getAbsolutePath() );
      // TODO: as soon as DBFile was merged, reintroduce encoding here, its crucial for the wasy exe
      final DBaseFile dbf = new DBaseFile( wqFilename, fds /* , "CP850" */);

      for( final Iterator iter = paramMap.entrySet().iterator(); iter.hasNext(); )
      {
        final Map.Entry entry = (Entry) iter.next();
        final WasyCalcJob.WQInfo info = (WQInfo) entry.getKey();
        final WechmannParams[] paramArray = (WechmannParams[]) entry.getValue();

        final ArrayList record = new ArrayList();
        record.add( info.getName() );

        for( int i = 0; i < paramArray.length; i++ )
        {
          final WechmannParams params = paramArray[i];
          if( params == null )
          {
            record.add( new Double( 0.0 ) ); // W0_
            record.add( new Double( 0.0 ) ); // LNK_
            record.add( new Double( 0.0 ) ); // K2_

            if( i < wechmanParamCount - 1 )
            {
              record.add( new Double( 9999.9 ) ); // WGR_
              record.add( new Integer( 0 ) ); // DUMMY
            }
          }
          else
          {
            record.add( new Double( params.getW1() ) ); // W0_
            record.add( new Double( params.getLNK1() ) ); // LNK_
            record.add( new Double( params.getK2() ) ); // K2_

            if( i < wechmanParamCount - 1 )
            {
              /** If we have no following parameters, also write 9999.9; if not the calculation core gets problems. */
              if( paramArray[i + 1] != null && params.hasWGR() )
                record.add( new Double( params.getWGR() ) ); // WGR_
              else
                record.add( new Double( 9999.9 ) );
              record.add( new Integer( 0 ) ); // DUMMY
            }
          }
        }

        record.add( Boolean.TRUE ); // WQ_OK

        final MetadataList metadata = tsmap.getMetadataFor( info.getZmlId() );

        final String ast1 = metadata == null ? "999" : metadata.getProperty( TimeserieConstants.MD_ALARM_1, "999" );
        final String ast2 = metadata == null ? "999" : metadata.getProperty( TimeserieConstants.MD_ALARM_2, "999" );
        final String ast3 = metadata == null ? "999" : metadata.getProperty( TimeserieConstants.MD_ALARM_3, "999" );
        final String ast4 = metadata == null ? "999" : metadata.getProperty( TimeserieConstants.MD_ALARM_4, "999" );

        record.add( alarmValue( ast1 ) ); // AST1
        record.add( alarmValue( ast2 ) ); // AST2
        record.add( alarmValue( ast3 ) ); // AST3
        record.add( alarmValue( ast4 ) ); // AST4
        record.add( Boolean.TRUE ); // AST_OK
        record.add( new Integer( info.getTotzeit() ) ); // TOTZEIT

        dbf.setRecord( record );
      }

      dbf.writeAllToFile();
      dbf.close();
    }
    catch( final DBaseException e )
    {
      throw new SimulationException( "Fehler beim Erzeugen der HW_WQ.DBF", e );
    }
    catch( final IOException e )
    {
      throw new SimulationException( "Fehler beim Schreiben der HW_WQ.DBF", e );
    }

  }

  /* Helper method in order to parse alarm-level strings */
  private static Double alarmValue( final String ast1 )
  {
    try
    {
      final Double value = new Double( ast1 );
      /* If it is near zero, return non-value */
      if( Math.abs( value.doubleValue() ) > 1 )
        return value;
    }
    catch( final NumberFormatException e )
    {
      // ignore, return non-value
    }

    return new Double( "999" );
  }

  /**
   * Liest die Parameter für die Umhüllende (accuracyPrediction) aus dem Workspace und schreibt sie an die jeweilige
   * Zeitreihe in der TSMap
   */
  private static void applyAccuracyPrediction( final GMLWorkspace workspace, final TSMap tsmap )
  {
    final FeatureVisitor fv = new FeatureVisitor()
    {
      public boolean visit( final Feature f )
      {
        final Object property = f.getProperty( "accuracyPrediction" );
        if( property instanceof Double )
        {
          // die Zuordnung erfolgt über den Namen des Links
          final TimeseriesLinkType tsLink = (TimeseriesLinkType) f.getProperty( "Wasserstand_vorhersage" );
          final String tsHref = tsLink.getHref();

          final String path = FileUtilities.nameWithoutExtension( tsHref );
          final String name = FileUtilities.nameFromPath( path );
          tsmap.setAccuracy( name, (Double) property );
        }

        return true;
      }
    };

    workspace.accept( fv, "PegelCollectionAssociation/PegelMember", FeatureVisitor.DEPTH_ZERO );
  }

  private static void calcNiederschlagsummen( final TSMap tsmap, final TSDesc[] TS_DESCRIPTOR )
  {
    final Date[] dates = tsmap.getDates();

    for( final TSDesc element : TS_DESCRIPTOR )
    {
      final String id = element.id;
      if( id.startsWith( "PA_" ) )
      {
        final String pgid = "PG" + id.substring( 2 );
        final String ppid = "PP" + id.substring( 2 );

        final Map datesToValuesMap = tsmap.getTimeserie( id );

        Double lastValue = null;
        for( int j = 0; j < dates.length; j++ )
        {
          final Date date = dates[j];

          // die beiden ersten dürfen nicht gesetzt werden, sonst rechnet das Modell Mist
          final Double value = (j == 0 || j == 1) ? null : (Double) datesToValuesMap.get( date );

          if( j % 2 == 0 )
            tsmap.putValue( pgid, date, null );
          else if( value != null && lastValue != null && j % 2 == 1 )
          {
            final double summe = value.doubleValue() + lastValue.doubleValue();
            tsmap.putValue( pgid, date, new Double( summe ) );
          }
          else
            tsmap.putValue( pgid, date, new Double( -99.9 ) );

          if( value == null )
            tsmap.putValue( id, date, new Double( -99.9 ) );

          tsmap.putValue( ppid, date, new Double( 50.0 ) );

          lastValue = value;
        }
      }
    }
  }

  private static void setAnfangsstauvolumen( final String name, final Date startDate, final String fid, final TSMap tsmap, final GMLWorkspace workspace, final PrintWriter logwriter ) throws SimulationException
  {
    // das Anfangsstauvolumen aus der GMl raussuchen und als Wert in den
    // Zeitreihen setzen
    final Feature feature = workspace.getFeature( fid );
    if( feature == null )
      throw new SimulationException( "Kein Feature mit id: " + fid, null );

    final Object property = feature.getProperty( "Anfangsstauvolumen" );
    if( property != null && property instanceof Double )
      tsmap.putValue( name, startDate, (Double) property );
    else
      logwriter.println( "Kein Anfangsstauvolumen angegeben für: " + fid );
  }

  private static String writeNonTs( final Properties props, final PrintWriter logwriter, final GMLWorkspace workspace, final boolean writeVHS ) throws IOException, FileNotFoundException, SimulationException
  {
    // REMARK: die Reihenfolge der Zeilen im DBF ist wichtig!
    // Das GML muss in der richtigen Reihenfolge sein.

    final File vhsFile = (File) props.get( WasyCalcJob.DATA_VHSFILE );
    final String flpFilename = (String) props.get( WasyCalcJob.DATA_FLPFILENAME );
    final String napFilename = (String) props.get( WasyCalcJob.DATA_NAPFILENAME );
    final String tsFilename = (String) props.get( WasyCalcJob.DATA_TSFILENAME );

    if( writeVHS )
    {
      logwriter.println( "Erzeuge _vhs Datei: " + vhsFile.getName() );
      StreamUtilities.streamCopy( WasyInputWorker.class.getResourceAsStream( "resources/" + WasyCalcJob.VHS_FILE ), new FileOutputStream( vhsFile ) );
    }

    logwriter.println( "Erzeuge _flp Datei: " + flpFilename );
    findAndWriteLayer( workspace, WasyCalcJob.FLP_NAME, WasyCalcJob.FLP_MAP, WasyCalcJob.FLP_GEOM, flpFilename );

    logwriter.println( "Erzeuge _nap Datei: " + napFilename );
    writeNapfile( workspace, napFilename );

    final File shpfile = new File( tsFilename + ".shp" );
    final InputStream shpresource = WasyInputWorker.class.getResourceAsStream( "resources/HW.shp" );
    FileUtilities.makeFileFromStream( false, shpfile, shpresource );

    final File shxfile = new File( tsFilename + ".shx" );
    final InputStream shxresource = WasyInputWorker.class.getResourceAsStream( "resources/HW.shx" );
    FileUtilities.makeFileFromStream( false, shxfile, shxresource );

    return tsFilename;
  }

  private static void writeNapfile( final GMLWorkspace workspace, final String napFilename ) throws SimulationException
  {
    try
    {
      final IFeatureType featureType = workspace.getFeatureType( WasyCalcJob.NAP_NAME );
      if( featureType == null )
        throw new SimulationException( "Eingabedatei für Rechenmodell konnte nicht erzeugt werden. Einzugsgebiete nicht gefunden: ", null );

      final FieldDescriptor[] fds = new FieldDescriptor[4];
      fds[0] = new FieldDescriptor( "PEGEL", "C", (byte) 15, (byte) 0 );
      fds[1] = new FieldDescriptor( "MIN", "N", (byte) 8, (byte) 2 );
      fds[2] = new FieldDescriptor( "VORFEUCHTE", "N", (byte) 8, (byte) 2 );
      fds[3] = new FieldDescriptor( "MAX", "N", (byte) 8, (byte) 2 );

      // TODO: as soon as DBFile was merged, reintroduce encoding here, its crucial for the wasy exe
      final DBaseFile dbfFile = new DBaseFile( napFilename, fds/* , "CP1252" */);
      Charset.availableCharsets();

      final Feature[] features = workspace.getFeatures( featureType );
      for( final Feature feature : features )
      {
        final ArrayList record = new ArrayList( 4 );

        final String name = (String) feature.getProperty( "Name" );
        final String nameOut = name.length() > 15 ? name.substring( 0, 14 ) : name;

        record.add( nameOut );
        record.add( feature.getProperty( "BodenfeuchteMin" ) );
        record.add( feature.getProperty( "Bodenfeuchte" ) );
        record.add( feature.getProperty( "BodenfeuchteMax" ) );

        dbfFile.setRecord( record );
      }

      dbfFile.writeAllToFile();
      dbfFile.close();
    }
    catch( final IOException e )
    {
      throw new SimulationException( "Fehler beim Schreiben der NA-Korrekturparameter: " + napFilename, e );
    }
    catch( final DBaseException e )
    {
      throw new SimulationException( "Fehler beim Schreiben der NA-Korrekturparameter: " + napFilename, e );
    }

  }

  public static void createTimeseriesFile( final String tsFilename, final TSMap valuesMap, final TSDesc[] TS_DESCRIPTOR, final boolean isSpreeFormat ) throws SimulationException
  {
    try
    {
      final Map fdList = new LinkedHashMap();

      if( isSpreeFormat )
        fdList.put( "DZAHL", new FieldDescriptor( "DZAHL", "N", (byte) 7, (byte) 2 ) );
      else
        fdList.put( "DATUM", new FieldDescriptor( "DATUM", "N", (byte) 7, (byte) 2 ) );

      fdList.put( "STUNDE", new FieldDescriptor( "STUNDE", "N", (byte) 2, (byte) 0 ) );

      if( isSpreeFormat )
      {
        fdList.put( "DATUM", new FieldDescriptor( "DATUM", "C", (byte) 10, (byte) 0 ) );
        fdList.put( "VON", new FieldDescriptor( "VON", "N", (byte) 2, (byte) 0 ) );
        fdList.put( "AB", new FieldDescriptor( "AB", "N", (byte) 2, (byte) 0 ) );
      }

      for( final TSDesc desc : TS_DESCRIPTOR )
      {
        final String name = desc.id;

        final FieldDescriptor fd;
        if( name.startsWith( "S_" ) )
          fd = new FieldDescriptor( name, "C", (byte) 1, (byte) 0 );
        else if( name.startsWith( "W_" ) )
          fd = new FieldDescriptor( name, "N", (byte) 5, (byte) 0 );
        else if( name.startsWith( "Q_" ) )
          fd = new FieldDescriptor( name, "N", (byte) 7, (byte) 2 );
        else if( name.startsWith( "QX_" ) )
          fd = new FieldDescriptor( name, "N", (byte) 7, (byte) 2 );
        else if( name.startsWith( "WV_" ) )
          fd = new FieldDescriptor( name, "N", (byte) 5, (byte) 0 );
        else if( name.startsWith( "QV_" ) )
          fd = new FieldDescriptor( name, "N", (byte) 7, (byte) 2 );
        else if( name.startsWith( "QP_" ) )
          fd = new FieldDescriptor( name, "N", (byte) 8, (byte) 3 );
        else if( name.startsWith( "PG_" ) )
          fd = new FieldDescriptor( name, "N", (byte) 5, (byte) 1 );
        else if( name.startsWith( "PP_" ) )
          fd = new FieldDescriptor( name, "N", (byte) 3, (byte) 0 );
        else if( name.startsWith( "PA_" ) )
          fd = new FieldDescriptor( name, "N", (byte) 6, (byte) 2 );
        else if( name.startsWith( "V_" ) ) // REMARK: bei der Spree ists hier 6:2; sollte aber keinen Unterschied
          // machen
          fd = new FieldDescriptor( name, "N", (byte) 7, (byte) 3 );
        else if( name.startsWith( "ZG_" ) )
          fd = new FieldDescriptor( name, "N", (byte) 7, (byte) 2 );
        else
          throw new IllegalStateException( "TS Descriptor with wrong type: " + name );

        fdList.put( name, fd );
      }

      final DBaseFile dbf = new DBaseFile( tsFilename, (FieldDescriptor[]) fdList.values().toArray( new FieldDescriptor[fdList.size()] ) );

      // Werte schreiben
      final Calendar calendar = Calendar.getInstance();

      final Date[] dateArray = valuesMap.getDates();

      final int datelength = (dateArray.length % 2 == 0) ? dateArray.length : dateArray.length - 1;

      for( int i = 0; i < datelength; i++ )
      {
        final Date date = dateArray[i];

        final ArrayList record = new ArrayList();

        calendar.setTime( date );
        record.add( new Double( WasyCalcJob.DF_DZAHL.format( date ) ) );
        record.add( new Integer( calendar.get( Calendar.HOUR_OF_DAY ) ) );

        if( isSpreeFormat )
        {
          record.add( WasyCalcJob.DF_DATUM.format( date ) );
          calendar.add( Calendar.HOUR_OF_DAY, -3 );
          record.add( new Integer( calendar.get( Calendar.HOUR_OF_DAY ) ) );
          calendar.add( Calendar.HOUR_OF_DAY, -3 );
          record.add( new Integer( calendar.get( Calendar.HOUR_OF_DAY ) ) );
        }

        for( final TSDesc element : TS_DESCRIPTOR )
        {
          final String id = element.id;
          final Map datesToValuesMap = valuesMap.getTimeserie( id );

          Double outVal = null;

          if( datesToValuesMap != null )
          {
            final Double value = ((Double) datesToValuesMap.get( date ));

            // HACK: THE DBF Writer does not round double, when
            // written without decimals, it just prints out the integer part
            // so we round ourselfs
            final FieldDescriptor fd = (FieldDescriptor) fdList.get( id );
            final byte[] fddata = fd.getFieldDescriptor();
            final char type = (char) fddata[11];
            final int decimalcount = fddata[17];
            if( value != null && type == 'N' && decimalcount == 0 )
              outVal = new Double( Math.round( value.doubleValue() ) );
            else
              outVal = value;
          }

          // die Erste Zeile darf keine Talsperrenabgabe enthalten
          if( i == 0 )
          {
            if( id.startsWith( "QV_TS" ) )
              outVal = null;
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

      throw new SimulationException( "Fehler beim Schreiben der Zeitreihen", e1 );
    }
  }

  /**
   * Liest die Zeitreihen und erzeugt daraus eine Tabelle (Map)
   */
  public static TSMap readZML( final ISimulationDataProvider inputProvider, final TSMap tsmap, final TSDesc[] TS_DESCRIPTOR ) throws IOException, SimulationException
  {
    final URL zmlURL = (URL) inputProvider.getInputForID( "ZML" );
    final String zmlURLstr = zmlURL.toExternalForm();
    final URL zmlURLDir = new URL( zmlURLstr + "/" );

    final IUrlResolver urlUtilities = new UrlUtilities();

    // alle Zeitreihen lesen
    for( final TSDesc tsDesc : TS_DESCRIPTOR )
    {
      final URL obsURL = urlUtilities.resolveURL( zmlURLDir, tsDesc.id + ".zml" );
      try
      {
        final IObservation obs = ZmlFactory.parseXML( obsURL, "" );

        tsmap.addObservation( obs, tsDesc.id );

        if( tsDesc.id.startsWith( "W_" ) )
        {
          // neuen Namen generieren
          try
          {
            final String qName = "Q_" + tsDesc.id.substring( 2 );

            final IAxis runoffAxis = ObservationUtilities.findAxisByTypeNoEx( obs.getAxisList(), TimeserieConstants.TYPE_RUNOFF );
            /* Falls bereits eine Q-Achse da ist direkt nehmen, ansonsten versuchen umzurechnen */
            if( runoffAxis == null )
            {
              final WQTimeserieProxy filter = new WQTimeserieProxy( TimeserieConstants.TYPE_WATERLEVEL, TimeserieConstants.TYPE_RUNOFF, obs );

              tsmap.addObservation( filter, qName );
            }
            else
              tsmap.addObservation( obs, qName );
          }
          catch( final Exception e )
          {
            LOGGER.log( Level.INFO, "WQ-Umrechnung klappt nicht für: " + obsURL, e );
          }
        }
      }
      catch( final SensorException se )
      {
        // todo: besser gleich ganz abbrechen?
        if( tsDesc.isRequired )
          LOGGER.log( Level.INFO, "ZML wurde nicht geladen: " + obsURL, se );
      }
    }

    return tsmap;
  }

  public static void findAndWriteLayer( final GMLWorkspace workspace, final String layerName, final Map mapping, final String geoName, final String filenameBase ) throws SimulationException
  {
    try
    {
      final IFeatureType featureType = workspace.getFeatureType( layerName );
      if( featureType == null )
        throw new SimulationException( "Eingabedatei für Rechenmodell konnte nicht erzeugt werden. Layer nicht gefunden: " + layerName, null );

      final Feature[] features = workspace.getFeatures( featureType );

      ShapeSerializer.serializeFeatures( features, mapping, geoName, filenameBase, null );
    }
    catch( final GmlSerializeException e )
    {
      e.printStackTrace();

      throw new SimulationException( "Fehler beim Schreiben der Eingabedateien", e );
    }
  }

  public static Map parseControlFile( final URL gmlURL, final File nativedir, final WasyCalcJob wasyJob ) throws SimulationException
  {
    try
    {
      final Feature controlFeature = GmlSerializer.createGMLWorkspace( gmlURL, null ).getRootFeature();

      final Date startSimTime = (Date) DateUtilities.toDate( (XMLGregorianCalendar) controlFeature.getProperty( "startsimulation" ) );
      final Date startForecastTime = (Date) DateUtilities.toDate( (XMLGregorianCalendar) controlFeature.getProperty( "startforecast" ) );

      final String startTimeString = new SimpleDateFormat( "yyMMdd" ).format( startForecastTime );
      final String baseFileName = "HW" + startTimeString;

      final String tsFilename = new File( nativedir, baseFileName ).getAbsolutePath();
      final File tsFile = new File( tsFilename + ".dbf" );

      final String napFilename = wasyJob.makeNapFilename( nativedir, tsFilename );
      final File napFile = new File( napFilename + ".dbf" );

      final File vhsFile = new File( tsFilename + WasyCalcJob.VHS_FILE );

      final String flpFilename = wasyJob.makeFlpFilename( nativedir, tsFilename );
      final File flpFile = new File( flpFilename + ".dbf" );

      final File wqFile = new File( nativedir, "HW_WQ.dbf" );

      final Map dataMap = new HashMap();
      dataMap.put( WasyCalcJob.DATA_STARTSIM_DATE, startSimTime );
      dataMap.put( WasyCalcJob.DATA_STARTFORECAST_DATE, startForecastTime );
      dataMap.put( WasyCalcJob.DATA_STARTDATESTRING, startTimeString );
      dataMap.put( WasyCalcJob.DATA_BASEFILENAME, baseFileName );
      dataMap.put( WasyCalcJob.DATA_FLPFILE, flpFile );
      dataMap.put( WasyCalcJob.DATA_VHSFILE, vhsFile );
      dataMap.put( WasyCalcJob.DATA_NAPFILE, napFile );
      dataMap.put( WasyCalcJob.DATA_FLPFILENAME, flpFilename );
      dataMap.put( WasyCalcJob.DATA_NAPFILENAME, napFilename );
      dataMap.put( WasyCalcJob.DATA_TSFILENAME, tsFilename );
      dataMap.put( WasyCalcJob.DATA_TSFILE, tsFile );
      dataMap.put( WasyCalcJob.DATA_WQFILE, wqFile );

      return dataMap;
    }
    catch( final Exception e )
    {
      throw new SimulationException( "Fehler beim Einlesen der Berechnungsparameter", e );
    }
  }

  /**
   * Gibt die Datei zum entsprechenden index zurück
   * 
   * @param id
   * @param input
   * @param basedir
   * @return file
   * @throws CalcJobServiceException
   */
  public static File checkInput( final String id, final Map input, final File basedir ) throws SimulationException
  {
    final SimulationDataPath bean = (SimulationDataPath) input.get( id );
    if( bean == null )
      throw new SimulationException( "Eingabedatei für Index <" + id + "> fehlt", null );

    final File file = new File( basedir, bean.getPath() );
    if( !file.exists() || !file.isFile() )
      throw new SimulationException( "Eingabedatei für Index <" + id + "> fehlt: " + file.getAbsolutePath(), null );

    return file;
  }

  /**
   * Opens a stream on the given url and copies its content into a string.
   * 
   * @throws IOException
   */
  public static String toString( final URL url, final String encoding ) throws IOException
  {
    InputStream is = null;
    try
    {
      is = url.openStream();
      final String content = IOUtils.toString( is, encoding );
      is.close();
      return content;
    }
    finally
    {
      IOUtils.closeQuietly( is );
    }
  }
}