package org.kalypso.lhwzsachsen.spree;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
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

import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.io.ReaderUtilities;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.services.calculation.job.impl.AbstractCalcJob;
import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypso.util.progress.NullProgressMonitor;
import org.kalypso.zml.ObservationType;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * <p>
 * Der Rechenservice für das SpreeModell
 * </p>
 * 
 * @author Belger
 */
public class SpreeCalcJob extends AbstractCalcJob
{
  public static final String CALC_PROP_STARTTIME = "startTime";

  public static final String VHS_FILE = "_vhs.dbf";

  public static final String FLP_FILE = "_flp";

  public static final String FLP_NAME = "Flusslaufmodell";

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

  public static final Object DATA_STARTDATE = "startDate";

  public static final Object DATA_STARTDATESTRING = "startDateString";

  public static final Object DATA_BASEFILENAME = "baseFileName";

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
      new TSDesc( "W_SCHIRG" ),
      new TSDesc( "Q_SCHIRG" ),
      new TSDesc( "QX_SCHIRG" ),
      new TSDesc( "WV_SCHIRG" ),
      new TSDesc( "QV_SCHIRG" ),
      new TSDesc( "QP_SCHIRG" ),
      new TSDesc( "PG_SCHIRG" ),
      new TSDesc( "PP_SCHIRG" ),
      new TSDesc( "PA_SCHIRG" ),
      new TSDesc( "S_BAUTZWB" ),
      new TSDesc( "W_BAUTZWB" ),
      new TSDesc( "Q_BAUTZWB" ),
      new TSDesc( "QX_BAUTZWB" ),
      new TSDesc( "WV_BAUTZWB" ),
      new TSDesc( "QV_BAUTZWB" ),
      new TSDesc( "QP_BAUTZWB" ),
      new TSDesc( "PG_BAUTZWB" ),
      new TSDesc( "PP_BAUTZWB" ),
      new TSDesc( "PA_BAUTZWB" ),
      new TSDesc( "S_TSBAUTZ" ),
      new TSDesc( "Q_TSBAUTZ" ),
      new TSDesc( "QV_TSBAUTZ" ),
      new TSDesc( "QP_TSBAUTZ" ),
      new TSDesc( "V_TSBAUTZ" ),
      new TSDesc( "S_GROEDI" ),
      new TSDesc( "W_GROEDI" ),
      new TSDesc( "Q_GROEDI" ),
      new TSDesc( "QX_GROEDI" ),
      new TSDesc( "WV_GROEDI" ),
      new TSDesc( "QV_GROEDI" ),
      new TSDesc( "QP_GROEDI" ),
      new TSDesc( "ZG_GROEDI" ),
      new TSDesc( "PG_GROEDI" ),
      new TSDesc( "PP_GROEDI" ),
      new TSDesc( "PA_GROEDI" ),
      new TSDesc( "S_SPWIESE" ),
      new TSDesc( "QV_SPWIESE" ),
      new TSDesc( "QP_SPWIESE" ),
      new TSDesc( "S_LIESKE" ),
      new TSDesc( "W_LIESKE" ),
      new TSDesc( "Q_LIESKE" ),
      new TSDesc( "QX_LIESKE" ),
      new TSDesc( "WV_LIESKE" ),
      new TSDesc( "QV_LIESKE" ),
      new TSDesc( "QP_LIESKE" ),
      new TSDesc( "S_JAENKD" ),
      new TSDesc( "W_JAENKD" ),
      new TSDesc( "Q_JAENKD" ),
      new TSDesc( "QX_JAENKD" ),
      new TSDesc( "WV_JAENKD" ),
      new TSDesc( "QV_JAENKD" ),
      new TSDesc( "QP_JAENKD" ),
      new TSDesc( "PG_JAENKD" ),
      new TSDesc( "PP_JAENKD" ),
      new TSDesc( "PA_JAENKD" ),
      new TSDesc( "S_TSQUITZ" ),
      new TSDesc( "Q_TSQUITZ" ),
      new TSDesc( "QV_TSQUITZ" ),
      new TSDesc( "QP_TSQUITZ" ),
      new TSDesc( "V_TSQUITZ" ),
      new TSDesc( "S_SAERI" ),
      new TSDesc( "W_SAERI" ),
      new TSDesc( "Q_SAERI" ),
      new TSDesc( "QX_SAERI" ),
      new TSDesc( "WV_SAERI" ),
      new TSDesc( "QV_SAERI" ),
      new TSDesc( "QP_SAERI" ),
      new TSDesc( "ZG_SAERI" ),
      new TSDesc( "PG_SAERI" ),
      new TSDesc( "PP_SAERI" ),
      new TSDesc( "PA_SAERI" ),
      new TSDesc( "S_BOXBRG" ),
      new TSDesc( "W_BOXBRG" ),
      new TSDesc( "Q_BOXBRG" ),
      new TSDesc( "QX_BOXBRG" ),
      new TSDesc( "WV_BOXBRG" ),
      new TSDesc( "QV_BOXBRG" ),
      new TSDesc( "QP_BOXBRG" ),
      new TSDesc( "S_BWALDE" ),
      new TSDesc( "QV_BWALDE" ),
      new TSDesc( "QP_BWALDE" ),
      new TSDesc( "S_LOHSA" ),
      new TSDesc( "QV_LOHSA" ),
      new TSDesc( "QP_LOHSA" ),
      new TSDesc( "S_SPREY" ),
      new TSDesc( "W_SPREY" ),
      new TSDesc( "Q_SPREY" ),
      new TSDesc( "QX_SPREY" ),
      new TSDesc( "WV_SPREY" ),
      new TSDesc( "QV_SPREY" ),
      new TSDesc( "QP_SPREY" ),
      new TSDesc( "S_BURGNEU" ),
      new TSDesc( "QP_BURGNEU" ),
      new TSDesc( "S_SPWITZ" ),
      new TSDesc( "W_SPWITZ" ),
      new TSDesc( "Q_SPWITZ" ),
      new TSDesc( "QX_SPWITZ" ),
      new TSDesc( "WV_SPWITZ" ),
      new TSDesc( "QV_SPWITZ" ),
      new TSDesc( "QP_SPWITZ" ),
      new TSDesc( "S_RLKETTE" ),
      new TSDesc( "QV_RLKETTE" ),
      new TSDesc( "QP_RLKETTE" ),
      new TSDesc( "S_SPREMB" ),
      new TSDesc( "W_SPREMB" ),
      new TSDesc( "Q_SPREMB" ),
      new TSDesc( "QX_SPREMB" ),
      new TSDesc( "WV_SPREMB" ),
      new TSDesc( "QV_SPREMB" ),
      new TSDesc( "QP_SPREMB" ) };

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.service.CalcJobDataBean[])
   */
  public void run( final File basedir, final CalcJobDataBean[] input )
      throws CalcJobServiceException
  {
    if( isCanceled() )
      return;

    final File inputdir = new File( basedir, "input" );
    final File nativedir = new File( basedir, "native" );

    final Map props = SpreeInputWorker.createNativeInput( inputdir, nativedir, input );

    progress( 33 );
    if( isCanceled() )
      return;

    startCalculation( nativedir, props );

    progress( 33 );

    if( isCanceled() )
      return;

    final File outputdir = new File( basedir, "output" );
    loadNativeOutput( basedir, outputdir, props );

    progress( 34 );

    if( isCanceled() )
      return;
  }

  private void loadNativeOutput( final File basedir, final File outputdir, final Map props )
      throws CalcJobServiceException
  {
    try
    {
      final File napFile = (File)props.get( DATA_NAPFILE );
      final File vhsFile = (File)props.get( DATA_VHSFILE );
      final File flpFile = (File)props.get( DATA_FLPFILE );
      final String tsFilename = (String)props.get( DATA_TSFILENAME );

      writeResultsToFolder( tsFilename, basedir, outputdir, props );

      addResult( new CalcJobDataBean( "", "", FileUtilities.getRelativeFileTo( basedir, napFile ).getPath() ) );
      addResult( new CalcJobDataBean( "", "", FileUtilities.getRelativeFileTo( basedir, vhsFile ).getPath() ) );
      addResult( new CalcJobDataBean( "", "", FileUtilities.getRelativeFileTo( basedir, flpFile ).getPath() ) );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "", e );
    }
  }

  private void startCalculation( final File nativedir, final Map m_data ) throws CalcJobServiceException
  {
    InputStreamReader inStream = null;
    InputStreamReader errStream = null;

    try
    {
      final File batFile = new File( "D:\\VSpree\\Debug\\trick.bat" );
      batFile.delete();
      final File tsFile = (File)m_data.get( DATA_TSFILE );
      final String commandString = "D:/VSpree/Debug/hw.exe" + " " + tsFile.getAbsolutePath();
      final Date startTime = (Date)m_data.get( DATA_STARTDATE );
      final Date time = new Date();

      // create crackfile
      final PrintWriter crackWriter = new PrintWriter( new OutputStreamWriter(
          new FileOutputStream( batFile ) ) );
      crackWriter.println( "date " + new SimpleDateFormat( "dd-MM-yyyy" ).format( startTime ) );
      crackWriter.println( "time " + new SimpleDateFormat( "HH:mm" ).format( startTime ) );
      crackWriter.println( commandString );
      crackWriter.println( "date " + new SimpleDateFormat( "dd-MM-yyyy" ).format( time ) );
      crackWriter.println( "time " + new SimpleDateFormat( "HH:mm" ).format( time ) );
      crackWriter.close();

      final Process process = Runtime.getRuntime().exec( "cmd.exe /C " + batFile.getAbsolutePath(),
          null, nativedir );

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

        if( isCanceled() )
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
    catch( InterruptedException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Fehler beim Ausführen der hw.exe", e );
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
      catch( final IOException e1 )
      {
        e1.printStackTrace();
      }
    }
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#disposeJob()
   */
  public void disposeJob()
  {
  // die dateien werden extern gelöscht, sonst hab ich nix gemacht
  }

  public void writeResultsToFolder( final String tsFilename, final File basedir, final File outdir,
      final Map dataMap ) throws Exception
  {
    final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    final CS_CoordinateSystem crs = org.deegree_impl.model.cs.Adapters.getDefault().export(
        csFac.getCSByName( "EPSG:4326" ) );

    final KalypsoFeatureLayer layer = ShapeSerializer.deserialize( tsFilename, crs, crs,
        new NullProgressMonitor() );

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
      final int hour = ( (Number)feature.getProperty( "STUNDE" ) ).intValue();
      calendar.setTime( date );
      calendar.add( Calendar.HOUR_OF_DAY, hour );
      dates.add( calendar.getTime() );

      for( int j = 0; j < TS_DESCRIPTOR.length; j++ )
      {
        final TSDesc desc = TS_DESCRIPTOR[j];
        final String column = desc.id;

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

    final DefaultAxis dateAxis = new DefaultAxis( "Datum", "datum", "", Date.class, 0, true );
    final IAxis valueAxis = new DefaultAxis( "Wert", "wert", "", Double.class, 1, false );
    final IAxis[] achsen = new IAxis[]
    {
        dateAxis,
        valueAxis };

    final Date[] dateArray = (Date[])dates.toArray( new Date[dates.size()] );

    // create ZML for each timeserie
    for( int i = 0; i < TS_DESCRIPTOR.length; i++ )
    {
      final TSDesc desc = TS_DESCRIPTOR[i];
      final String column = desc.id;

      final String outdirname = "Zeitreihen";
      final String outfilename = column + ".zml";
      final File outputDir = new File( outdir, outdirname );
      outputDir.mkdirs();
      final File outFile = new File( outputDir, outfilename );
      final File outFileRelative = FileUtilities.getRelativeFileTo( basedir, outFile );

      final Collection values = (Collection)valuesMap.get( column );
      if( values == null )
      {
        FileUtilities.makeFileFromStream( false, outFile, getClass().getResourceAsStream(
            "resources/empty.zml" ) );
        addResult( new CalcJobDataBean( column, column, outFileRelative.getPath() ) );
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
        //metadata.setProperty( "Berechnung", (String)dataMap.get( DATA_LABEL ) );
        metadata.setProperty( "StartZeit", (String)dataMap.get( DATA_STARTDATESTRING ) );

        final IObservation observation = new SimpleObservation( column, column, false, null, metadata,
            achsen );
        observation.setValues( model );

        final ObservationType observationType = ZmlFactory.createXML( observation, null );
        final FileOutputStream outStream = new FileOutputStream( outFile );
        ZmlFactory.getMarshaller().marshal( observationType, outStream );
        outStream.close();

      }
      else
        FileUtilities.makeFileFromStream( false, outFile, getClass().getResourceAsStream(
            "resources/empty.zml" ) );

      addResult( new CalcJobDataBean( column, column, outFileRelative.getPath() ) );
    }
  }

}