package org.kalypso.ogc.sensor.diagview.grafik;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.io.Writer;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentThread;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.ObservationTemplateHelper;
import org.kalypso.ogc.sensor.diagview.grafik.GrafikYAchsen.GrafikAchse;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obsdiagview.TypeCurve;
import org.kalypso.template.obsdiagview.TypeObservation;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.util.url.UrlResolver;
import org.xml.sax.InputSource;

/**
 * GrafikLauncher
 * 
 * @author schlienger
 */
public class GrafikLauncher
{
  /** file extension of the grafik template files */
  public final static String TPL_FILE_EXTENSION = "tpl";

  /** date format understood by the grafik tool */
  protected final static DateFormat GRAFIK_DF = new SimpleDateFormat(
      "dd.MM.yyyy HH:mm:ss" );

  private GrafikLauncher( )
  {
    // no instanciation
  }

  /**
   * Opens the grafik tool using an observation template file.
   * 
   * @param odtFile
   * @param dest
   * @param monitor
   * @return the created tpl file
   * 
   * @throws SensorException
   */
  public static IFile startGrafikODT( final IFile odtFile, final IFolder dest,
      final IProgressMonitor monitor ) throws SensorException
  {
    try
    {
      final IFile tplFile = dest.getFile( FileUtilities
          .nameWithoutExtension( odtFile.getName() )
          + ".tpl" );

      final StringWriter strWriter = new StringWriter();

      odt2tpl( odtFile, dest, strWriter, monitor );

      // use the windows encoding for the vorlage because of the grafik tool
      // which uses it when reading...
      SetContentThread thread = new SetContentThread( tplFile, !tplFile
          .exists(), false, false, new NullProgressMonitor(), "Cp1252" )
      {
        protected void write( final Writer writer ) throws Throwable
        {
          writer.write( strWriter.toString() );
        }
      };

      thread.start();
      thread.join();

      if( thread.getFileException() != null )
        throw thread.getFileException();

      if( thread.getThrown() != null )
        throw thread.getThrown();

      startGrafikTPL( tplFile );

      return tplFile;
    }
    catch( Throwable e ) // generic exception caught
    {
      throw new SensorException( e );
    }
  }

  /**
   * Starts the grafik.exe with an eclipse IFile tpl-File.
   * 
   * @param tplFile
   *          the Grafik-Vorlage
   * 
   * @throws SensorException
   */
  public static void startGrafikTPL( final IFile tplFile )
      throws SensorException
  {
    final File file = ResourceUtilities
        .makeFileFromPath( tplFile.getFullPath() );

    startGrafikTPL( file );
  }

  /**
   * Starts the grafik with a java.lang.File tpl-File.
   * 
   * @param tplFile
   * @throws SensorException
   */
  public static void startGrafikTPL( final File tplFile )
      throws SensorException
  {
    try
    {
      // create the grafik exe
      final File grafikExe = FileUtilities
          .makeFileFromStream(
              false,
              "grafik",
              ".exe",
              ObservationTemplateHelper.class
                  .getResourceAsStream( "/org/kalypso/ui/resources/exe/grafik.exe_" ),
              true );

      // also create the help file if not already existing
      final File grafikHelp = new File( grafikExe.getParentFile(),
          FileUtilities.nameWithoutExtension( grafikExe.getName() ) + ".hlp" );
      if( !grafikHelp.exists() )
      {
        final File tmp = FileUtilities
            .makeFileFromStream(
                false,
                "grafik",
                ".hlp",
                ObservationTemplateHelper.class
                    .getResourceAsStream( "/org/kalypso/ui/resources/exe/grafik.hlp" ),
                true );

        // the help must have the same name as the exe (except file-extension)
        FileUtils.copyFile( tmp, grafikHelp );
        tmp.delete();
      }

      Runtime.getRuntime().exec(
          grafikExe.getAbsolutePath() + " /V\"" + tplFile.getAbsolutePath()
              + '"' );
    }
    catch( IOException e )
    {
      throw new SensorException( e );
    }
  }

  /**
   * Converts a diagram template file to a grafik tpl.
   * 
   * @param odtFile
   * @param dest
   * @param writer
   * @param monitor
   * @throws SensorException
   */
  public static void odt2tpl( final IFile odtFile, final IFolder dest,
      final Writer writer, final IProgressMonitor monitor )
      throws SensorException
  {
    InputStream ins = null;

    try
    {
      final ObsdiagviewType odt = ObservationTemplateHelper
          .loadDiagramTemplateXML( odtFile.getContents() );

      final UrlResolver urlRes = new UrlResolver();
      final URL context = ResourceUtilities.createURL( odtFile );

      final GrafikYAchsen yAchsen = new GrafikYAchsen( odt.getAxis() );
      String dateAxisLabel = "Datum";

      final List xLines = new ArrayList();
      final List yLines = new ArrayList();
      
      int ixObs = 1;

      final List tobsList = odt.getObservation();
      for( final Iterator ito = tobsList.iterator(); ito.hasNext(); )
      {
        if( monitor.isCanceled() )
          return;

        final TypeObservation tobs = (TypeObservation) ito.next();

        // maps obs axis to diag axis
        final Map axisNames = new HashMap();

        final List tcurveList = tobs.getCurve();
        for( Iterator itc = tcurveList.iterator(); itc.hasNext(); )
        {
          final TypeCurve tc = (TypeCurve) itc.next();

          final List tmList = tc.getMapping();
          for( Iterator itm = tmList.iterator(); itm.hasNext(); )
          {
            final TypeAxisMapping tm = (TypeAxisMapping) itm.next();

            axisNames.put( tm.getObservationAxis(), tm.getDiagramAxis() );
          }
        }

        final URL url = urlRes.resolveURL( context, tobs.getHref() );
        final IFile zmlFile = ResourceUtilities.findFileFromURL( url );

        // if file cannot be found, that probably means it is not local...
        // maybe make a better test later?
        if( zmlFile == null )
        {
          final String msg = "Konvertierung nicht möglich, Zml-Datei ist keine lokale Datei: "
              + url.toExternalForm();
          Logger.getLogger( ObservationTemplateHelper.class.getName() )
              .warning( msg );
          continue;
        }

        ins = zmlFile.getContents();
        final IObservation obs = ZmlFactory.parseXML( new InputSource( ins ),
            zmlFile.toString(), context );

        // find out which axes to use
        final IAxis[] axes = obs.getAxisList();
        final IAxis dateAxis = ObservationUtilities.findAxisByClass( axes,
            Date.class )[0];
        final IAxis[] numberAxes = ObservationUtilities.findAxisByClass( axes,
            Number.class );

        // remove date axis from names list, we always take it
        dateAxisLabel = dateAxis.getName();
        axisNames.remove( dateAxisLabel );

        final IFile datFile = dest.getFile( FileUtilities
            .nameWithoutExtension( zmlFile.getName() )
            + ".dat" );
        zml2dat( obs, datFile, dateAxis, numberAxes, axisNames, monitor );

        // adapt grafik-axis type according to real axis type (mapping)
        final String grafikType = GrafikYAchsen.axis2grafikType( numberAxes[0]
            .getType() );

        String grafikAxis = "1";
        final GrafikAchse achse = yAchsen.getFor( (String) axisNames
            .get( numberAxes[0].getName() ) );
        if( achse != null )
          grafikAxis = String.valueOf( achse.getId() );

        final String title = zmlFile.getName()
            + (tobs.getTitle() != null ? tobs.getTitle() : "");

        writer.write( ixObs++ + "- " + datFile.getName() + " J " + grafikType
            + " " + grafikAxis + " " + title + "\n" );
        
        // is this obs a forecast?
        final DateRangeArgument fr = TimeserieUtils.isForecast( obs );
        if( fr != null )
          xLines.add( GRAFIK_DF.format( fr.getFrom() ) );
        
        // does is have Alarmstufen?
        final MetadataList mdl = obs.getMetadataList();
        final String[] mds = TimeserieUtils.findOutMDAlarmstufen( obs );
        for( int i = 0; i < mds.length; i++ )
          yLines.add( mdl.getProperty( mds[i] ) );
      }

      writer.write( "\n" );
      writer.write( "HTitel:\t" + odt.getTitle() + "\n" );
      writer.write( "xTitel:\t" + dateAxisLabel + "\n" );
      writer.write( "yTitel1:\t" + yAchsen.getLabelAt( 1 ) + "\n" );
      writer.write( "yTitel2:\t" + yAchsen.getLabelAt( 2 ) + "\n" );
      
      // TODO: find out which command means vertical line in the grafik tool...
      // writer.write( verticalLines... + '\n' );
      
      // TODO: find out command for constant horizontal lines...
      // writer.write( alarmstufen...  + '\n' );
    }
    catch( Exception e )
    {
      throw new SensorException( e );
    }
    finally
    {
      IOUtils.closeQuietly( ins );
      IOUtils.closeQuietly( writer );
    }
  }

  /**
   * Converts a zml file to a dat file that the grafik tool can load.
   * 
   * @param obs
   * @param datFile
   * @param dateAxis
   * @param numberAxes
   * @param axisNames
   * @param monitor
   * @throws SensorException
   */
  private static void zml2dat( final IObservation obs, final IFile datFile,
      final IAxis dateAxis, final IAxis[] numberAxes, final Map axisNames,
      final IProgressMonitor monitor ) throws SensorException
  {
    try
    {
      final SetContentThread thread = new SetContentThread( datFile, !datFile
          .exists(), false, false, new NullProgressMonitor() )
      {
        protected void write( final Writer writer ) throws Throwable
        {
          final ITuppleModel values = obs.getValues( null );
          for( int i = 0; i < values.getCount(); i++ )
          {
            if( monitor.isCanceled() )
              return;

            final Object elt = values.getElement( i, dateAxis );
            final String text = GRAFIK_DF.format( elt );
            writer.write( text );
            writer.write( '\t' );

            for( int j = 0; j < numberAxes.length; j++ )
            {
              final IAxis axis = numberAxes[j];

              if( axisNames.containsKey( axis.getName() ) )
              {
                writer.write( values.getElement( i, axis ).toString() );
                writer.write( '\t' );
              }
            }

            writer.write( '\n' );
          }
        }
      };

      thread.start();
      thread.join();

      final CoreException fileException = thread.getFileException();
      if( fileException != null )
        throw fileException;

      final Throwable thrown = thread.getThrown();
      if( thrown != null )
        throw thrown;
    }
    catch( Throwable e )
    {
      throw new SensorException( e );
    }
  }
}