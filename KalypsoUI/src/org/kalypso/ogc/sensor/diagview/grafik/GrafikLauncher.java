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
package org.kalypso.ogc.sensor.diagview.grafik;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.net.URL;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;
import java.util.logging.Logger;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.auth.KalypsoAuthPlugin;
import org.kalypso.auth.scenario.IScenario;
import org.kalypso.auth.scenario.ScenarioUtilities;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.io.ProcessWraper;
import org.kalypso.commons.java.net.UrlResolver;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.MultiStatus;
import org.kalypso.contribs.java.util.DoubleComparator;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IAxisRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationConstants;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.DiagViewUtils;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.template.NameUtils;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obsdiagview.TypeCurve;
import org.kalypso.template.obsdiagview.TypeObservation;
import org.kalypso.ui.KalypsoGisPlugin;
import org.xml.sax.InputSource;

/**
 * GrafikLauncher
 * 
 * @author schlienger
 */
public class GrafikLauncher
{
  public final static String GRAFIK_ENCODING = "Cp1252";

  /** file extension of the grafik template files */
  public final static String TPL_FILE_EXTENSION = "tpl";

  /** date format understood by the grafik tool */
  protected final static DateFormat GRAFIK_DF = new SimpleDateFormat( "dd.MM.yyyy HH:mm:ss" );

  private final static NumberFormat GRAFIK_NF_W = NumberFormat.getIntegerInstance();

  static
  {
    GRAFIK_NF_W.setMaximumFractionDigits( 0 );
  }

  private GrafikLauncher()
  {
  // no instanciation
  }

  /**
   * Opens the grafik tool using an observation template file. Note: this method should be called using a
   * WorkspaceModifyOperation.
   * 
   * @return the created tpl file
   * 
   * @throws SensorException
   */
  public static IStatus startGrafikODT( final IFile odtFile, final IFolder dest, final IProgressMonitor monitor )
      throws SensorException
  {
    final ObsdiagviewType odt;
    try
    {
      odt = DiagViewUtils.loadDiagramTemplateXML( odtFile.getContents() );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new SensorException( e );
    }

    return startGrafikODT( odtFile.getName(), odt, dest, monitor );
  }

  /**
   * Open the grafik tool using a zml file.
   */
  public static IStatus startGrafikZML( final IFile zmlFile, final IFolder dest, final IProgressMonitor monitor )
      throws SensorException
  {
    final DiagView diag = new DiagView( zmlFile.getName(), "Legende", true );

    try
    {
      final URL context = ResourceUtilities.createURL( zmlFile );

      final IStatus status = diag.loadObservation( context, context.toExternalForm(), false, "",
          NameUtils.DEFAULT_ITEM_NAME, ObsView.DEFAULT_ITEM_DATA, true );

      if( !status.isOK() )
        return status;

      final ObsdiagviewType odt = DiagViewUtils.buildDiagramTemplateXML( diag );
      return startGrafikODT( zmlFile.getName(), odt, dest, monitor );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SensorException( e );
    }
  }

  /**
   * Opens the grafik tool using an observation template xml object. Note: this method should be called using a
   * WorkspaceModifyOperation.
   * 
   * @param fileName
   *          the filename to use for the grafik template file
   * @param odt
   *          the xml binding object
   * 
   * @throws SensorException
   */
  public static IStatus startGrafikODT( final String fileName, final ObsdiagviewType odt, final IFolder dest,
      final IProgressMonitor monitor ) throws SensorException
  {
    List sync = new Vector();
    StringWriter strWriter = null;
    try
    {
      if( !dest.exists() )
        dest.create( true, true, monitor );

      final IFile tplFile = dest.getFile( FileUtilities.nameWithoutExtension( fileName ) + ".tpl" );

      strWriter = new StringWriter();
      final IStatus status = odt2tpl( odt, dest, strWriter, monitor, sync );
      strWriter.close();

      // status might not be ok but we still want to start the grafik tool
      // so inform the use here with the current info
      if( !status.isOK() )
        return status;

      // redeclared final for being used in SetContentHelper
      final StringWriter schWriter = strWriter;

      // use the windows encoding for the vorlage because of the grafik tool
      // which uses it when reading...
      final SetContentHelper sch = new SetContentHelper( "Datei Konvertierung - Grafik öffnen" )
      {
        protected void write( final OutputStreamWriter writer ) throws Throwable
        {
          writer.write( schWriter.toString() );
        }
      };

      sch.setFileContents( tplFile, false, false, new NullProgressMonitor(), GRAFIK_ENCODING );

      startGrafikTPL( tplFile, sync );

      return Status.OK_STATUS;
    }
    catch( Throwable e ) // generic exception caught
    {
      throw new SensorException( e );
    }
    finally
    {
      IOUtils.closeQuietly( strWriter );

      if( sync != null )
        sync.clear();
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
  public static IStatus startGrafikTPL( final IFile tplFile, final List sync ) throws SensorException
  {
    final File file = ResourceUtilities.makeFileFromPath( tplFile.getFullPath() );

    return startGrafikTPL( file, sync );
  }

  /**
   * Starts the grafik with a java.lang.File tpl-File.
   */
  private static IStatus startGrafikTPL( final File tplFile, final List sync ) throws SensorException
  {
    try
    {
      final File grafikExe = getGrafikProgramPath();

      final Process proc = Runtime.getRuntime().exec(
          grafikExe.getAbsolutePath() + " /V\"" + tplFile.getAbsolutePath() + '"' );

      final MultiStatus ms = new MultiStatus( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
          "Grafik kann nicht gestartet werden" );

      final ProcessWraper wraper = new ProcessWraper( proc, null )
      {
        public void processCanceled()
        {
        // empty
        }

        public void processTerminated( final int returnCode )
        {
          synchroniseZml();
        }

        private void synchroniseZml()
        {
          for( final Iterator it = sync.iterator(); it.hasNext(); )
          {
            final RememberForSync rfs = (RememberForSync)it.next();

            try
            {
              rfs.synchronizeZml();
            }
            catch( final Exception e )
            {
              e.printStackTrace();

              ms.addMessage( "Synchronisation von " + rfs.getDatFile().getName() + " in " + rfs.getZmlFile().getName(),
                  e );
            }
          }
        }
      };

      // wait for grafik to finish and eventually synchronise data
      wraper.waitForProcess();

      return ms;
    }
    catch( Exception e )
    {
      throw new SensorException( e );
    }
  }

  /**
   * Return the file representing the Grafik Program. Since the Grafik.exe is located in the resources of this plugin,
   * it must first be extracted into a temp file in the local file system.
   */
  public static File getGrafikProgramPath() throws IOException
  {
    // create the grafik exe
    final File grafikExe = FileUtilities.makeFileFromStream( false, "grafik", ".exe", GrafikLauncher.class
        .getResourceAsStream( "/org/kalypso/ui/resources/exe/grafik.exe_" ), true );
    grafikExe.deleteOnExit();

    // also create the help file if not already existing
    final File grafikHelp = new File( grafikExe.getParentFile(), FileUtilities.nameWithoutExtension( grafikExe
        .getName() )
        + ".hlp" );
    grafikHelp.deleteOnExit();
    if( !grafikHelp.exists() )
    {
      final File tmp = FileUtilities.makeFileFromStream( false, "grafik", ".hlp", GrafikLauncher.class
          .getResourceAsStream( "/org/kalypso/ui/resources/exe/grafik.hlp" ), true );

      // the help must have the same name as the exe (except file-extension)
      FileUtils.copyFile( tmp, grafikHelp );
      tmp.delete();
    }

    return grafikExe;
  }

  /**
   * Converts a diagram template file to a grafik tpl.
   * <p>
   * Important note: the XML-Schema for the diag template file says that if no curve element or specified for a given
   * observation, then all curves of that observation should be displayed. This is not possible here using the grafik
   * tool. As a conclusion: when a template file is meant to be used with the grafik tool, then curves need to be
   * explicitely specified in the xml.
   */
  private static IStatus odt2tpl( final ObsdiagviewType odt, final IFolder dest, final Writer writer,
      final IProgressMonitor monitor, final List sync ) throws CoreException, IOException
  {
    final UrlResolver urlRes = new UrlResolver();
    final URL context = ResourceUtilities.createURL( dest.getParent() );

    final GrafikAchsen gAchsen = new GrafikAchsen( odt.getAxis() );
    final GrafikKurven gKurven = new GrafikKurven( gAchsen );

    String scenarioName = null;
    Date xLower = null;
    Date xUpper = null;
    Number yLower = new Double( Double.MAX_VALUE );
    Number yUpper = new Double( Double.MIN_VALUE );
    final Set xLines = new TreeSet();
    final Map yLines = new HashMap();

    final Logger logger = Logger.getLogger( GrafikLauncher.class.getName() );

    final MultiStatus multiStatus = new MultiStatus( IStatus.WARNING, KalypsoGisPlugin.getId(), 0,
        "Konnte nicht alle spezifizierte Zeitreihe öffnen." );

    final TypeObservation[] tobs = (TypeObservation[])odt.getObservation().toArray( new TypeObservation[0] );
    for( int i = 0; i < tobs.length; i++ )
    {
      if( monitor.isCanceled() )
        return Status.CANCEL_STATUS;

      // now try to locate observation file
      final URL url = urlRes.resolveURL( context, tobs[i].getHref() );
      final IFile zmlFile = ResourceUtilities.findFileFromURL( url );

      // if file cannot be found, that probably means it is not local...
      // maybe make a better test later?
      if( zmlFile == null )
      {
        final String msg = "Konvertierung nicht möglich, Zml-Datei ist möglicherweise keine lokale Datei: "
            + url.toExternalForm();
        logger.warning( msg );
        multiStatus.addMessage( msg );
        continue;
      }

      final IObservation obs;
      final ITuppleModel values;
      InputStream ins = null;
      try
      {
        ins = zmlFile.getContents();
        obs = ZmlFactory.parseXML( new InputSource( ins ), zmlFile.toString(), context );
        ins.close();

        values = obs.getValues( null );
      }
      catch( final Exception e )
      {
        final String msg = "Zeitreihe konnte nicht eingelesen werden. Datei: " + zmlFile.getName() + " Grund: "
            + e.getLocalizedMessage();
        logger.warning( msg );
        multiStatus.addMessage( msg, e );
        continue;
      }
      finally
      {
        IOUtils.closeQuietly( ins );
      }

      // find out which axes to use
      final IAxis[] axes = obs.getAxisList();
      final IAxis dateAxis = ObservationUtilities.findAxisByClass( axes, Date.class );
      final IAxis[] numberAxes = KalypsoStatusUtils.findAxesByClass( axes, Number.class, true );

      final List displayedAxes = new ArrayList( numberAxes.length );

      int cc = 1;
      final List curves = tobs[i].getCurve();
      for( final Iterator itc = curves.iterator(); itc.hasNext(); )
      {
        final TypeCurve tc = (TypeCurve)itc.next();

        // create a corresponding dat-File for the current observation file
        final IFile datFile = dest
            .getFile( FileUtilities.nameWithoutExtension( zmlFile.getName() ) + "-" + cc + ".dat" );

        final IAxis axis = gKurven.addCurve( datFile, tc, numberAxes );

        displayedAxes.add( axis );

        // convert to dat-file, ready to be read by the grafik tool
        zml2dat( values, datFile, dateAxis, axis, monitor );

        final RememberForSync rfs = new RememberForSync( zmlFile, datFile, axis );
        sync.add( rfs );

        cc++;

        try
        {
          // fetch Y axis range for placing possible scenario text item
          final IAxisRange range = values.getRangeFor( axis );

          final DoubleComparator dc = new DoubleComparator( 0.001 );
          if( dc.compare( range.getLower(), yLower ) < 0 )
            yLower = (Number)range.getLower();
          if( dc.compare( range.getUpper(), yUpper ) > 0 )
            yUpper = (Number)range.getUpper();
        }
        catch( final SensorException e )
        {
          e.printStackTrace();
        }
      }

      try
      {
        // fetch X axis range for placing possible scenario text item
        final IAxisRange range = values.getRangeFor( dateAxis );
        final Date d1 = (Date)range.getLower();
        final Date d2 = (Date)range.getUpper();

        if( xLower == null || d1.before( xLower ) )
          xLower = d1;
        if( xUpper == null || d2.after( xUpper ) )
          xUpper = d2;
      }
      catch( final SensorException e )
      {
        e.printStackTrace();
      }

      // check observation for specific scenario
      if( scenarioName == null ) // not already set in this session?
      {
        final MetadataList mdl = obs.getMetadataList();
        final String scenarioId = mdl.getProperty( ObservationConstants.MD_SCENARIO );

        if( !ScenarioUtilities.isDefaultScenario( scenarioId ) )
        {
          final IScenario scenario = KalypsoAuthPlugin.getDefault().getScenario( scenarioId );
          if( scenario != null )
            scenarioName = scenario.getName();
        }
      }

      // is this obs a forecast?
      final DateRange fr = TimeserieUtils.isForecast( obs );
      if( fr != null )
      {
        final String strDate = GRAFIK_DF.format( fr.getFrom() );
        xLines.add( new XLine( "Vorhersage Start: " + strDate, strDate ) );
      }

      // does is have Alarmstufen? only check if we are displaying at least a
      // W-axis
      try
      {
        ObservationUtilities.findAxisByType( (IAxis[])displayedAxes.toArray( new IAxis[displayedAxes.size()] ),
            TimeserieConstants.TYPE_WATERLEVEL );

        final MetadataList mdl = obs.getMetadataList();
        final String[] mds = TimeserieUtils.findOutMDAlarmLevel( obs );
        for( int j = 0; j < mds.length; j++ )
        {
          final Double value = new Double( mdl.getProperty( mds[j] ) );
          yLines.put( value, new ValueAndColor( mds[j] + " (" + GRAFIK_NF_W.format( value ) + ")", value.doubleValue(),
              null ) );
        }
      }
      catch( NoSuchElementException e )
      {
        // ignored
      }

      displayedAxes.clear();
    }

    writer.write( gKurven.toVorlagentext() );
    writer.write( "\n" );
    writer.write( "HTitel:\t" + odt.getTitle() + "\n" );
    writer.write( "xTitel:\t" + gAchsen.getBottomLabel() + "\n" );
    writer.write( "yTitel1:\t" + gAchsen.getLeftLabel() + "\n" );
    writer.write( "yTitel2:\t" + gAchsen.getRightLabel() + "\n" );

    // Scenario stuff as free text items
    if( scenarioName != null )
    {
      final double xPos = ( xUpper.getTime() - xLower.getTime() ) / 60000 / 2;
      final double yPos = ( yUpper.doubleValue() - yLower.doubleValue() ) / 2;
      writer.write( "Text1: " + xPos + " " + yPos + " 0 " + scenarioName + "\n" );
      writer.write( "TextFont6: -29 0 255 400 0 3 2 1 34 0 Arial\n" ); // the font is global for all the free text items
    }

    // constant vertical lines...
    for( Iterator it = xLines.iterator(); it.hasNext(); )
    {
      final String strDate = it.next().toString();
      writer.write( "Senkrechte: " + strDate + '\n' );
    }
    xLines.clear();

    // constant horizontal lines...
    for( final Iterator it = yLines.keySet().iterator(); it.hasNext(); )
    {
      final ValueAndColor vac = (ValueAndColor)yLines.get( it.next() );
      writer.write( "yKonst: " + GRAFIK_NF_W.format( vac.value ) + " " + vac.label + '\n' );
    }
    yLines.clear();

    return multiStatus;
  }

  /**
   * Converts a zml file to a dat file that the grafik tool can load.
   */
  private static IStatus zml2dat( final ITuppleModel values, final IFile datFile, final IAxis dateAxis,
      final IAxis axis, final IProgressMonitor monitor ) throws CoreException
  {
    final SetContentHelper sch = new SetContentHelper( ".zml --> .dat Konvertierung" )
    {
      protected void write( final OutputStreamWriter writer ) throws Throwable
      {
        for( int i = 0; i < values.getCount(); i++ )
        {
          if( monitor.isCanceled() )
            return;

          final Object elt = values.getElement( i, dateAxis );
          final String text = GRAFIK_DF.format( elt );
          writer.write( text );
          writer.write( '\t' );

          writer.write( values.getElement( i, axis ).toString() );

          writer.write( '\n' );
        }
      }
    };

    sch.setFileContents( datFile, false, false, monitor );

    return Status.OK_STATUS;
  }

  /**
   * mini helper class for storing a value and a color
   * 
   * @author schlienger
   */
  private final static class ValueAndColor
  {
    final double value;

    final Color color;

    final String label;

    public ValueAndColor( final String lbl, final double val, final Color col )
    {
      this.label = lbl;
      this.value = val;
      this.color = col;
    }
  }

  private final static class XLine implements Comparable
  {
    public final String label;
    public final String strDate;

    public XLine( final String lbl, final String strdate )
    {
      label = lbl;
      strDate = strdate;
    }

    public String toString()
    {
      return strDate;
    }

    public int compareTo( Object o )
    {
      return strDate.compareTo( ( (XLine)o ).strDate );
    }
  }
}