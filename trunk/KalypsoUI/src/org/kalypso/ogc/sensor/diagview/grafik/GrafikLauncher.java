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
import java.io.StringWriter;
import java.io.Writer;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Logger;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentHelper;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagViewUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
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
   * Opens the grafik tool using an observation template file. Note: this method
   * should be called using a WorkspaceModifyOperation.
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
   * Opens the grafik tool using an observation template xml object. Note: this
   * method should be called using a WorkspaceModifyOperation.
   * 
   * @param fileName
   *          the filename to use for the grafik template file
   * @param odt
   *          the xml binding object
   * @param dest
   * @param monitor
   * @return the created tpl file
   * 
   * @throws SensorException
   */
  public static IFile startGrafikODT( final String fileName,
      final ObsdiagviewType odt, final IFolder dest,
      final IProgressMonitor monitor ) throws SensorException
  {
    StringWriter strWriter = null;
    try
    {
      if( !dest.exists() )
        dest.create( true, true, monitor );

      final IFile tplFile = dest.getFile( FileUtilities
          .nameWithoutExtension( fileName )
          + ".tpl" );

      strWriter = new StringWriter();

      odt2tpl( odt, dest, strWriter, monitor );

      // redeclared final for being used in SetContentHelper
      final StringWriter schWriter = strWriter;

      // use the windows encoding for the vorlage because of the grafik tool
      // which uses it when reading...
      final SetContentHelper sch = new SetContentHelper()
      {
        protected void write( final Writer writer ) throws Throwable
        {
          writer.write( schWriter.toString() );
        }
      };

      sch.setFileContents( tplFile, false, false, new NullProgressMonitor(),
          "Cp1252" );

      startGrafikTPL( tplFile );

      return tplFile;
    }
    catch( Throwable e ) // generic exception caught
    {
      throw new SensorException( e );
    }
    finally
    {
      IOUtils.closeQuietly( strWriter );
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
      // TODO: check if version has changed an then try to delete old file
      // TODO: always use file name 'grafik.exe'
      final File grafikExe = FileUtilities
          .makeFileFromStream(
              false,
              "grafik",
              ".exe",
              DiagViewUtils.class
                  .getResourceAsStream( "/org/kalypso/ui/resources/exe/grafik.exe_" ),
              true );
      grafikExe.deleteOnExit();

      // also create the help file if not already existing
      final File grafikHelp = new File( grafikExe.getParentFile(),
          FileUtilities.nameWithoutExtension( grafikExe.getName() ) + ".hlp" );
      grafikHelp.deleteOnExit();
      if( !grafikHelp.exists() )
      {
        final File tmp = FileUtilities
            .makeFileFromStream(
                false,
                "grafik",
                ".hlp",
                DiagViewUtils.class
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
   * <p>
   * Important note: the XML-Schema for the diag template file says that if no
   * curve element or specified for a given observation, then all curves of that
   * observation should be displayed. This is not possible here using the grafik
   * tool. As a conclusion: when a template file is meant to be used with the
   * grafik tool, then curves need to be explicitely specified in the xml.
   * 
   * @param odt
   * @param dest
   * @param writer
   * @param monitor
   * 
   * @throws CoreException
   * @throws IOException
   */
  private static void odt2tpl( final ObsdiagviewType odt, final IFolder dest,
      final Writer writer, final IProgressMonitor monitor )
      throws CoreException, IOException
  {
    final UrlResolver urlRes = new UrlResolver();
    final URL context = ResourceUtilities.createURL( dest.getParent() );

    final GrafikAchsen gAchsen = new GrafikAchsen( odt.getAxis() );
    final GrafikKurven gKurven = new GrafikKurven( gAchsen );

    final Set xLines = new TreeSet();
    final Map yLines = new HashMap();

    final TypeObservation[] tobs = (TypeObservation[]) odt.getObservation()
        .toArray( new TypeObservation[0] );
    for( int i = 0; i < tobs.length; i++ )
    {
      if( monitor.isCanceled() )
        return;

      // now try to locate observation file
      final URL url = urlRes.resolveURL( context, tobs[i].getHref() );
      final IFile zmlFile = ResourceUtilities.findFileFromURL( url );

      // if file cannot be found, that probably means it is not local...
      // maybe make a better test later?
      if( zmlFile == null )
      {
        final String msg = "Konvertierung nicht möglich, Zml-Datei ist möglicherweise keine lokale Datei: "
            + url.toExternalForm();
        Logger.getLogger( GrafikLauncher.class.getName() ).warning( msg );
        continue;
      }

      final IObservation obs;
      InputStream ins = null;
      try
      {
        ins = zmlFile.getContents();
        obs = ZmlFactory.parseXML( new InputSource( ins ), zmlFile.toString(),
            context );
      }
      catch( Exception e )
      {
        e.printStackTrace();
        Logger.getLogger( GrafikLauncher.class.getName() ).throwing(
            GrafikLauncher.class.getName(), "odt2tpl", e );
        continue;
      }
      finally
      {
        IOUtils.closeQuietly( ins );
      }

      // find out which axes to use
      final IAxis[] axes = obs.getAxisList();
      final IAxis dateAxis = ObservationUtilities.findAxisByClass( axes,
          Date.class, true )[0];
      final IAxis[] numberAxes = ObservationUtilities.findAxisByClass( axes,
          Number.class, true );

      // create a corresponding dat-File for the current observation file
      final IFile datFile = dest.getFile( FileUtilities
          .nameWithoutExtension( zmlFile.getName() )
          + ".dat" );
      
      // convert to dat-file, ready to be read by the grafik tool
      zml2dat( obs, datFile, dateAxis, numberAxes, monitor );

      final List curves = tobs[i].getCurve();
      for( final Iterator itc = curves.iterator(); itc.hasNext(); )
      {
        final TypeCurve tc = (TypeCurve) itc.next();

        gKurven.addCurve( datFile.getName(), tc, numberAxes );
      }

      // is this obs a forecast?
      final DateRangeArgument fr = TimeserieUtils.isForecast( obs );
      if( fr != null )
        xLines.add( GRAFIK_DF.format( fr.getFrom() ) );

      // does is have Alarmstufen?
      final MetadataList mdl = obs.getMetadataList();
      final String[] mds = TimeserieUtils.findOutMDAlarmLevel( obs );
      for( int j = 0; j < mds.length; j++ )
      {
        final Double value = new Double( mdl.getProperty( mds[j] ) );
        yLines.put( value, new ValueAndColor( mds[j] + " ("
            + mdl.getProperty( mds[j] ) + ")", value.doubleValue(), null ) );
      }
    }

    writer.write( gKurven.toVorlagentext() );
    writer.write( "\n" );
    writer.write( "HTitel:\t" + odt.getTitle() + "\n" );
    writer.write( "xTitel:\t" + gAchsen.getBottomLabel() + "\n" );
    writer.write( "yTitel1:\t" + gAchsen.getLeftLabel() + "\n" );
    writer.write( "yTitel2:\t" + gAchsen.getRightLabel() + "\n" );

    // constant vertical lines...
    for( Iterator it = xLines.iterator(); it.hasNext(); )
    {
      final String strDate = it.next().toString();
      writer.write( "Senkrechte: " + strDate + '\n' );
    }

    // constant horizontal lines...
    for( final Iterator it = yLines.keySet().iterator(); it.hasNext(); )
    {
      final ValueAndColor vac = (ValueAndColor) yLines.get( it.next() );
      writer.write( "yKonst: " + vac.value + " " + vac.label + '\n' );
    }
  }

  /**
   * Converts a zml file to a dat file that the grafik tool can load.
   * 
   * @param obs
   * @param datFile
   * @param dateAxis
   * @param numberAxes
   * @param monitor
   * @throws CoreException
   */
  private static void zml2dat( final IObservation obs, final IFile datFile,
      final IAxis dateAxis, final IAxis[] numberAxes,
      final IProgressMonitor monitor ) throws CoreException
  {
    final SetContentHelper sch = new SetContentHelper()
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

            writer.write( values.getElement( i, axis ).toString() );
            writer.write( '\t' );
          }

          writer.write( '\n' );
        }
      }
    };

    sch.setFileContents( datFile, false, false, monitor );
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
}