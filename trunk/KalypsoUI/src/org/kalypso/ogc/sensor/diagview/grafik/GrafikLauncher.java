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
import org.kalypso.eclipse.util.SetContentHelper;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.util.StringUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagViewUtils;
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
   * Opens the grafik tool using an observation template xml object. Note: this method
   * should be called using a WorkspaceModifyOperation.
   * 
   * @param fileName the filename to use for the grafik template file
   * @param odt the xml binding object
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
    try
    {
      final IFile tplFile = dest.getFile( FileUtilities
          .nameWithoutExtension( fileName )
          + ".tpl" );

      final StringWriter strWriter = new StringWriter();

      odt2tpl( odt, dest, strWriter, monitor );

      // use the windows encoding for the vorlage because of the grafik tool
      // which uses it when reading...
      final SetContentHelper sch = new SetContentHelper()
      {
        protected void write( final Writer writer ) throws Throwable
        {
          writer.write( strWriter.toString() );
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
   * 
   * @param odt
   * @param dest
   * @param writer
   * @param monitor
   * @throws SensorException
   */
  private static void odt2tpl( final ObsdiagviewType odt, final IFolder dest,
      final Writer writer, final IProgressMonitor monitor )
      throws SensorException
  {
    InputStream ins = null;

    try
    {
      final UrlResolver urlRes = new UrlResolver();
      final URL context = ResourceUtilities.createURL( dest );

      final GrafikYAchsen yAchsen = new GrafikYAchsen( odt.getAxis() );
      String dateAxisLabel = "Datum";
      String colorSpec = "KNr:  Farbe\tLTyp\tLBreite\tPTyp\n";

      final List xLines = new ArrayList();
      final Map yLines = new HashMap();

      int ixObs = 1;

      final List tobsList = odt.getObservation();
      for( final Iterator ito = tobsList.iterator(); ito.hasNext(); )
      {
        if( monitor.isCanceled() )
          return;

        final TypeObservation tobs = (TypeObservation) ito.next();

        // maps obs axis to diag axis. Can be empty if there are no
        // curves specified in the xml. In that case, all axes are
        // taken ( see zml2dat() )
        final Map obsAxis2Diag = new HashMap();
        final Map obsAxis2Color = new HashMap();

        final List tcurveList = tobs.getCurve();
        for( Iterator itc = tcurveList.iterator(); itc.hasNext(); )
        {
          final TypeCurve tc = (TypeCurve) itc.next();

          tc.getColor();
          
          final List tmList = tc.getMapping();
          for( Iterator itm = tmList.iterator(); itm.hasNext(); )
          {
            final TypeAxisMapping tm = (TypeAxisMapping) itm.next();

            obsAxis2Diag.put( tm.getObservationAxis(), tm.getDiagramAxis() );
            obsAxis2Color.put( tm.getObservationAxis(), tc.getColor() );
          }
        }

        final URL url = urlRes.resolveURL( context, tobs.getHref() );
        final IFile zmlFile = ResourceUtilities.findFileFromURL( url );

        // if file cannot be found, that probably means it is not local...
        // maybe make a better test later?
        if( zmlFile == null )
        {
          final String msg = "Konvertierung nicht m�glich, Zml-Datei ist keine lokale Datei: "
              + url.toExternalForm();
          Logger.getLogger( DiagViewUtils.class.getName() ).warning( msg );
          continue;
        }

        ins = zmlFile.getContents();
        final IObservation obs = ZmlFactory.parseXML( new InputSource( ins ),
            zmlFile.toString(), context );

        // find out which axes to use
        final IAxis[] axes = obs.getAxisList();
        final IAxis dateAxis = ObservationUtilities.findAxisByClass( axes,
            Date.class, true )[0];
        final IAxis[] numberAxes = ObservationUtilities.findAxisByClass( axes,
            Number.class, true );

        // remove date axis from names list, we always take it
        dateAxisLabel = dateAxis.getName();
        obsAxis2Diag.remove( dateAxisLabel );

        final IFile datFile = dest.getFile( FileUtilities
            .nameWithoutExtension( zmlFile.getName() )
            + ".dat" );
        zml2dat( obs, datFile, dateAxis, numberAxes, obsAxis2Diag, monitor );

        // adapt grafik-axis type according to real axis type (mapping)
        final String grafikType = GrafikYAchsen.axis2grafikType( numberAxes[0]
            .getType() );

        String title = zmlFile.getName()
            + (tobs.getTitle() != null ? tobs.getTitle() : "");
        
        String grafikAxis = "1";
        final GrafikAchse achse = yAchsen.getFor( (String) obsAxis2Diag
            .get( numberAxes[0].getName() ) );
        if( achse != null )
        {
          grafikAxis = String.valueOf( achse.getId() );
          title = achse.getName() + " (" + title + ")";
        }

        final String strColor = (String) obsAxis2Color.get( numberAxes[0].getName() );
        if( strColor != null )
          colorSpec += "K" + grafikAxis + ":\t" + toGrafikColor( strColor ) + "\t0\t1\t" + grafikAxis + "\n";
        
        writer.write( ixObs++ + "- " + datFile.getName() + " J " + grafikType
            + " " + grafikAxis + " " + title + "\n" );

        // is this obs a forecast?
        final DateRangeArgument fr = TimeserieUtils.isForecast( obs );
        if( fr != null )
          xLines.add( GRAFIK_DF.format( fr.getFrom() ) );

        // does is have Alarmstufen?
        final MetadataList mdl = obs.getMetadataList();
        final String[] mds = TimeserieUtils.findOutMDAlarmLevel( obs );
        for( int i = 0; i < mds.length; i++ )
        {
          final Double value = new Double( mdl.getProperty( mds[i] ) );
          yLines.put( value, new ValueAndColor( mds[i] + " ("
              + mdl.getProperty( mds[i] ) + ")", value.doubleValue(), null ) );
        }
      }

      writer.write( "\n" );
      writer.write( "HTitel:\t" + odt.getTitle() + "\n" );
      writer.write( "xTitel:\t" + dateAxisLabel + "\n" );
      writer.write( "yTitel1:\t" + yAchsen.getLeftLabel() + "\n" );
      writer.write( "yTitel2:\t" + yAchsen.getRightLabel() + "\n" );
      writer.write( colorSpec );

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
   * @throws CoreException
   */
  private static void zml2dat( final IObservation obs, final IFile datFile,
      final IAxis dateAxis, final IAxis[] numberAxes, final Map axisNames,
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

            // either there are no names or the names are specified and the
            // current one is one of them
            if( axisNames.size() == 0 || axisNames.containsKey( axis.getName() ) )
            {
              writer.write( values.getElement( i, axis ).toString() );
              writer.write( '\t' );
            }
          }

          writer.write( '\n' );
        }
      }
    };

    sch.setFileContents( datFile, false, false, monitor );
  }
  
  /**
   * Converts the string representation of the color into an integer as used in the grafik template using
   * the getRGB() method of the color class.
   * 
   * @param strColor
   * @return integer representation
   */
  private static String toGrafikColor( final String strColor )
  {
    // TODO: J�rg fragen warum wir die Rot/Blau Komponente tauschen m�ssen
    // damit die Farben im Grafik richtig sind...
    Color c = StringUtilities.stringToColor( strColor );
    c = new Color( c.getBlue(), c.getGreen(), c.getRed() );
    
    // resets the alpha bits since there's no support for it in the grafik tool
    return Integer.toString( c.getRGB() & 0x00ffffff );
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