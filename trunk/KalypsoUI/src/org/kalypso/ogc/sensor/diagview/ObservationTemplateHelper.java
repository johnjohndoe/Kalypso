package org.kalypso.ogc.sensor.diagview;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Writer;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentThread;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlObservation;
import org.kalypso.template.obsdiagview.ObjectFactory;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obsdiagview.TypeAxis;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obsdiagview.TypeCurve;
import org.kalypso.template.obsdiagview.TypeObservation;
import org.kalypso.template.obsdiagview.ObsdiagviewType.LegendType;
import org.kalypso.util.url.UrlResolver;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.xml.sax.InputSource;

/**
 * Helper Class.
 * 
 * @author schlienger
 */
public class ObservationTemplateHelper
{
  public final static String ODT_FILE_EXTENSION = "odt";

  public final static String TPL_FILE_EXTENSION = "tpl";

  private final static ObjectFactory ODT_OF = new ObjectFactory();

  private final static org.kalypso.zml.ObjectFactory ZML_OF = new org.kalypso.zml.ObjectFactory();

  protected final static DateFormat GRAFIK_DF = new SimpleDateFormat(
      "dd.MM.yyyy HH:mm:ss" );

  private ObservationTemplateHelper( )
  {
    // not to be instanciated
  }

  /**
   * Saves the given template (binding). Closes the stream.
   * 
   * @param tpl
   * @param out
   * @throws JAXBException
   */
  public static void saveDiagramTemplateXML( final ObsdiagviewType tpl,
      final OutputStream out ) throws JAXBException
  {
    try
    {
      final Marshaller m = ODT_OF.createMarshaller();
      m.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      m.marshal( tpl, out );
    }
    finally
    {
      IOUtils.closeQuietly( out );
    }
  }

  /**
   * Saves the given template (binding). Closes the writer.
   * 
   * @param tpl
   * @param writer
   * @throws JAXBException
   */
  public static void saveDiagramTemplateXML( final ObsdiagviewType tpl,
      final Writer writer ) throws JAXBException
  {
    try
    {
      final Marshaller m = ODT_OF.createMarshaller();
      m.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      m.marshal( tpl, writer );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  /**
   * Loads a binding template. Closes the stream.
   * 
   * @param ins
   * @return diagram template object parsed from the file
   * @throws CoreException
   * @throws JAXBException
   */
  public static ObsdiagviewType loadDiagramTemplateXML( final InputStream ins )
      throws CoreException, JAXBException
  {
    try
    {
      final ObsdiagviewType baseTemplate = (ObsdiagviewType) ODT_OF
          .createUnmarshaller().unmarshal( ins );

      return baseTemplate;
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }

  /**
   * Builds the template binding type using the given template.
   * 
   * @param template
   * @return binding type, ready for marshalling
   * @throws JAXBException
   */
  public static ObsdiagviewType buildDiagramTemplateXML(
      final IDiagramTemplate template ) throws JAXBException
  {
    final ObsdiagviewType bdgTemplate = ODT_OF.createObsdiagview();

    final LegendType bdgLegend = ODT_OF.createObsdiagviewTypeLegendType();
    bdgLegend.setTitle( template.getLegendName() );
    bdgLegend.setVisible( template.isShowLegend() );

    bdgTemplate.setLegend( bdgLegend );
    bdgTemplate.setTitle( template.getTitle() );

    final List bdgAxes = bdgTemplate.getAxis();
    final Iterator itAxes = template.getDiagramAxes().iterator();
    while( itAxes.hasNext() )
    {
      final IDiagramAxis axis = (IDiagramAxis) itAxes.next();

      final TypeAxis bdgAxis = ODT_OF.createTypeAxis();
      bdgAxis.setDatatype( axis.getDataType() );
      bdgAxis.setDirection( axis.getDirection() );
      bdgAxis.setId( axis.getIdentifier() );
      bdgAxis.setInverted( axis.isInverted() );
      bdgAxis.setLabel( axis.getLabel() );
      bdgAxis.setPosition( axis.getPosition() );
      bdgAxis.setUnit( axis.getUnit() );

      bdgAxes.add( bdgAxis );
    }

    int ixCurve = 1;

    final List bdgThemes = bdgTemplate.getObservation();
    final Iterator itThemes = template.getThemes().iterator();
    while( itThemes.hasNext() )
    {
      final IDiagramTemplateTheme theme = (IDiagramTemplateTheme) itThemes
          .next();

      // can only deal with ZML observations
      final IObservation obs = theme.getObservation();
      if( !(obs instanceof ZmlObservation) )
        continue;

      final TypeObservation bdgTheme = ODT_OF.createTypeObservation();
      bdgTheme.setLinktype( "zml" );
      bdgTheme.setHref( ((ZmlObservation) obs).getHref() );

      final List bdgCurves = bdgTheme.getCurve();

      final Iterator itCurves = theme.getCurves().iterator();
      while( itCurves.hasNext() )
      {
        final IDiagramCurve curve = (IDiagramCurve) itCurves.next();

        final TypeCurve bdgCurve = ODT_OF.createTypeCurve();
        bdgCurve.setId( "C" + ixCurve++ );
        bdgCurve.setName( curve.getName() );

        final List bdgMappings = bdgCurve.getMapping();

        final IAxisMapping[] mappings = curve.getMappings();
        for( int i = 0; i < mappings.length; i++ )
        {
          final TypeAxisMapping bdgMapping = ODT_OF.createTypeAxisMapping();
          bdgMapping.setDiagramAxis( mappings[i].getDiagramAxis()
              .getIdentifier() );
          bdgMapping.setObservationAxis( mappings[i].getObservationAxis()
              .getName() );

          bdgMappings.add( bdgMapping );
        }

        bdgCurves.add( bdgCurve );
      }

      bdgThemes.add( bdgTheme );
    }

    return bdgTemplate;
  }

  /**
   * TODO: doc
   * 
   * @param tpl
   * @param lnk
   * @param name
   * @param diagDateAxis
   * @param diagValueAxis
   * @throws JAXBException
   */
  public static void addTimeseriesLink( final ObsdiagviewType tpl,
      final TimeseriesLink lnk, final String name, final String diagDateAxis,
      final String diagValueAxis ) throws JAXBException
  {
    final TypeObservation tobs = ODT_OF.createTypeObservation();
    tobs.setHref( lnk.getHref() );
    tobs.setLinktype( lnk.getLinktype() );

    final TypeCurve c = ODT_OF.createTypeCurve();
    c.setId( String.valueOf( new Date().getTime() ) );
    c.setName( name );

    final List mapping = c.getMapping();

    final TypeAxisMapping mpDate = ODT_OF.createTypeAxisMapping();
    mpDate.setDiagramAxis( diagDateAxis );
    mpDate.setObservationAxis( lnk.getTimeaxis() );

    final TypeAxisMapping mpValue = ODT_OF.createTypeAxisMapping();
    mpValue.setDiagramAxis( diagValueAxis );
    mpValue.setObservationAxis( lnk.getValueaxis() );

    mapping.add( mpDate );
    mapping.add( mpValue );

    final List curves = tobs.getCurve();
    curves.add( c );

    final List list = tpl.getObservation();
    list.add( tobs );
  }

  //  /**
  //   * Starts the grafik.exe on the given diagram template file. It also firsts
  //   * converts the diagram template (.odt) to a grafik template (.tpl).
  //   *
  //   * @param odtFile
  //   * the diagram template file.
  //   *
  //   * @throws SensorException
  //   */
  //  public static void openGrafik4odt( final IFile odtFile )
  //      throws SensorException
  //  {
  //    openGrafik4odt( odtFile.getLocation().toFile(), odtFile.getProject() );
  //  }

  /**
   * Opens the grafik tool using an observation template file.
   * 
   * @param odtFile
   * @param dest
   * @return the created tpl file
   * 
   * @throws SensorException
   */
  public static IFile openGrafik4odt( final IFile odtFile, final IFolder dest )
      throws SensorException
  {
    try
    {
      final IFile tplFile = dest.getFile( FileUtilities.nameWithoutExtension( odtFile.getName() ) + ".tpl" );

      SetContentThread thread = new SetContentThread( tplFile, true, false,
          false, new NullProgressMonitor() )
      {
        protected void write( final Writer writer ) throws Throwable
        {
          odt2tpl( odtFile, dest, writer );
        }
      };

      thread.start();
      thread.join();
      
      if( thread.getFileException() != null )
        throw thread.getFileException();
      
      if( thread.getThrown() != null )
        throw thread.getThrown();

      final File grafikExe = FileUtilities
          .makeFileFromStream(
              false,
              "grafik",
              ".exe",
              ObservationTemplateHelper.class
                  .getResourceAsStream( "/org/kalypso/ui/resources/exe/grafik.exe_" ),
              true );

      final File file = ResourceUtilities.makeFileFromPath( tplFile.getFullPath() );
      
      Runtime.getRuntime().exec(
          grafikExe.getAbsolutePath() + " /V"
              + file.getAbsolutePath() );
      
      return tplFile;
    }
    catch( Throwable e ) // generic exception caught
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
   * @throws SensorException
   */
  public static void odt2tpl( final IFile odtFile, final IFolder dest,
      final Writer writer ) throws SensorException
  {
    try
    {
      final ObsdiagviewType odt = loadDiagramTemplateXML( odtFile.getContents() );

      final UrlResolver urlRes = new UrlResolver();
      final URL context = ResourceUtilities.createURL( odtFile );

      int ixObs = 1;

      final List tobsList = odt.getObservation();
      for( final Iterator ito = tobsList.iterator(); ito.hasNext(); )
      {
        final TypeObservation tobs = (TypeObservation) ito.next();

        final Set axisNames = new TreeSet();

        final List tcurveList = tobs.getCurve();
        for( Iterator itc = tcurveList.iterator(); itc.hasNext(); )
        {
          final TypeCurve tc = (TypeCurve) itc.next();

          final List tmList = tc.getMapping();
          for( Iterator itm = tmList.iterator(); itm.hasNext(); )
          {
            final TypeAxisMapping tm = (TypeAxisMapping) itm.next();

            axisNames.add( tm.getObservationAxis() );
          }
        }

        final URL url = urlRes.resolveURL( context, tobs.getHref() );
        final IFile zmlFile = ResourceUtilities.findFileFromURL( url );

        final IFile datFile = dest.getFile( FileUtilities.nameWithoutExtension( zmlFile.getName() ) + ".dat" );
        zml2dat( zmlFile, datFile, axisNames, context );

        // TODO: adapt grafik-axis type according to real axis type (mapping)
        final String grafikAxis = "L";
        final String title = zmlFile.getName() + tobs.getTitle() != null ? tobs.getTitle() : "";
        
        writer.write( ixObs++ + "- " + datFile.getName() + " J " + grafikAxis
            + " " + title + "\n" );
      }

      writer.write( "\n" );
      writer.write( "HTitel:\t" + odt.getTitle() + "\n" );
      writer.write( "xTitel:\t" + "TODO" + "\n" );
      writer.write( "yTitel1:\t" + "TODO" + "\n" );
      writer.write( "yTitel2:\t" + "TODO" + "\n" );
    }
    catch( Exception e )
    {
      throw new SensorException( e );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  /**
   * Converts a zml file to a dat file that the grafik tool can load.
   * 
   * @param zmlFile
   * @param datFile
   * @param axisNames
   * @param context
   * @throws SensorException
   */
  private static void zml2dat( final IFile zmlFile, final IFile datFile,
      final Set axisNames, final URL context ) throws SensorException
  {
    InputStream ins = null;
    try
    {
      ins = zmlFile.getContents();
      final IObservation obs = ZmlFactory.parseXML( new InputSource( ins ),
          zmlFile.toString(), context );

      final SetContentThread thread = new SetContentThread( datFile, true,
          true, false, new NullProgressMonitor() )
      {
        protected void write( final Writer writer ) throws Throwable
        {
          final IAxis[] axes = obs.getAxisList();
          final IAxis dateAxis = ObservationUtilities.findAxisByClass( axes,
              Date.class )[0];
          final IAxis[] numberAxes = ObservationUtilities.findAxisByClass(
              axes, Number.class );

          // remove date axis from names list, we always take it
          axisNames.remove( dateAxis.getName() );

          final ITuppleModel values = obs.getValues( null );
          for( int i = 0; i < values.getCount(); i++ )
          {
            writer.write( GRAFIK_DF.format( values.getElement( i, dateAxis ) ) );
            writer.write( '\t' );
            
            for( int j = 0; j < numberAxes.length; j++ )
            {
              final IAxis axis = numberAxes[j];

              if( axisNames.contains( axis.getName() ) )
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
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }

  /**
   * Starts the grafik.exe on the given grafik-template file.
   * 
   * @param tplFile
   *          the Grafik-Vorlage
   * 
   * @throws SensorException
   */
  public static void openGrafik4tpl( final IFile tplFile )
      throws SensorException
  {
    try
    {
      final File grafikExe = FileUtilities
          .makeFileFromStream(
              false,
              "grafik",
              ".exe",
              ObservationTemplateHelper.class
                  .getResourceAsStream( "/org/kalypso/plugin/resources/exe/grafik.exe_" ),
              true );

//      /*
//       * Blöder Workaround weil das Grafik-Tool nicht mit relativen Pfad
//       * arbeiten kann: wir ersetzen _XXXX_ aus der .tpl Datei mit dem aktuellen
//       * Projektpfad.
//       * 
//       * Deswegen: wenn _XXXX_ als Pfadteil in der Vorlage benutzt wird, sollte
//       * es sich auf dem Projekt beziehen.
//       */
//      final BufferedReader reader = new BufferedReader( new FileReader( tplFile
//          .getLocation().toFile() ) );
//
//      final StringBuffer buffer = new StringBuffer();
//      String line = reader.readLine();
//      while( line != null )
//      {
//        buffer.append( line ).append( '\n' );
//        line = reader.readLine();
//      }
//
//      reader.close();
//
//      final String string = buffer.toString().replaceAll( "_XXXX_",
//          tplFile.getProject().getLocation().toString() );
//
//      final File file = File.createTempFile( "vorlage", ".tpl" );
//      final FileWriter writer = new FileWriter( file );
//
//      writer.write( string );
//
//      writer.close();

      Runtime.getRuntime().exec(
          grafikExe.getAbsolutePath() + " /V" + tplFile.getFullPath().toOSString() ); //.getAbsolutePath() );
    }
    catch( IOException e )
    {
      throw new SensorException( e );
    }
  }
}