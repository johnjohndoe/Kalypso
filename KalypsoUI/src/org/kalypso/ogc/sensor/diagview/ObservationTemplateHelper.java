package org.kalypso.ogc.sensor.diagview;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.IOUtils;
import org.deegree_impl.gml.schema.XMLHelper;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlObservation;
import org.kalypso.template.obsdiagview.ObjectFactory;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obsdiagview.TypeAxis;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obsdiagview.TypeCurve;
import org.kalypso.template.obsdiagview.TypeObservation;
import org.kalypso.template.obsdiagview.ObsdiagviewType.LegendType;
import org.kalypso.ui.navigator.GrafikViewActionDelegate;
import org.kalypso.zml.obslink.TimeseriesLink;

/**
 * Helper Class.
 * 
 * @author schlienger
 */
public class ObservationTemplateHelper
{
  public final static String ODT_FILE_EXTENSION = "odt";

  public final static String TPL_FILE_EXTENSION = "tpl";

  private final static ObjectFactory OF = new ObjectFactory();

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
      final Marshaller m = OF.createMarshaller();
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
      final Marshaller m = OF.createMarshaller();
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
      final ObsdiagviewType baseTemplate = (ObsdiagviewType) OF
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
    final ObsdiagviewType bdgTemplate = OF.createObsdiagview();

    final LegendType bdgLegend = OF.createObsdiagviewTypeLegendType();
    bdgLegend.setTitle( template.getLegendName() );
    bdgLegend.setVisible( template.isShowLegend() );

    bdgTemplate.setLegend( bdgLegend );
    bdgTemplate.setTitle( template.getTitle() );

    final List bdgAxes = bdgTemplate.getAxis();
    final Iterator itAxes = template.getDiagramAxes().iterator();
    while( itAxes.hasNext() )
    {
      final IDiagramAxis axis = (IDiagramAxis) itAxes.next();

      final TypeAxis bdgAxis = OF.createTypeAxis();
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

      final TypeObservation bdgTheme = OF.createTypeObservation();
      bdgTheme.setLinktype( "zml" );
      bdgTheme.setHref( ((ZmlObservation) obs).getHref() );

      final List bdgCurves = bdgTheme.getCurve();

      final Iterator itCurves = theme.getCurves().iterator();
      while( itCurves.hasNext() )
      {
        final IDiagramCurve curve = (IDiagramCurve) itCurves.next();

        final TypeCurve bdgCurve = OF.createTypeCurve();
        bdgCurve.setId( "C" + ixCurve++ );
        bdgCurve.setName( curve.getName() );

        final List bdgMappings = bdgCurve.getMapping();

        final IAxisMapping[] mappings = curve.getMappings();
        for( int i = 0; i < mappings.length; i++ )
        {
          final TypeAxisMapping bdgMapping = OF.createTypeAxisMapping();
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
    final TypeObservation tobs = OF.createTypeObservation();
    tobs.setHref( lnk.getHref() );
    tobs.setLinktype( lnk.getLinktype() );

    final TypeCurve c = OF.createTypeCurve();
    c.setId( String.valueOf( new Date().getTime() ) );
    c.setName( name );

    final List mapping = c.getMapping();

    final TypeAxisMapping mpDate = OF.createTypeAxisMapping();
    mpDate.setDiagramAxis( diagDateAxis );
    mpDate.setObservationAxis( lnk.getTimeaxis() );

    final TypeAxisMapping mpValue = OF.createTypeAxisMapping();
    mpValue.setDiagramAxis( diagValueAxis );
    mpValue.setObservationAxis( lnk.getValueaxis() );

    mapping.add( mpDate );
    mapping.add( mpValue );

    final List curves = tobs.getCurve();
    curves.add( c );

    final List list = tpl.getObservation();
    list.add( tobs );
  }

  /**
   * Starts the grafik.exe on the given diagram template file. It also firsts
   * converts the diagram template (.odt) to a grafik template (.tpl).
   * 
   * @param odtFile
   *          the diagram template file.
   * 
   * @throws SensorException
   */
  public static void openGrafik4odt( final IFile odtFile )
      throws SensorException
  {
    openGrafik4odt( odtFile.getLocation().toFile(), odtFile.getProject() );
  }

  /**
   * Opens the grafik tool using an observation template file.
   * 
   * @param file
   * @param project
   * 
   * @throws SensorException
   *           if problems occur during operation.
   */
  public static void openGrafik4odt( final File file, final IProject project )
      throws SensorException
  {
    // TODO: bessere konfigurierbarkeit von der Transformation?
    final InputStream xsl = ObservationTemplateHelper.class
        .getResourceAsStream( "/org/kalypso/ui/resources/xsl/grafik-vorlage.xsl" );

    FileWriter fw = null;
    InputStream ins = null;

    try
    {
      final File grafikExe = FileUtilities
          .makeFileFromStream(
              false,
              "grafik",
              ".exe",
              GrafikViewActionDelegate.class
                  .getResourceAsStream( "/org/kalypso/ui/resources/exe/grafik.exe_" ),
              true );

      // get the file where project resides in order to complete the relative
      // path
      final String projectDir = project.getLocation().toString();

      ins = new FileInputStream( file );
      final String str = XMLHelper.xslTransform( ins, xsl );

      // complete relative path (prepared by the xslt, all relative path are
      // preceded by _XXXX_)
      final String strOk = str.replaceAll( "_XXXX_", projectDir );

      // create the template file for the grafik tool
      final File tmp = File.createTempFile( "grafik", ".tpl" );

      fw = new FileWriter( tmp );
      fw.write( strOk );

      Runtime.getRuntime().exec(
          grafikExe.getAbsolutePath() + " /V" + tmp.getAbsolutePath() );

      // delete the tmp vorlage right away
      tmp.deleteOnExit();
    }
    catch( Exception e ) // generic exception caught because of
    // XMLHelper.xslTransform
    {
      throw new SensorException( e );
    }
    finally
    {
      try
      {
        if( xsl != null )
          xsl.close();

        if( fw != null )
          fw.close();

        if( ins != null )
          ins.close();
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
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

      /*
       * Blöder Workaround weil das Grafik-Tool nicht mit relativen Pfad
       * arbeiten kann: wir ersetzen _XXXX_ aus der .tpl Datei mit dem aktuellen
       * Projektpfad.
       * 
       * Deswegen: wenn _XXXX_ als Pfadteil in der Vorlage benutzt wird, sollte
       * es sich auf dem Projekt beziehen.
       */
      final BufferedReader reader = new BufferedReader( new FileReader( tplFile
          .getLocation().toFile() ) );

      final StringBuffer buffer = new StringBuffer();
      String line = reader.readLine();
      while( line != null )
      {
        buffer.append( line ).append( '\n' );
        line = reader.readLine();
      }

      reader.close();

      final String string = buffer.toString().replaceAll( "_XXXX_",
          tplFile.getProject().getLocation().toString() );

      final File file = File.createTempFile( "vorlage", ".tpl" );
      final FileWriter writer = new FileWriter( file );

      writer.write( string );

      writer.close();

      Runtime.getRuntime().exec(
          grafikExe.getAbsolutePath() + " /V" + file.getAbsolutePath() );
    }
    catch( IOException e )
    {
      throw new SensorException( e );
    }
  }
}