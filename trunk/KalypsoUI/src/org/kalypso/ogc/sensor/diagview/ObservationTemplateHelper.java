package org.kalypso.ogc.sensor.diagview;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Date;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.apache.commons.io.IOUtils;
import org.deegree_impl.gml.schema.XMLHelper;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obsdiagview.TypeCurve;
import org.kalypso.template.obsdiagview.TypeObservation;
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

  private final static org.kalypso.template.obsdiagview.ObjectFactory m_obsDiagFactory = new org.kalypso.template.obsdiagview.ObjectFactory();

  private ObservationTemplateHelper( )
  {
    // not to be instanciated
  }

  /**
   * Saves the XML using the marshaller.
   * 
   * @param tpl
   * @param out
   * @throws JAXBException
   */
  public static void saveDiagramTemplateXML( final ObsdiagviewType tpl,
      final OutputStream out ) throws JAXBException
  {
    m_obsDiagFactory.createMarshaller().marshal( tpl, out );
  }

  /**
   * Loads a LinkedDiagramTemplate from the given file.
   * 
   * @param file
   * @return diagram template object parsed from the file
   * @throws CoreException
   * @throws JAXBException
   * @throws IOException
   */
  public static ObsdiagviewType loadDiagramTemplateXML( final IFile file )
      throws CoreException, JAXBException, IOException
  {
    final InputStream ins = file.getContents();

    try
    {
      final ObsdiagviewType baseTemplate = (ObsdiagviewType) m_obsDiagFactory
          .createUnmarshaller().unmarshal( ins );

      return baseTemplate;
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
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
    final TypeObservation tobs = m_obsDiagFactory.createTypeObservation();
    tobs.setHref( lnk.getHref() );
    tobs.setLinktype( lnk.getLinktype() );

    final TypeCurve c = m_obsDiagFactory.createTypeCurve();
    c.setId( String.valueOf( new Date().getTime() ) );
    c.setName( name );

    final List mapping = c.getMapping();

    final TypeAxisMapping mpDate = m_obsDiagFactory.createTypeAxisMapping();
    mpDate.setDiagramAxis( diagDateAxis );
    mpDate.setObservationAxis( lnk.getTimeaxis() );

    final TypeAxisMapping mpValue = m_obsDiagFactory.createTypeAxisMapping();
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