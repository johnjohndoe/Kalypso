package org.kalypso.ogc.sensor.template;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Iterator;
import java.util.Properties;

import javax.xml.bind.JAXBException;

import org.deegree_impl.gml.schema.XMLHelper;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.template.LinkedDiagramCurve;
import org.kalypso.ogc.sensor.diagview.template.LinkedDiagramTemplate;
import org.kalypso.ogc.sensor.tableview.template.LinkedTableViewTemplate;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.ui.navigator.GrafikViewActionDelegate;
import org.kalypso.util.xml.xlink.JAXBXLink;

/**
 * @author schlienger
 */
public class ObservationTemplateHelper
{
  public final static String ODT_FILE_EXTENSION = "odt";

  public final static String TPL_FILE_EXTENSION = "tpl";

  private final static org.kalypso.template.obsdiagview.ObjectFactory m_obsDiagFactory = new org.kalypso.template.obsdiagview.ObjectFactory();

  private final static org.kalypso.template.obstableview.ObjectFactory m_obsTableFactory = new org.kalypso.template.obstableview.ObjectFactory();

  private ObservationTemplateHelper()
  {
  // not to be instanciated
  }

  /**
   * Loads the binded template.
   * @param file
   * @return XML-binded object
   * @throws CoreException
   * @throws JAXBException
   * @throws IOException
   */
  public static ObsdiagviewType loadDiagramTemplateXML( final IFile file ) throws CoreException,
      JAXBException, IOException
  {
    final InputStream ins = file.getContents();
    final ObsdiagviewType baseTemplate = (ObsdiagviewType)m_obsDiagFactory.createUnmarshaller()
        .unmarshal( ins );
    ins.close();

    return baseTemplate;
  }

  /**
   * Loads a LinkedDiagramTemplate from the given file.
   * @param file
   * @return diagram template object parsed from the file
   * @throws CoreException
   * @throws JAXBException
   * @throws IOException
   */
  public static IDiagramTemplate loadDiagramTemplate( final IFile file ) throws CoreException,
      JAXBException, IOException
  {
    return new LinkedDiagramTemplate( loadDiagramTemplateXML( file ), ResourceUtilities.createURL( file ) );
  }

  /**
   * Creates a LinkedDiagramCurve for the given binding object.
   * @param baseCurve
   * @param template
   * @param context
   * @return curve for the template
   */
  public static LinkedDiagramCurve createCurve( final ObsdiagviewType.CurveType baseCurve,
      final IDiagramTemplate template, final URL context )
  {
    Properties mappings = new Properties();

    for( Iterator it = baseCurve.getMapping().iterator(); it.hasNext(); )
    {
      TypeAxisMapping am = (TypeAxisMapping)it.next();

      mappings.setProperty( am.getObservationAxis(), am.getDiagramAxis() );
    }

    return new LinkedDiagramCurve( baseCurve.getLinktype(), new JAXBXLink( baseCurve ), baseCurve
        .getName(), mappings, template, context );
  }

  /**
   * @param file
   * @return table view template
   * @throws CoreException
   * @throws JAXBException
   * @throws IOException
   */
  public static LinkedTableViewTemplate loadTableViewTemplate( final IFile file )
      throws CoreException, JAXBException, IOException
  {
    final InputStream ins = file.getContents();
    final ObstableviewType baseTemplate = (ObstableviewType)m_obsTableFactory.createUnmarshaller()
        .unmarshal( ins );
    ins.close();

    return new LinkedTableViewTemplate( baseTemplate, ResourceUtilities.createURL( file ) );
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
  public static void openGrafik4odt( final IFile odtFile ) throws SensorException
  {
    openGrafik4odt( odtFile.getLocation().toFile(), odtFile.getProject() );
  }

  /**
   * Opens the grafik tool using an observation template file.
   * @param file
   * @param project
   * 
   * @throws SensorException if problems occur during operation.
   */
  public static void openGrafik4odt( final File file, final IProject project ) throws SensorException
  {
    // TODO: bessere konfigurierbarkeit von der Transformation?
    final InputStream xsl = ObservationTemplateHelper.class
        .getResourceAsStream( "/org/kalypso/ui/resources/xsl/grafik-vorlage.xsl" );

    FileWriter fw = null;
    InputStream ins = null;
    
    try
    {
      final File grafikExe = FileUtilities.makeFileFromStream( false, "grafik", ".exe",
          GrafikViewActionDelegate.class
              .getResourceAsStream( "/org/kalypso/ui/resources/exe/grafik.exe_" ), true );

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

      Runtime.getRuntime().exec( grafikExe.getAbsolutePath() + " /V" + tmp.getAbsolutePath() );
      
      // delete the tmp vorlage right away
      tmp.deleteOnExit();
    }
    catch( Exception e ) // generic exception caught because of XMLHelper.xslTransform
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
  public static void openGrafik4tpl( final IFile tplFile ) throws SensorException
  {
    try
    {
      final File grafikExe = FileUtilities.makeFileFromStream( false, "grafik", ".exe",
          ObservationTemplateHelper.class
              .getResourceAsStream( "/org/kalypso/plugin/resources/exe/grafik.exe_" ), true );

      /*
       * Blöder Workaround weil das Grafik-Tool nicht mit relativen Pfad
       * arbeiten kann: wir ersetzen _XXXX_ aus der .tpl Datei mit dem aktuellen
       * Projektpfad.
       * 
       * Deswegen: wenn _XXXX_ als Pfadteil in der Vorlage benutzt wird, sollte
       * es sich auf dem Projekt beziehen.
       */
      final BufferedReader reader = new BufferedReader( new FileReader( tplFile.getLocation()
          .toFile() ) );

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

      Runtime.getRuntime().exec( grafikExe.getAbsolutePath() + " /V" + file.getAbsolutePath() );
    }
    catch( IOException e )
    {
      throw new SensorException( e );
    }
  }
}