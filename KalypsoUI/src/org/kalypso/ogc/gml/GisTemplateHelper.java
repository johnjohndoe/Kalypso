package org.kalypso.ogc.gml;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.StringReader;
import java.util.Properties;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.java.io.ReaderUtilities;
import org.kalypso.template.featureview.Featuretemplate;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.ObjectFactory;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.template.types.ExtentType;
import org.xml.sax.InputSource;

/**
 * Hilfsklasse, um aus den Binding-Klassen 'echte' Objekte zu erzeugen und
 * umgekehrt
 * 
 * @author Belger
 */
public class GisTemplateHelper
{
  // TODO: das sollte alles nicht statisch sein, da es hier zu threading
  // problemen kommt
  //  private static Unmarshaller GMT_UNMARSHALLER;

  //  private static Marshaller GMT_MARSHALLER;

  //  private static Unmarshaller GTT_UNMARSHALLER;

  //  private static Unmarshaller GFT_UNMARSHALLER;

  //  static
  //  {
  //    try
  //    {
  //      final ObjectFactory objectFactory = new ObjectFactory();
  //      GMT_UNMARSHALLER = objectFactory.createUnmarshaller();
  //      GMT_MARSHALLER.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE
  // );
  //      GMT_MARSHALLER = objectFactory.createMarshaller();

  //      GTT_UNMARSHALLER = new
  // org.kalypso.template.gistableview.ObjectFactory().createUnmarshaller();
  //      GMT_MARSHALLER.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE
  // );

  //      GFT_UNMARSHALLER = new
  // org.kalypso.template.featureview.ObjectFactory().createUnmarshaller();
  //      GMT_MARSHALLER.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE
  // );
  //    }
  //    catch( final JAXBException e )
  //    {
  //      e.printStackTrace();
  //    }
  //  }

  private GisTemplateHelper()
  {
  // never instantiate this class
  }

  public static final Featuretemplate loadGisFeatureTemplate( final IFile file,
      final Properties replaceProps ) throws CoreException, IOException, JAXBException
  {
    // TODO: replace with 'ReplaceToken'
    final InputStreamReader inputStreamReader = new InputStreamReader( file.getContents(), file
        .getCharset() );
    final String contents = ReaderUtilities.readAndReplace( inputStreamReader, replaceProps );

    return loadGisFeatureTemplate( new InputSource( new StringReader( contents ) ) );
  }

  public static final Featuretemplate loadGisFeatureTemplate( final InputSource is )
      throws JAXBException
  {
    Unmarshaller unmarshaller = new org.kalypso.template.featureview.ObjectFactory()
        .createUnmarshaller();
    return (Featuretemplate)unmarshaller.unmarshal( is );
  }

  public static final Gismapview loadGisMapView( final IFile file, final Properties replaceProps )
      throws CoreException, IOException, JAXBException
  {
    // TODO: replace with 'ReplaceToken'
    final InputStreamReader inputStreamReader = new InputStreamReader( file.getContents(), file
        .getCharset() );
    final String contents = ReaderUtilities.readAndReplace( inputStreamReader, replaceProps );

    return loadGisMapView( new InputSource( new StringReader( contents ) ) );
  }

  public static final Gismapview loadGisMapView( final IFile file ) throws JAXBException,
      CoreException
  {
    final InputSource is = new InputSource( file.getContents() );
    is.setEncoding( file.getCharset() );

    return loadGisMapView( is );
  }

  public static final Gismapview loadGisMapView( final InputSource is ) throws JAXBException
  {
    final ObjectFactory objectFactory = new ObjectFactory();
    Unmarshaller unmarshaller = objectFactory.createUnmarshaller();

    return (Gismapview)unmarshaller.unmarshal( is );
  }

  /**
   * Führt ein Pattern-Ersetzen durch, bevor die Gistableview geparst wird Jeder
   * key der Properties wird durch seinen value ersetzt. Funktioniert nur
   * zeilenweise, d.h.
   * 
   * @throws CoreException
   * @throws CoreException
   * @throws IOException
   * @throws JAXBException
   */
  public static Gistableview loadGisTableview( final IFile file, final Properties replaceProps )
      throws CoreException, IOException, JAXBException
  {
    final InputStreamReader inputStreamReader = new InputStreamReader( file.getContents(), file
        .getCharset() );
    final String contents = ReaderUtilities.readAndReplace( inputStreamReader, replaceProps );

    return loadGisTableview( new InputSource( new StringReader( contents ) ) );
  }

  public static Gistableview loadGisTableview( final IFile file ) throws CoreException,
      JAXBException
  {
    final InputSource is = new InputSource( file.getContents() );
    is.setEncoding( file.getCharset() );

    return loadGisTableview( is );
  }

  public static Gistableview loadGisTableview( final InputSource is ) throws JAXBException
  {
    Unmarshaller unmarshaller = new org.kalypso.template.gistableview.ObjectFactory()
        .createUnmarshaller();

    return (Gistableview)unmarshaller.unmarshal( is );
  }

  public static void saveGisMapView( final Gismapview modellTemplate, final OutputStream outStream )
      throws JAXBException
  {
    final ObjectFactory objectFactory = new ObjectFactory();
    final Marshaller marshaller = objectFactory.createMarshaller();
    marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
    marshaller.marshal( modellTemplate, outStream );
  }

  public static GM_Envelope getBoundingBox( Gismapview gisview )
  {
    final ExtentType extent = gisview.getExtent();
    return GeometryFactory.createGM_Envelope( extent.getLeft(), extent.getBottom(), extent
        .getRight(), extent.getTop() );
  }
}