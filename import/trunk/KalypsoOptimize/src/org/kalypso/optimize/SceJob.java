package org.kalypso.optimize;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Writer;
import java.net.URL;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.io.StreamUtilities;
import org.kalypso.optimizer.AutoCalibration;
import org.kalypso.optimizer.ObjectFactory;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * 
 * this class encapsulates the optimizing fortran SCE optimizing tool
 * 
 * @author doemming
 *  
 */
public class SceJob
{
  private final URL XML2SCE_URL;

  private final File m_sceExe;

  private boolean m_isCanceled = false;

  private final File m_sceTmpDir;

  private final AutoCalibration m_autoCalibration;

  public SceJob( AutoCalibration autoCalibration, File sceTmpDir )
  {
    m_autoCalibration = autoCalibration;
    m_sceTmpDir = sceTmpDir;
    XML2SCE_URL = getClass().getResource( "resource/xml2sceInput.xsl" );
    m_sceExe = prepareSCE();
  }

  public void optimize( SceIOHandler sceIO ) throws Exception
  {
    makeinputFiles();
    //    Parameter
    startSCEOptimization( sceIO );
  }

  private File prepareSCE()
  {
    final InputStream sceStream = getClass().getResourceAsStream( "resource/sce.exe_" );
    final File tmpDir = FileUtilities.createNewTempDir( "sce", m_sceTmpDir );
    final File sceExe = new File( tmpDir, "sce.exe" );
    try
    {
      StreamUtilities.streamCopy( sceStream, new FileOutputStream( sceExe ) );
    }
    catch( FileNotFoundException e )
    {
      e.printStackTrace();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    return sceExe;
  }

  /**
   * prepare SCE configuration file "scein.dat"
   */
  private void makeinputFiles() throws TransformerException, ParserConfigurationException,
      SAXException, IOException, JAXBException
  {
    //prepare scein.dat

    final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware( true );
    final DocumentBuilder docuBuilder = factory.newDocumentBuilder();
    final ObjectFactory fac = new ObjectFactory();
    final Marshaller marshaller = fac.createMarshaller();
    final Document xmlDOM = docuBuilder.newDocument();
    marshaller.marshal( m_autoCalibration, xmlDOM );

    final File outputFile = new File( m_sceExe.getParent(), "scein.dat" );
    final FileWriter writer = new FileWriter( outputFile );
    final Document xslDOM = docuBuilder.parse( XML2SCE_URL.openStream() );
    final TransformerFactory transformerFactory = TransformerFactory.newInstance();
    final Transformer transformer = transformerFactory.newTransformer( new DOMSource( xslDOM ) );
    transformer.transform( new DOMSource( xmlDOM ), new StreamResult( writer ) );
    writer.close();
  }

  private void startSCEOptimization( SceIOHandler sceIO ) throws CalcJobServiceException
  {
    InputStreamReader inStream = null;
    InputStreamReader errStream = null;

    try
    {
      final File exeDir = m_sceExe.getParentFile();
      final String commandString = m_sceExe.getAbsolutePath();

      final Process process = Runtime.getRuntime().exec( commandString, null, exeDir );

      final StringBuffer outBuffer = new StringBuffer();
      final StringBuffer errBuffer = new StringBuffer();

      Writer inputWriter = new PrintWriter( process.getOutputStream(), false );

      inStream = new InputStreamReader( process.getInputStream() );
      errStream = new InputStreamReader( process.getErrorStream() );
      while( true )
      {
        if( inStream.ready() )
        {
          char buffer[] = new char[100];
          int bufferC = inStream.read( buffer );
          outBuffer.append( buffer, 0, bufferC );
        }
        if( errStream.ready() )
        {
          char buffer[] = new char[100];
          int bufferC = errStream.read( buffer );
          errBuffer.append( buffer, 0, bufferC );
        }
        if( isCanceled() )
        {
          process.destroy();
          return;
        }
        try
        {
          process.exitValue();
          return;
        }
        catch( IllegalThreadStateException e )
        {
          // noch nicht fertig
          sceIO.handleStreams( outBuffer, errBuffer, inputWriter );
        }

        Thread.sleep( 100 );
      }
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Fehler beim Ausfuehren", e );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Fehler beim Ausfuehren", e );
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
      catch( final IOException e )
      {
        e.printStackTrace();
      }
    }
  }

  private boolean isCanceled()
  {
    return m_isCanceled;
  }

  public void cancel()
  {
    m_isCanceled = true;
  }
}