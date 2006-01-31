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

import javax.xml.bind.JAXBContext;
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

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.lang.ProcessHelper.ProcessControlThread;
import org.kalypso.commons.java.lang.ProcessHelper.ProcessTimeoutException;
import org.kalypso.contribs.java.io.StreamUtilities;
import org.kalypso.optimizer.AutoCalibration;
import org.kalypso.optimizer.ObjectFactory;
import org.kalypso.services.calculation.job.ICalcMonitor;
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

  private final File m_sceTmpDir;

  private final AutoCalibration m_autoCalibration;

  public SceJob( AutoCalibration autoCalibration, File sceTmpDir )
  {
    m_autoCalibration = autoCalibration;
    m_sceTmpDir = sceTmpDir;
    XML2SCE_URL = getClass().getResource( "resource/xml2sceInput.xsl" );
    m_sceExe = prepareSCE();
  }

  public void optimize( final SceIOHandler sceIO, final ICalcMonitor monitor ) throws Exception
  {
    makeinputFiles();
    //    Parameter
    startSCEOptimization( sceIO, monitor );
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
  private void makeinputFiles() throws TransformerException, ParserConfigurationException, SAXException, IOException,
      JAXBException
  {
    //prepare scein.dat

    final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware( true );
    final DocumentBuilder docuBuilder = factory.newDocumentBuilder();
    
//    final Marshaller marshaller = fac.createMarshaller();
    // TODO: @Andreas: die nächsten beiden Zeilen ersetzen die vorhergehende
    // teste mal, obs immer noch klappt. In Zukunft sollten die Marshaller und Unmarshaller immer so erzeugt
    //  werden, denn ObjectFactory leitet anscheinend nicht immer automatisch von JAXBContext ab (wie hier nach der Umstellung auf jwsdp-2.0)
    final JAXBContext context = JAXBContext.newInstance(ObjectFactory.class);
    final Marshaller marshaller = context.createMarshaller();
    //marshaller.setProperty( "jaxb.encoding", "UTF-8" );

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

  private void startSCEOptimization( final SceIOHandler sceIO, final ICalcMonitor monitor )
      throws CalcJobServiceException
  {
    InputStreamReader inStream = null;
    InputStreamReader errStream = null;

    ProcessControlThread procCtrlThread = null;
    try
    {
      final File exeDir = m_sceExe.getParentFile();
      final String commandString = m_sceExe.getAbsolutePath();

      final Process process = Runtime.getRuntime().exec( commandString, null, exeDir );
      final long lTimeOut = 1000l * 60l * 15l;// 15 minutes
      if( lTimeOut > 0 )
      {
        procCtrlThread = new ProcessControlThread( process, lTimeOut );
        procCtrlThread.start();
      }

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
        if( monitor.isCanceled() )
        {
          process.destroy();
          if( procCtrlThread != null )
          {
            procCtrlThread.endProcessControl();
          }
          return;
        }
        try
        {
          process.exitValue();
          break;
        }
        catch( IllegalThreadStateException e )
        {
          sceIO.handleStreams( outBuffer, errBuffer, inputWriter );
        }
        Thread.sleep( 100 );
      }
      if( procCtrlThread != null )
      {
        procCtrlThread.endProcessControl();
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
      throw new CalcJobServiceException( "beim Ausfuehren unterbrochen", e );
    }
    finally
    {
      IOUtils.closeQuietly( inStream );
      IOUtils.closeQuietly( errStream );
      if( procCtrlThread != null && procCtrlThread.procDestroyed() )
      {
        throw new CalcJobServiceException( "beim Ausfuehren unterbrochen", new ProcessTimeoutException(
            "Timeout bei der Abarbeitung der Optimierung" ) );
      }
    }
  }
}