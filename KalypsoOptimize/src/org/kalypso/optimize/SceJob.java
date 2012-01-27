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
import java.io.IOException;
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
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.commons.java.lang.ProcessHelper.ProcessControlThread;
import org.kalypso.commons.java.lang.ProcessHelper.ProcessTimeoutException;
import org.kalypso.optimizer.AutoCalibration;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.SimulationException;
import org.w3c.dom.Document;

/**
 * this class encapsulates the optimizing fortran SCE optimizing tool
 * 
 * @author doemming
 */
public class SceJob
{
  private static final TransformerFactory TRANSFORMER_FACTORY = TransformerFactory.newInstance();

  private final URL XML2SCE_URL = getClass().getResource( "resource/xml2sceInput.xsl" );

  private final File m_sceTmpDir;

  private final AutoCalibration m_autoCalibration;

  private final File m_sceDir;

  private final File m_sceExe;

  private final DocumentBuilderFactory m_factory = DocumentBuilderFactory.newInstance();

  private DocumentBuilder m_docuBuilder;

  private Marshaller m_marshaller;

  public SceJob( final AutoCalibration autoCalibration, final File sceTmpDir )
  {
    m_autoCalibration = autoCalibration;
    m_sceTmpDir = sceTmpDir;
    m_sceDir = new File( m_sceTmpDir, "sce" );
    m_sceExe = new File( m_sceDir, "sce.exe" );

    m_factory.setNamespaceAware( true );

    try
    {
      m_docuBuilder = m_factory.newDocumentBuilder();
      m_marshaller = JaxbUtilities.createMarshaller( OptimizeJaxb.JC );
    }
    catch( final ParserConfigurationException e )
    {
      e.printStackTrace();
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
    }
  }

  public void optimize( final SceIOHandler sceIO, final ISimulationMonitor monitor ) throws SimulationException
  {
    prepareExe();

    writeSceIn();

    startSCEOptimization( sceIO, monitor );
  }

  private void prepareExe( ) throws SimulationException
  {
    try
    {
      final URL sceExeLocation = getClass().getResource( "resource/sce.exe" );
      FileUtils.copyURLToFile( sceExeLocation, m_sceExe );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new SimulationException( "sce.exe konnte nicht entpackt werden", e );
    }
  }

  /**
   * prepare SCE configuration file "scein.dat"
   */
  private void writeSceIn( ) throws SimulationException
  {
    try
    {
      final File outputFile = new File( m_sceDir, "scein.dat" );

      // prepare scein.dat
      final Document xmlDOM = m_docuBuilder.newDocument();
      m_marshaller.marshal( m_autoCalibration, xmlDOM );

      final Document xslDOM = m_docuBuilder.parse( XML2SCE_URL.toURI().toASCIIString() );
      final Transformer transformer = TRANSFORMER_FACTORY.newTransformer( new DOMSource( xslDOM ) );
      transformer.transform( new DOMSource( xmlDOM ), new StreamResult( outputFile ) );
    }
    catch( final Exception e )
    {
      throw new SimulationException( "Fehler beim Schreiben der sce Konfiguration", e );
    }
  }

  private void startSCEOptimization( final SceIOHandler sceIO, final ISimulationMonitor monitor ) throws SimulationException
  {
    InputStreamReader inStream = null;
    InputStreamReader errStream = null;

    ProcessControlThread procCtrlThread = null;
    try
    {
      final String[] commands = new String[] { m_sceExe.getAbsolutePath() };

      final Process process = Runtime.getRuntime().exec( commands, null, m_sceDir );
      final long lTimeOut = 1000l * 60l * 15l;// 15 minutes
      if( lTimeOut > 0 )
      {
        procCtrlThread = new ProcessControlThread( process, lTimeOut );
        procCtrlThread.start();
      }

      final StringBuffer outBuffer = new StringBuffer();
      final StringBuffer errBuffer = new StringBuffer();

      final Writer inputWriter = new PrintWriter( process.getOutputStream(), false );

      inStream = new InputStreamReader( process.getInputStream() );
      errStream = new InputStreamReader( process.getErrorStream() );

      final int stepMax = m_autoCalibration.getOptParameter().getMaxN();

      while( true )
      {
        final int step = sceIO.getStep();
        monitor.setProgress( 100 * step / (stepMax + 1) );
        if( step > stepMax )
        {
          final String monitorMsg = String.format( "Optimierungsrechnung abgeschlossen, Ergebnisauswertung", step + 1, stepMax + 1 );
          monitor.setMessage( monitorMsg );
        }
        else
        {
          final String monitorMsg = String.format( "Optimierungsrechnung %d von %d", step + 1, stepMax + 1 );
          monitor.setMessage( monitorMsg );
        }

        if( inStream.ready() )
        {
          final char buffer[] = new char[100];
          final int bufferC = inStream.read( buffer );
          outBuffer.append( buffer, 0, bufferC );
        }
        if( errStream.ready() )
        {
          final char buffer[] = new char[100];
          final int bufferC = errStream.read( buffer );
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
        catch( final IllegalThreadStateException e )
        {
          final OptimizeMonitor subMonitor = new OptimizeMonitor( monitor );
          sceIO.handleStreams( outBuffer, errBuffer, inputWriter, subMonitor );
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
      throw new SimulationException( "Fehler beim Ausfuehren", e );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
      throw new SimulationException( "beim Ausfuehren unterbrochen", e );
    }
    finally
    {
      IOUtils.closeQuietly( inStream );
      IOUtils.closeQuietly( errStream );
      if( procCtrlThread != null && procCtrlThread.procDestroyed() )
      {
        throw new SimulationException( "beim Ausfuehren unterbrochen", new ProcessTimeoutException( "Timeout bei der Abarbeitung der Optimierung" ) );
      }
    }
  }
}