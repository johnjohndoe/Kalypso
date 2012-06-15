/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.sim;

import java.net.MalformedURLException;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.opengeospatial.wps.IOValueType.ComplexValueReference;

import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileSystemManagerWrapper;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.service.wps.refactoring.AsynchronousWPSWatchdog;
import org.kalypso.service.wps.refactoring.DefaultWPSProcess;
import org.kalypso.service.wps.refactoring.IWPSObserver;
import org.kalypso.service.wps.refactoring.IWPSProcess;

/**
 * @author ig
 * 
 */
public class ExecuteSWANKalypsoSimulation
{

  private final String m_serviceEndpoint;

  private final IWPSObserver m_wpsObserver;

  private final Map<String, Object> m_inputs;

  // this manager is closed when the results dir is not needed anymore
  private FileSystemManagerWrapper m_manager;

  private FileObject m_resultsDir;

  private IWPSProcess m_wpsRequest;

  /**
   * Create execute request to SWANKalypsoSimulation WPS
   * 
   * @throws URISyntaxException
   *           if any of the inputs is not a valid URI
   */
  public ExecuteSWANKalypsoSimulation( final String serviceEndpoint, final IWPSObserver wpsObserver, final String pStrExe, final URI pSWANModelPath )
  {
    m_serviceEndpoint = serviceEndpoint;
    m_wpsObserver = wpsObserver;
    m_inputs = createInputs( pStrExe, pSWANModelPath );
  }

  public IStatus run( final IProgressMonitor monitor )
  {
    final SubMonitor progress = SubMonitor.convert( monitor, 100 );

    try
    {
      final List<String> outputs = new ArrayList<String>();
      outputs.add( SWANKalypsoSimulation.OUTPUT_RESULTS );

      // run the simulation
      m_manager = VFSUtilities.getNewManager();
      m_wpsRequest = new DefaultWPSProcess( SWANKalypsoSimulation.ID, m_serviceEndpoint, m_manager );
      m_wpsRequest.startProcess( m_inputs, outputs, progress );

//      final AsynchronousWPSWatchdog swanWatchdog = new AsynchronousWPSWatchdog( m_wpsRequest, m_wpsObserver, 60 * 60 * 1000 );
      final AsynchronousWPSWatchdog swanWatchdog = new AsynchronousWPSWatchdog( m_wpsRequest, m_wpsObserver, 0 );
      final IStatus executeStatus = swanWatchdog.waitForProcess( progress );

      return executeStatus;
    }
    catch( final CoreException e )
    {
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.22" ) ); //$NON-NLS-1$
    }
    catch( final FileSystemException e )
    {
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.SWANCalculation.3" ) ); //$NON-NLS-1$
    }
  }

  /**
   * Return results dir if it is already available, null otherwise. <br>
   * It is the caller's responsibility to close the {@link FileSystemManagerWrapper} of the results dir. <br>
   * 
   * @see VFSUtilities#getNewManager()
   * 
   * @throws Exception
   *           if something goes wrong parsing the execute response or resolving the results dir
   */
  public FileObject getResultsDir( ) throws Exception
  {
    if( m_resultsDir == null )
    {
      // try to resolve results dir
      final Object[] resultArray = m_wpsRequest.getResult( SWANKalypsoSimulation.OUTPUT_RESULTS );
      if( resultArray == null )
        return null;

      final ComplexValueReference complexValueReference = (ComplexValueReference) resultArray[0];
      final String resultsDirReference = complexValueReference.getReference();
      m_resultsDir = m_manager.resolveFile( resultsDirReference );
    }

    return m_resultsDir;
  }

  private Map<String, Object> createInputs( final String pStrExe, final URI pSWANModelPath )
  {
    final Map<String, Object> inputs = new HashMap<String, Object>();
    try
    {
      inputs.put( PreSWANKalypso.CALC_CORE_EXE, pStrExe );
      inputs.put( PreSWANKalypso.OUTPUT_PATH_SWAN, pSWANModelPath.toURL().toExternalForm() );
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
    }
    return inputs;
  }

  public final IWPSProcess getWpsRequest( ) 
  {
    return m_wpsRequest;
  }
  
}
