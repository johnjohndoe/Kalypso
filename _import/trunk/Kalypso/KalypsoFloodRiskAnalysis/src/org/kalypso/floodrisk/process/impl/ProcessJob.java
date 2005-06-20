/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.floodrisk.process.impl;

import java.rmi.RemoteException;
import java.util.Iterator;
import java.util.Map;

import javax.xml.rpc.ServiceException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ILock;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.floodrisk.process.ProcessExtension;
import org.kalypso.model.xml.ModeldataType;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.proxy.ICalculationService;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.nature.calcjob.CalcJobHandler;

/**
 * ProcessJob
 * <p>
 * 
 * created by
 * 
 * @author Nadja Peiler (19.05.2005)
 */
public class ProcessJob extends Job
{

  private ModeldataType m_modelData;

  private ICalcJob m_calcJob;

  private IProject m_project;

  private ILock m_lock;

  private ProcessExtension m_extension;

  /**
   * Constructor
   * 
   * @param modelData
   *          modelData for the process
   * @param project
   * @param extension
   *          information of process
   * @param lock
   */
  public ProcessJob( final ModeldataType modelData, final IProject project, final ProcessExtension extension,
      final ILock lock )
  {
    super( "Berechne: " + modelData.getTypeID() );
    m_modelData = modelData;
    m_project = project;
    m_extension = extension;
    m_lock = lock;
    setUser( true );
  }

  /**
   * 
   * @see org.eclipse.core.internal.jobs.InternalJob#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus run( IProgressMonitor monitor )
  {

    try
    {
      m_lock.acquire();
      return runCalculation( monitor );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
      return e.getStatus();
    }
    catch( ServiceException e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, "ServiceException", null );
    }
    finally
    {
      m_lock.release();
    }
  }

  /**
   * get CalculationService, create CalcJobHandler, run Job
   * 
   * @param monitor
   * @return Status of the calculation
   * @throws CoreException
   * @throws ServiceException
   *  
   */
  private IStatus runCalculation( IProgressMonitor monitor ) throws CoreException, ServiceException
  {

    monitor.beginTask( "Berechnung wird gestartet", 100 );

    ICalculationService calcService = findCalulationServiceForType( m_modelData.getTypeID() );
    IStatus runStatus = null;
    if( m_extension.getType().equals( "local" ) )
    {
      final LocalCalcJobHandler cjHandler = new LocalCalcJobHandler( m_modelData, calcService );
      runStatus = cjHandler.runJob( m_project, monitor );
    }
    else
    {
      final CalcJobHandler cjHandler = new CalcJobHandler( m_modelData, calcService );
      //TODO: get calculationFolder
      runStatus = cjHandler.runJob( null, monitor );
    }

    if( runStatus.matches( IStatus.ERROR | IStatus.CANCEL ) )
      return runStatus;

    return new Status( IStatus.OK, KalypsoGisPlugin.getId(), 0, "Berechnung abgeschlossen.", null );
  }

  /**
   * 
   * @param typeID
   * @return CalculationService for given typeID of process
   * @throws ServiceException
   *  
   */
  private ICalculationService findCalulationServiceForType( final String typeID ) throws ServiceException
  {
    final Map proxies = KalypsoGisPlugin.getDefault().getCalculationServiceProxies();
    for( final Iterator iter = proxies.values().iterator(); iter.hasNext(); )
    {
      final ICalculationService proxy = (ICalculationService)iter.next();
      try
      {
        final String[] jobTypes = proxy.getJobTypes();
        for( int i = 0; i < jobTypes.length; i++ )
        {
          if( typeID.equals( jobTypes[i] ) )
            return proxy;
        }
      }
      catch( final RemoteException e )
      {
        // ignore, this service is invalid
        e.printStackTrace();
      }
    }

    throw new ServiceException( "Keiner der konfigurierten Berechnungsdienste kann den gewünschten Modelltyp rechnen: "
        + typeID );
  }

}