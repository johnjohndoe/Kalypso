/*
 * --------------- Kalypso-Header
 * --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal
 * engineering Denickestr. 22 21073 Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany
 * http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
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
import java.util.Timer;
import java.util.TimerTask;
import java.util.Vector;

import javax.activation.DataHandler;

import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.service.CalcJobClientBean;
import org.kalypso.services.calculation.service.CalcJobInfoBean;
import org.kalypso.services.calculation.service.CalcJobServerBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * LocalCalculationService
 * <p>
 * 
 * created by
 * 
 * @author Nadja Peiler (19.05.2005)
 */
public class LocalCalculationService //implements ICalculationService
{
  /** Vector of {@link LocalCalcJobThread}s */
  private final Vector m_threads = new Vector();

  private Timer m_timer;

  private long m_schedulingPeriod =  2000;

  private int m_maxThreads = 1;
  
  public LocalCalculationService(){
    super();
  }

  public long getSchemaValidity( final String namespace ) throws CalcJobServiceException
  {
    // not used in local calculation service
    return 0;
  }

  public String[] getSupportedSchemata() throws CalcJobServiceException
  {
    // not used in local calculation service
    return null;
  }

  public DataHandler getSchema( final String namespace ) throws CalcJobServiceException
  {
//  not used in local calculation service
    return null;
  }

  public String[] getJobTypes() throws CalcJobServiceException
  {
    // local calculation service does not know
    // TODO: über ProcessExtensions abfragen, z.B. Methode, die nur Prozesse vom
    // Typ "local" zurückgibt
    return null;
  }

  public CalcJobServerBean[] getRequiredInput( final String typeID ) throws CalcJobServiceException
  {
    // not used in local calculation service
    return null;
  }

  public CalcJobServerBean[] getDeliveringResults( final String typeID ) throws CalcJobServiceException
  {
//  not used in local calculation service
    return null;
  }

  public CalcJobInfoBean[] getJobs() throws CalcJobServiceException
  {
    synchronized( m_threads )
    {
      final CalcJobInfoBean[] jobBeans = new CalcJobInfoBean[m_threads.size()];
      int count = 0;

      for( final Iterator jIt = m_threads.iterator(); jIt.hasNext(); count++ )
      {
        final LocalCalcJobThread cjt = (LocalCalcJobThread)jIt.next();
        jobBeans[count] = cjt.getJobBean();
      }

      return jobBeans;
    }
  }

  public CalcJobInfoBean getJob( String jobID ) throws CalcJobServiceException
  {
    return findJobThread( jobID ).getJobBean();
  }

  private LocalCalcJobThread findJobThread( final String jobID ) throws CalcJobServiceException
  {
    synchronized( m_threads )
    {
      for( final Iterator jIt = m_threads.iterator(); jIt.hasNext(); )
      {
        final LocalCalcJobThread cjt = (LocalCalcJobThread)jIt.next();

        if( cjt.getJobBean().getId().equals( jobID ) )
          return cjt;
      }
    }

    throw new CalcJobServiceException( "Job not found: " + jobID, null );
  }

  public CalcJobInfoBean startJob( final String typeID, final String description,
      final DataHandler zipHandler, final CalcJobClientBean[] input,
      final CalcJobClientBean[] output ) throws CalcJobServiceException
  {
    // startLocalJob-method has to be used!
    return null;
  }

  public CalcJobInfoBean startLocalJob( final String typeID, final String description, ICalcJob job,
      final CalcJobClientBean[] input, final CalcJobClientBean[] output )
  {
    LocalCalcJobThread cjt = null;
    synchronized( m_threads )
    {
      // eine unbenutzte ID finden
      int id = -1;
      for( int i = 0; i < m_threads.size(); i++ )
      {
        if( m_threads.get( i ) == null )
        {
          id = i;
          break;
        }
      }
      if( id == -1 )
        id = m_threads.size();

      cjt = new LocalCalcJobThread( "" + id, description, typeID, job, input, output );

      if( id == m_threads.size() )
        m_threads.add( cjt );
      else
        m_threads.set( id, cjt );

    }

    startScheduling();

    return cjt == null ? null : cjt.getJobBean();
  }
  
  private void startScheduling()
  {
    if( m_timer == null )
    {
      m_timer = new Timer();
      final TimerTask timerTask = new TimerTask()
      {
        public void run()
        {
          scheduleJobs();
        }
      };
      m_timer.schedule( timerTask, m_schedulingPeriod = 2000, m_schedulingPeriod );
    }
  }
  
  public void scheduleJobs()
  {
    synchronized( m_threads )
    {
      // count running thread
      int runningCount = 0;
      int waitingCount = 0;
      for( final Iterator jIt = m_threads.iterator(); jIt.hasNext(); )
      {
        final LocalCalcJobThread cjt = (LocalCalcJobThread)jIt.next();
        if( cjt.isAlive() )
          runningCount++;

        final CalcJobInfoBean jobBean = cjt.getJobBean();
        if( jobBean.getState() == ICalcServiceConstants.WAITING )
          waitingCount++;
      }

      if( waitingCount == 0 )
      {
        stopScheduling();
        return;
      }

      // Maximal einen Job auf einmal starten
      if( runningCount >= m_maxThreads )
      {
        return;
      }

      // start one waiting job, if maximum is not reached
      for( final Iterator jIt = m_threads.iterator(); jIt.hasNext(); )
      {
        final LocalCalcJobThread cjt = (LocalCalcJobThread)jIt.next();

        final CalcJobInfoBean jobBean = cjt.getJobBean();
        if( jobBean.getState() == ICalcServiceConstants.WAITING )
        {
          cjt.start();
          return;
        }
      }
    }
  }
  
  private void stopScheduling()
  {
    if( m_timer != null )
    {
      m_timer.cancel();
      m_timer = null;
    }
  }

  public void cancelJob( String jobID ) throws CalcJobServiceException
  {
    findJobThread( jobID ).getJobBean().cancel();
  }

  public DataHandler transferCurrentResults( String arg0 ) throws CalcJobServiceException
  {
    // not used in local calculation service
    return null;
  }

  public String[] getCurrentResults( String jobID ) throws CalcJobServiceException
  {
    final LocalCalcJobThread thread = findJobThread( jobID );
    return thread.getCurrentResults();
  }

  public void disposeJob( String jobID ) throws CalcJobServiceException
  {
    final LocalCalcJobThread cjt = findJobThread( jobID );

    if( cjt.isAlive() )
      throw new CalcJobServiceException( "Cannot dispose a running job! Cancel it first.", null );

    cjt.dispose();

    synchronized( m_threads )
    {
      m_threads.remove( cjt );
      if( m_threads.size() == 0 )
        stopScheduling();
    }
  }

  public int getServiceVersion() throws RemoteException
  {
    return 0;
  }

}