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
package org.kalypso.services.calculation.service.impl;

import java.net.URL;
import java.net.URLConnection;
import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.Vector;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.activation.URLDataSource;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.kalypso.contribs.java.net.IUrlCatalog;
import org.kalypso.model.xml.ObjectFactory;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.service.CalcJobClientBean;
import org.kalypso.services.calculation.service.CalcJobInfoBean;
import org.kalypso.services.calculation.service.CalcJobServerBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypso.services.calculation.service.ICalculationService;

/**
 * A straight forward {@link org.kalypso.services.calculation.service.ICalculationService}-Implementation. All jobs go
 * in one fifo-queue. Support parallel processing of jobs.
 * 
 * @author Belger
 */
public class QueuedCalcJobService implements ICalculationService
{
  private static final Logger LOGGER = Logger.getLogger( QueuedCalcJobService.class.getName() );

  /** Vector of {@link CalcJobThread}s */
  private final Vector m_threads = new Vector();

  private final ICalcJobFactory m_calcJobFactory;

  private Timer m_timer = null;

  /** maximale Anzahl von parallel laufenden Job */
  private final int m_maxThreads;

  /** So oft (in ms) wird die queue nach wartenden Jobs durchsucht */
  private final long m_schedulingPeriod;

  private final Unmarshaller m_unmarshaller;

  private final Map m_modelspecMap = new HashMap();

  private final IUrlCatalog m_catalog;

  public QueuedCalcJobService( final ICalcJobFactory factory, final IUrlCatalog catalog, final int maxThreads,
      final long schedulingPeriod ) throws RemoteException
  {
    m_calcJobFactory = factory;
    m_catalog = catalog;
    m_maxThreads = maxThreads;
    m_schedulingPeriod = schedulingPeriod;

    try
    {
      m_unmarshaller = new ObjectFactory().createUnmarshaller();
    }
    catch( final JAXBException e )
    {
      throw new RemoteException( "Unmarshaller für Modellspezifikation konnte nicht erzeugt werden.", e );
    }
  }

  /**
   * @see org.kalypso.services.IKalypsoService#getServiceVersion()
   */
  public int getServiceVersion()
  {
    return 0;
  }

  public synchronized final String[] getJobTypes()
  {
    return m_calcJobFactory.getSupportedTypes();
  }

  public synchronized CalcJobInfoBean[] getJobs()
  {
    synchronized( m_threads )
    {
      final CalcJobInfoBean[] jobBeans = new CalcJobInfoBean[m_threads.size()];
      int count = 0;

      for( final Iterator jIt = m_threads.iterator(); jIt.hasNext(); count++ )
      {
        final CalcJobThread cjt = (CalcJobThread)jIt.next();
        jobBeans[count] = cjt.getJobBean();
      }

      return jobBeans;
    }
  }

  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.service.ICalculationService#getJob(java.lang.String)
   */
  public CalcJobInfoBean getJob( final String jobID ) throws CalcJobServiceException
  {
    return findJobThread( jobID ).getJobBean();
  }

  private void startScheduling()
  {
    if( m_timer == null )
    {
      LOGGER.info( "Start scheduling with period: " + m_schedulingPeriod + "ms" );

      m_timer = new Timer();
      final TimerTask timerTask = new TimerTask()
      {
        public void run()
        {
          scheduleJobs();
        }
      };
      m_timer.schedule( timerTask, m_schedulingPeriod, m_schedulingPeriod );
    }
  }

  private void stopScheduling()
  {
    if( m_timer != null )
    {
      m_timer.cancel();
      m_timer = null;

      LOGGER.info( "Stopped scheduling" );
    }
  }

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#cancelJob(java.lang.String)
   */
  public void cancelJob( final String jobID ) throws CalcJobServiceException
  {
    findJobThread( jobID ).getJobBean().cancel();
  }

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#disposeJob(java.lang.String)
   */
  public void disposeJob( final String jobID ) throws CalcJobServiceException
  {
    final CalcJobThread cjt = findJobThread( jobID );

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

  private CalcJobThread findJobThread( final String jobID ) throws CalcJobServiceException
  {
    synchronized( m_threads )
    {
      for( final Iterator jIt = m_threads.iterator(); jIt.hasNext(); )
      {
        final CalcJobThread cjt = (CalcJobThread)jIt.next();

        if( cjt.getJobBean().getId().equals( jobID ) )
          return cjt;
      }
    }

    throw new CalcJobServiceException( "Job not found: " + jobID, null );
  }

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#startJob(java.lang.String, java.lang.String,
   *      javax.activation.DataHandler,
   *      org.kalypso.services.calculation.service.CalcJobClientBean[],org.kalypso.services.calculation.service.CalcJobClientBean[])
   */
  public final CalcJobInfoBean startJob( final String typeID, final String description, final DataHandler zipHandler,
      final CalcJobClientBean[] input, final CalcJobClientBean[] output ) throws CalcJobServiceException
  {
    CalcJobThread cjt = null;
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

      final ModelspecData modelspec = getModelspec( typeID );

      final ICalcJob job = m_calcJobFactory.createJob( typeID );

      cjt = new CalcJobThread( "" + id, description, typeID, job, modelspec, zipHandler, input, output );

      if( id == m_threads.size() )
        m_threads.add( cjt );
      else
        m_threads.set( id, cjt );

      LOGGER.info( "Job waiting for scheduling: " + id );
    }

    startScheduling();

    return cjt == null ? null : cjt.getJobBean();
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
        final CalcJobThread cjt = (CalcJobThread)jIt.next();
        if( cjt.isAlive() )
          runningCount++;

        final CalcJobInfoBean jobBean = cjt.getJobBean();
        if( jobBean.getState() == ICalcServiceConstants.WAITING )
          waitingCount++;
      }

      LOGGER.info( "Scheduler: Running jobs: " + runningCount );
      LOGGER.info( "Scheduler: Waiting jobs: " + waitingCount );

      if( waitingCount == 0 )
      {
        stopScheduling();
        return;
      }

      // Maximal einen Job auf einmal starten
      if( runningCount >= m_maxThreads )
      {
        LOGGER.info( "Scheduler: Maximum reached" );
        return;
      }

      // start one waiting job, if maximum is not reached
      for( final Iterator jIt = m_threads.iterator(); jIt.hasNext(); )
      {
        final CalcJobThread cjt = (CalcJobThread)jIt.next();

        final CalcJobInfoBean jobBean = cjt.getJobBean();
        if( jobBean.getState() == ICalcServiceConstants.WAITING )
        {
          LOGGER.info( "Scheduler: Starting job: " + jobBean.getId() );
          cjt.start();
          return;
        }
      }
    }
  }

  /**
   * Falls dieses Objekt wirklich mal zerstört wird und wir es mitkriegen, dann alle restlichen Jobs zerstören und
   * insbesondere alle Dateien löschen
   * 
   * @see java.lang.Object#finalize()
   */
  protected void finalize() throws Throwable
  {
    synchronized( m_threads )
    {
      for( final Iterator iter = m_threads.iterator(); iter.hasNext(); )
      {
        final CalcJobThread cjt = (CalcJobThread)iter.next();
        final CalcJobInfoBean jobBean = cjt.getJobBean();
        disposeJob( jobBean.getId() );
      }

    }

    super.finalize();
  }

  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.service.ICalculationService#transferCurrentResults(java.lang.String)
   */
  public DataHandler transferCurrentResults( final String jobID ) throws CalcJobServiceException
  {
    final CalcJobThread thread = findJobThread( jobID );
    return thread.packCurrentResults();
  }

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#getCurrentResults(java.lang.String)
   */
  public String[] getCurrentResults( String jobID ) throws CalcJobServiceException
  {
    final CalcJobThread thread = findJobThread( jobID );
    return thread.getCurrentResults();
  }

  private ModelspecData getModelspec( final String typeID ) throws CalcJobServiceException
  {
    ModelspecData data = (ModelspecData)m_modelspecMap.get( typeID );
    if( data != null )
      return data;

    final ICalcJob job = m_calcJobFactory.createJob( typeID );
    final URL modelspecURL = job.getSpezifikation();
    data = new ModelspecData( modelspecURL, m_unmarshaller );
    m_modelspecMap.put( typeID, data );

    return data;
  }

  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.service.ICalculationService#getRequiredInput(java.lang.String)
   */
  public CalcJobServerBean[] getRequiredInput( final String typeID ) throws CalcJobServiceException
  {
    return getModelspec( typeID ).getInput();
  }

  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.service.ICalculationService#getDeliveringResults(java.lang.String)
   */
  public CalcJobServerBean[] getDeliveringResults( final String typeID ) throws CalcJobServiceException
  {
    return getModelspec( typeID ).getOutput();
  }

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#getSchema(java.lang.String)
   */
  public DataHandler getSchema( final String namespace )
  {
    final URL url = m_catalog.getURL( namespace );
    if( url == null )
      return null;

    return new DataHandler( new URLDataSource( url ) );
  }

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#getSchemaValidity(java.lang.String)
   */
  public long getSchemaValidity( final String namespace ) throws CalcJobServiceException
  {
    try
    {
      final URL url = m_catalog.getURL( namespace );
      if( url == null )
        throw new CalcJobServiceException( "Unknown schema namespace: " + namespace, null );

      final URLConnection connection = url.openConnection();
      return connection.getLastModified();
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new CalcJobServiceException( "Unknown schema namespace: " + namespace, e );
    }
  }

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#getSupportedSchemata()
   */
  public String[] getSupportedSchemata()
  {
    final Map catalog = m_catalog.getCatalog();
    final String[] namespaces = new String[catalog.size()];
    int count = 0;
    for( final Iterator mapIt = catalog.keySet().iterator(); mapIt.hasNext(); )
      namespaces[count++] = (String)mapIt.next();

    return namespaces;
  }
}