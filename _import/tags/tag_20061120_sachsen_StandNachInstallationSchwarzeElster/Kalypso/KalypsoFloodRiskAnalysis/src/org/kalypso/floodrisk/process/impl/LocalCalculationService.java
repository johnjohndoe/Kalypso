/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.floodrisk.process.impl;

import java.net.URL;
import java.net.URLConnection;
import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.Vector;

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
import org.kalypso.services.calculation.service.impl.ICalcJobFactory;
import org.kalypso.services.calculation.service.impl.ModelspecData;

/**
 * LocalCalculationService
 * <p>
 * 
 * @see org.kalypso.services.calculation.service.impl.QueuedCalcJobService
 * 
 * created by
 * 
 * @author Nadja Peiler (19.05.2005)
 */
public class LocalCalculationService implements ICalculationService
{
  /** Vector of {@link LocalCalcJobThread}s */
  private final Vector m_threads = new Vector();

  private Timer m_timer;

  private long m_schedulingPeriod = 2000;

  private int m_maxThreads = 1;

  private ICalcJobFactory m_calcJobFactory;

  private IUrlCatalog m_catalog;

  private final Unmarshaller m_unmarshaller;

  private final Map m_modelspecMap = new HashMap();

  /**
   * @see org.kalypso.services.calculation.service.impl.QueuedCalcJobService#QueuedCalcJobService(org.kalypso.services.calculation.service.impl.ICalcJobFactory,
   *      IUrlCatalog, int, long)
   * @param factory
   *          LocalCalcJobFactory
   * @param catalog
   *          catalog with needed schemas for the calcJobs
   * @throws RemoteException
   */
  public LocalCalculationService( final ICalcJobFactory factory, final IUrlCatalog catalog ) throws RemoteException
  {
    m_calcJobFactory = factory;
    m_catalog = catalog;

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
  public int getServiceVersion() throws RemoteException
  {
    return 0;
  }

  /**
   * 
   * @see org.kalypso.services.calculation.service.ICalculationService#getJobTypes()
   */
  public synchronized final String[] getJobTypes()
  {
    return m_calcJobFactory.getSupportedTypes();
  }

  /**
   * 
   * @see org.kalypso.services.calculation.service.ICalculationService#getJobs()
   */
  public synchronized CalcJobInfoBean[] getJobs() throws CalcJobServiceException
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

  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.service.ICalculationService#getJob(java.lang.String)
   */
  public CalcJobInfoBean getJob( String jobID ) throws CalcJobServiceException
  {
    return findJobThread( jobID ).getJobBean();
  }

  /**
   * 
   * @param jobID
   * @return LocalCalcJobThread for the given jobID
   * @throws CalcJobServiceException
   *  
   */
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

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#startJob(java.lang.String, java.lang.String,
   *      javax.activation.DataHandler,
   *      org.kalypso.services.calculation.service.CalcJobClientBean[],org.kalypso.services.calculation.service.CalcJobClientBean[])
   */
  public CalcJobInfoBean startJob( final String typeID, final String description, final DataHandler zipHandler,
      final CalcJobClientBean[] input, final CalcJobClientBean[] output ) throws CalcJobServiceException
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

      final ModelspecData modelspec = getModelspec( typeID );

      ICalcJob job = m_calcJobFactory.createJob( typeID );

      cjt = new LocalCalcJobThread( "" + id, description, typeID, job, modelspec, input, output );

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

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#cancelJob(java.lang.String)
   */
  public void cancelJob( String jobID ) throws CalcJobServiceException
  {
    findJobThread( jobID ).getJobBean().cancel();
  }

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#disposeJob(java.lang.String)
   */
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
        final LocalCalcJobThread cjt = (LocalCalcJobThread)iter.next();
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
  public DataHandler transferCurrentResults( String jobID ) throws CalcJobServiceException
  {
    // not implemented
    return null;
  }

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#getCurrentResults(java.lang.String)
   */
  public String[] getCurrentResults( String jobID ) throws CalcJobServiceException
  {
    final LocalCalcJobThread thread = findJobThread( jobID );
    return thread.getCurrentResults();
  }

  /**
   * 
   * @param typeID
   * @return ModelspecData for given typeID
   * @throws CalcJobServiceException
   *  
   */
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
  public DataHandler getSchema( final String namespace ) throws CalcJobServiceException
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
  public String[] getSupportedSchemata() throws CalcJobServiceException
  {
    final Map catalog = m_catalog.getCatalog();
    final String[] namespaces = new String[catalog.size()];
    int count = 0;
    for( final Iterator mapIt = catalog.keySet().iterator(); mapIt.hasNext(); )
      namespaces[count++] = (String)mapIt.next();

    return namespaces;
  }

}