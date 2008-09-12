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
package org.kalypso.services.observation.server;

import java.rmi.RemoteException;

import javax.activation.DataHandler;
import javax.jws.WebService;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.osgi.framework.internal.core.FrameworkProperties;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.repository.RepositoryException;
import org.kalypso.services.observation.KalypsoServiceObsActivator;
import org.kalypso.services.observation.sei.DataBean;
import org.kalypso.services.observation.sei.IObservationService;
import org.kalypso.services.observation.sei.ItemBean;
import org.kalypso.services.observation.sei.ObservationBean;

/**
 * Kalypso Observation Service.<br>
 * It delegates to the correct observation service, which is reinitialized after a period of time.
 * 
 * @author Holger Albert
 */
@SuppressWarnings("restriction")
@WebService(endpointInterface = "org.kalypso.services.observation.sei.IObservationService")
public class ObservationServiceImpl implements IObservationService
{
  /**
   * A listener for job change events.
   */
  private final IJobChangeListener m_listener = new JobChangeAdapter()
  {
    /**
     * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
     */
    @Override
    public void done( final IJobChangeEvent event )
    {
      /* Get the job. */
      final Job job = event.getJob();

      /* Is it the right one. */
      if( !(job instanceof ObservationServiceJob) )
        return;

      /* Handle this event. */
      onJobFinished( event.getResult() );
    }
  };

  /**
   * The observation service job initializes the observation service.
   */
  private Job m_observationServiceJob;

  /**
   * This variable stores the reinitialize time intervall.
   */
  private final long m_intervall;

  /**
   * The observation service delegate. It is reloaded after a period of time.
   */
  private IObservationService m_delegate;

  public ObservationServiceImpl( )
  {
    m_observationServiceJob = null;
    m_delegate = null;

    final String reinitStr = FrameworkProperties.getProperty( KalypsoServiceObsActivator.SYSPROP_REINIT_SERVICE, "600000" );
    long reinit = 600000; // 10 min
    try
    {
      reinit = Long.parseLong( reinitStr );
    }
    catch( final NumberFormatException e )
    {
      e.printStackTrace();
    }
    m_intervall = reinit;

    /* Start the reloading. */
    m_observationServiceJob = new ObservationServiceJob( this );
    m_observationServiceJob.addJobChangeListener( m_listener );
    // TRICKY: give a bit of time for first schedule, as this will access a HttpResource,
    // which may not be accessible right now
    m_observationServiceJob.schedule( 3000 );
  }

  /**
   * @see org.kalypso.services.observation.sei.IObservationService#adaptItem(org.kalypso.services.observation.sei.ItemBean)
   */
  @Override
  public ObservationBean adaptItem( final ItemBean ib ) throws SensorException
  {
    /* Get the observation service delegate. */
    final IObservationService delegate = getDelegate();

    /* If it is existing, delegate to it. */
    if( delegate != null )
      return delegate.adaptItem( ib );

    return null;
  }

  /**
   * @see org.kalypso.services.observation.sei.IObservationService#clearTempData(java.lang.String)
   */
  @Override
  public void clearTempData( final String dataId ) throws SensorException
  {
    /* Get the observation service delegate. */
    final IObservationService delegate = getDelegate();

    /* If it is existing, delegate to it. */
    if( delegate != null )
      delegate.clearTempData( dataId );
  }

  /**
   * @see org.kalypso.services.observation.sei.IObservationService#getServiceVersion()
   */
  @Override
  public int getServiceVersion( ) throws RemoteException
  {
    /* Get the observation service delegate. */
    final IObservationService delegate = getDelegate();

    /* If it is existing, delegate to it. */
    if( delegate != null )
      return delegate.getServiceVersion();

    return 0;
  }

  /**
   * @see org.kalypso.services.observation.sei.IObservationService#readData(java.lang.String)
   */
  @Override
  public DataBean readData( final String href ) throws SensorException
  {
    /* Get the observation service delegate. */
    final IObservationService delegate = getDelegate();

    /* If it is existing, delegate to it. */
    if( delegate != null )
      return delegate.readData( href );

    return null;
  }

  /**
   * @see org.kalypso.services.observation.sei.IObservationService#writeData(org.kalypso.services.observation.sei.ObservationBean,
   *      javax.activation.DataHandler)
   */
  @Override
  public void writeData( final ObservationBean observation, final DataHandler data ) throws SensorException
  {
    /* Get the observation service delegate. */
    final IObservationService delegate = getDelegate();

    /* If it is existing, delegate to it. */
    if( delegate != null )
      delegate.writeData( observation, data );
  }

  /**
   * @see org.kalypso.services.observation.sei.IRepositoryService#findItem(java.lang.String)
   */
  @Override
  public ItemBean findItem( final String id ) throws RepositoryException
  {
    /* Get the observation service delegate. */
    final IObservationService delegate = getDelegate();

    /* If it is existing, delegate to it. */
    if( delegate != null )
      return delegate.findItem( id );

    return null;
  }

  /**
   * @see org.kalypso.services.observation.sei.IRepositoryService#getChildren(org.kalypso.services.observation.sei.ItemBean)
   */
  @Override
  public ItemBean[] getChildren( final ItemBean parent ) throws RepositoryException
  {
    /* Get the observation service delegate. */
    final IObservationService delegate = getDelegate();

    /* If it is existing, delegate to it. */
    if( delegate != null )
      return delegate.getChildren( parent );

    return new ItemBean[] {};
  }

  /**
   * @see org.kalypso.services.observation.sei.IRepositoryService#hasChildren(org.kalypso.services.observation.sei.ItemBean)
   */
  @Override
  public boolean hasChildren( final ItemBean parent ) throws RepositoryException
  {
    /* Get the observation service delegate. */
    final IObservationService delegate = getDelegate();

    /* If it is existing, delegate to it. */
    if( delegate != null )
      return delegate.hasChildren( parent );

    return false;
  }

  /**
   * @see org.kalypso.services.observation.sei.IRepositoryService#reload()
   */
  @Override
  public void reload( )
  {
    /* When the client requests a reload, do nothing, because every short period of time it is reloaded automatically. */
    /* The client should only refresh itself. */

    // /* Cancel the old one. */
    // m_observationServiceJob.cancel();
    // m_observationServiceJob.removeJobChangeListener( m_listener );
    //
    // /* Set to null, so all requests will wait, not only these after the first initializing. */
    // m_delegate = null;
    //
    // /* Reschedule it with no delay. */
    // m_observationServiceJob = new ObservationServiceJob( this );
    // m_observationServiceJob.addJobChangeListener( m_listener );
    // m_observationServiceJob.schedule();
  }

  /**
   * This function returns the delegate and waits for it to be initialized, if neccessary. If the initialization fails,
   * the result could be null.
   * 
   * @return The delegate or null.
   */
  protected IObservationService getDelegate( )
  {
    final IObservationService delegate = getDelegateInternal();
    if( delegate != null )
      return delegate;

    try
    {
      /* Wait for the job, if he has finished loading. */
      m_observationServiceJob.join();
    }
    catch( final InterruptedException ex )
    {
      ex.printStackTrace();
    }

    return getDelegateInternal();
  }

  /**
   * This function returns the delegate or null, if not initialized.
   * 
   * @return The delegate or null, if not initialized.
   */
  protected synchronized IObservationService getDelegateInternal( )
  {
    return m_delegate;
  }

  /**
   * This function sets the delegate.
   * 
   * @param delegate
   *          The delegate.
   */
  protected synchronized void setDelegate( final IObservationService delegate )
  {
    m_delegate = delegate;
  }

  /**
   * This function is executed, if the job has finished (i.e. done, canceled, failure).
   * 
   *@param status
   *          The status of the job.
   */
  protected void onJobFinished( final IStatus status )
  {
    if( !status.isOK() )
    {
      /* What to do on error or cancelation? */
      return;
    }

    /* Reschedule the job. */
    m_observationServiceJob.schedule( m_intervall );
  }
}