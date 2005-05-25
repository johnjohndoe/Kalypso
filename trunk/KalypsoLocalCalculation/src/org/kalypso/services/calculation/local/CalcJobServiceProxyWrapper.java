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
package org.kalypso.services.calculation.local;

import java.rmi.RemoteException;

import javax.activation.DataHandler;

import org.kalypso.services.proxy.CalcJobClientBean;
import org.kalypso.services.proxy.CalcJobInfoBean;
import org.kalypso.services.proxy.CalcJobServerBean;
import org.kalypso.services.proxy.ICalculationService;

/**
 * Wraps an original
 * {@link org.kalypso.services.calculation.service.ICalculationService}as stub.
 * 
 * @author belger
 */
public class CalcJobServiceProxyWrapper implements ICalculationService
{
  private final org.kalypso.services.calculation.service.ICalculationService m_service;

  public CalcJobServiceProxyWrapper(
      final org.kalypso.services.calculation.service.ICalculationService service )
  {
    m_service = service;
  }

  /**
   * @see org.kalypso.services.proxy.ICalculationService#getServiceVersion()
   */
  public int getServiceVersion() throws RemoteException
  {
    return m_service.getServiceVersion();
  }

  /**
   * @see org.kalypso.services.proxy.ICalculationService#cancelJob(java.lang.String)
   */
  public void cancelJob( final String arg0 ) throws RemoteException
  {
    m_service.cancelJob( arg0 );
  }

  /**
   * @see org.kalypso.services.proxy.ICalculationService#disposeJob(java.lang.String)
   */
  public void disposeJob( String arg0 ) throws RemoteException
  {
    m_service.disposeJob( arg0 );
  }

  /**
   * @see org.kalypso.services.proxy.ICalculationService#getCurrentResults(java.lang.String)
   */
  public String[] getCurrentResults( String arg0 ) throws RemoteException
  {
    return m_service.getCurrentResults( arg0 );
  }

  /**
   * @see org.kalypso.services.proxy.ICalculationService#getDeliveringResults(java.lang.String)
   */
  public CalcJobServerBean[] getDeliveringResults( String arg0 ) throws RemoteException
  {
    return wrapCalcJobServerBeans( m_service.getDeliveringResults( arg0 ) );
  }

  private CalcJobServerBean[] wrapCalcJobServerBeans(
      org.kalypso.services.calculation.service.CalcJobServerBean[] beans )
  {
    final CalcJobServerBean[] newBeans = new CalcJobServerBean[beans.length];
    for( int i = 0; i < beans.length; i++ )
    {
      final org.kalypso.services.calculation.service.CalcJobServerBean bean = beans[i];
      newBeans[i] = new CalcJobServerBean();
      newBeans[i].setId( bean.getId() );
      newBeans[i].setDescription( bean.getDescription() );
    }
    return newBeans;
  }

  /**
   * @see org.kalypso.services.proxy.ICalculationService#getJob(java.lang.String)
   */
  public CalcJobInfoBean getJob( String arg0 ) throws RemoteException
  {
    return wrapCalcJobInfoBean( m_service.getJob( arg0 ) );
  }

  private CalcJobInfoBean wrapCalcJobInfoBean(
      org.kalypso.services.calculation.service.CalcJobInfoBean bean )
  {
    CalcJobInfoBean newbean = new CalcJobInfoBean();
    newbean.setId( bean.getId() );
    newbean.setDescription( bean.getDescription() );
    newbean.setType( bean.getType() );
    newbean.setState( bean.getState() );
    newbean.setProgress( bean.getProgress() );
    newbean.setFinishText( bean.getFinishText() );
    newbean.setFinishStatus( bean.getFinishStatus() );
    return newbean;
  }

  /**
   * @see org.kalypso.services.proxy.ICalculationService#getJobTypes()
   */
  public String[] getJobTypes() throws RemoteException
  {
    return m_service.getJobTypes();
  }

  /**
   * @see org.kalypso.services.proxy.ICalculationService#getJobs()
   */
  public CalcJobInfoBean[] getJobs() throws RemoteException
  {
    return wrapCalcJobInfoBeans( m_service.getJobs() );
  }

  private CalcJobInfoBean[] wrapCalcJobInfoBeans(
      org.kalypso.services.calculation.service.CalcJobInfoBean[] beans )
  {
    final CalcJobInfoBean[] newbeans = new CalcJobInfoBean[beans.length];
    for( int i = 0; i < beans.length; i++ )
      newbeans[i] = wrapCalcJobInfoBean( beans[i] );

    return newbeans;
  }

  /**
   * @see org.kalypso.services.proxy.ICalculationService#getRequiredInput(java.lang.String)
   */
  public CalcJobServerBean[] getRequiredInput( String arg0 ) throws RemoteException
  {
    return wrapCalcJobServerBeans( m_service.getRequiredInput( arg0 ) );
  }

  /**
   * @see org.kalypso.services.proxy.ICalculationService#getSchema(java.lang.String)
   */
  public DataHandler getSchema( String arg0 ) throws RemoteException
  {
    return m_service.getSchema( arg0 );
  }

  /**
   * @see org.kalypso.services.proxy.ICalculationService#getSchemaValidity(java.lang.String)
   */
  public long getSchemaValidity( String arg0 ) throws RemoteException
  {
    return m_service.getSchemaValidity( arg0 );
  }

  /**
   * @see org.kalypso.services.proxy.ICalculationService#getSupportedSchemata()
   */
  public String[] getSupportedSchemata() throws RemoteException
  {
    return m_service.getSupportedSchemata();
  }

  /**
   * @see org.kalypso.services.proxy.ICalculationService#startJob(java.lang.String,
   *      java.lang.String, javax.activation.DataHandler,
   *      org.kalypso.services.proxy.CalcJobClientBean[],
   *      org.kalypso.services.proxy.CalcJobClientBean[])
   */
  public CalcJobInfoBean startJob( String arg0, String arg1, DataHandler arg2,
      CalcJobClientBean[] arg3, CalcJobClientBean[] arg4 ) throws RemoteException
  {
    return wrapCalcJobInfoBean( m_service.startJob( arg0, arg1, arg2, wrapCalcJobClientBeans( arg3 ), wrapCalcJobClientBeans( arg4 ) ) );
  }

  private org.kalypso.services.calculation.service.CalcJobClientBean[] wrapCalcJobClientBeans( final CalcJobClientBean[] beans )
  {
    final org.kalypso.services.calculation.service.CalcJobClientBean[] newbeans = new org.kalypso.services.calculation.service.CalcJobClientBean[beans.length];
    for( int i = 0; i < beans.length; i++ )
    {
      final CalcJobClientBean bean = beans[i];
    
      final org.kalypso.services.calculation.service.CalcJobClientBean newbean = new org.kalypso.services.calculation.service.CalcJobClientBean();
      newbean.setId( bean.getId() );
      newbean.setPath( bean.getPath() );
      
      newbeans[i] = newbean;
    }
    
    return newbeans;
  }

  /**
   * @see org.kalypso.services.proxy.ICalculationService#transferCurrentResults(java.lang.String)
   */
  public DataHandler transferCurrentResults( final String arg0 ) throws RemoteException
  {
    return m_service.transferCurrentResults( arg0 );
  }

}
