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
import java.util.Vector;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.floodrisk.process.ProcessExtension;
import org.kalypso.floodrisk.process.ProcessExtensionReader;
import org.kalypso.floodrisk.schema.UrlCatalogFloodRisk;
import org.kalypso.services.calculation.ICalculationServiceProxyFactory;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.local.CalcJobServiceProxyWrapper;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypso.services.calculation.service.impl.ICalcJobFactory;
import org.kalypso.services.proxy.ICalculationService;

/**
 * LocalCalculationServiceFactory
 * <p>
 * 
 * created by
 * 
 * @author Nadja Peiler (07.06.2005)
 */
public class LocalCalculationServiceFactory implements
    ICalculationServiceProxyFactory
{

  /**
   * 
   * @see org.kalypso.services.calculation.ICalculationServiceProxyFactory#createService()
   */
  public ICalculationService createService()
  {
    try
    {
      //get registered process extensions
      ProcessExtension[] extensions = ProcessExtensionReader
          .retrieveExtensions();
      //create CalcJobFactory for local processes
      ICalcJobFactory localCalcJobFactory = createCalcJobFactoryForLocalProcesses( extensions );
      return new CalcJobServiceProxyWrapper( new LocalCalculationService(
          localCalcJobFactory, new UrlCatalogFloodRisk() ) );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }
    catch( RemoteException e )
    {
      e.printStackTrace();
    }
    return null;
  }

  /**
   * 
   * @param extensions ProcessExtensions
   * @return LocalCalcJobFactory
   *  
   */
  private ICalcJobFactory createCalcJobFactoryForLocalProcesses(
      ProcessExtension[] extensions )
  {
    Vector localProcessExtensions = new Vector();
    for( int i = 0; i < extensions.length; i++ )
    {
      if( extensions[i].getType().equals( "local" ) )
      {
        localProcessExtensions.add( extensions[i] );
      }
    }
    return new LocalCalcJobFactory( (ProcessExtension[])localProcessExtensions
        .toArray( new ProcessExtension[localProcessExtensions.size()] ) );

  }

  /**
   * 
   * LocalCalcJobFactory
   * <p>
   * Factory for local calculation jobs
   * 
   * created by
   * 
   * @author Nadja Peiler (17.06.2005)
   */
  class LocalCalcJobFactory implements ICalcJobFactory
  {

    private ProcessExtension[] m_localExtensions;

    /**
     * Constructor
     * 
     * @param localExtensions
     */
    LocalCalcJobFactory( ProcessExtension[] localExtensions )
    {
      m_localExtensions = localExtensions;
    }

    /**
     * 
     * @see org.kalypso.services.calculation.service.impl.ICalcJobFactory#getSupportedTypes()
     */
    public String[] getSupportedTypes()
    {
      String[] supportedTypes = new String[m_localExtensions.length];
      for( int i = 0; i < m_localExtensions.length; i++ )
      {
        supportedTypes[i] = m_localExtensions[i].getId();
      }
      return supportedTypes;
    }

    /**
     * 
     * @see org.kalypso.services.calculation.service.impl.ICalcJobFactory#createJob(java.lang.String)
     */
    public ICalcJob createJob( String typeId ) throws CalcJobServiceException
    {
      ICalcJob calcJob = null;
      for( int i = 0; i < m_localExtensions.length; i++ )
      {
        if( m_localExtensions[i].getId().equals( typeId ) )
        {
          calcJob = m_localExtensions[i].getCalcJob();
          break;
        }
      }
      return calcJob;
    }
  }
}

