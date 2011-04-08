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

import net.opengeospatial.wps.ExecuteResponseType;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.service.wps.client.exceptions.WPSException;
import org.kalypso.service.wps.refactoring.IWPSObserver;

/**
 * @author kurzbach
 *
 */
public class RMAKalypsoSimulationObserver implements IWPSObserver
{

  /**
   * @see org.kalypso.service.wps.refactoring.IWPSObserver#handleAccepted(net.opengeospatial.wps.ExecuteResponseType)
   */
  @Override
  public void handleAccepted( ExecuteResponseType exState )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.service.wps.refactoring.IWPSObserver#handleCancel()
   */
  @Override
  public IStatus handleCancel( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.service.wps.refactoring.IWPSObserver#handleFailed(net.opengeospatial.wps.ExecuteResponseType)
   */
  @Override
  public IStatus handleFailed( ExecuteResponseType exState )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.service.wps.refactoring.IWPSObserver#handleStarted(org.eclipse.core.runtime.IProgressMonitor, net.opengeospatial.wps.ExecuteResponseType)
   */
  @Override
  public void handleStarted( IProgressMonitor monitor, ExecuteResponseType exState ) throws WPSException
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.service.wps.refactoring.IWPSObserver#handleSucceeded(net.opengeospatial.wps.ExecuteResponseType)
   */
  @Override
  public IStatus handleSucceeded( ExecuteResponseType exState )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.service.wps.refactoring.IWPSObserver#handleTimeout()
   */
  @Override
  public IStatus handleTimeout( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.service.wps.refactoring.IWPSObserver#handleUnknownState(net.opengeospatial.wps.ExecuteResponseType)
   */
  @Override
  public IStatus handleUnknownState( ExecuteResponseType exState )
  {
    // TODO Auto-generated method stub
    return null;
  }

}
