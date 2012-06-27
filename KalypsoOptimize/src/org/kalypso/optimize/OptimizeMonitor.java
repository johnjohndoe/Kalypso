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
package org.kalypso.optimize;

import org.kalypso.simulation.core.ISimulationMonitor;

/**
 * FIXME: actually some kind ob SubMonitor -> should be renamed
 * 
 * @author Gernot Belger
 */
public class OptimizeMonitor implements ISimulationMonitor
{
  private final ISimulationMonitor m_monitor;

  private final String m_prefix;

  public OptimizeMonitor( final ISimulationMonitor monitor )
  {
    m_monitor = monitor;
    m_prefix = monitor.getMessage();
  }

  /**
   * @see org.kalypso.simulation.core.ISimulationMonitor#getFinishStatus()
   */
  @Override
  public int getFinishStatus( )
  {
    return m_monitor.getFinishStatus();
  }

  /**
   * @see org.kalypso.simulation.core.ISimulationMonitor#getFinishText()
   */
  @Override
  public String getFinishText( )
  {
    return m_monitor.getFinishText();
  }

  /**
   * @see org.kalypso.simulation.core.ISimulationMonitor#getMessage()
   */
  @Override
  public String getMessage( )
  {
    return m_monitor.getMessage();
  }

  /**
   * @see org.kalypso.simulation.core.ISimulationMonitor#getProgress()
   */
  @Override
  public int getProgress( )
  {
    return m_monitor.getProgress();
  }

  /**
   * @see org.kalypso.simulation.core.ISimulationMonitor#setFinishInfo(int, java.lang.String)
   */
  @Override
  public void setFinishInfo( final int status, final String text )
  {
    m_monitor.setFinishInfo( status, text );
  }

  /**
   * @see org.kalypso.simulation.core.ISimulationMonitor#setMessage(java.lang.String)
   */
  @Override
  public void setMessage( final String message )
  {
    final String prefixedMessage = String.format( "%s - %s", m_prefix, message );
    setMessageInternal( prefixedMessage );
  }

  private void setMessageInternal( final String message )
  {
    if( m_monitor instanceof OptimizeMonitor )
      ((OptimizeMonitor) m_monitor).setMessageInternal( message );
    else
      m_monitor.setMessage( message );
  }

  /**
   * @see org.kalypso.simulation.core.ISimulationMonitor#setProgress(int)
   */
  @Override
  public void setProgress( final int progress )
  {
// ignore, do not disturb outer monitor
  }

  /**
   * @see org.kalypso.contribs.java.lang.ICancelable#cancel()
   */
  @Override
  public void cancel( )
  {
    m_monitor.cancel();
  }

  /**
   * @see org.kalypso.contribs.java.lang.ICancelable#isCanceled()
   */
  @Override
  public boolean isCanceled( )
  {
    return m_monitor.isCanceled();
  }

}
