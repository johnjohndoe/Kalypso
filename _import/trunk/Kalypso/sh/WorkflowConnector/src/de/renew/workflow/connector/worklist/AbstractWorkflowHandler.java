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
package de.renew.workflow.connector.worklist;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.HandlerEvent;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;

import de.renew.workflow.connector.IWorklistChangeListener;
import de.renew.workflow.connector.WorkflowConnector;

/**
 * @author Stefan Kurzbach
 * 
 */
public abstract class AbstractWorkflowHandler extends AbstractHandler implements IWorklistChangeListener, IExecutableExtension
{

  public static final String TASK = "task";

  private boolean m_enabled;

  private String m_task;

  public AbstractWorkflowHandler( )
  {
    WorkflowConnector.getConnector().addWorklistChangeListener( this );
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#isEnabled()
   */
  @Override
  public boolean isEnabled( )
  {
    return m_enabled;
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#dispose()
   */
  @Override
  public void dispose( )
  {
    WorkflowConnector.getConnector().removeWorklistChangeListener( this );
    super.dispose();
  }

  /**
   * @see de.renew.workflow.connector.IWorklistChangeListener#worklistChanged()
   */
  public void worklistChanged( )
  {
    final boolean newEnabled = WorkflowConnector.getConnector().canRequest( m_task );
    if( m_enabled != newEnabled )
    {
      m_enabled = newEnabled;
      fireHandlerChanged( new HandlerEvent( this, true, false ) );
    }
  }

  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  @SuppressWarnings("unchecked")
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
    if( data instanceof Map )
    {
      final Map<String, String> map = (Map<String, String>) data;
      m_task = map.get( TASK );
    }
  }

  public String getTask( )
  {
    return m_task;
  }
}
