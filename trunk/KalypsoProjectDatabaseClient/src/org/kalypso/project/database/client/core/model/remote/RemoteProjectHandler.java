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
package org.kalypso.project.database.client.core.model.remote;

import org.kalypso.afgui.extension.IKalypsoProjectOpenAction;
import org.kalypso.afgui.extension.IProjectDatabaseUiLocker;
import org.kalypso.afgui.extension.IProjectRowBuilder;
import org.kalypso.project.database.client.core.model.AbstractProjectHandler;
import org.kalypso.project.database.client.core.model.interfaces.IRemoteProject;
import org.kalypso.project.database.client.ui.project.database.internal.RemoteProjectRowBuilder;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author Dirk Kuch
 */
public class RemoteProjectHandler extends AbstractProjectHandler implements IRemoteProject
{
  private final KalypsoProjectBean m_bean;

  public RemoteProjectHandler( final KalypsoProjectBean bean )
  {
    m_bean = bean;
  }

  /**
   * @see org.kalypso.afgui.extension.IProjectHandler#getName()
   */
  @Override
  public String getName( )
  {
    return m_bean.getName();
  }

  /**
   * @see org.kalypso.afgui.extension.IProjectHandler#getUniqueName()
   */
  @Override
  public String getUniqueName( )
  {
    return m_bean.getUnixName();
  }

  /**
   * @see org.kalypso.project.database.client.core.model.IRemoteProjectHandler#getBean()
   */
  @Override
  public KalypsoProjectBean getBean( )
  {
    return m_bean;
  }

  /**
   * @see org.kalypso.afgui.extension.IProjectHandler#getBuilder()
   */
  @Override
  public IProjectRowBuilder getBuilder( final IKalypsoProjectOpenAction action, final IProjectDatabaseUiLocker locker )
  {
    return new RemoteProjectRowBuilder( this, action, locker );
  }

}
