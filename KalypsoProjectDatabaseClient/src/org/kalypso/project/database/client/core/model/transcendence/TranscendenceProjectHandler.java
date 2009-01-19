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
package org.kalypso.project.database.client.core.model.transcendence;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.afgui.extension.IKalypsoProjectOpenAction;
import org.kalypso.afgui.extension.IProjectDatabaseUiLocker;
import org.kalypso.afgui.extension.IProjectRowBuilder;
import org.kalypso.project.database.client.core.model.AbstractProjectHandler;
import org.kalypso.project.database.client.core.model.interfaces.ILocalProject;
import org.kalypso.project.database.client.core.model.interfaces.IRemoteProject;
import org.kalypso.project.database.client.core.model.interfaces.ITranscendenceProject;
import org.kalypso.project.database.client.ui.project.database.internal.TranscendenceProjectRowBuilder;
import org.kalypso.project.database.common.nature.IRemoteProjectPreferences;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author Dirk Kuch
 */
public class TranscendenceProjectHandler extends AbstractProjectHandler implements ITranscendenceProject
{

  private final ILocalProject m_local;

  private final IRemoteProject m_remote;

  public TranscendenceProjectHandler( final ILocalProject local, final IRemoteProject remote )
  {
    m_local = local;
    m_remote = remote;
  }

  /**
   * @see org.kalypso.project.database.client.core.model.interfaces.ILocalProject#getProject()
   */
  @Override
  public IProject getProject( )
  {
    return m_local.getProject();
  }

  /**
   * @see org.kalypso.project.database.client.core.model.interfaces.ILocalProject#getRemotePreferences()
   */
  @Override
  public IRemoteProjectPreferences getRemotePreferences( ) throws CoreException
  {
    return m_local.getRemotePreferences();
  }

  /**
   * @see org.kalypso.afgui.extension.IProjectHandler#getName()
   */
  @Override
  public String getName( )
  {
    return m_local.getName();
  }

  /**
   * @see org.kalypso.afgui.extension.IProjectHandler#getUniqueName()
   */
  @Override
  public String getUniqueName( )
  {
    return m_local.getUniqueName();
  }

  /**
   * @see org.kalypso.project.database.client.core.model.interfaces.IRemoteProject#getBean()
   */
  @Override
  public KalypsoProjectBean getBean( )
  {
    return m_remote.getBean();
  }

  /**
   * @see org.kalypso.afgui.extension.IProjectHandler#getBuilder()
   */
  @Override
  public IProjectRowBuilder getBuilder( final IKalypsoProjectOpenAction action, final IProjectDatabaseUiLocker locker )
  {
    return new TranscendenceProjectRowBuilder( this, action, locker );
  }
}
