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
package org.kalypso.project.database.client.core.model.local;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.kalypso.project.database.common.nature.IRemoteProjectPreferences;
import org.kalypso.project.database.common.nature.RemoteProjectNature;

/**
 * @author Dirk Kuch
 */
public class LocalProjectHandler implements ILocalProject, IPreferenceChangeListener
{
  private final IProject m_project;

  private final LocalWorkspaceModel m_model;

  private IRemoteProjectPreferences m_preferences = null;

  public LocalProjectHandler( final IProject project, final LocalWorkspaceModel localWorkspaceModel )
  {
    m_project = project;
    m_model = localWorkspaceModel;
  }

  /**
   * @see org.kalypso.project.database.client.core.model.local.ILocalProject#getRemotePreferences()
   */
  @Override
  public IRemoteProjectPreferences getRemotePreferences( ) throws CoreException
  {
    if( m_preferences == null )
    {
      final IProjectNature nature = m_project.getNature( RemoteProjectNature.NATURE_ID );
      if( nature == null )
      {
        final IProjectDescription description = m_project.getDescription();
        final String[] natureIds = description.getNatureIds();
        ArrayUtils.add( natureIds, RemoteProjectNature.NATURE_ID );

        description.setNatureIds( natureIds );
        m_project.setDescription( description, new NullProgressMonitor() );
      }

      final RemoteProjectNature myNature = (RemoteProjectNature) nature;
      m_preferences = myNature.getRemotePreferences( m_project, this );

    }

    return m_preferences;
  }

  /**
   * @see org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener#preferenceChange(org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent)
   */
  @Override
  public void preferenceChange( final PreferenceChangeEvent event )
  {
    m_model.fireLocalUpdateEvent();
  }

  /**
   * @see org.kalypso.project.database.client.core.model.local.ILocalProject#getProject()
   */
  @Override
  public IProject getProject( )
  {
    return m_project;
  }

}
