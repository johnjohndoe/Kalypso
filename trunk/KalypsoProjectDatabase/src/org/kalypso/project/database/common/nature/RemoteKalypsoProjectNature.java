/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.project.database.common.nature;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.project.database.KalypsoProjectDatabase;

/**
 * ProjectNature of remote Kalypso Projects
 * 
 * @author Dirk Kuch
 */
public class RemoteKalypsoProjectNature implements IProjectNature
{
  private static final String NATURE_ID = "org.kalypso.project.database.project.nature";

  IProject m_project = null;

  /**
   * This function returns the nature of the Planer-Client if the project is Planer-Client project.
   * 
   * @return The nature or null.
   */
  public static RemoteKalypsoProjectNature getNature( final IProject project )
  {
    try
    {
      return (RemoteKalypsoProjectNature) project.getNature( NATURE_ID );
    }
    catch( final CoreException ex )
    {
      /* Log the error. */
      KalypsoProjectDatabase.getDefault().getLog().log( ex.getStatus() );

      return null;
    }
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#configure()
   */
  @Override
  public void configure( ) throws CoreException
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#deconfigure()
   */
  @Override
  public void deconfigure( ) throws CoreException
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#getProject()
   */
  @Override
  public IProject getProject( )
  {
    return m_project;
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#setProject(org.eclipse.core.resources.IProject)
   */
  @Override
  public void setProject( final IProject project )
  {
    m_project = project;
  }

}
