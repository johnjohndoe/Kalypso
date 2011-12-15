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
package org.kalypso.statistics.project;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;

/**
 * Project Nature for statistics projects
 *
 */
public class KalypsoStatisticsProjectNature implements IProjectNature
{
  public static final String ID = "org.kalypso.statistics.project.KalypsoStatisticsProjectNature"; //$NON-NLS-1$

  public static final boolean isOfThisNature( final IProject project ) throws CoreException
  {
    return project == null ? false : project.hasNature( ID );
  }

  public static KalypsoStatisticsProjectNature getNature( final IProject project ) throws CoreException
  {
    return (KalypsoStatisticsProjectNature) project.getNature( ID );
  }

  private IProject m_project;

  /**
   * @see org.eclipse.core.resources.IProjectNature#configure()
   */
  @Override
  public void configure( )
  {
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#deconfigure()
   */
  @Override
  public void deconfigure( )
  {
    // does nothing by default
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
    this.m_project = project;
  }
}