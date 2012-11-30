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
package org.kalypso.ui.rrm.internal.welcome;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.resources.ProjectTemplate;
import org.kalypso.module.INewProjectHandler;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;

/**
 * @author Gernot Belger
 */
public class KalypsoRrmNewProjectHandler implements INewProjectHandler
{
  @Override
  public IStatus postCreateProject( final IProject project, final ProjectTemplate template, final IProgressMonitor monitor ) throws CoreException
  {
    final String localeId = template.getId();

    final IFolder localeFolder = project.getFolder( "nl" ); //$NON-NLS-1$
    final IFolder localeData = localeFolder.getFolder( localeId ); //$NON-NLS-1$

    /* Move all locale data into the project root */
    final File localDataDir = localeData.getLocation().toFile();
    final File projectDir = project.getLocation().toFile();

    try
    {
      FileUtils.copyDirectory( localDataDir, projectDir );
      project.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
    }
    catch( final IOException e )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to move localized data into project", e ); //$NON-NLS-1$
      throw new CoreException( status );
    }

    /* Delete all locale folders */
    localeFolder.delete( false, new NullProgressMonitor() );

    return Status.OK_STATUS;
  }

  @Override
  public void openProject( final IProject project )
  {
  }
}