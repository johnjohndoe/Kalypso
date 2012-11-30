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
package org.kalypso.ui.rrm.internal.utils;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.resources.FolderUtilities;
import org.kalypso.model.hydrology.project.RrmProject;
import org.kalypso.module.ISetAsBaseScenarioHandler;

import de.renew.workflow.connector.cases.CopyScenarioContentsOperation;

/**
 * @author Holger Albert
 */
public class KalypsoRrmSetAsBaseScenarioHandler implements ISetAsBaseScenarioHandler
{
  @Override
  public IStatus postCreateProject( final IProject sourceProject, final IProject targetProject, final IProgressMonitor monitor ) throws CoreException
  {
    final RrmProject sourceRrmProject = new RrmProject( sourceProject );
    final RrmProject targetRrmProject = new RrmProject( targetProject );

    final IFolder sourceFolder = sourceRrmProject.getTimeseriesFolder();
    final IFolder targetFolder = targetRrmProject.getTimeseriesFolder();
    if( !targetFolder.exists() )
      FolderUtilities.mkdirs( targetFolder );

    final CopyScenarioContentsOperation operation = new CopyScenarioContentsOperation( sourceFolder, targetFolder, new IFolder[] {}, null );
    return operation.execute( monitor );
  }
}