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
package org.kalypso.ui.action;

import java.io.File;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.debug.internal.ui.actions.StatusInfo;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author tgu
 */
public class ModelActionHelper
{
  private ModelActionHelper()
  {
  // wir nicht instatiiert
  }

  public static File getServerRoot() throws CoreException
  {
    final File serverRoot = KalypsoGisPlugin.getDefault().getServerModelRoot();
    if( serverRoot == null )
      throw new CoreException( new StatusInfo( IStatus.WARNING,
          "Die Liste der auf dem Server gespeicherten Modelle ist nicht verfügbar." ) );

    return serverRoot;
  }

  /**
   * Prüft, ob genau ein serverseitig-gespiegeltes Projekt ausgewählt wurde und
   * gibt das Projekt zurück
   * 
   * @param window
   * @return project
   * @throws CoreException
   */
  public final static IProject chooseOneProject( final IWorkbenchWindow window )
      throws CoreException
  {
    final ISelection selection = window.getSelectionService().getSelection( IPageLayout.ID_RES_NAV );

    final IProject[] projects = ResourceUtilities.findeProjectsFromSelection( selection );

    if( projects == null || projects.length == 0 )
      throw new CoreException( new StatusInfo( IStatus.WARNING,
          "Kein Projekt im Navigator selektiert." ) );

    if( projects.length > 1 )
      throw new CoreException( new StatusInfo( IStatus.WARNING,
          "Mehr als ein Projekt im Navigator selektiert." ) );

    return projects[0];
  }

  public static File checkIsSeverMirrored( final File serverRoot, final IProject project )
      throws CoreException
  {
    final File serverProject = new File( serverRoot, project.getName() );
    if( !serverProject.exists() )
      throw new CoreException(
          new StatusInfo(
              IStatus.WARNING,
              "Sie haben kein Server-gespeichertes Projekt gewählt.\nNur Server-gespeicherte Projekt können aktualisiert werden." ) );

    return serverProject;
  }
}