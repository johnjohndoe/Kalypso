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