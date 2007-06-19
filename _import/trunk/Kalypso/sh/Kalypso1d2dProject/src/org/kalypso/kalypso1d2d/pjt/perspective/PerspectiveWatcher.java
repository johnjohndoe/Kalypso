package org.kalypso.kalypso1d2d.pjt.perspective;

import java.util.Collection;
import java.util.Collections;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.kalypso1d2d.pjt.views.WorkflowView;

import de.renew.workflow.cases.Case;
import de.renew.workflow.connector.context.CaseHandlingProjectNature;
import de.renew.workflow.connector.context.IActiveContextChangeListener;

/**
 * @author Stefan Kurzbach
 */
public class PerspectiveWatcher<T extends Case> implements IActiveContextChangeListener<T>
{
  private CaseHandlingProjectNature m_currentProject;

  private T m_currentScenario;

  /**
   * @see org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener#activeProjectChanged(org.eclipse.core.resources.IProject)
   */
  public void activeContextChanged( final CaseHandlingProjectNature newProject, final T scenario )
  {
    if( newProject != m_currentProject || scenario != m_currentScenario )
    {
      final IWorkbench workbench = PlatformUI.getWorkbench();
      if( workbench.isClosing() )
        return;

      final UIJob job = new UIJob( "Perspektive öffnen" )
      {
        @SuppressWarnings("unchecked")
        @Override
        public IStatus runInUIThread( final IProgressMonitor monitor )
        {
          final IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
          final IWorkbenchPage workbenchPage = activeWorkbenchWindow.getActivePage();
          cleanPerspective( workbenchPage, Collections.EMPTY_LIST );
          return Status.OK_STATUS;
        }

      };
      job.schedule();
      m_currentProject = newProject;
      m_currentScenario = scenario;
    }
  }

  public static void cleanPerspective( final IWorkbenchPage workbenchPage, final Collection<String> partsToKeep )
  {
    final IViewReference[] viewReferences = workbenchPage.getViewReferences();
    for( final IViewReference reference : viewReferences )
    {
      if( !partsToKeep.contains( reference.getId() ) && !shouldKeepPart( reference ) )
      {
        workbenchPage.hideView( reference );
      }
    }
    final IEditorReference[] editorReferences = workbenchPage.getEditorReferences();
    for( final IEditorReference reference : editorReferences )
    {
      if( !partsToKeep.contains( reference.getId() ) && !shouldKeepPart( reference ) )
      {
        workbenchPage.closeEditor( reference.getEditor( true ), true );
      }
    }
    if( workbenchPage.getEditorReferences().length == 0 )
    {
      workbenchPage.setEditorAreaVisible( false );
    }
  }

  private static boolean shouldKeepPart( final IWorkbenchPartReference reference )
  {
    final String viewId = reference.getId();
    if( WorkflowView.ID.equals( viewId ) )
    {
      return true;
    }
    else if( Perspective.SCENARIO_VIEW_ID.equals( viewId ) )
    {
      return true;
    }
    else if( reference.getPartName().equals( "Welcome" ) )
    {
      return true;
    }
    else
    {
      return false;
    }
  }
}
