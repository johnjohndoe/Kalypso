package org.kalypso.afgui.scenarios;

import java.util.Collection;
import java.util.Collections;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.afgui.perspective.Perspective;
import org.kalypso.afgui.views.WorkflowView;

import de.renew.workflow.cases.Case;
import de.renew.workflow.connector.cases.CaseHandlingProjectNature;
import de.renew.workflow.connector.context.IActiveContextChangeListener;

/**
 * @author Stefan Kurzbach
 */
public class PerspectiveWatcher<T extends Case> implements IActiveContextChangeListener<T>
{

  public static final String SCENARIO_VIEW_ID = "org.kalypso.kalypso1d2d.pjt.views.ScenarioView"; //$NON-NLS-1$

  private CaseHandlingProjectNature m_currentProject;

  private T m_currentScenario;

  public PerspectiveWatcher( final T currentScenario )
  {
    m_currentScenario = currentScenario;
  }

  /**
   * @see org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener#activeProjectChanged(org.eclipse.core.resources.IProject)
   */
  public void activeContextChanged( final CaseHandlingProjectNature newProject, final T scenario )
  {
    if( newProject != m_currentProject || scenario != m_currentScenario )
    {
      final IWorkbench workbench = PlatformUI.getWorkbench();
      cleanPerspective( workbench, Collections.EMPTY_LIST );
      m_currentProject = newProject;
      m_currentScenario = scenario;
    }
  }

  public static void cleanPerspective( final IWorkbench workbench, final Collection<String> partsToKeep )
  {
    final UIJob job = new UIJob( Messages.getString( "PerspectiveWatcher.0" ) ) //$NON-NLS-1$
    {
      @SuppressWarnings("unchecked")
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
        if( activeWorkbenchWindow == null )
          return Status.CANCEL_STATUS;
        IWorkbenchPage workbenchPage = activeWorkbenchWindow.getActivePage();
        if( workbenchPage == null )
          return Status.CANCEL_STATUS;

        // remember previous perspective
        final IPerspectiveDescriptor perspective = workbenchPage.getPerspective();
        final String previousPerspectiveId = perspective.getId();

        try
        {
          // show workflow perspective
          // these are the new page and its window, it might be a different one
          workbenchPage = workbench.showPerspective( Perspective.ID, activeWorkbenchWindow );
          activeWorkbenchWindow = workbenchPage.getWorkbenchWindow();
        }
        catch( final WorkbenchException e )
        {
          return e.getStatus();
        }

        // close all unnecessary views and editors in workflow perspective
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

        try
        {
          // convert to previous perspective
          workbench.showPerspective( previousPerspectiveId, activeWorkbenchWindow );
        }
        catch( final WorkbenchException e )
        {
          return e.getStatus();
        }

        return Status.OK_STATUS;
      }

      private boolean shouldKeepPart( final IWorkbenchPartReference reference )
      {
        final String viewId = reference.getId();
        if( WorkflowView.ID.equals( viewId ) )
          return true;
        else if( SCENARIO_VIEW_ID.equals( viewId ) )
          return true;
        else if( reference.getPartName().equals( "Welcome" ) )
          return true;
        else
          return false;
      }
    };
    job.schedule();
  }
}
