package org.kalypso.afgui.scenarios;

import java.util.Collection;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.afgui.perspective.Perspective;
import org.kalypso.afgui.views.WorkflowView;

import de.renew.workflow.base.ITask;
import de.renew.workflow.contexts.ContextType;
import de.renew.workflow.contexts.PerspectiveContextType;

/**
 * @author Stefan Kurzbach
 */
public class PerspectiveWatcher
{
  public static final String SCENARIO_VIEW_ID = "org.kalypso.kalypso1d2d.pjt.views.ScenarioView"; //$NON-NLS-1$

  /**
   * This function cleans up the perspective.
   *
   * @param workbench
   *            The workbench.
   * @param partsToKeep
   *            This parts will be kept, aside of the important parts.
   */
  public static void cleanPerspective( final IWorkbench workbench, final Collection<String> partsToKeep )
  {
    final UIJob job = new UIJob( Messages.getString( "PerspectiveWatcher.0" ) ) //$NON-NLS-1$
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        final IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
        if( activeWorkbenchWindow == null )
          return Status.CANCEL_STATUS;

        final IWorkbenchPage workbenchPage = activeWorkbenchWindow.getActivePage();
        if( workbenchPage == null )
          return Status.CANCEL_STATUS;

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

  /**
   * This function checks each tasks parent, until a perspective context is found. Than the id of the perspecive
   * configured there will be returned.
   *
   * @param context
   *            The context, to start the search with.
   *
   * @return The perspective id.
   */
  public static String getPerspectiveID( final ITask task )
  {
    if( task == null )
      return Perspective.ID;

    ContextType context = task.getContext();
    while( context != null )
    {
      if( context instanceof PerspectiveContextType )
      {
        final PerspectiveContextType perspectiveContext = (PerspectiveContextType) context;
        return perspectiveContext.getPerspectiveId();
      }

      context = context.getParent();
    }

    return Perspective.ID;
  }
}