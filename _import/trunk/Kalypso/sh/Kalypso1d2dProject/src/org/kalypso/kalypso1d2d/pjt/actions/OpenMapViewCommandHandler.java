/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsosimulationmodel.core.ISimulationModelProvider;

/**
 * Opens the map view on a given resource and activates a given layer
 * 
 * @author Stefan Kurzbach
 */
public class OpenMapViewCommandHandler extends WorkflowCommandHandler implements
        IHandler {

    private static final String PARAM_RESOURCE = "org.kalypso.kalypso1d2dpjt.OpenMapViewCommand.resource";

    /**
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    protected IStatus executeInternal(final ExecutionEvent event)
            throws CoreException {
        final IEvaluationContext context = (IEvaluationContext) event
                .getApplicationContext();
        final String resource = event.getParameter(PARAM_RESOURCE);
        if (resource == null) {
            throw new CoreException(StatusUtilities
                    .createErrorStatus("Resource parameter was null."));
        }
        final IWorkbenchWindow activeWorkbenchWindow = (IWorkbenchWindow) context
                .getVariable(ISources.ACTIVE_WORKBENCH_WINDOW_NAME);
        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        final IFolder projectPath = (IFolder) context
                .getVariable(ISimulationModelProvider.ACTIVE_SIMULATION_MODEL_BASE_FOLDER_NAME);
        final IResource file = workspace.getRoot().findMember(
                projectPath.getFullPath().append(resource));

        if (file.getType() == IResource.FILE) {
            final IWorkbenchPage workbenchPage = activeWorkbenchWindow
                    .getActivePage();
            // IDE.openEditor(workbenchPage, (IFile) file);
        }
        return Status.OK_STATUS;
    }
}
