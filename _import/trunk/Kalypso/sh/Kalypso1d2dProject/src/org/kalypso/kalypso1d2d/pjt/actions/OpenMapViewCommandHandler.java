/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.ide.IDE;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.pjt.SzenarioSourceProvider;
import org.kalypso.ui.command.WorkflowCommandHandler;

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
        final String resource = "maps/base.gmt";//event.getParameter(PARAM_RESOURCE);
        if (resource == null) {
            throw new CoreException(StatusUtilities
                    .createErrorStatus("Resource parameter was null."));
        }
        final IWorkbenchWindow activeWorkbenchWindow = (IWorkbenchWindow) context
                .getVariable(ISources.ACTIVE_WORKBENCH_WINDOW_NAME);
        final IFolder szenarioPath = (IFolder) context
                .getVariable(SzenarioSourceProvider.ACTIVE_SZENARIO_FOLDER_NAME);
        
        final IFile file = szenarioPath.getFile( new Path(resource) );
        
        if (file.exists()) {
            final IWorkbenchPage workbenchPage = activeWorkbenchWindow
                    .getActivePage();
             IDE.openEditor(workbenchPage, file);
        }
        return Status.OK_STATUS;
    }
}
