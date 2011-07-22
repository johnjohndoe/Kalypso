package org.kalypso.model.wspm.tuhh.ui.light.documents;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.contribs.eclipse.core.commands.HandlerUtils;

public class ShowDocumentsHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );

    try
    {
      final IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindowChecked( event );
      final IWorkbenchPage page = window.getActivePage();
      page.showView( WspmLightFeatureView.WSPMLIGHTFEATUREVIEW_ID, null, IWorkbenchPage.VIEW_ACTIVATE );
    }
    catch( final PartInitException e )
    {
      e.printStackTrace();
      final String title = HandlerUtils.getCommandName( event );
      ErrorDialog.openError( shell, title, "Failed to open documents view", e.getStatus() ); //$NON-NLS-1$
    }

    return null;
  }
}