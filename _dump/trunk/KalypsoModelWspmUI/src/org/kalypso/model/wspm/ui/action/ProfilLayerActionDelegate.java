/**
 * 
 */
package org.kalypso.model.wspm.ui.action;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.PartInitException;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;

/**
 * @author Belger
 */
public class ProfilLayerActionDelegate implements IObjectActionDelegate
{
  private IWorkbenchPart m_targetPart;

  public void setActivePart( final IAction action, final IWorkbenchPart targetPart )
  {
    m_targetPart = targetPart;
  }

  public void run( final IAction action )
  {
    final IWorkbenchPartSite site = m_targetPart.getSite();
    try
    {
      site.getPage().showView( "com.bce.profil.eclipse.view.LayerView" );
    }
    catch( final PartInitException ex )
    {
      ErrorDialog.openError( site.getShell(), "Profil-Legende",
          "Konnte Themeneigenschaften nicht öffnen", ex.getStatus() );
      KalypsoModelWspmUIPlugin.getDefault().getLog().log( ex.getStatus() );
    }
  }

  public void selectionChanged( final IAction action, final ISelection selection )
  {
  }
}
