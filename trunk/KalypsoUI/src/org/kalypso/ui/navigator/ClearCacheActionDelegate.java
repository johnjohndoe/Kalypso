package org.kalypso.ui.navigator;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Clears all the cache of the Kalypso Plugin.
 * 
 * @author schlienger
 */
public class ClearCacheActionDelegate implements IViewActionDelegate
{
  /**
   * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
   */
  public void init( IViewPart view )
  {
    // empty
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( IAction action )
  {
    // TODO: wird das noch ben�tigt?
    //KalypsoGisPlugin.getDefault().getPool().clear();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    // empty
  }
}
