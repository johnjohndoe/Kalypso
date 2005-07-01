package org.kalypso.interpolation.action;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.kalypso.interpolation.grid.GridFactory;
import org.kalypso.interpolation.grid.IGrid;
import org.kalypso.interpolation.mesh.Mesh;
import org.kalypso.interpolation.wizard.GridInterpolationWizard;

/**
 * Our sample action implements workbench action delegate. The action proxy will be created by the workbench and shown
 * in the UI. When the user tries to use the action, this delegate will be created and execution will be delegated to
 * it.
 * 
 * @see IWorkbenchWindowActionDelegate
 */
public class GridInterpolationAction implements IWorkbenchWindowActionDelegate
{
  private IWorkbenchWindow window;

  private IGrid grid;

  public GridInterpolationAction()
  {
    System.out.println( "constructor grid action" );
  }

  /**
   * The action has been activated. The argument of the method represents the 'real' action sitting in the workbench UI.
   * 
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( IAction action )
  {
    MessageDialog.openInformation( window.getShell(), "KalypsoGridInterpolation", "Interpolate grid" );
    GridInterpolationWizard wizard = new GridInterpolationWizard();
    WizardDialog dialog = new WizardDialog( window.getShell(), wizard );
    dialog.open();
    Mesh mesh = wizard.getMesh();
    try
    {
      grid = GridFactory.getInstance().createGrid( null, wizard.getCoordinateSystem(), wizard.getSize(), mesh );
      //String target = "";
      mesh.interpolateGrid( wizard.getTarget(), wizard.getBorderLine(), grid );
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  /**
   * Selection in the workbench has been changed. We can change the state of the 'real' action here if we want, but this
   * can only happen after the delegate has been created.
   * 
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {}

  /**
   * We can use this method to dispose of any system resources we previously allocated.
   * 
   * @see IWorkbenchWindowActionDelegate#dispose
   */
  public void dispose()
  {}

  /**
   * We will cache window object in order to be able to provide parent shell for the message dialog.
   * 
   * @see IWorkbenchWindowActionDelegate#init
   */
  public void init( IWorkbenchWindow wnd )
  {
    this.window = wnd;
  }
}