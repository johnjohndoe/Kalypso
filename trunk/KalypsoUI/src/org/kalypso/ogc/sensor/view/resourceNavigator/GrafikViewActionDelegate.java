package org.kalypso.ogc.sensor.view.resourceNavigator;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.kalypso.template.ObservationTemplateHelper;

/**
 * @author schlienger
 */
public class GrafikViewActionDelegate implements IViewActionDelegate
{
  private IFile m_currentFile = null;

  public GrafikViewActionDelegate()
  {
  // empty
  }

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
    if( m_currentFile != null )
    {
      if( m_currentFile.getFileExtension().equalsIgnoreCase(
          ObservationTemplateHelper.ODT_FILE_EXTENSION ) )
        ObservationTemplateHelper.openGrafik4odt( m_currentFile );
      else if( m_currentFile.getFileExtension().equalsIgnoreCase(
          ObservationTemplateHelper.TPL_FILE_EXTENSION ) )
        ObservationTemplateHelper.openGrafik4tpl( m_currentFile );
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    IStructuredSelection sel = (IStructuredSelection)selection;

    if( sel.getFirstElement() instanceof IFile )
      m_currentFile = (IFile)sel.getFirstElement();
    else
      m_currentFile = null;
  }
}