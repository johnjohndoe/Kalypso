package org.kalypso.ui.editor.mapeditor.actiondelegates;

import org.eclipse.jface.action.IAction;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.kalypso.ogc.gml.map.widgets.EditFeatureWidget;


/**
 * @author belger
 */
public class EditFeatureWidgetDelegate extends AbstractWidgetActionDelegate
{
  public EditFeatureWidgetDelegate(  )
  {
    super( new EditFeatureWidget() );
  }
  
  /**
   * @see org.kalypso.ui.editor.mapeditor.actiondelegates.AbstractWidgetActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction, org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( IAction action, IEditorPart targetEditor )
  {
    super.setActiveEditor( action, targetEditor );
    
    final Shell shell = targetEditor == null ? null : targetEditor.getEditorSite().getShell();
    
    ((EditFeatureWidget)getWidget()).setShell( shell );
  }
}
