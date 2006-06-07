package org.kalypso.model.wspm.ui.editor.actions;

import org.eclipse.ui.IEditorPart;
import org.kalypso.contribs.eclipse.ui.AbstractEditorPartAction;
import org.kalypso.model.wspm.ui.editor.ProfilchartEditor;


/**
 * @author gernot
 */
public class ShowTableAction extends AbstractEditorPartAction
{
  @Override
  public void run( )
  {
    final IEditorPart editor = getEditorPart();
    if( editor instanceof ProfilchartEditor )
      ((ProfilchartEditor)editor).openTableview();
  }
}
