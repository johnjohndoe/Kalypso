package org.kalypso.ui.wizard.ocs;

import org.eclipse.jface.viewers.DialogCellEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * OcsDialogCellEditor
 * 
 * @author schlienger
 */
public class OcsDialogCellEditor extends DialogCellEditor
{
  public OcsDialogCellEditor( )
  {
    super();
  }

  public OcsDialogCellEditor( Composite parent )
  {
    super( parent );
  }

  public OcsDialogCellEditor( Composite parent, int style )
  {
    super( parent, style );
  }

  /**
   * @see org.eclipse.jface.viewers.DialogCellEditor#openDialogBox(org.eclipse.swt.widgets.Control)
   */
  protected Object openDialogBox( Control cellEditorWindow )
  {
    return null;
  }
}
