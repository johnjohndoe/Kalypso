package org.kalypso.eclipse.jface.viewers;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

/**
 * @author belger
 */
public class DialogCellEditor extends CellEditor
{
  private Object m_value;

  public DialogCellEditor()
  {
    super();
  }

  public DialogCellEditor( final Composite parent )
  {
    super( parent );
  }

  public DialogCellEditor( final Composite parent, int style )
  {
    super( parent, style );
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#createControl(org.eclipse.swt.widgets.Composite)
   */
  protected Control createControl( final Composite parent )
  {
    final Text text = new Text( parent, SWT.NONE );
    text.setText( "editing" );
    return text;
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#doGetValue()
   */
  protected Object doGetValue()
  {
    return m_value;
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#doSetFocus()
   */
  protected void doSetFocus()
  {
   // oder hier?
    final boolean b = MessageDialog.openConfirm( getControl().getShell(), "Editor", "Hall" );

    if( b )
    {
      setValueValid( true );
      fireApplyEditorValue();
    }
    else
      fireCancelEditor();
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#doSetValue(java.lang.Object)
   */
  protected void doSetValue( final Object value )
  {
    m_value = value;
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#activate()
   */
  public void activate()
  {
    super.activate();
  }
}
