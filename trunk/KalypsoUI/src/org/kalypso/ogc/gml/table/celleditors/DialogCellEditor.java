package org.kalypso.ogc.gml.table.celleditors;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

/**
 * @author Belger
 */
public abstract class DialogCellEditor extends CellEditor
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
    final Text text = new Text( parent, SWT.CENTER );
    text.setText( "<Element wird gerade editiert>" );
    text.setBackground( parent.getDisplay().getSystemColor( SWT.COLOR_DARK_GRAY ) );
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
    if( openDialog( getControl() ) )
    {
      setValueValid( true );
      fireApplyEditorValue();
    }
    else
      fireCancelEditor();
  }

  protected abstract boolean openDialog( final Control control );

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
