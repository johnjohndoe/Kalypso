package org.kalypso.eclipse.jface.viewers;

import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.widgets.Composite;

/**
 * @author gernot
 */
public class DoubleCellEditor extends TextCellEditor
{
  public DoubleCellEditor()
  {
    super();
    
    setValidator( DefaultCellValidators.DOUBLE_VALIDATOR );
  }

  public DoubleCellEditor( final Composite parent )
  {
    super( parent );
  }

  public DoubleCellEditor( final Composite parent, final int style )
  {
    super( parent, style );
  }
  
  /**
   * @see org.eclipse.jface.viewers.CellEditor#doGetValue()
   */
  protected Object doGetValue()
  {
    final Object object = super.doGetValue();
    final String value = object == null ? null : object.toString();
    return new Double( value );
  }
  /**
   * @see org.eclipse.jface.viewers.CellEditor#doSetValue(java.lang.Object)
   */
  protected void doSetValue( final Object value )
  {
    super.doSetValue( value == null ? "" : ((Double)value).toString() );
  }
}
