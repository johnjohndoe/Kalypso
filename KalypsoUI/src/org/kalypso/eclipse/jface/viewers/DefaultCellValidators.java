package org.kalypso.eclipse.jface.viewers;

import org.eclipse.jface.viewers.ICellEditorValidator;

/**
 * @author gernot
 */
public class DefaultCellValidators
{
  public static final ICellEditorValidator DOUBLE_VALIDATOR = new DoubleCellValidator();
  public static final ICellEditorValidator INTEGER_VALIDATOR = new IntegerCellValidator();
  
  public static final class DoubleCellValidator implements ICellEditorValidator
  {
    /**
     * @see org.eclipse.jface.viewers.ICellEditorValidator#isValid(java.lang.Object)
     */
    public String isValid( final Object value )
    {
      try
      {
        if( value != null )
          Double.parseDouble(value.toString());
        
        return null;
      }
      catch( final NumberFormatException nfe )
      {
        return "Dezimalzahl erwartet (z.B. 10.4)";
      }
    }

  }
  
  public static final class IntegerCellValidator implements ICellEditorValidator
  {
    /**
     * @see org.eclipse.jface.viewers.ICellEditorValidator#isValid(java.lang.Object)
     */
    public String isValid( final Object value )
    {
      try
      {
        if( value != null )
          Integer.parseInt(value.toString());
        
        return null;
      }
      catch( final NumberFormatException nfe )
      {
        return "Ganzzahl erwartet (z.B. 10)";
      }
    }

  }

}
