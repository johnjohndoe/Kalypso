package org.kalypso.eclipse.jface.viewers;

import org.eclipse.jface.viewers.ICellEditorValidator;

/**
 * @author gernot
 */
public class DefaultCellValidators
{
  public static final ICellEditorValidator DOUBLE_VALIDATOR = new DoubleCellValidator();
  
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
        {
          Double.parseDouble(value.toString());
          System.out.println( "Valid: " + value.toString() );
        }
        
        return null;
      }
      catch( final NumberFormatException nfe )
      {
        System.out.println( "Not valid: " + value.toString() );
        return "Dezimalzahl erwartet (z.B. 10.4)";
      }
    }

  }

}
