package org.kalypso.ui.repository.dialogs;

import java.text.NumberFormat;
import java.text.ParseException;

import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.swt.widgets.Shell;

/**
 * Simple input dialog for setting the number of days.
 * 
 * @author schlienger
 */
public class NumberOfDaysInputDialog extends InputDialog
{
  private final static String msg = "";
  
  protected final static NumberFormat m_nf = NumberFormat.getIntegerInstance();

  private final int m_initialValue;
  
  
  public NumberOfDaysInputDialog( final Shell parentShell, int initialValue )
  {
    super( parentShell, "Konfiguration", msg, m_nf.format(initialValue), new DayInputValidator() );
    
    m_initialValue = initialValue;
  }

  public int getDays()
  {
    final String sv = getValue();
    
    try
    {
      return m_nf.parse( sv ).intValue();
    }
    catch( ParseException e )
    {
      return m_initialValue;
    }
  }
  
  /**
   * Simple input validation: checks if number of days is greater than zero and valid integer.
   * 
   * @author schlienger
   */
  private static class DayInputValidator implements IInputValidator
  {
    /**
     * @see org.eclipse.jface.dialogs.IInputValidator#isValid(java.lang.String)
     */
    public String isValid( final String newText )
    {
      try
      {
        final Number number = m_nf.parse( newText );
        
        if( number.intValue() <= 0 )
          return "Tagesanzahl muss größer als '0' sein.";
        
        return null;
      }
      catch( final ParseException e )
      {
        return e.getLocalizedMessage();
      }
    }
  }
}
