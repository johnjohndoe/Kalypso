package org.kalypso.ui.repository.file;

import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * @author schlienger
 */
public class ZmlRepositoryConfigDialog extends InputDialog
{
  private final static String msg = "hier kommt der message...";
  
  private String m_location;
  private String m_filters;

  private DirectoryFieldEditor m_dir;
  
  
  
  public ZmlRepositoryConfigDialog( final Shell parentShell, final String location, final String filters )
  {
    super( parentShell, "ZML-Repository Konfiguration", msg, filters, null );
    
    m_location = location;
    m_filters = filters;
  }


  /**
   * @see org.eclipse.jface.dialogs.InputDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  protected Control createDialogArea( final Composite parent )
  {
    final Composite c = (Composite)super.createDialogArea( parent );
    
    Composite sub = new Composite( c, SWT.BEGINNING );
    
    m_dir = new DirectoryFieldEditor( "", "Basis-Verzeichnis:", sub );
    m_dir.setStringValue( m_location );
    
    return c;
  }
  
  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  protected void okPressed()
  {
    super.okPressed();
    
    m_filters = getValue();
    m_location = m_dir.getStringValue();
  }
  
  public String getLocation()
  {
    return m_location;
  }
  
  public String getFilters()
  {
    return m_filters;
  }
}
