package org.kalypso.ui.repository.file;

import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * Config Dialog for ZmlRepositoryFactory.
 * 
 * @author schlienger
 */
public class ZmlRepositoryConfigDialog extends TitleAreaDialog
{
  private final static String msg = "Bitte wählen Sie zuerst ein Basisverzeichnis aus.\n" +
      "Geben Sie anschliessend eine oder mehrere Dateiendungen (Komma getrennt)";
  
  private String m_location;
  private String m_filters;

  private StringFieldEditor m_fFilters;
  private DirectoryFieldEditor m_fLocation;
  
  
  public ZmlRepositoryConfigDialog( final Shell parentShell, final String location, final String filters )
  {
    super( parentShell );
    
    m_location = location;
    m_filters = filters;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  protected Control createDialogArea( final Composite parent )
  {
    setTitle( "ZML-Repository Konfiguration" );
    setMessage( msg );

    final PreferenceStore store = new PreferenceStore();
    
    final Composite c = (Composite)super.createDialogArea( parent );
    
    final Composite sub1 = new Composite( c, SWT.TOP );
    m_fLocation = new DirectoryFieldEditor( "basedir", "Basis-Verzeichnis:", sub1 );
    m_fLocation.setPreferenceStore( store );
    m_fLocation.setStringValue( m_location );
    sub1.setSize( 200, 20 );
    
    final Composite sub2 = new Composite( c, SWT.BOTTOM );
    m_fFilters = new StringFieldEditor( "filter", "Dateiendung:", sub2 );
    m_fFilters.setPreferenceStore( store );
    m_fFilters.setStringValue( m_filters );
    sub2.setSize( 200, 20 );
    
    return c;
  }
  
  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  protected void okPressed()
  {
    m_fLocation.store();
    m_fFilters.store();
    
    m_location = m_fLocation.getStringValue().trim();
    m_filters = m_fFilters.getStringValue().trim();
    
    super.okPressed();
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
