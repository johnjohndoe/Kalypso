package org.kalypso.ui.repository.dialogs;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * Config Dialog for FileRepositoryFactory.
 * 
 * TODO: use plugin's dialog store to store default values
 * 
 * @author schlienger
 */
public class FileRepositoryConfigDialog extends TitleAreaDialog
{
  private final static String msg = "Bitte wählen Sie zuerst ein Basisverzeichnis aus.\n"
      + "Geben Sie anschliessend einen Kennzeichen und eine oder mehrere Dateiendungen (Komma getrennt) ein.";

  protected final static String BASEDIR = "basedir";
  protected final static String IDENTIFIER = "identifier";
  protected final static String FILTER = "filter";

  private PreferenceStore m_store;

  private StringFieldEditor m_fFilters;
  
  private StringFieldEditor m_fIdentifier;

  private DirectoryFieldEditor m_fLocation;

  public FileRepositoryConfigDialog( final Shell parentShell, final String location,
      final String identifier, final String filters )
  {
    super( parentShell );

    m_store = new PreferenceStore();
    m_store.setDefault( BASEDIR, location );
    m_store.setDefault( IDENTIFIER, identifier );
    m_store.setDefault( FILTER, filters );
  }

  public void dispose()
  {
    m_fFilters.dispose();
    m_fIdentifier.dispose();
    m_fLocation.dispose();
  }
  
  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  protected Control createDialogArea( final Composite parent )
  {
    setTitle( "ZML-Repository Konfiguration" );
    setMessage( msg );

    final Composite c = (Composite)super.createDialogArea( parent );
    
    final Composite sub = new Composite( c, SWT.FILL );

    m_fLocation = new DirectoryFieldEditor( BASEDIR, "Basis-Verzeichnis:", sub );
    m_fIdentifier = new StringFieldEditor( IDENTIFIER, "Kennzeichen:", sub );
    m_fFilters = new StringFieldEditor( FILTER, "Dateiendung:", sub );
    
    m_fLocation.setPreferenceStore( m_store );
    m_fLocation.loadDefault();
    m_fLocation.setEmptyStringAllowed( false );
    
    m_fIdentifier.setPreferenceStore( m_store );
    m_fIdentifier.loadDefault();
    
    m_fFilters.setPreferenceStore( m_store );
    m_fFilters.loadDefault();

    final GridLayout gridLayout = new GridLayout( 5, true );
    sub.setLayout( gridLayout );
    sub.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    m_fLocation.fillIntoGrid( sub, 5 );
    m_fIdentifier.fillIntoGrid( sub, 5 );
    m_fFilters.fillIntoGrid( sub, 3 );

    return c;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  protected void okPressed()
  {
    if( !m_fLocation.isValid() )
    {
      MessageDialog.openInformation( getParentShell(), "Basis-Verzeichnis", "Bitte geben Sie einen gültigen Verzeichnis ein" );
      return;
    }
    
    if( !m_fIdentifier.isValid() )
    {
      MessageDialog.openInformation( getParentShell(), "Kennzeichen", "Bitte geben Sie einen gültigen Kennzeichen ein" );
      return;
    }
    
    if( !m_fFilters.isValid() )
    {
      MessageDialog.openInformation( getParentShell(), "Dateiendung", m_fFilters.getErrorMessage() );
      return;
    }

    m_fLocation.store();
    m_fIdentifier.store();
    m_fFilters.store();

    super.okPressed();
  }

  public String getLocation()
  {
    return m_store.getString( BASEDIR );
  }

  public String getIdentifier()
  {
    return m_store.getString( IDENTIFIER );
  }
  
  public String getFilters()
  {
    return m_store.getString( FILTER );
  }
}