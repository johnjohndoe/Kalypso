package org.kalypso.ui.repository.wizard;

import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

/**
 * FileSelectWizardPage
 * 
 * @author schlienger
 */
public class FileSelectWizardPage extends WizardPage
{
  private final PreferenceStore m_store;
  private FileFieldEditor m_ffe;

  protected FileSelectWizardPage( final String pageName, final String fileName )
  {
    super( pageName );
    
    m_store = new PreferenceStore();
    m_store.setDefault( "FILE", fileName );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
   */
  public void dispose( )
  {
    if( m_ffe != null )
      m_ffe.dispose();
    
    super.dispose();
  }
  
  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    final Composite sub = new Composite( parent, SWT.FILL );

    m_ffe = new FileFieldEditor( "FILE", "Datei:", sub );
    m_ffe.setPreferenceStore( m_store );
    m_ffe.loadDefault();
    m_ffe.setEmptyStringAllowed( false );
    
    final GridLayout gridLayout = new GridLayout( 4, true );
    sub.setLayout( gridLayout );
    sub.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    m_ffe.fillIntoGrid( sub, 4 );
    
    setControl( sub );
  }
  
  public String getFilePath()
  {
    if( m_ffe == null )
      return null;
    
    m_ffe.store();
    
    return m_store.getString( "FILE" );
  }
}
