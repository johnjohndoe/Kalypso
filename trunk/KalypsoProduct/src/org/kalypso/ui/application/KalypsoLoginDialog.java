package org.kalypso.ui.application;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.java.util.StringUtilities;
import org.kalypso.product.KalypsoProductPlugin;

/**
 * The KalypsoLoginDialog allows to enter user login information
 * as well as make scenario selection. Both are optional since
 * Kalypso has various login possibilities depending on the
 * installation.
 * 
 * @author schlienger
 */
public class KalypsoLoginDialog extends TitleAreaDialog
{
  private final boolean m_userNameChangeable;

  private String m_userName;

  private final boolean m_passwordEnabled;

  private String m_passerword;

  private String m_selectedScenario;

  private final boolean m_useScenarios;

  private final Map m_scenarios; // scenarios --> scenarioDescriptions

  private final String m_message;

  private final String m_title;

  private Image m_image;

  private ListViewer m_sceViewer;

  private Text m_txtPass;

  private Text m_txtName;

  private ISelectionChangedListener m_listener;

  public KalypsoLoginDialog( final Shell shell, final String title,
      final String message, final String userName,
      final boolean userNameChangeable, final boolean passwordEnabled,
      final boolean scenarioSelection, final String[] scenarios,
      final String[] scenariosDesc )
  {
    super( shell );

    m_title = title;
    m_message = message;

    m_userNameChangeable = userNameChangeable;
    m_userName = userName;
    m_passwordEnabled = passwordEnabled;
    m_useScenarios = scenarioSelection;
    
    m_scenarios = new HashMap();
    for( int i = 0; i < scenarios.length; i++ )
      m_scenarios.put( scenarios[i], scenariosDesc[i] );

    setShellStyle( 0 );
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  protected Control createDialogArea( Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.FILL );

    final GridLayout gridLayout = new GridLayout( 2, false );
    composite.setLayout( gridLayout );
    composite.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    final Label lblName = new Label( composite, SWT.LEFT );
    lblName.setText( "Benutzername:" );
    m_txtName = new Text( composite, SWT.LEFT | SWT.BORDER );
    m_txtName.setText( m_userName );
    m_txtName.setEditable( m_userNameChangeable );
    m_txtName.setSize( 100, m_txtName.getSize().y );
    m_txtName.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    final Label lblPass = new Label( composite, SWT.LEFT );
    lblPass.setText( "Passwort:" );
    m_txtPass = new Text( composite, SWT.LEFT | SWT.PASSWORD | SWT.BORDER );
    m_txtPass.setEnabled( m_passwordEnabled );
    m_txtPass.setSize( 100, m_txtName.getSize().y );
    m_txtPass.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    if( m_useScenarios )
    {
      // separator
      final GridData gd = new GridData( GridData.FILL_HORIZONTAL );
      gd.horizontalSpan = 2;
      new Label( composite, SWT.SEPARATOR | SWT.HORIZONTAL ).setLayoutData( gd );

      final Label lblSce = new Label( composite, SWT.LEFT );
      lblSce.setText( "W�hlen Sie bitte einen Szenario aus:" );
      lblSce
          .setToolTipText( "Mit Szenarios werden zum Beispiel spezielle Anwendungsf�lle"
              + " wie Katastrophentest unterst�tzt" );
      lblSce.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL,
          GridData.VERTICAL_ALIGN_FILL, true, false, 2, 1 ) );

      // fake label in order to eat one column
      new Label( composite, SWT.NONE );

      // scenario selection
      m_sceViewer = new ListViewer( composite, SWT.SINGLE | SWT.BORDER );
      m_sceViewer.getControl()
          .setLayoutData( new GridData( GridData.FILL_BOTH ) );
      m_sceViewer.add( m_scenarios.keySet().toArray() );

      // fake label
      new Label( composite, SWT.NONE );
      
      final Label lblDesc = new Label( composite, SWT.LEFT );
      lblDesc.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL,
          GridData.VERTICAL_ALIGN_FILL, true, false, 1, 2 ) );

      final Map scenarios = m_scenarios;
      m_listener = new ISelectionChangedListener()
      {
        public void selectionChanged( final SelectionChangedEvent event )
        {
          final IStructuredSelection sel = (IStructuredSelection) event.getSelection();
          final String scenario = sel.getFirstElement().toString();
          String desc = (String) scenarios.get( scenario );
          
          if( desc == null )
            desc = "";
          
          lblDesc.setText( StringUtilities.spanOverLines( desc, 40, true ) );
          lblDesc.update();
        }
      };
      
      m_sceViewer.addSelectionChangedListener( m_listener );
    }

    m_image = KalypsoProductPlugin.IMG_DESC_KALYPSO32.createImage();
    setTitleImage( m_image );
    setTitle( m_title );
    setMessage( m_message );

    return composite;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  protected void okPressed( )
  {
    m_userName = m_txtName == null ? null : m_txtName.getText();
    m_passerword = m_txtPass == null ? null : m_txtPass.getText();

    if( m_sceViewer != null )
    {
      final ISelection sel = m_sceViewer.getSelection();
      if( sel.isEmpty() )
        m_selectedScenario = sel.toString();
    }

    super.okPressed();
  }

  public boolean close( )
  {
    if( m_listener != null && m_sceViewer != null )
      m_sceViewer.removeSelectionChangedListener( m_listener );
      
    if( m_image != null )
      m_image.dispose();

    return super.close();
  }

  public String getSelectedScenario( )
  {
    return m_selectedScenario;
  }

  public String getUserName( )
  {
    return m_userName;
  }

  public String getPassword( )
  {
    return m_passerword;
  }
}
