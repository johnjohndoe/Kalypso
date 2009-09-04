package org.kalypso.dcadapter;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.factory.AbstractRepositoryFactory;

/**
 * DataCenterRepositoryFactory
 * 
 * @author marc
 */
public class DataCenterRepositoryFactory extends AbstractRepositoryFactory
{
  /**
   * @see org.kalypso.repository.factory.IRepositoryFactory#configureRepository()
   */
  public boolean configureRepository()
  {
    final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

    final ConfigDialog dlg = new ConfigDialog( shell );
    if( dlg.open() == Window.OK )
    {
      setConfiguration( dlg.getUrl() + "#" + dlg.getUsername() + "#" + dlg.getPassword() );

      return true;
    }

    return false;
  }

  /**
   * The configuration string should be build in the following way:
   * 
   * <pre>
   * 
   *  url#username#password
   *  
   * </pre>
   * 
   * <p>
   * url: the url for the connection Example: jdbc:edbc://LOCALHOST:II7/vnode::kalypso/INGRES
   * <p>
   * username: the name of the user under which the connection will be established
   * <p>
   * password: the password for that user
   * 
   * @see org.kalypso.repository.factory.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository() throws RepositoryException
  {
    final String[] conf = getConfiguration().split( "#" );

    if( conf.length < 3 )
      throw new RepositoryException( "Invalid configuration in " + getClass().getName() + ": " + getConfiguration() );

    final String url = conf[0];
    final String userName = conf[1];
    final String password = conf[2];

    return new DataCenterRepository( getRepositoryName(), getClass().getName(), getConfiguration(), isReadOnly(), url,
        userName, password );
  }

  private static class ConfigDialog extends TitleAreaDialog
  {
    protected String m_url = "jdbc:edbc://134.28.87.75:II7/vn_datacenter::abwb_v2_flows/INGRES";

    protected String m_username = "";

    protected String m_password = "";

    public ConfigDialog( final Shell parentShell )
    {
      super( parentShell );

      final IDialogSettings settings = DataCenterPlugin.getDefault().getDialogSettings();
      IDialogSettings section = settings.getSection( "connection" );
      if( section == null )
        section = settings.addNewSection( "connection" );
      else
      {
        m_url = section.get( "url" ) == null ? "" : section.get( "url" );
        m_username = section.get( "username" ) == null ? "" : section.get( "username" );
      }
    }

    /**
     * @see org.eclipse.jface.dialogs.Dialog#close()
     */
    @Override
    public boolean close()
    {
      final IDialogSettings section = DataCenterPlugin.getDefault().getDialogSettings().getSection( "connection" );
      section.put( "url", m_url );
      section.put( "username", m_username );

      return super.close();
    }

    /**
     * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea( Composite parent )
    {
      final Composite panel = new Composite( parent, SWT.FILL );
      panel.setLayout( new GridLayout( 2, false ) );
      panel.setLayoutData( new GridData( GridData.FILL_BOTH ) );

      // Ingres URL
      final Label lblUrl = new Label( panel, SWT.LEFT );
      lblUrl.setText( "Ingres-Url:" );

      final Text txtUrl = new Text( panel, SWT.BORDER );
      txtUrl.setText( m_url );
      txtUrl.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
      txtUrl.addModifyListener( new ModifyListener()
      {
        public void modifyText( ModifyEvent e )
        {
          m_url = txtUrl.getText();
        }
      } );
      txtUrl.setSize( 200, txtUrl.getSize().y );

      // Username
      final Label lblName = new Label( panel, SWT.LEFT );
      lblName.setText( "Benutzername:" );

      final Text txtName = new Text( panel, SWT.BORDER );
      txtName.setText( m_username );
      txtName.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
      txtName.addModifyListener( new ModifyListener()
      {
        public void modifyText( ModifyEvent e )
        {
          m_username = txtName.getText();
        }
      } );
      txtName.setSize( 100, txtUrl.getSize().y );

      // Password
      final Label lblPw = new Label( panel, SWT.LEFT );
      lblPw.setText( "Password:" );

      final Text txtPw = new Text( panel, SWT.BORDER | SWT.PASSWORD );
      txtPw.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
      txtPw.addModifyListener( new ModifyListener()
      {
        public void modifyText( ModifyEvent e )
        {
          m_password = txtPw.getText();
        }
      } );
      txtPw.setSize( 100, txtUrl.getSize().y );

      setMessage( "Geben Sie hier die Verbindungsinformation ein" );
      setTitle( "DataCenter-Verbindung" );

      return panel;
    }

    public String getPassword()
    {
      return m_password;
    }

    public String getUrl()
    {
      return m_url;
    }

    public String getUsername()
    {
      return m_username;
    }
  }
}