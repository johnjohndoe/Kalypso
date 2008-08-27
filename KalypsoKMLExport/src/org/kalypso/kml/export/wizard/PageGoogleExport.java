/**
 *
 */
package org.kalypso.kml.export.wizard;

import java.io.File;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChooserDelegate;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChooserDelegate.FILE_CHOOSER_GROUP_TYPE;
import org.kalypso.kml.export.constants.IKMLExportSettings;
import org.kalypso.kml.export.i18n.Messages;

/**
 * @author kuch
 */
public class PageGoogleExport extends WizardPage implements IKMLExportSettings
{

  protected File m_file; // file target

  protected String m_name;

  protected String m_description;

  /**
   * @param file
   * @param pageName
   */
  protected PageGoogleExport( final File targetFile )
  {
    super( "googleEarthExportPage" ); //$NON-NLS-1$

    m_file = targetFile;

    setTitle( Messages.PageGoogleExport_1 );
    setDescription( Messages.PageGoogleExport_2 );
  }

  /**
   *
   */
  private void checkPageCompleted( )
  {

    if( m_name == null )
    {
      setMessage( null );
      setErrorMessage( Messages.PageGoogleExport_3 );

      setPageComplete( false );
      return;
    }

    if( m_file == null )
    {
      setMessage( null );
      setErrorMessage( Messages.PageGoogleExport_4 );

      setPageComplete( false );
      return;
    }

    setMessage( null );
    setErrorMessage( null );

    setPageComplete( true );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    setPageComplete( false );

    final Composite container = new Composite( parent, SWT.NULL );
    container.setLayout( new GridLayout( 2, false ) );
    setControl( container );

    /* name */
    final Label lName = new Label( container, SWT.NONE );
    lName.setText( Messages.PageGoogleExport_5 );

    final Text tName = new Text( container, SWT.BORDER );
    tName.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    tName.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        m_name = tName.getText();
        checkPageCompleted();
      }
    } );

    /* description */
    final Label lDescription = new Label( container, SWT.NONE );
    lDescription.setText( Messages.PageGoogleExport_6 );
    lDescription.setLayoutData( new GridData( GridData.FILL, GridData.BEGINNING, false, false ) );

    final Text tDescription = new Text( container, SWT.BORDER | SWT.MULTI | SWT.WRAP );
    tDescription.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    tDescription.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        m_description = tDescription.getText();
      }
    } );

    /* fileChooser */

    final FileChooserDelegate delegate = new FileChooserGroup.FileChooserDelegate( FILE_CHOOSER_GROUP_TYPE.eSave )
    {
      @Override
      public String[] getFilterExtensions( )
      {
        return new String[] { "kmz", "KMZ" }; //$NON-NLS-1$ //$NON-NLS-2$
      }

      @Override
      public String[] getFilterNames( )
      {

        return new String[] { Messages.PageGoogleExport_9 };
      }
    };

    if( m_file == null )
    {
      final FileChooserGroup fc = new FileChooserGroup( delegate );
      final Group fcGroup = fc.createControl( container, SWT.NONE );
      fcGroup.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false, 2, 0 ) );

      fc.addFileChangedListener( new FileChooserGroup.FileChangedListener()
      {
        public void fileChanged( final File file )
        {
          m_file = file;

          checkPageCompleted();
        }
      } );
    }

    checkPageCompleted();
  }

  public String getExportDescription( )
  {
    return m_description;
  }

  public File getExportFile( )
  {
    return m_file;
  }

  public String getExportName( )
  {
    return m_name;
  }

}
