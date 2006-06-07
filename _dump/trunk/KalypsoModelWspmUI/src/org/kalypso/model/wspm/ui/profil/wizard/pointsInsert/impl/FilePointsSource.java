/**
 * 
 */
package org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.impl;

import java.io.File;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoints;
import org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.AbstractPointsSource;

import serializer.prf.PrfSource;
import serializer.prf.ProfilesSerializer;



/**
 * @author Belger
 */
public class FilePointsSource extends AbstractPointsSource
{
  protected Text m_fileName;

  /**
   * @see org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.IPointsSource#getPoints()
   */
  public IProfilPoints getPoints( )
  {
    final PrfSource prfS = ProfilesSerializer.load( new File( m_fileName.getText() ) );
    if( prfS == null )
      return null;
    final IProfil profil = prfS.createProfil();
    if( profil == null )
      return null;

    return profil.getProfilPoints();
  }

  @Override
  public Control doCreateControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout( 3, false ) );

    final Label label = new Label( panel, SWT.NONE );
    label.setLayoutData( new GridData() );
    label.setText( "Datei: " );
    m_fileName = new Text( panel, SWT.BORDER );
    m_fileName.setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );

    final Button button = new Button( panel, SWT.NONE );
    button.setText( "..." );
    button.setLayoutData( new GridData() );

    button.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        FileDialog dlg = new FileDialog( parent.getShell() );
        final String fileName = dlg.open();
        if( fileName != null )
        {
          m_fileName.setText( fileName );
        }
      }
    } );

    return panel;
  }

  @Override
  protected void loadState( IDialogSettings settings )
  {
    final String fileName = settings.get( "DLG_SETTINGS_FILENAME" );
    if( fileName != null )
      m_fileName.setText( fileName );

  }

  public void saveState( IDialogSettings settings )
  {
    settings.put( "DLG_SETTINGS_FILENAME", m_fileName.getText()  );

  }
}
