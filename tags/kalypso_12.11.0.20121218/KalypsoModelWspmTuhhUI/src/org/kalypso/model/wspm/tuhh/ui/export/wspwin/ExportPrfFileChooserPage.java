/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.tuhh.ui.export.wspwin;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.jface.wizard.IFileChooserDelegate;
import org.kalypso.contribs.eclipse.ui.forms.MessageProvider;
import org.kalypso.model.wspm.tuhh.core.profile.pattern.ProfilePatternInputReplacer;
import org.kalypso.model.wspm.tuhh.ui.export.ExportFileChooserPage;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * FIXME: additionally ask user for roughness component, preference for klasses or values (both roughness, vegetation).
 * Use databinding!
 *
 * @author kimwerner
 */
public class ExportPrfFileChooserPage extends ExportFileChooserPage
{
  private static final String SETTINGS_FILENAME_PATTERN = "filenamePattern"; //$NON-NLS-1$

  private String m_filenamePattern = "<Name>_<Station>"; //$NON-NLS-1$

  public ExportPrfFileChooserPage( final IFileChooserDelegate fileChooser )
  {
    super( fileChooser );
  }

  @Override
  protected void createPageContent( final Composite parent )
  {
    super.createPageContent( parent );

    createFilenamePattern( parent );
  }

  private void createFilenamePattern( final Composite parent )
  {
    initializeFilenamePattern();

    final Group group = new Group( parent, SWT.None );
    group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    group.setLayout( new GridLayout( 3, false ) );
    group.setText( Messages.getString( "ExportPrfFileChooserPage_0" ) ); //$NON-NLS-1$

    final Label label = new Label( group, SWT.NONE );
    label.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false ) );
    label.setText( Messages.getString( "ExportPrfFileChooserPage_1" ) ); //$NON-NLS-1$

    final Text text = new Text( group, SWT.BORDER );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    text.setText( m_filenamePattern );

    text.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        handleFilenamePatternChanged( text.getText() );
      }
    } );

    ProfilePatternInputReplacer.getINSTANCE().createPatternButton( group, text );
  }

  protected void handleFilenamePatternChanged( final String currentValue )
  {
    m_filenamePattern = currentValue;

    updateMessage();

    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings == null )
      return;

    dialogSettings.put( SETTINGS_FILENAME_PATTERN, currentValue );
  }

  private void initializeFilenamePattern( )
  {
    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings == null )
      return;

    final String setting = dialogSettings.get( SETTINGS_FILENAME_PATTERN );
    if( setting == null )
      return;

    m_filenamePattern = setting;
  }

  @Override
  protected IMessageProvider validatePage( )
  {
    final IMessageProvider validate = super.validatePage();
    if( validate != null )
      return validate;

    if( StringUtils.isEmpty( m_filenamePattern ) )
      return new MessageProvider( Messages.getString( "ExportPrfFileChooserPage_2" ), IMessageProvider.ERROR ); //$NON-NLS-1$

    // check for invalid filename

    return null;
  }

  public String getFilenamePattern( )
  {
    return m_filenamePattern;
  }
}