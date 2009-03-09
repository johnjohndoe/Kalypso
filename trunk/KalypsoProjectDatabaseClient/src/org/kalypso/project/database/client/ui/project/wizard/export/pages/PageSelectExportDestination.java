/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.project.database.client.ui.project.wizard.export.pages;

import java.io.File;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.osgi.framework.internal.core.FrameworkProperties;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.project.database.client.i18n.Messages;

/**
 * @author Dirk Kuch
 */
public class PageSelectExportDestination extends WizardPage
{
  protected String m_sFile;

  public PageSelectExportDestination( )
  {
    super( "pageSelectExportDestination" ); //$NON-NLS-1$

    setTitle( Messages.getString("org.kalypso.project.database.client.ui.project.wizard.export.pages.PageSelectExportDestination.0") ); //$NON-NLS-1$
    setDescription( Messages.getString("org.kalypso.project.database.client.ui.project.wizard.export.pages.PageSelectExportDestination.1") ); //$NON-NLS-1$
  }

  private IPath browse( )
  {
    final FileDialog dialog = new FileDialog( getShell(), SWT.SAVE );
    dialog.setFileName( m_sFile );

    final String[][] filters = getFilters();
    dialog.setFilterExtensions( filters[0] );
    dialog.setFilterNames( filters[1] );

    final String result = dialog.open();
    if( result == null )
    {
      return null;
    }

    return new Path( result );
  }

  protected String browseForFile( )
  {
    final IPath path = browse();

    if( path != null )
    {
      return path.toString();
    }

    return null;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    setPageComplete( false );

    final Composite container = new Composite( parent, SWT.NULL );
    container.setLayout( new GridLayout() );
    setControl( container );

    final Group group = new Group( container, SWT.NONE );
    group.setLayout( new GridLayout( 2, false ) );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    group.setText( Messages.getString("org.kalypso.project.database.client.ui.project.wizard.export.pages.PageSelectExportDestination.2") ); //$NON-NLS-1$

    final Text text = new Text( group, SWT.BORDER );
    text.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    text.setEnabled( false );

    /* button open dialog */
    final Button button = new Button( group, SWT.NONE );
    button.setText( "..." ); //$NON-NLS-1$
    button.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final String string = browseForFile();
        if( string != null && !"".equals( string.trim() ) ) //$NON-NLS-1$
        {
          text.setText( string );
          m_sFile = string;

          checkPage();
        }
      }
    } );

    /* propose export target */
    final String javaTmpDir = FrameworkProperties.getProperty( FileUtilities.JAVA_IO_TMPDIR );
    final String target = String.format( Messages.getString("org.kalypso.project.database.client.ui.project.wizard.export.pages.PageSelectExportDestination.3"), javaTmpDir ); //$NON-NLS-1$
    m_sFile = target;
    text.setText( target );

    setPageComplete( false );
  }

  private String[][] getFilters( )
  {
    final String[] ext = new String[] { "*.zip" }; //$NON-NLS-1$
    final String[] name = new String[] { "Zipfile" }; //$NON-NLS-1$

    return new String[][] { ext, name };
  }

  public File getSelectedFile( )
  {
    final File file = new File( m_sFile );

    return file;
  }

  /**
   * @see org.kalypso.nofdpidss.core.view.widgets.MyWizardPage#checkPage()
   */
  protected void checkPage( )
  {
    if( m_sFile == null || "".equals( m_sFile.trim() ) ) //$NON-NLS-1$
    {
      setErrorMessage( Messages.getString("org.kalypso.project.database.client.ui.project.wizard.export.pages.PageSelectExportDestination.4") ); //$NON-NLS-1$
      setMessage( null );

      setPageComplete( false );
      return;
    }

    final File file = getSelectedFile();
    if( file.exists() )
    {
      setErrorMessage( Messages.getString("org.kalypso.project.database.client.ui.project.wizard.export.pages.PageSelectExportDestination.5") ); //$NON-NLS-1$
      setMessage( null );

      setPageComplete( false );
      return;
    }

    setMessage( null );
    setErrorMessage( null );
    setPageComplete( true );
  }
}
