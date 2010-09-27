/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.ui.export.sobek;

import java.io.File;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateSave;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup;
import org.kalypso.model.wspm.core.gml.IProfileFeature;

/**
 * @author Gernot Belger
 */
public class SobekFileChooser
{
  private FileChooserGroup m_fileChooserGroup;

  private final FileChooserDelegateSave m_delegate;

  private final SobekProfileExportFileChooserPage m_page;

  private File m_file;

  private final IDialogSettings m_dialogSettings;

  public SobekFileChooser( final SobekProfileExportFileChooserPage page, final IDialogSettings dialogSettings, final String filterLabel, final String extension )
  {
    m_page = page;
    m_dialogSettings = dialogSettings;
    m_delegate = new FileChooserDelegateSave();
    m_delegate.addFilter( filterLabel, "*." + extension ); //$NON-NLS-1$
  }

  public final void createControl( final Composite parent, final String groupLabel )
  {
    m_fileChooserGroup = new FileChooserGroup( m_delegate );
    m_fileChooserGroup.addFileChangedListener( new FileChooserGroup.FileChangedListener()
    {
      @Override
      public void fileChanged( final File file )
      {
        setFile( file );
      }
    } );

    m_fileChooserGroup.setShowLabel( false );

    final Group group = new Group( parent, SWT.NONE );
    group.setLayout( new GridLayout( 3, false ) );
    group.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    group.setText( groupLabel );

    m_fileChooserGroup.createControlsInGrid( group );

    createOtherControls( group );

    m_fileChooserGroup.setDialogSettings( m_dialogSettings );

    final File file = m_fileChooserGroup.getFile();
    if( file != null )
      setFile( file );
  }

  protected void createOtherControls( @SuppressWarnings("unused") final Composite parent )
  {
  }

  protected void setFile( final File file )
  {
    m_file = file;

    m_page.updateMessage();
  }

  public File getFile( )
  {
    return m_file;
  }

  public IMessageProvider validate( )
  {
    // Ignore empty file -> we will not export it
    if( m_file == null )
      return null;

    return m_delegate.validate( m_file );
  }

  public ISobekProfileExportOperation createOperation( final IProfileFeature[] profiles, final String idPattern )
  {
    return new SobekDefExportOperation( getFile(), profiles, idPattern );
  }
}
