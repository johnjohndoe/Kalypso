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
package org.kalypso.ui.rrm.wizards.conversion;

import java.io.File;

import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateDirectory;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChangedListener;
import org.kalypso.contribs.eclipse.ui.forms.MessageProvider;

/**
 * Lets the user choose which project to import and convert (only external projects supported now). <br/>
 * TODO: move this page to a more common place.
 * 
 * @author Gernot Belger
 */
public class ProjectConversionPage extends WizardPage
{
  private FileChooserGroup m_projectChooserGroup;

  private IProjectDescription m_projectDescription;

  protected ProjectConversionPage( final String pageName )
  {
    super( pageName );

    setTitle( "Daten konvertieren" );
    setDescription( "Bitte wählen Sie das Projekt aus, dessen Daten Sie in das aktuelle Kalypso Format übernehmen möchten." );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    initializeDialogUnits( parent );

    final Group panel = new Group( parent, SWT.NONE );
    panel.setLayout( new GridLayout( 3, false ) );
    setControl( panel );

    panel.setText( "Projekt" );

    final FileChooserDelegateDirectory dirDelegate = new FileChooserDelegateDirectory();
    m_projectChooserGroup = new FileChooserGroup( dirDelegate );
    m_projectChooserGroup.setShowLabel( false );

    m_projectChooserGroup.createControlsInGrid( panel );
    m_projectChooserGroup.addFileChangedListener( new FileChangedListener()
    {
      @Override
      public void fileChanged( final File file )
      {
        handleFileChanged( file );
      }
    } );
  }

  protected void handleFileChanged( final File file )
  {
    updateProjectInfo( file );

    final IMessageProvider meessage = validatePage();
    setMessage( meessage );
  }

  private void updateProjectInfo( final File file )
  {
    // TODO: reset info
    m_projectDescription = null;

    if( file == null )
      return;

    if( !file.isDirectory() )
      return;

    try
    {
      final IPath projectPath = new Path( file.getPath() );
      final IPath projectFilePath = projectPath.append( IProjectDescription.DESCRIPTION_FILE_NAME );
      m_projectDescription = ResourcesPlugin.getWorkspace().loadProjectDescription( projectFilePath );
      // actually, what will we do with it?

      // TODO: read project name
      // TODO: check if we have the right natue (model-nature) and also read the version of that project

      return;
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }

  }

  private IMessageProvider validatePage( )
  {
    final IMessageProvider groupMessage = m_projectChooserGroup.validate();
    if( groupMessage != null )
      return groupMessage;

    if( m_projectDescription == null )
    {
      final String message = String.format( "Chosen directory does not contain a project ('%s' file not present)", IProjectDescription.DESCRIPTION_FILE_NAME );
      return new MessageProvider( message, IMessageProvider.ERROR );
    }

    return null;
  }

  private void setMessage( final IMessageProvider message )
  {
    if( message == null )
      setMessage( (String) null );
    else
      setMessage( message.getMessage(), message.getMessageType() );
  }

  public File getProjectDir( )
  {
    return m_projectChooserGroup.getFile();
  }
}
