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
package org.kalypso.model.wspm.tuhh.ui.actions;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChangedListener;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChooserDelegate;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChooserDelegate.FILE_CHOOSER_GROUP_TYPE;
import org.kalypso.contribs.eclipse.ui.forms.MessageProvider;

/**
 * @author Gernot Belger
 */
public class WProfImportFilePage extends WizardPage
{
  private FileChooserGroup m_shapeChooser;

  public WProfImportFilePage( final String pageName )
  {
    super( pageName );
  }

  public WProfImportFilePage( final String pageName, final String title, final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    final FileChooserDelegate openDelegate = new FileChooserDelegate( FILE_CHOOSER_GROUP_TYPE.eOpen )
    {
      /**
       * @see org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChooserDelegate#getFilterNames()
       */
      @Override
      public String[] getFilterNames( )
      {
        return new String[] { "All Files (*.*)", "ESRI Shape File (*.shp)" };
      }

      /**
       * @see org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChooserDelegate#getFilterExtensions()
       */
      @Override
      public String[] getFilterExtensions( )
      {
        return new String[] { "*.*", "*.shp" };
      }
    };

    m_shapeChooser = new FileChooserGroup( openDelegate );
    final IDialogSettings dialogSettings = getDialogSettings();
    final IDialogSettings shapeSection = PluginUtilities.getSection( dialogSettings, "shapeFile" );
    m_shapeChooser.setDialogSettings( shapeSection );
    m_shapeChooser.addFileChangedListener( new FileChangedListener()
    {
      @Override
      public void fileChanged( final File file )
      {
        inputChanged();
      }
    } );

    final Control control = m_shapeChooser.createControl( parent, SWT.NONE );
    setControl( control );
  }

  protected void inputChanged( )
  {
    final IMessageProvider message = checkInput();
    setMessage( message.getMessage(), message.getMessageType() );

    setPageComplete( message.getMessageType() == IMessageProvider.NONE );
  }

  private IMessageProvider checkInput( )
  {
    final File file = m_shapeChooser.getFile();
    if( !file.exists() )
      return new MessageProvider( "File does not exist", IMessageProvider.ERROR );

    return new MessageProvider( null, IMessageProvider.NONE );
  }

  public File getWProfFile( )
  {
    return m_shapeChooser.getFile();
  }

  public String getShapeDefaultSrs( )
  {
    // FIXME
    return "EPSG:31467";
  }

  public URL getPhotoContext( )
  {
    // FIXME
    try
    {
      return new URL( "file:///P:\\bwg0715223\\HWGK_471_5_Hydraulik_work\\Vermessung\\" );
    }
    catch( final MalformedURLException e )
    {
      return null;
    }
  }

}
