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
package org.kalypso.model.wspm.tuhh.ui.imports;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.Charset;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.viewers.CharsetViewer;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateDirectory;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateOpen;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChangedListener;
import org.kalypso.contribs.eclipse.ui.forms.MessageProvider;
import org.kalypso.transformation.ui.CRSSelectionListener;
import org.kalypso.transformation.ui.CRSSelectionPanel;

/**
 * @author Gernot Belger
 */
public class WProfImportFilePage extends WizardPage
{
  static final String SETTINGS_CRS = "wprofShapeCrs";

  private static final String SETTINGS_CHARSET = "wprofShapeCharset";

  private FileChooserGroup m_shapeChooser;

  private FileChooserGroup m_imageChooser;

  private CRSSelectionPanel m_crsPanel;

  private CharsetViewer m_charsetViewer;

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
    final Composite panel = new Composite( parent, SWT.NONE );
    final GridLayout panelLayout = new GridLayout();
    panelLayout.marginWidth = 0;
    panelLayout.marginHeight = 0;
    panel.setLayout( panelLayout );

    final Control fileControl = createFileControl( panel );
    fileControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Control srsControl = createSrsControl( panel );
    srsControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Control charsetControl = createCharsetControl( panel );
    charsetControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Control imageContextControl = createImageContextControl( panel );
    imageContextControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    setControl( panel );
    setMessage( null, IMessageProvider.NONE );
  }

  private Control createCharsetControl( final Composite panel )
  {
    final Group group = new Group( panel, SWT.NONE );
    group.setText( "Character Encoding" );
    group.setLayout( new GridLayout() );

    m_charsetViewer = new CharsetViewer( group );
    m_charsetViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
    {
      final String charsetName = dialogSettings.get( SETTINGS_CHARSET );
      if( charsetName == null )
        m_charsetViewer.setCharset( Charset.defaultCharset() );
      else
      {
        final Charset charset = Charset.forName( charsetName );
        m_charsetViewer.setCharset( charset );
      }

      m_charsetViewer.addSelectionChangedListener( new ISelectionChangedListener()
      {
        @Override
        public void selectionChanged( final SelectionChangedEvent event )
        {
          handleCharsetChanged( dialogSettings );
        }
      } );
    }

    return group;
  }

  protected void handleCharsetChanged( final IDialogSettings dialogSettings )
  {
    final Charset charset = m_charsetViewer.getCharset();
    if( charset == null )
      dialogSettings.put( SETTINGS_CHARSET, (String) null );
    else
    {
      final String charsetName = charset.name();
      dialogSettings.put( SETTINGS_CHARSET, charsetName );
    }
  }

  private Control createSrsControl( final Composite parent )
  {
    m_crsPanel = new CRSSelectionPanel( parent, SWT.NONE );
    m_crsPanel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final IDialogSettings dialogSettings = getDialogSettings();

    if( dialogSettings != null )
    {
      final String lastCrs = dialogSettings.get( SETTINGS_CRS );
      m_crsPanel.setSelectedCRS( lastCrs );
    }

    m_crsPanel.addSelectionChangedListener( new CRSSelectionListener()
    {
      /**
       * @see org.kalypso.transformation.ui.CRSSelectionListener#selectionChanged(java.lang.String)
       */
      @Override
      protected void selectionChanged( final String selectedCRS )
      {
        if( dialogSettings != null )
          dialogSettings.put( SETTINGS_CRS, selectedCRS );

        inputChanged();
      }
    } );

    return m_crsPanel;
  }

  private Control createImageContextControl( final Composite panel )
  {
    final FileChooserDelegateDirectory directoryDelegate = new FileChooserDelegateDirectory();
    m_imageChooser = new FileChooserGroup( directoryDelegate );
    m_imageChooser.setShowLabel( false );

    final IDialogSettings dialogSettings = getDialogSettings();
    final IDialogSettings shapeSection = PluginUtilities.getSection( dialogSettings, "imageContext" );
    m_imageChooser.setDialogSettings( shapeSection );
    m_imageChooser.addFileChangedListener( new FileChangedListener()
    {
      @Override
      public void fileChanged( final File file )
      {
        inputChanged();
      }
    } );

    final Group chooserGroup = m_imageChooser.createControl( panel, SWT.NONE );
    chooserGroup.setText( "Image Folder (Optional)" );
    return chooserGroup;
  }

  private Control createFileControl( final Composite panel )
  {
    final FileChooserDelegateOpen openDelegate = new FileChooserDelegateOpen();
    openDelegate.addFilter( "All Files (*.*)", "*.*" );
    openDelegate.addFilter( "ESRI Shape File (*.shp)", "*.shp" );

    m_shapeChooser = new FileChooserGroup( openDelegate );
    m_shapeChooser.setShowLabel( false );

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

    final Group chooserGroup = m_shapeChooser.createControl( panel, SWT.NONE );
    chooserGroup.setText( "WProf Import File" );
    return chooserGroup;
  }

  protected void inputChanged( )
  {
    final IMessageProvider message = checkInput();
    setMessage( message.getMessage(), message.getMessageType() );

    setPageComplete( message.getMessageType() == IMessageProvider.NONE );
  }

  private IMessageProvider checkInput( )
  {
    final File wprofFile = m_shapeChooser.getFile();
    if( wprofFile == null )
      return new MessageProvider( "Please choose a WProf file", IMessageProvider.ERROR );

    if( !wprofFile.exists() )
      return new MessageProvider( "WProf file does not exist", IMessageProvider.ERROR );

    final String selectedCRS = m_crsPanel == null ? null : m_crsPanel.getSelectedCRS();
    if( selectedCRS == null )
      return new MessageProvider( "Pleae choose the coordinate system of the WProf file", IMessageProvider.ERROR );

    final File imageDir = m_imageChooser.getFile();
    if( imageDir != null )
    {
      if( !imageDir.exists() )
        return new MessageProvider( "Image folder does not exist", IMessageProvider.ERROR );

      if( !imageDir.isDirectory() )
        return new MessageProvider( "Image folder is not a directory", IMessageProvider.ERROR );
    }

    return new MessageProvider( null, IMessageProvider.NONE );
  }

  public File getWProfFile( )
  {
    return m_shapeChooser.getFile();
  }

  public String getShapeDefaultSrs( )
  {
    return m_crsPanel.getSelectedCRS();
  }

  public URL getPhotoContext( )
  {
    try
    {
      final File imageDir = m_imageChooser.getFile();
      if( imageDir == null )
        return null;

      return imageDir.toURI().toURL();
    }
    catch( final MalformedURLException e )
    {
      return null;
    }
  }

  public Charset getShapeCharset( )
  {
    return m_charsetViewer.getCharset();
  }

}
