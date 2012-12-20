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
package org.kalypso.model.wspm.tuhh.ui.imports;

import java.io.File;
import java.nio.charset.Charset;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.ToolTip;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.Hyperlink;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.viewers.CharsetViewer;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateDirectory;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateOpen;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChangedListener;
import org.kalypso.contribs.eclipse.ui.forms.MessageProvider;
import org.kalypso.gml.ui.jface.ShapeCharsetUI;
import org.kalypso.model.wspm.tuhh.core.wprof.WProfContextPatternReplacer;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.transformation.ui.CRSSelectionPanel;
import org.kalypso.transformation.ui.listener.CRSSelectionListener;

/**
 * @author Gernot Belger
 */
public class WProfImportFilePage extends WizardPage
{
  static final String SETTINGS_CRS = "wprofShapeCrs"; //$NON-NLS-1$

  private FileChooserGroup m_shapeChooser;

  private FileChooserGroup m_imageChooser;

  private CRSSelectionPanel m_crsPanel;

  private CharsetViewer m_charsetViewer;

  private FileChooserGroup m_pdfChooser;

  public WProfImportFilePage( final String pageName )
  {
    super( pageName );
  }

  public WProfImportFilePage( final String pageName, final String title, final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.fillDefaults().applyTo( panel );

    final Control fileControl = createFileControl( panel );
    fileControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Control srsControl = createSrsControl( panel );
    srsControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Control charsetControl = createCharsetControl( panel );
    charsetControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    m_imageChooser = createContextControl( "imageContext" ); //$NON-NLS-1$
    createChooserControl( panel, m_imageChooser, Messages.getString( "WProfImportFilePage_0" ), new GridData( SWT.FILL, SWT.CENTER, true, false ) ); //$NON-NLS-1$

    m_pdfChooser = createContextControl( "pdfContext" ); //$NON-NLS-1$
    createChooserControl( panel, m_pdfChooser, Messages.getString( "WProfImportFilePage_1" ), new GridData( SWT.FILL, SWT.CENTER, true, false ) ); //$NON-NLS-1$

    setControl( panel );
    setMessage( null, IMessageProvider.NONE );
  }

  private Control createCharsetControl( final Composite panel )
  {
    final Group group = new Group( panel, SWT.NONE );
    group.setText( Messages.getString( "WProfImportFilePage_2" ) ); //$NON-NLS-1$
    group.setLayout( new GridLayout() );

    final IDialogSettings dialogSettings = getDialogSettings();
    m_charsetViewer = ShapeCharsetUI.createCharsetViewer( group, dialogSettings );
    m_charsetViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    return group;
  }

  private Control createSrsControl( final Composite parent )
  {
    m_crsPanel = new CRSSelectionPanel( parent, SWT.NONE );

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
        {
          dialogSettings.put( SETTINGS_CRS, selectedCRS );
        }

        inputChanged();
      }
    } );

    return m_crsPanel;
  }

  private FileChooserGroup createContextControl( final String settings )
  {
    final FileChooserDelegateDirectory directoryDelegate = new FileChooserDelegateDirectory();
    final FileChooserGroup contextChooser = new FileChooserGroup( directoryDelegate );
    contextChooser.setLabel( null );

    final IDialogSettings dialogSettings = getDialogSettings();
    final IDialogSettings shapeSection = DialogSettingsUtils.getSection( dialogSettings, settings );
    contextChooser.setDialogSettings( shapeSection );
    contextChooser.addFileChangedListener( new FileChangedListener()
    {
      @Override
      public void fileChanged( final File file )
      {
        inputChanged();
      }
    } );

    return contextChooser;
  }

  private void createChooserControl( final Composite parent, final FileChooserGroup chooser, final String text, final GridData gridData )
  {
    final Group chooserGroup = chooser.createGroup( parent, SWT.NONE );
    chooserGroup.setText( text );
    chooserGroup.setLayoutData( gridData );

    final Hyperlink hyperlink = new Hyperlink( parent, SWT.NONE );
    hyperlink.setLayoutData( new GridData( SWT.END, SWT.TOP, true, false ) );
    hyperlink.setText( Messages.getString( "WProfImportFilePage_3" ) ); //$NON-NLS-1$
    hyperlink.setUnderlined( true );

    final ToolTip tip = new ToolTip( hyperlink.getShell(), SWT.ICON_INFORMATION );
    tip.setText( Messages.getString( "WProfImportFilePage_4" ) ); //$NON-NLS-1$
    final String tipMessage = WProfContextPatternReplacer.getInstance().getMessage();
    tip.setMessage( tipMessage );
    tip.setVisible( false );

    hyperlink.addHyperlinkListener( new HyperlinkAdapter()
    {
      @Override
      public void linkEntered( final HyperlinkEvent e )
      {
        // REMARK: tip not on hyperlink, else the popup will go under the mouse pointer, hence we loose the 'entered'
        // state, and the tip vanishes (-> tip blinks)
        final Point tipLocation = parent.toDisplay( chooserGroup.getLocation() );
        tip.setLocation( tipLocation );
        tip.setVisible( true );
      }

      @Override
      public void linkExited( final HyperlinkEvent e )
      {
        tip.setVisible( false );
      }
    } );

    hyperlink.addDisposeListener( new DisposeListener()
    {
      @Override
      public void widgetDisposed( final DisposeEvent e )
      {
        tip.dispose();
      }
    } );
  }

  private Control createFileControl( final Composite panel )
  {
    final FileChooserDelegateOpen openDelegate = new FileChooserDelegateOpen();
    openDelegate.addFilter( Messages.getString( "WProfImportFilePage_5" ), "*.*" ); //$NON-NLS-1$//$NON-NLS-2$
    openDelegate.addFilter( Messages.getString( "WProfImportFilePage_6" ), "*.shp" ); //$NON-NLS-1$//$NON-NLS-2$

    m_shapeChooser = new FileChooserGroup( openDelegate );
    m_shapeChooser.setLabel( null );

    final IDialogSettings dialogSettings = getDialogSettings();
    final IDialogSettings shapeSection = DialogSettingsUtils.getSection( dialogSettings, "shapeFile" ); //$NON-NLS-1$
    m_shapeChooser.setDialogSettings( shapeSection );
    m_shapeChooser.addFileChangedListener( new FileChangedListener()
    {
      @Override
      public void fileChanged( final File file )
      {
        inputChanged();
      }
    } );

    final Group chooserGroup = m_shapeChooser.createGroup( panel, SWT.NONE );
    chooserGroup.setText( Messages.getString( "WProfImportFilePage_7" ) ); //$NON-NLS-1$
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
      return new MessageProvider( Messages.getString( "WProfImportFilePage_8" ), IMessageProvider.ERROR ); //$NON-NLS-1$

    if( !wprofFile.exists() )
      return new MessageProvider( Messages.getString( "WProfImportFilePage_9" ), IMessageProvider.ERROR ); //$NON-NLS-1$

    final String selectedCRS = m_crsPanel == null ? null : m_crsPanel.getSelectedCRS();
    if( selectedCRS == null )
      return new MessageProvider( Messages.getString( "WProfImportFilePage_10" ), IMessageProvider.ERROR ); //$NON-NLS-1$

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

  public String getPhotoContext( )
  {
    return m_imageChooser.getPath();
  }

  public String getPdfContext( )
  {
    return m_pdfChooser.getPath();
  }

  public Charset getShapeCharset( )
  {
    return m_charsetViewer.getCharset();
  }
}
