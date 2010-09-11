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
package org.kalypso.model.flood.ui.map;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateOpen;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChangedListener;
import org.kalypso.core.gml.provider.GmlSource;
import org.kalypso.core.gml.provider.IGmlSource;
import org.kalypso.core.gml.provider.IGmlSourceProvider;
import org.kalypso.model.flood.KalypsoModelFloodPlugin;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypso.transformation.ui.CRSSelectionPanel;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * @author Thomas Jung
 * @author Gernot Belger
 */
public class ExternalFileGmlSourceProvider implements IGmlSourceProvider, ITreeContentProvider
{
  private static final Object THE_ELEMENT = Messages.getString("org.kalypso.model.flood.ui.map.ExternalFileGmlSourceProvider.0"); //$NON-NLS-1$

  private final FileChooserDelegateOpen m_fileDelegate = new FileChooserDelegateOpen();

  private FileChooserGroup m_fileChooserGroup;

  private final FileChangedListener m_fileChangedListener = new FileChangedListener()
  {
    @Override
    public void fileChanged( final File file )
    {
      handleFileChanged( file );
    }
  };

  private File m_file;

  private CRSSelectionPanel m_crsPanel;

  public ExternalFileGmlSourceProvider( )
  {
    m_fileDelegate.addFilter( Messages.getString( "org.kalypso.model.flood.ui.map.ExternalFileGmlSourceProvider.4" ), "*.gml" );//$NON-NLS-1$ //$NON-NLS-2$
    m_fileDelegate.addFilter( Messages.getString( "org.kalypso.model.flood.ui.map.ExternalFileGmlSourceProvider.2" ), "*.hmo" );//$NON-NLS-1$ //$NON-NLS-2$
    m_fileDelegate.addFilter( Messages.getString( "org.kalypso.model.flood.ui.map.ExternalFileGmlSourceProvider.3" ), "*.shp" );//$NON-NLS-1$ //$NON-NLS-2$
    m_fileDelegate.addFilter( Messages.getString( "org.kalypso.model.flood.ui.map.ExternalFileGmlSourceProvider.1" ), "*.*" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.core.gml.provider.IGmlSourceProvider#createContentProvider()
   */
  @Override
  public ITreeContentProvider createContentProvider( )
  {
    return this;
  }

  /**
   * @see org.kalypso.core.gml.provider.IGmlSourceProvider#createInfoControl(org.eclipse.swt.widgets.Composite,
   *      java.lang.Object)
   */
  @Override
  public void createInfoControl( final Composite parent, final Object element )
  {
    Assert.isTrue( element == THE_ELEMENT );

    m_fileChooserGroup = new FileChooserGroup( m_fileDelegate );
    m_fileChooserGroup.addFileChangedListener( m_fileChangedListener );

    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout() );

    final Label label = new Label( panel, SWT.NONE );
    label.setText( Messages.getString("org.kalypso.model.flood.ui.map.ExternalFileGmlSourceProvider.9") ); //$NON-NLS-1$

    final Group fileControl = m_fileChooserGroup.createGroup( panel, SWT.NONE );
    fileControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    fileControl.setText( Messages.getString("org.kalypso.model.flood.ui.map.ExternalFileGmlSourceProvider.10") ); //$NON-NLS-1$

    m_crsPanel = new CRSSelectionPanel( panel, SWT.NONE );
    m_crsPanel.setSelectedCRS( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
    m_crsPanel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    // Set dialog settings at the end, so update is correctly done for all controls
    final IDialogSettings dialogSettings = PluginUtilities.getDialogSettings( KalypsoModelFloodPlugin.getDefault(), "externalWspFileImport" ); //$NON-NLS-1$
    m_fileChooserGroup.setDialogSettings( dialogSettings );
  }

  /**
   * @see org.kalypso.core.gml.provider.IGmlSourceProvider#createLabelProvider()
   */
  @Override
  public ILabelProvider createLabelProvider( )
  {
    return new LabelProvider();
  }

  /**
   * @see org.kalypso.core.gml.provider.IGmlSourceProvider#createSource(java.lang.Object)
   */
  @Override
  public IGmlSource createSource( final Object element )
  {
    Assert.isTrue( element == THE_ELEMENT );

    if( m_file == null )
      return null;

    final boolean needsCrs = checkNeedsCrs( m_file );
    final String crs = needsCrs ? m_crsPanel.getSelectedCRS() : null;
    // FIXME:commented out, due to racing condition in crs-combo, this will never be true when the page tries to figure
    // out if the page is Complete.
    // if( needsCrs && crs == null )
    // return null;

    final String name = m_file.getName();
    final String description = ""; //$NON-NLS-1$

    try
    {
      final URL fileUrl = m_file.toURI().toURL();
      final URL location = crs == null ? fileUrl : new URL( fileUrl.toExternalForm() + "?srs=" + crs ); //$NON-NLS-1$

      // TODO: we should set here a correct xpath to the GM_TriangualtedSurface (for tins)
      return new GmlSource( name, description, location, null );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();

      return null;
    }
  }

  /**
   * Checks, if the given file needs a coordinate system defined by the user.
   */
  private boolean checkNeedsCrs( final File file )
  {
    final String ext = FileUtilities.getSuffix( file );
    if( ext == null )
      return false;

    final String extLow = ext.toLowerCase();
    if( "hmo".equals( extLow ) || "shp".equals( extLow ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return true;

    return false;
  }

  protected void handleFileChanged( final File file )
  {
    m_file = file;

    m_crsPanel.setVisible( file != null && checkNeedsCrs( file ) );

    // TODO: should notifiy dialog that page is not complete
  }


  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object parentElement )
  {
    return new Object[] {};
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  @Override
  public Object getParent( final Object element )
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  @Override
  public boolean hasChildren( final Object element )
  {
    return false;
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  @Override
  public Object[] getElements( final Object inputElement )
  {
    return new Object[] { THE_ELEMENT };
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  @Override
  public void dispose( )
  {
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  @Override
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
  }

}
