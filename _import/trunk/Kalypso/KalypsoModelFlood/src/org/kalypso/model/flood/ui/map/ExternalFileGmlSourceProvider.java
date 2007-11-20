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
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChangedListener;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChooserDelegate;
import org.kalypso.core.gml.provider.GmlSource;
import org.kalypso.core.gml.provider.IGmlSource;
import org.kalypso.core.gml.provider.IGmlSourceProvider;
import org.kalypso.model.flood.KalypsoModelFloodPlugin;

/**
 * @author Thomas Jung
 * @author Gernot Belger
 */
public class ExternalFileGmlSourceProvider implements IGmlSourceProvider, ITreeContentProvider
{
  private static final Object THE_ELEMENT = "Externe Datei";

  private final FileChooserDelegate m_fileDelegate = new FileChooserDelegate( FileChooserDelegate.FILE_CHOOSER_GROUP_TYPE.eOpen )
  {
    /**
     * @see org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChooserDelegate#getFilterNames()
     */
    @Override
    public String[] getFilterNames( )
    {
      return new String[] { "HMO-Dateien (*.hmo)", "ESRI-Shape Dateien (*.shp)", "GML-Dateien (*.gml)" };
    }

    /**
     * @see org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChooserDelegate#getFilterExtensions()
     */
    @Override
    public String[] getFilterExtensions( )
    {
      return new String[] { "*.hmo", "*.shp", "*.gml" };
    }
  };

  private final FileChooserGroup m_fileChooserGroup = new FileChooserGroup( m_fileDelegate );

  private final FileChangedListener m_fileChangedListener = new FileChangedListener()
  {
    @SuppressWarnings("synthetic-access")
    public void fileChanged( File file )
    {
      m_file = file;
    }
  };

  private File m_file;

  public ExternalFileGmlSourceProvider( )
  {
    m_fileChooserGroup.addFileChangedListener( m_fileChangedListener );
  }

  /**
   * @see org.kalypso.core.gml.provider.IGmlSourceProvider#createContentProvider()
   */
  public ITreeContentProvider createContentProvider( )
  {
    return this;
  }

  /**
   * @see org.kalypso.core.gml.provider.IGmlSourceProvider#createInfoControl(org.eclipse.swt.widgets.Composite,
   *      java.lang.Object)
   */
  public void createInfoControl( Composite parent, Object element )
  {
    Assert.isTrue( element == THE_ELEMENT );

    Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout() );

    final IDialogSettings dialogSettings = PluginUtilities.getDialogSettings( KalypsoModelFloodPlugin.getDefault(), "externalWspFileImport" );
    m_fileChooserGroup.setDialogSettings( dialogSettings );

    Label label = new Label( panel, SWT.NONE );
    label.setText( "Externe Datei als Wasserspiegel-Tin importieren: " );

    Group fileControl = m_fileChooserGroup.createControl( panel, SWT.NONE );
    fileControl.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );
    fileControl.setText( "Externe Datei" );

    // SRS. TODO: ask Dirk for reusable SRS-ComboViewer

  }

  /**
   * @see org.kalypso.core.gml.provider.IGmlSourceProvider#createLabelProvider()
   */
  public ILabelProvider createLabelProvider( )
  {
    return new LabelProvider();
  }

  /**
   * @see org.kalypso.core.gml.provider.IGmlSourceProvider#createSource(java.lang.Object)
   */
  public IGmlSource createSource( Object element )
  {
    Assert.isTrue( element == THE_ELEMENT );

    final File externalFile = m_file;
    final String srs = "EPSG:31466"; // TODO: ask user for that

    if( externalFile == null || srs == null )
      return null;

    final String name = externalFile.getName();
    final String description = "";

    try
    {
      final URL fileUrl = externalFile.toURL();
      final URL location = new URL( fileUrl.toExternalForm() + "?srs=" + srs );

      return new GmlSource( name, description, location, null );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();

      return null;
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren( Object parentElement )
  {
    return new Object[] {};
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( Object element )
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren( Object element )
  {
    return false;
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( Object inputElement )
  {
    return new Object[] { THE_ELEMENT };
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {
    m_fileChooserGroup.removeFileChangedListener( m_fileChangedListener );
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  public void inputChanged( Viewer viewer, Object oldInput, Object newInput )
  {
  }

}
