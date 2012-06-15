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
package org.kalypso.model.km.internal.ui.kmupdate;

import java.io.File;
import java.net.URL;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.km.internal.KMPlugin;
import org.kalypso.model.km.internal.binding.KMBindingUtils;
import org.kalypso.model.km.internal.i18n.Messages;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;

import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovGroupType;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType;
import de.tu_harburg.wb.kalypso.rrm.kalininmiljukov.KalininMiljukovType.Profile;

/**
 * @author doemming
 */
public class KMUpdateWizardPage extends WizardPage
{
  private KMViewer m_kmViewer = null;

  private final Feature[] m_selection;

  private final CommandableWorkspace m_workspace;

  private CheckboxTableViewer m_channelListViewer;

  private KalininMiljukovGroupType m_kmGroup = null;

  private String m_configPath = null;

  private final KMUpdateLabelProvider m_kmUpdateLabelProvider = new KMUpdateLabelProvider();

  private final NaModell m_naModel;

  public KMUpdateWizardPage( final CommandableWorkspace workspace, final IFeatureSelection selection )
  {
    super( "kmPage", Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.0" ), null ); //$NON-NLS-1$ //$NON-NLS-2$

    setDescription( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.1" ) ); //$NON-NLS-1$

    m_workspace = workspace;
    m_naModel = (NaModell) m_workspace.getRootFeature();

    m_selection = FeatureSelectionHelper.getFeatures( selection );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    final Composite top = new Composite( parent, SWT.NONE );
    top.setLayout( new GridLayout( 2, true ) );

    // column 1 tree-group
    final Group treeGroup = createKMChannelListViewer( top );
    treeGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    // column 2 KM detailed
    final Group kmGroup = new Group( top, SWT.NONE );
    kmGroup.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.3" ) ); //$NON-NLS-1$
    kmGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_kmViewer = new KMViewer( kmGroup );

    final Composite configPanel = createGenerateArea( top );
    configPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false, 2, 1 ) );

    setControl( top );
  }

  private Composite createGenerateArea( final Composite parent )
  {
    final Composite configPanel = new Composite( parent, SWT.NONE );
    configPanel.setLayout( new GridLayout( 4, false ) );

    final Label label = new Label( configPanel, SWT.NONE );
    label.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.4" ) ); //$NON-NLS-1$

    final Text text = new Text( configPanel, SWT.READ_ONLY | SWT.BORDER );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Button loadButton = new Button( configPanel, SWT.PUSH );
    loadButton.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.5" ) ); //$NON-NLS-1$
    loadButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    final Button saveButton = new Button( configPanel, SWT.PUSH );
    saveButton.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.6" ) ); //$NON-NLS-1$
    saveButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    loadButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleLoadConfig( text );
      }
    } );

    saveButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleSaveconfig( text );
      }
    } );

    final String path = getDialogSettings().get( getResourceKey() );
    m_configPath = path;
    final KalininMiljukovGroupType kmGroup = KMBindingUtils.toKMConfiguration( m_naModel );
    setKMGroup( kmGroup );

    if( !StringUtils.isBlank( path ) )
    {
      text.setText( m_configPath );
      loadAs( path );
    }

    return configPanel;
  }

  protected void handleSaveconfig( final Text text )
  {
    final FileDialog dialog = new FileDialog( getShell(), SWT.SAVE );
    dialog.setFilterExtensions( new String[] { "*.km_xml" } ); //$NON-NLS-1$
    final String path = dialog.open();
    if( path == null || path.length() <= 0 )
      return;

    setConfigPath( text, path );
    saveAs( path );
  }

  protected void handleLoadConfig( final Text text )
  {
    final FileDialog dialog = new FileDialog( getShell(), SWT.OPEN );
    dialog.setFilterExtensions( new String[] { "*.km_xml" } ); //$NON-NLS-1$
    final String path = dialog.open();
    if( path == null || path.length() <= 0 )
      return;

    setConfigPath( text, path );
    loadAs( path );
  }

  private void setConfigPath( final Text text, final String path )
  {
    text.setText( path );
    m_configPath = path;
    getDialogSettings().put( getResourceKey(), m_configPath );
  }

  protected void setKMGroup( final KalininMiljukovGroupType kmGroup )
  {
    m_kmGroup = kmGroup;
    if( m_channelListViewer == null )
      return;

    m_channelListViewer.refresh();
    m_channelListViewer.setSelection( StructuredSelection.EMPTY );
  }

  private Group createKMChannelListViewer( final Composite parent )
  {
    final Group treeGroup = new Group( parent, SWT.FILL );
    treeGroup.setLayout( new FillLayout() );
    treeGroup.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.2" ) ); //$NON-NLS-1$

    m_channelListViewer = CheckboxTableViewer.newCheckList( treeGroup, SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER );

    final KMChannel[] kmChannels = m_naModel.getKMChannels();

    m_channelListViewer.setContentProvider( new ArrayContentProvider() );

    m_channelListViewer.setLabelProvider( m_kmUpdateLabelProvider );
    m_channelListViewer.setInput( kmChannels );

    m_channelListViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        handleChannelListChanged( selection );
      }
    } );

    if( m_selection.length > 0 )
      m_channelListViewer.setSelection( new StructuredSelection( m_selection[0] ) );
    m_channelListViewer.setCheckedElements( m_selection );

    return treeGroup;
  }

  protected void handleChannelListChanged( final IStructuredSelection selection )
  {
    if( m_kmViewer == null )
      return;

    final Object firstElement = selection.getFirstElement();
    if( firstElement instanceof KMChannel )
    {
      final KMChannel channel = (KMChannel) firstElement;
      final String fid = channel.getId();
      final KalininMiljukovType km = getForID( fid );

      final String label = m_kmUpdateLabelProvider.getText( channel );
      m_kmViewer.setInput( label, km );
    }
    else
      m_kmViewer.setInput( "", null ); //$NON-NLS-1$
  }

  void loadAs( final String path )
  {
    try
    {
      final File file = new File( path );
      if( !file.exists() )
        return;

      final KalininMiljukovGroupType kmGroup = KMBindingUtils.load( file );
      applyKMGroup( kmGroup );
    }
    catch( final JAXBException ex )
    {
      final String windowTitle = getWizard().getWindowTitle();
      final String message = Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.12", ex.toString() ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KMPlugin.getID(), message, ex );
      KMPlugin.getDefault().getLog().log( status );
      new StatusDialog( getShell(), status, windowTitle ).open();
    }
  }

  private void applyKMGroup( final KalininMiljukovGroupType loadedGroup )
  {
    final List<KalininMiljukovType> kalininMiljukov = loadedGroup.getKalininMiljukov();
    for( final KalininMiljukovType loadedKmType : kalininMiljukov )
    {
      final String id = loadedKmType.getId();
      final KalininMiljukovType kmType = getForID( id );
      if( kmType != null )
      {
        // clone into
        kmType.setFilePattern( loadedKmType.getFilePattern() );
        kmType.setPath( loadedKmType.getPath() );
        kmType.setKmStart( loadedKmType.getKmStart() );
        kmType.setKmEnd( loadedKmType.getKmEnd() );
        kmType.setRiverName( loadedKmType.getRiverName() );

        final List<Profile> profiles = kmType.getProfile();
        profiles.clear();
        final List<Profile> loadedProfiles = loadedKmType.getProfile();
        profiles.addAll( loadedProfiles );
      }
    }
  }

  boolean saveAs( final String path )
  {
    try
    {
      final File file = new File( path );
      KMBindingUtils.save( m_kmGroup, file );
      return true;
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
      final String msg = Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.16", e.getLocalizedMessage() ); //$NON-NLS-1$
      MessageDialog.openError( getShell(), getWizard().getWindowTitle(), msg );
      return false;
    }
  }

// FIXME: move into own class!
  public boolean finish( )
  {
    return StringUtils.isBlank( m_configPath ) || saveAs( m_configPath );
  }

  protected KalininMiljukovType getForID( final String fid )
  {
    if( m_kmGroup == null )
      return null;

    final List<KalininMiljukovType> kalininMiljukov = m_kmGroup.getKalininMiljukov();
    for( final Object element : kalininMiljukov )
    {
      final KalininMiljukovType km = (KalininMiljukovType) element;
      if( fid.equals( km.getId() ) )
        return km;
    }
    return null;
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#isPageComplete()
   */
  @Override
  public boolean isPageComplete( )
  {
    return m_channelListViewer.getCheckedElements().length > 0;
  }

  private String getResourceKey( )
  {
    final String base = "kalypsoRRM.kmUpdate.configPath"; //$NON-NLS-1$
    final URL context = m_workspace.getContext();
    if( context == null )
      return base;

    return base + context.toString();
  }

  public Map<KMChannel, KalininMiljukovType> getSelectedChannels( )
  {
    final Object[] checkedElements = m_channelListViewer.getCheckedElements();
    final Map<KMChannel, KalininMiljukovType> channels = new HashMap<KMChannel, KalininMiljukovType>();

    for( final Object checkedElement : checkedElements )
    {
      final KMChannel channel = (KMChannel) checkedElement;
      final KalininMiljukovType km = getForID( channel.getId() );
      channels.put( channel, km );
    }

    return Collections.unmodifiableMap( channels );
  }

  public KMUpdateOperation createOperation( )
  {
    final Map<KMChannel, KalininMiljukovType> checkedChannels = getSelectedChannels();
    return new KMUpdateOperation( m_workspace, checkedChannels );
  }
}
