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
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
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
 * A wizard page for calculating the KM parameter with the results of a wspm calculation.
 * 
 * @author Andreas Doemming (original)
 * @author Holger Albert (modified)
 */
public class KMUpdateWizardPage extends WizardPage
{
  /**
   * The commandable workspace of the NA model.
   */
  private final CommandableWorkspace m_workspace;

  /**
   * The NA model.
   */
  private final NaModell m_naModel;

  /**
   * The selected features.
   */
  private final Feature[] m_features;

  /**
   * The channel list viewer.
   */
  private CheckboxTableViewer m_channelListViewer;

  /**
   * The KM viewer.
   */
  private KMViewer m_kmViewer = null;

  /**
   * The KM group (data model).
   */
  private KalininMiljukovGroupType m_kmGroup;

  /**
   * The configuration path.
   */
  protected String m_configPath;

  /**
   * The constructor.
   * 
   * @param workspace
   *          The commandable workspace of the NA model.
   * @param selection
   *          The selected features.
   */
  public KMUpdateWizardPage( final CommandableWorkspace workspace, final IFeatureSelection selection )
  {
    super( "kmPage", Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.0" ), null ); //$NON-NLS-1$ //$NON-NLS-2$

    /* Initialize the members. */
    m_workspace = workspace;
    m_naModel = (NaModell) workspace.getRootFeature();
    m_features = FeatureSelectionHelper.getFeatures( selection );
    m_channelListViewer = null;
    m_kmViewer = null;
    m_kmGroup = null;
    m_configPath = null;

    /* Set the description. */
    setDescription( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.1" ) ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    /* Create the main composite. */
    final Composite main = new Composite( parent, SWT.NONE );
    main.setLayout( new GridLayout( 2, true ) );

    /* Create the group of the left side. */
    final Group treeGroup = new Group( main, SWT.FILL );
    treeGroup.setLayout( new FillLayout() );
    treeGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    treeGroup.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.2" ) ); //$NON-NLS-1$

    /* Create the channel list viewer. */
    m_channelListViewer = createChannelListViewer( treeGroup );

    /* Create the group of the right side. */
    final Group kmGroup = new Group( main, SWT.NONE );
    kmGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    kmGroup.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.3" ) ); //$NON-NLS-1$
    m_kmViewer = new KMViewer( getContainer() );
    m_kmViewer.createControls( kmGroup );

    /* Create the config composite. */
    final Composite configComposite = createConfigComposite( main );
    configComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false, 2, 1 ) );

    /* Set the control. */
    setControl( main );
  }

  @Override
  public boolean isPageComplete( )
  {
    return m_channelListViewer.getCheckedElements().length > 0;
  }

  /**
   * This function creates the channel list viewer.
   * 
   * @param parent
   *          The parent composite.
   * @return The channel list viewer.
   */
  private CheckboxTableViewer createChannelListViewer( final Composite parent )
  {
    /* Get the km channels of the NA model. */
    final KMChannel[] kmChannels = m_naModel.getKMChannels();

    /* Create a checkbox table viewer. */
    final CheckboxTableViewer channelListViewer = CheckboxTableViewer.newCheckList( parent, SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER );

    /* Configure it. */
    channelListViewer.setContentProvider( new ArrayContentProvider() );
    channelListViewer.setLabelProvider( new KMUpdateLabelProvider() );
    channelListViewer.setInput( kmChannels );

    /* Add a listener. */
    channelListViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        handleChannelListChanged( selection );
      }
    } );

    /* If there are selected features, select the first one. */
    if( m_features.length > 0 )
    {
      /* Select the first selected feature. */
      channelListViewer.setSelection( new StructuredSelection( m_features[0] ) );

      /* Check all selected features. */
      channelListViewer.setCheckedElements( m_features );
    }

    return channelListViewer;
  }

  /**
   * This function creates the config composite.
   * 
   * @param parent
   *          The parent composite.
   * @return The config composite.
   */
  private Composite createConfigComposite( final Composite parent )
  {
    /* Create the config composite. */
    final Composite configComposite = new Composite( parent, SWT.NONE );
    configComposite.setLayout( new GridLayout( 4, false ) );

    /* Create a label. */
    final Label configLabel = new Label( configComposite, SWT.NONE );
    configLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );
    configLabel.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.4" ) ); //$NON-NLS-1$

    /* Create a text. */
    final Text configText = new Text( configComposite, SWT.READ_ONLY | SWT.BORDER );
    configText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Create a button. */
    final Button loadButton = new Button( configComposite, SWT.PUSH );
    loadButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );
    loadButton.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.5" ) ); //$NON-NLS-1$

    /* Create a button. */
    final Button saveButton = new Button( configComposite, SWT.PUSH );
    saveButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );
    saveButton.setText( Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.6" ) ); //$NON-NLS-1$

    /* Set a default value, if the dialog was used before. */
    m_configPath = getDialogSettings().get( getResourceKey() );

    /* Add a listener. */
    configText.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        final Text source = (Text) e.getSource();
        final String text = source.getText();
        m_configPath = text;
      }
    } );

    /* Add a listener. */
    loadButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleConfigButtonClicked( configText, SWT.OPEN );
      }
    } );

    /* Add a listener. */
    saveButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleConfigButtonClicked( configText, SWT.SAVE );
      }
    } );

    /* Create the KM group. */
    m_kmGroup = KMBindingUtils.toKMConfiguration( m_naModel );
    m_channelListViewer.refresh();
    m_channelListViewer.setSelection( StructuredSelection.EMPTY );

    /* If there is a config path set, load it. */
    if( !StringUtils.isBlank( m_configPath ) )
      loadAs( m_configPath );

    return configComposite;
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

      final KMUpdateLabelProvider labelProvider = new KMUpdateLabelProvider();
      final String label = labelProvider.getText( channel );
      m_kmViewer.setInput( label, km );
      return;
    }

    m_kmViewer.setInput( StringUtils.EMPTY, null );
  }

  protected void handleConfigButtonClicked( final Text text, final int style )
  {
    final FileDialog dialog = new FileDialog( getShell(), style );
    dialog.setFilterExtensions( new String[] { "*.km_xml" } ); //$NON-NLS-1$
    final String path = dialog.open();
    if( path == null || path.length() <= 0 )
      return;

    setConfigPath( text, path );

    if( style == SWT.SAVE )
      saveAs( path );
    else
      loadAs( path );
  }

  private void setConfigPath( final Text text, final String path )
  {
    text.setText( path );
    m_configPath = path;
    getDialogSettings().put( getResourceKey(), m_configPath );
  }

  private void loadAs( final String path )
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
      final IStatus status = new Status( IStatus.ERROR, KMPlugin.getID(), Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.12", ex.toString() ), ex );//$NON-NLS-1$

      KMPlugin.getDefault().getLog().log( status );

      final StatusDialog statusDialog = new StatusDialog( getShell(), status, getWizard().getWindowTitle() );
      statusDialog.open();
    }
  }

  private boolean saveAs( final String path )
  {
    try
    {
      KMBindingUtils.save( m_kmGroup, new File( path ) );

      return true;
    }
    catch( final JAXBException ex )
    {
      final IStatus status = new Status( IStatus.ERROR, KMPlugin.getID(), Messages.getString( "org.kalypso.ui.rrm.kmupdate.KMUpdateWizardPage.16", ex.toString() ), ex );//$NON-NLS-1$

      KMPlugin.getDefault().getLog().log( status );

      final StatusDialog statusDialog = new StatusDialog( getShell(), status, getWizard().getWindowTitle() );
      statusDialog.open();

      return false;
    }
  }

  private void applyKMGroup( final KalininMiljukovGroupType loadedGroup )
  {
    final List<KalininMiljukovType> loadedKalininMiljukov = loadedGroup.getKalininMiljukov();
    for( final KalininMiljukovType loadedKmType : loadedKalininMiljukov )
    {
      final String id = loadedKmType.getId();
      final KalininMiljukovType kmType = getForID( id );
      if( kmType != null )
      {
        /* Copy values in the km type of the km group, which is created in the wizard. */
        kmType.setFilePattern( loadedKmType.getFilePattern() );
        kmType.setPath( loadedKmType.getPath() );
        kmType.setKmStart( loadedKmType.getKmStart() );
        kmType.setKmEnd( loadedKmType.getKmEnd() );
        kmType.setRiverName( loadedKmType.getRiverName() );

        /* Clear old profiles. */
        final List<Profile> profiles = kmType.getProfile();
        profiles.clear();

        /* Add new profiles. */
        final List<Profile> loadedProfiles = loadedKmType.getProfile();
        profiles.addAll( loadedProfiles );
      }
    }
  }

  private KalininMiljukovType getForID( final String fid )
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

  private String getResourceKey( )
  {
    final String base = "kalypsoRRM.kmUpdate.configPath"; //$NON-NLS-1$
    final URL context = m_workspace.getContext();
    if( context == null )
      return base;

    return base + context.toString();
  }

  public boolean finish( )
  {
    return StringUtils.isBlank( m_configPath ) || saveAs( m_configPath );
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