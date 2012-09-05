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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.chart.ui.editor.commandhandler.ChartSourceProvider;
import org.kalypso.commons.databinding.forms.DatabindingForm;
import org.kalypso.commons.eclipse.ui.EmbeddedSourceToolbarManager;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.contribs.eclipse.jface.action.CommandWithStyle;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.IProfileData;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.overlay.ProfilOverlayLayerProvider;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.ui.dialog.compare.ProfileChartComposite;
import org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class CreateChannelProfileSection extends Composite
{
  private EmbeddedSourceToolbarManager m_sourceManager;

  private Label m_noProfileLabel;

  private ProfileChartComposite m_profilComposite;

  private ToolBarManager m_toolbarManager;

  private Composite m_toolbarPanel;

  public CreateChannelProfileSection( final FormToolkit toolkit, final Composite parent, final CreateChannelData data, final DatabindingForm binding )
  {
    super( parent, SWT.NONE );

    toolkit.adapt( this );

    GridLayoutFactory.fillDefaults().margins( 0, 0 ).spacing( 0, 0 ).applyTo( this );

    createControls( toolkit, this, data, binding );

    ControlUtils.addDisposeListener( this );

    data.addPropertyChangeListener( CreateChannelData.PROPERTY_ACTIVE_PROFILE, new PropertyChangeListener()
    {
      @Override
      public void propertyChange( final PropertyChangeEvent evt )
      {
        onProfileChanged( data.getActiveProfile() );
      }
    } );

    onProfileChanged( data.getActiveProfile() );
  }

  @Override
  public void dispose( )
  {
    super.dispose();

    if( m_sourceManager != null )
      m_sourceManager.dispose();
  }

  private void createControls( final FormToolkit toolkit, final Composite parent, final CreateChannelData data, final DatabindingForm binding )
  {
    createNoProfileLabel( toolkit, parent );

    createToolbarElements( toolkit, parent, data, binding );

    createChart( toolkit, parent );
  }

  private void createToolbarElements( final FormToolkit toolkit, final Composite parent, final CreateChannelData data, final DatabindingForm binding )
  {
    m_toolbarPanel = toolkit.createComposite( parent );

    final RowLayout layout = new RowLayout();
    layout.center = true;
    layout.wrap = false;
    layout.marginTop = 0;
    layout.marginBottom = 0;
    layout.marginLeft = 0;
    layout.marginRight = 0;

    m_toolbarPanel.setLayout( layout );
    // GridLayoutFactory.fillDefaults().numColumns( 3 ).applyTo( m_toolbarPanel );
    m_toolbarPanel.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false ) );

    createToolbar( toolkit, m_toolbarPanel );
    createProfileSelector( toolkit, m_toolbarPanel, data, binding );
    toolkit.createLabel( m_toolbarPanel, StringUtils.EMPTY );
    createAutoZoomCheckbox( toolkit, m_toolbarPanel, data, binding );
  }

  private void createNoProfileLabel( final FormToolkit toolkit, final Composite parent )
  {
    m_noProfileLabel = toolkit.createLabel( parent, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.41" ) ); //$NON-NLS-1$
    m_noProfileLabel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
  }

  private void createProfileSelector( final FormToolkit toolkit, final Composite parent, final CreateChannelData data, final DatabindingForm binding )
  {
    /* prev button */
    final Action prevProfileAction = new SwitchProfileAction( data, -1 );
    ActionButton.createButton( toolkit, parent, prevProfileAction, SWT.ARROW | SWT.LEFT );

    /* selector */
    final ComboViewer profileChooser = new ComboViewer( parent, SWT.READ_ONLY | SWT.DROP_DOWN );

    final Control profileChooserControl = profileChooser.getControl();
    // profileChooserControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    toolkit.adapt( profileChooserControl, true, true );

    profileChooser.setContentProvider( new ArrayContentProvider() );
    profileChooser.setLabelProvider( new LabelProvider() );

    final IObservableValue targetChooserInput = ViewersObservables.observeInput( profileChooser );
    final IObservableValue modelChooserInput = BeansObservables.observeValue( data, CreateChannelData.PROPERTY_PROFILE_DATA_CHOOSER_INPUT );
    binding.bindValue( targetChooserInput, modelChooserInput );

    final IObservableValue targetChooserEnabled = SWTObservables.observeEnabled( profileChooserControl );
    final IObservableValue modelProfileEditingEnabled = BeansObservables.observeValue( data, CreateChannelData.PROPERTY_PROFILE_EDITING_ENABLED );
    binding.bindValue( targetChooserEnabled, modelProfileEditingEnabled );

    final IObservableValue targetChooserSelection = ViewersObservables.observeSinglePostSelection( profileChooser );
    final IObservableValue modelChooserSelection = BeansObservables.observeValue( data, CreateChannelData.PROPERTY_ACTIVE_PROFILE );
    binding.bindValue( targetChooserSelection, modelChooserSelection );

    /* next button */
    final Action nextProfileAction = new SwitchProfileAction( data, +1 );
    ActionButton.createButton( toolkit, parent, nextProfileAction, SWT.ARROW | SWT.RIGHT );
  }

  private void createAutoZoomCheckbox( final FormToolkit toolkit, final Composite parent, final CreateChannelData data, final DatabindingForm binding )
  {
    /* zoom to extent button */
    final String checkboxAutoZoomLabel = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.2" ); //$NON-NLS-1$

    final Button checkboxAutoZoom = toolkit.createButton( parent, checkboxAutoZoomLabel, SWT.CHECK );
    // checkboxAutoZoom.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    checkboxAutoZoom.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.43" ) ); //$NON-NLS-1$

    final ISWTObservableValue targetAutoZoom = SWTObservables.observeSelection( checkboxAutoZoom );
    final IObservableValue modelAutoZoom = BeansObservables.observeValue( data, CreateChannelData.PROPERTY_PROFILE_AUTO_ZOOM );
    binding.bindValue( targetAutoZoom, modelAutoZoom );

    final IObservableValue targetAutoZoomEnabled = SWTObservables.observeEnabled( checkboxAutoZoom );
    final IObservableValue modelProfileEditingEnabled = BeansObservables.observeValue( data, CreateChannelData.PROPERTY_PROFILE_EDITING_ENABLED );
    binding.bindValue( targetAutoZoomEnabled, modelProfileEditingEnabled );
  }

  private void createToolbar( final FormToolkit toolkit, final Composite parent )
  {
    m_toolbarManager = new ToolBarManager( SWT.HORIZONTAL | SWT.FLAT );

    final ToolBar toolbar = m_toolbarManager.createControl( parent );
    // toolbar.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    toolkit.adapt( toolbar );
  }

  private void createChart( final FormToolkit toolkit, final Composite parent )
  {
    final IProfilLayerProvider layerProvider = new ProfilOverlayLayerProvider();

    m_profilComposite = new ProfileChartComposite( parent, SWT.BORDER, layerProvider, null );
    toolkit.adapt( m_profilComposite );

    m_profilComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
  }

  protected void onProfileChanged( final IProfileData profileData )
  {
    final IProfileFeature feature = profileData == null ? null : profileData.getFeature();
    final IProfil profile = feature == null ? null : feature.getProfil();

    m_profilComposite.setProfil( profile, null );

    // FIXME: set segmented profile to overlay layer
    // final ILayerManager mngr = chartModel.getLayerManager();
    // final IChartLayer overlayLayer = mngr.findLayer( IWspmOverlayConstants.LAYER_OVERLAY );
    //
    // if( overlayLayer instanceof ProfilOverlayLayer )
    // {
    // if( currentSegment != null )
    // {
    // final IProfil layerData;
    // if( m_data.getCurrentProfile() == PROF.UP )
    // layerData = currentSegment.getProfUpIntersProfile();
    // else
    // layerData = currentSegment.getProfDownIntersProfile();
    //
    // ((ProfilOverlayLayer) overlayLayer).setProfile( layerData, m_data, m_widget );
    // m_widget.getMapPanel().repaintMap();
    // }
    // }

    if( isDisposed() )
      return;

    final Display display = getDisplay();
    if( display.isDisposed() )
      return;

    final Runnable runner = new Runnable()
    {
      @Override
      public void run( )
      {
        doUpdateControls( profile );
      }
    };

    display.asyncExec( runner );
  }

  protected void doUpdateControls( final IProfil profile )
  {
    /* update toolbar */
    if( m_sourceManager != null )
      m_sourceManager.dispose();

    m_toolbarManager.removeAll();

    m_sourceManager = new EmbeddedSourceToolbarManager( PlatformUI.getWorkbench(), ChartSourceProvider.ACTIVE_CHART_NAME, m_profilComposite );

    final Collection<CommandWithStyle> commands = new ArrayList<>();
    // TODO: use constants for commands
    commands.add( CommandWithStyle.radio( "org.kalypso.chart.ui.commands.zoom_pan_maximize" ) ); //$NON-NLS-1$
    commands.add( CommandWithStyle.radio( "org.kalypso.chart.ui.commands.pan" )); //$NON-NLS-1$
    commands.add( CommandWithStyle.radio( "org.kalypso.chart.ui.commands.edit" ) ); //$NON-NLS-1$
    commands.add( CommandWithStyle.separator(  ) );
    commands.add( CommandWithStyle.push( "org.kalypso.chart.ui.commands.maximize") ); //$NON-NLS-1$
    commands.add( CommandWithStyle.separator() );
    commands.add( CommandWithStyle.push( "org.kalypso.chart.ui.commands.ExportClipboardCommand" ) ); //$NON-NLS-1$
    commands.add( CommandWithStyle.push( "org.kalypso.chart.ui.commands.export" ) ); //$NON-NLS-1$
    commands.add( CommandWithStyle.separator() );

    m_sourceManager.fillToolbar( m_toolbarManager, commands.toArray( new CommandWithStyle[commands.size()] ) );
    m_toolbarManager.update( true );

    /* hide/show components */
    final boolean hasProfile = profile != null;

    showComponent( m_noProfileLabel, !hasProfile );
    showComponent( m_profilComposite, hasProfile );
    showComponent( m_toolbarPanel, hasProfile );

    layout();
  }

  private void showComponent( final Control control, final boolean show )
  {
    control.setVisible( show );

    final GridData data = (GridData) control.getLayoutData();
    data.exclude = !show;
  }
}