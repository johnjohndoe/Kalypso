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
package org.kalypso.model.wspm.tuhh.ui.panel.buildings;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.changes.ProfileChangeHint;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr.WeirType;
import org.kalypso.model.wspm.tuhh.core.util.river.line.WspmSohlpunkte;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.observation.result.IComponent;

/**
 * @author kimwerner
 */
public class WeirPanel extends AbstractProfilView
{
  protected ComboViewer m_wehrart;

  protected Composite m_deviderGroup;

  private final GridData m_deviderGroupData;

  private ParameterLine m_parameterLine;

  /**
   * TODO:refactor or remove Parameter and DeviderLines, ugly unreadable code
   */
  private DeviderLine m_wehrStart;

  private DeviderLine m_wehrEnd;

  protected FormToolkit m_toolkit;

  protected final WeirLabelProvider m_labelProvider = new WeirLabelProvider();

  public WeirPanel( final IProfile profile )
  {
    super( profile );

    m_deviderGroupData = new GridData( SWT.FILL, SWT.FILL, true, false, 2, 1 );
    m_deviderGroupData.exclude = false;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite, org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final FormToolkit toolkit )
  {
    m_toolkit = toolkit;
    final IProfile profile = getProfile();
    final Composite panel = toolkit.createComposite( parent );
    panel.setLayout( new GridLayout( 2, false ) );

    // Wehrart ComboBox

    final Label label = toolkit.createLabel( panel, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.31" ), SWT.NONE ); //$NON-NLS-1$
    label.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false ) );

    m_wehrart = new ComboViewer( panel, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.BORDER );
    m_wehrart.getCombo().setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, true, false ) );
    toolkit.adapt( m_wehrart.getCombo() );

    m_wehrart.setContentProvider( new ArrayContentProvider() );
    m_wehrart.setLabelProvider( m_labelProvider );
    m_wehrart.setInput( WeirType.values() );
    m_wehrart.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final BuildingWehr building = WspmSohlpunkte.getBuilding( getProfile(), BuildingWehr.class );
        if( building == null )
          return;

        final IStructuredSelection selection = (IStructuredSelection)m_wehrart.getSelection();
        if( selection.isEmpty() )
          return;

        final WeirType weirType = (WeirType)selection.getFirstElement();
        if( weirType == building.getWehrart() )
          return;

        building.setWehrart( weirType );
      }
    } );

    final IProfilePointMarker[] devider = profile.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final IProfilePointMarker leftTF = devider.length < 1 ? null : devider[0];
    // final IProfilPointMarker rightTF = devider.length < 2 ? null : devider[1];

    m_wehrStart = new DeviderLine( m_toolkit, panel, 0, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, true, profile );
    m_parameterLine = new ParameterLine( m_toolkit, panel, leftTF, false, profile );

    // Wehrparameter Group
    m_deviderGroup = toolkit.createComposite( panel );
    m_deviderGroup.setLayout( new GridLayout( 1, false ) );
    m_deviderGroup.setLayoutData( m_deviderGroupData );

    m_wehrEnd = new DeviderLine( m_toolkit, panel, 1, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, false, profile );
    updateControls();
    return panel;
  }

  protected void updateControls( )
  {
    if( m_wehrart.getCombo().isDisposed() )
      return;

    final IProfile profile = getProfile();
    final BuildingWehr building = WspmSohlpunkte.getBuilding( profile, BuildingWehr.class );
    if( building == null )
      return;

    final WeirType weirType = building.getWehrart();
    m_wehrart.setSelection( new StructuredSelection( weirType ) );

    m_wehrStart.refresh();
    m_wehrEnd.refresh();
    m_parameterLine.refresh();
    updateDeviderGroup( profile );
    m_deviderGroup.getParent().layout( true, true );
  }

  private void updateDeviderGroup( final IProfile profile )
  {
    final Control[] ctrls = m_deviderGroup.getChildren();
    for( final Control ctrl : ctrls )
    {
      if( !ctrl.isDisposed() )
        ctrl.dispose();
    }

    final IComponent cmpWehrTrenner = profile.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_WEHR );
    final IProfilePointMarker[] deviders = profile.getPointMarkerFor( cmpWehrTrenner );
    int i = 0;
    for( final IProfilePointMarker devider : deviders )
    {
      final DeviderLine devLine = new DeviderLine( m_toolkit, m_deviderGroup, i++, IWspmTuhhConstants.MARKER_TYP_WEHR, true, profile );
      devLine.refresh();
      new ParameterLine( m_toolkit, m_deviderGroup, devider, true, profile );
    }
  }

  @Override
  public void onProfilChanged( final ProfileChangeHint hint )
  {
    if( hint.isObjectDataChanged() || hint.isProfilPropertyChanged() || hint.isMarkerMoved() || hint.isMarkerDataChanged() )
    {
      final UIJob job = new UIJob( Messages.getString( "WeirPanel.0" ) ) //$NON-NLS-1$
      {
        @Override
        public IStatus runInUIThread( final IProgressMonitor monitor )
        {
          updateControls();

          return Status.OK_STATUS;
        }
      };

      job.setUser( false );
      job.setSystem( true );
      job.schedule();
    }
  }
}