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
package org.kalypso.model.wspm.tuhh.ui.panel;

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
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.changes.ProfileObjectEdit;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
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

  protected Label m_parameterLabel;

  private DeviderLine m_wehrStart;

  private DeviderLine m_wehrEnd;

  protected FormToolkit m_toolkit;

  protected final WeirLabelProvider m_labelProvider = new WeirLabelProvider();

  public WeirPanel( final IProfil profile )
  {
    super( profile );

    m_deviderGroupData = new GridData( SWT.FILL, SWT.FILL, true, false, 2, 1 );
    m_deviderGroupData.exclude = false;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final FormToolkit toolkit )
  {
    m_toolkit = toolkit;
    final IProfil profile = getProfil();
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
    m_wehrart.setInput( m_labelProvider.getTypes() );
    m_wehrart.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final BuildingWehr building = WspmProfileHelper.getBuilding( getProfil(), BuildingWehr.class );
        if( building == null )
          return;

        final IStructuredSelection selection = (IStructuredSelection) m_wehrart.getSelection();
        if( selection.isEmpty() )
          return;

        final String id = selection.getFirstElement().toString();
        final IComponent cWehr = building.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART );

        if( id.equals( building.getValue( cWehr ) ) )
          return;
        final IProfilChange change = new ProfileObjectEdit( building, cWehr, id );
        final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.32" ), getProfil(), change, true ); //$NON-NLS-1$
        new ProfilOperationJob( operation ).schedule();
      }
    } );

    final IProfilPointMarker[] devider = profile.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final IProfilPointMarker leftTF = devider.length < 1 ? null : devider[0];
    final IProfilPointMarker rightTF = devider.length < 2 ? null : devider[1];

    m_wehrStart = new DeviderLine( m_toolkit, panel, leftTF, true, profile );
    new ParameterLine( m_toolkit, panel, leftTF, false, profile );

    // Wehrparameter Group
    m_deviderGroup = toolkit.createComposite( panel );
    m_deviderGroup.setLayout( new GridLayout( 1, false ) );
    m_deviderGroup.setLayoutData( m_deviderGroupData );

    m_wehrEnd = new DeviderLine( m_toolkit, panel, rightTF, false, profile );
    updateControls();
    panel.layout( true, true );
    return panel;
  }

  protected void updateControls( )
  {
    if( m_wehrart.getCombo().isDisposed() )
      return;

    final IProfil profile = getProfil();
    final BuildingWehr building = WspmProfileHelper.getBuilding( profile, BuildingWehr.class );
    if( building == null )
      return;

    // FIXME: this is too weak! Maybe a marker has been added/removed -> we need to re-create the DeviderLine's in that
    // case!

    final IComponent objProp = building.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART );
    final String id = (String) building.getValue( objProp );
    if( id != null )
      m_wehrart.setSelection( new StructuredSelection( id ) );
    m_wehrStart.refresh();
    m_wehrEnd.refresh();

    final IComponent cmpWehrTrenner = profile.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_WEHR );
    final IProfilPointMarker[] deviders = profile.getPointMarkerFor( cmpWehrTrenner );
    if( deviders.length * 2 != m_deviderGroup.getChildren().length )
    {
      for( final Control ctrl : m_deviderGroup.getChildren() )
      {
        if( ctrl != null && !ctrl.isDisposed() )
          ctrl.dispose();
      }

      for( final IProfilPointMarker devider : deviders )
      {
        new DeviderLine( m_toolkit, m_deviderGroup, devider, true, profile );
        new ParameterLine( m_toolkit, m_deviderGroup, devider, true, profile );
      }
    }

    // FIXME: all this layout makes no real sense. However, we would like to reflow the ScrolledForm this panel is
    // contained in
    // ergo -> we need to be able to fire an event in order to reflow/relayout the From.

    m_deviderGroup.getParent().layout();
  }

  @Override
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    if( hint.isObjectDataChanged() || hint.isProfilPropertyChanged() || hint.isMarkerMoved() || hint.isMarkerDataChanged() )
    {
      ControlUtils.asyncExec( getControl(), new Runnable()
      {
        @Override
        public void run( )
        {
          updateControls();
        }
      } );
    }
  }
}