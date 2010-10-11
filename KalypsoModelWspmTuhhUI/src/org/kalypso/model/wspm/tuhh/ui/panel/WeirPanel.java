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
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerEdit;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerSetPoint;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.changes.ProfileObjectEdit;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.BuildingUtil;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIImages;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */
public class WeirPanel extends AbstractProfilView
{
  private class DeviderLine
  {
    protected final IProfilPointMarker m_devider;

    protected final Text m_position;

    protected Composite m_composite;

    public DeviderLine( final Composite parent, final IProfilPointMarker devider, final boolean canAdd )
    {

      m_devider = devider;
      m_composite = m_toolkit.createComposite( parent );
      m_composite.setLayout( new GridLayout( 3, false ) );
      final GridData gD =new GridData( SWT.FILL, SWT.TOP, true, false, 2, 1 );
      gD.horizontalSpan = 2;
      m_composite.setLayoutData( gD );

      final Display display = parent.getDisplay();
      final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
      final Color badColor = display.getSystemColor( SWT.COLOR_RED );
      final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );

      m_toolkit.createLabel( m_composite, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.0" ) ); //$NON-NLS-1$
      m_position = m_toolkit.createText( m_composite, "", SWT.TRAIL | SWT.SINGLE | SWT.BORDER ); //$NON-NLS-1$
      m_position.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, true, false ) );

// final Label spacer = m_toolkit.createSeparator( m_composite, SWT.SEPARATOR | SWT.HORIZONTAL );
// spacer.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

      final Button btnAdd = m_toolkit.createButton( m_composite, "", SWT.NONE ); //$NON-NLS-1$
      btnAdd.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );
      btnAdd.setToolTipText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.7" ) ); //$NON-NLS-1$
      btnAdd.setImage( m_addImg );
      btnAdd.setEnabled( canAdd );
      btnAdd.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent e )
        {

          final IProfilPointMarker marker = m_devider;
          final IProfil profil = getProfil();
          final IRecord point = profil.getPoint( getProfil().indexOfPoint( marker.getPoint() ) + 1 );

          final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.8" ), profil, true ); //$NON-NLS-1$
          final IProfilPointMarker trenner = profil.createPointMarker( IWspmTuhhConstants.MARKER_TYP_WEHR, point );

          if( trenner != null )
          {
            final Object objVal = marker.getValue();

            final BuildingWehr building = WspmProfileHelper.getBuilding( profil, BuildingWehr.class );
            if( building == null )
              return;

            final Object dblVal = (objVal instanceof Double) ? objVal : BuildingUtil.getDoubleValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT, building );
            operation.addChange( new PointMarkerEdit( trenner, dblVal ) );
            operation.addChange( new ActiveObjectEdit( getProfil(), point, null ) );
            new ProfilOperationJob( operation ).schedule();
          }
        }
      } );
      m_position.addModifyListener( doubleModifyListener );
      m_position.addFocusListener( new FocusAdapter()
      {
        @Override
        public void focusGained( final FocusEvent e )
        {
          if( (m_position != null) && !m_position.isDisposed() )
            m_position.selectAll();
        }

        /**
         * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
         */
        @Override
        public void focusLost( final FocusEvent e )
        {
          if( m_position == null || m_position.isDisposed() || m_devider == null )
            return;

          final double value = NumberUtils.parseQuietDouble( m_position.getText() );
          if( Double.isNaN( value ) )
            return;

          final Double pos = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, m_devider.getPoint() );
          if( ProfilUtil.compareValues( value, pos, 0.0001 ) )
            return;
          final IRecord point = ProfilUtil.findNearestPoint( getProfil(), value );
          final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.2" ), getProfil(), true ); //$NON-NLS-1$
          operation.addChange( new PointMarkerSetPoint( m_devider, point ) );
          operation.addChange( new ActiveObjectEdit( getProfil(), point, null ) );
          new ProfilOperationJob( operation ).schedule();
        }
      } );
    }

    public void dispose( )
    {
      m_composite.dispose();
    }

    public void refresh( )
    {
      m_position.setText( String.format( "%.4f", ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, m_devider.getPoint() ) ) );

    }
  }

  private class ParameterLine
  {
    protected final IProfilPointMarker m_devider;

    protected final Composite m_composite;

    protected final Text m_value;

    public ParameterLine( final Composite parent, final IProfilPointMarker devider, final boolean canDelete )
    {
      m_composite = m_toolkit.createComposite( parent );
      m_composite.setLayout( new GridLayout( 3, false ) );
      final GridData gD =new GridData( SWT.CENTER, SWT.CENTER, true, false, 2, 1 );
      gD.horizontalSpan=2;
      m_composite.setLayoutData( gD );

      m_devider = devider;

      m_toolkit.createLabel( m_composite, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.9" ) ); //$NON-NLS-1$
      m_value = m_toolkit.createText( m_composite, "", SWT.TRAIL | SWT.SINGLE | SWT.BORDER ); //$NON-NLS-1$
      final Button btnDel = m_toolkit.createButton( m_composite, "", SWT.NONE ); //$NON-NLS-1$
      btnDel.setToolTipText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.4" ) ); //$NON-NLS-1$
      btnDel.setImage( m_deleteImg );
      btnDel.setEnabled( canDelete );
      btnDel.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent e )
        {

          final IProfilChange change = new PointMarkerEdit( m_devider, null );
          final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.5" ), getProfil(), change, true ); //$NON-NLS-1$
          new ProfilOperationJob( operation ).schedule();
        }
      } );

      m_composite.layout();
    }

    public void dispose( )
    {
      m_composite.dispose();
    }
  }

  protected ComboViewer m_wehrart;

  protected Composite m_deviderGroup;

  private final GridData m_deviderGroupData;

  protected final Image m_deleteImg;

  protected final Image m_addImg;

  protected Label m_parameterLabel;

  private DeviderLine m_wehrStart;

  private DeviderLine m_wehrEnd;

  protected FormToolkit m_toolkit;

  protected final WeirLabelProvider m_labelProvider = new WeirLabelProvider();

  public WeirPanel( final IProfil profile )
  {
    super( profile );
    m_deleteImg = KalypsoModelWspmUIImages.ID_BUTTON_WEHR_DELETE.createImage();
    m_addImg = KalypsoModelWspmUIImages.ID_BUTTON_WEHR_ADD.createImage();
    m_deviderGroupData = new GridData( SWT.FILL, SWT.FILL, true, false, 2, 1 );
    m_deviderGroupData.exclude = false;
  }

  @Override
  public void dispose( )
  {
    super.dispose();

    m_deleteImg.dispose();
    m_addImg.dispose();
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
    panel.setLayout( new GridLayout(1, false ) );
    panel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    // Wehrart ComboBox
 

    final Label label = toolkit.createLabel( panel, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.31" ), SWT.NONE ); //$NON-NLS-1$
    label.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false) );//new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING ) );

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

    m_wehrStart = new DeviderLine( panel, devider.length < 1 ? null : devider[0],true );
    new ParameterLine( panel, devider[0], false );
    // Wehrparameter Group
    m_deviderGroup = toolkit.createComposite( panel );
    m_deviderGroup.setLayout( new GridLayout( 1, false ) );
    m_deviderGroup.setLayoutData( m_deviderGroupData );

    m_wehrEnd = new DeviderLine( panel, devider.length < 2 ? null : devider[1],false );
    updateControls();
    panel.layout( true, true );
    return panel;
  }

  protected void updateControls( )
  {
    if( m_wehrart.getCombo().isDisposed() )
      return;

    final BuildingWehr building = WspmProfileHelper.getBuilding( getProfil(), BuildingWehr.class );
    if( building == null )
      return;

    final IComponent objProp = building.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART );
    final String id = (String) building.getValue( objProp );
    if( id != null )
      m_wehrart.setSelection( new StructuredSelection( id ) );
    m_wehrStart.refresh();
    m_wehrEnd.refresh();

    final IComponent cmpWehrTrenner = getProfil().hasPointProperty( IWspmTuhhConstants.MARKER_TYP_WEHR );
    final IProfilPointMarker[] deviders = getProfil().getPointMarkerFor( cmpWehrTrenner );
    if( deviders.length * 2 != m_deviderGroup.getChildren().length )
    {
      for( final Control ctrl : m_deviderGroup.getChildren() )
      {
        if( ctrl != null && !ctrl.isDisposed() )
          ctrl.dispose();
      }
      // m_deviderGroupData.exclude = deviders.length == 0;
      for( final IProfilPointMarker devider : deviders )
      {
        new DeviderLine( m_deviderGroup, devider,true );
        new ParameterLine( m_deviderGroup, devider, true );
      }
    }
    for( final Control ctrl : m_deviderGroup.getChildren() )
    {
      if( ctrl != null && !ctrl.isDisposed() )
      {

      }
    }
    m_deviderGroup.getParent().layout();
  }

  @Override
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    if( hint.isObjectDataChanged() || hint.isProfilPropertyChanged() || hint.isMarkerMoved() || hint.isMarkerDataChanged() )
    {
      final Control control = getControl();
      if( control != null && !control.isDisposed() )
        control.getDisplay().asyncExec( new Runnable()
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