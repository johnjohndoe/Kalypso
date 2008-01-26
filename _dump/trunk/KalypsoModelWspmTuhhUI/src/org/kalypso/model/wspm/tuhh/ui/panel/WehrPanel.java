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
package org.kalypso.model.wspm.tuhh.ui.panel;

import java.util.LinkedList;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
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
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerEdit;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerSetPoint;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.changes.ProfileObjectEdit;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.ProfilDevider;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr.WEHRART;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIImages;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.profil.operation.changes.VisibleMarkerEdit;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author belger
 */
/**
 * @author kimwerner
 */
public class WehrPanel extends AbstractProfilView
{

  private class DeviderLine extends Composite
  {
    protected IProfilPointMarker m_devider;

    private final int position;

    protected Text m_beiwert;

    protected Text m_point;

    private final Button m_buttonD;

    public DeviderLine( final int index )
    {
      super( m_deviderGroup, SWT.NONE );
      position = index;

      setLayout( new GridLayout( 4, false ) );
      final GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
      gridData.horizontalSpan = 4;
      setLayoutData( gridData );

      final Display display = m_deviderGroup.getDisplay();
      final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
      final Color badColor = display.getSystemColor( SWT.COLOR_RED );
      final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );

      final Label label1 = new Label( this, SWT.NONE );
      label1.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING ) );
      label1.setText( "Feld " + Integer.toString( position + 1 ) + ":" );

      m_beiwert = new Text( this, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
      m_beiwert.setLayoutData( new GridData( GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL ) );
      m_beiwert.addModifyListener( doubleModifyListener );
      m_beiwert.addFocusListener( new FocusAdapter()
      {
        @Override
        public void focusGained( final FocusEvent e )
        {
          m_beiwert.selectAll();
        }

        /**
         * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
         */
        @Override
        public void focusLost( final FocusEvent e )
        {
          final double value = NumberUtils.parseQuietDouble( m_beiwert.getText() );
          if( !Double.isNaN( value ) )
          {
            final IProfilChange[] changes = new IProfilChange[2];
            changes[0] = new PointMarkerEdit( m_devider, value );
            changes[1] = new ActiveObjectEdit( getProfil(), m_devider.getPoint(), null );
            final ProfilOperation operation = new ProfilOperation( "Wehrfeldparameter ändern", getProfilEventManager(), changes, true );
            new ProfilOperationJob( operation ).schedule();

          }
        }
      } );

      m_point = new Text( this, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
      m_point.setLayoutData( new GridData( GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL ) );
      m_point.addModifyListener( doubleModifyListener );
      m_point.addFocusListener( new FocusAdapter()
      {
        @Override
        public void focusGained( final FocusEvent e )
        {
          m_point.selectAll();
        }

        /**
         * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
         */
        @Override
        public void focusLost( final FocusEvent e )
        {
          final double value = NumberUtils.parseQuietDouble( m_point.getText() );
          if( !Double.isNaN( value ) )
          {
            final IRecord point = ProfilUtil.findNearestPoint( getProfil(), value );
            if( point != m_devider.getPoint() )
            {

              final IProfilChange[] changes = new IProfilChange[2];
              changes[0] = new PointMarkerSetPoint( m_devider, point );
              changes[1] = new ActiveObjectEdit( getProfil(), point, null );
              final ProfilOperation operation = new ProfilOperation( "Wehrfeld verschieben", getProfilEventManager(), changes, true );
              new ProfilOperationJob( operation ).schedule();
            }
          }
        }
      } );

      m_buttonD = new Button( this, SWT.NONE );
      m_buttonD.setImage( m_deleteImg );
      m_buttonD.setToolTipText( "löscht den linken Wehrfeldtrenner" );
      m_buttonD.addSelectionListener( new SelectionAdapter()
      {

        @Override
        public void widgetSelected( final SelectionEvent e )
        {
          final IProfilChange change = new PointMarkerEdit( m_devider, null );
          final ProfilOperation operation = new ProfilOperation( "Wehrfeld löschen", getProfilEventManager(), change, true );
          new ProfilOperationJob( operation ).schedule();
        }
      } );
    }

    public void refresh( final IProfilPointMarker dev )
    {
      m_devider = dev;
      double value;
      if( dev == null )
        value = 0.0;
      else
        value = (Double) dev.getValue() == null ? 0.0 : (Double) dev.getValue();
      m_beiwert.setText( Double.toString( value ) );
      m_point.setText( Double.toString( (Double) dev.getPoint().getValue( ProfilObsHelper.getPropertyFromId( dev.getPoint(), IWspmTuhhConstants.POINT_PROPERTY_BREITE ) ) ) );
    }

    @Override
    public void dispose( )
    {
      super.dispose();
      m_buttonD.dispose();
      m_beiwert.dispose();
    }
  }

  private ComboViewer m_Wehrart;

  protected Button m_WehrfeldVisible;

  protected Group m_deviderGroup;

  protected Text m_kronenParameter;

  protected Button m_DeviderAdd;

  private final LinkedList<DeviderLine> m_deviderLines;

  protected final Image m_deleteImg;

  private final Image m_addImg;

  protected WEHRART m_selection = null;

  public WehrPanel( final IProfilEventManager pem, final ProfilViewData viewdata )
  {
    super( pem, viewdata );
    m_deviderLines = new LinkedList<DeviderLine>();
    m_deleteImg = KalypsoModelWspmUIImages.ID_BUTTON_WEHR_DELETE.createImage();
    m_addImg = KalypsoModelWspmUIImages.ID_BUTTON_WEHR_ADD.createImage();
  }

  @Override
  public void dispose( )
  {
    super.dispose();

    m_deleteImg.dispose();
    m_addImg.dispose();
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite,
   *      int)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final int style )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout( 2, false );
    panel.setLayout( gridLayout );

    // Wehrart ComboBox
    final String tooltip = "";/* TODO:Kim getLabelProvider; */
    final Label label = new Label( panel, SWT.NONE );
    label.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING ) );
    label.setText( "Wehrart:" );
    label.setToolTipText( tooltip );
    // TODO: do not use combo! Use ComboViewer instead, than you do not need the .setData/.getData stuff

    m_Wehrart = new ComboViewer( panel, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.BORDER );
    m_Wehrart.getCombo().setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );

    m_Wehrart.setContentProvider( new ArrayContentProvider() );
    m_Wehrart.setLabelProvider( new LabelProvider() );

    final WEHRART[] input = new WEHRART[] { WEHRART.eScharfkantig, WEHRART.eRundkronig, WEHRART.eBreitkronig, WEHRART.eBeiwert };
    m_Wehrart.setInput( input );

    m_Wehrart.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( final SelectionChangedEvent event )
      {
        // TODO IProfileObjects now returned as list from IProfile
        final IProfileObject[] profileObjects = getProfil().getProfileObjects();
        if( profileObjects.length < 1 || !(profileObjects[0] instanceof BuildingWehr) )
          return;

        final BuildingWehr building = (BuildingWehr) profileObjects[0];
        final IStructuredSelection selection = (IStructuredSelection) m_Wehrart.getSelection();
        final WEHRART type = (WEHRART) selection.getFirstElement();

        if( type.equals( m_selection ) )
          return;

        final IProfilChange change = building.getWehrartProfileChange( type );
        final ProfilOperation operation = new ProfilOperation( "Wehrart ändern", getProfilEventManager(), change, true );
        new ProfilOperationJob( operation ).schedule();

        m_selection = type;
      }
    } );

    // Wehrparameter Group
    m_deviderGroup = new Group( panel, SWT.NONE );
    m_deviderGroup.setText( "Wehrfeldtrenner" );
    final GridLayout groupLayout = new GridLayout( 4, false );
    m_deviderGroup.setLayout( groupLayout );
    final GridData groupData = new GridData( GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL | GridData.FILL_HORIZONTAL | GridData.FILL_VERTICAL );
    groupData.horizontalSpan = 2;
    m_deviderGroup.setLayoutData( groupData );

    // Zeile 1
    final Label l5 = new Label( m_deviderGroup, SWT.NONE );
    l5.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING ) );
    l5.setText( "Feld 1: " );
    m_kronenParameter = new Text( m_deviderGroup, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    m_kronenParameter.setLayoutData( new GridData( GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL ) );
    m_kronenParameter.addModifyListener( new DoubleModifyListener( panel.getDisplay().getSystemColor( SWT.COLOR_BLACK ), panel.getDisplay().getSystemColor( SWT.COLOR_RED ) ) );
    m_kronenParameter.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( final FocusEvent e )
      {
        m_kronenParameter.selectAll();
      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        final double value = NumberUtils.parseQuietDouble( m_kronenParameter.getText() );
        if( !Double.isNaN( value ) )
        {
          // TODO IProfileObjects now returned as list from IProfile
          final IProfileObject[] profileObjects = getProfil().getProfileObjects();
          IProfileObject building = null;
          if( profileObjects.length > 0 )
            building = profileObjects[0];

          final IProfilChange change = new ProfileObjectEdit( building, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ), value );
          final ProfilOperation operation = new ProfilOperation( "Wehrfeldparameter ändern", getProfilEventManager(), change, true );
          new ProfilOperationJob( operation ).schedule();
        }
      }
    } );
    new Label( m_deviderGroup, SWT.NONE );
    m_DeviderAdd = new Button( m_deviderGroup, SWT.NONE );
    m_DeviderAdd.setImage( m_addImg );
    m_DeviderAdd.setToolTipText( "fügt einen neuen Wehrfeldtrenner ein" );
    m_DeviderAdd.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        // TODO IProfileObjects now returned as list from IProfile
        final IProfileObject[] profileObjects = getProfil().getProfileObjects();
        IProfileObject building = null;
        if( profileObjects.length > 0 )
          building = profileObjects[0];

        if( (building == null) || !IWspmTuhhConstants.BUILDING_TYP_WEHR.equals( building.getId() ) )
          return;
        final IProfilPointMarker[] trennFl = getProfil().getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
        final IRecord point = trennFl[0].getPoint();
        final IProfilChange[] changes = new IProfilChange[2];

        final IComponent comp = ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.MARKER_TYP_WEHR );
        changes[0] = new PointMarkerEdit( new ProfilDevider( comp, point ), 0.0 );
        changes[1] = new ActiveObjectEdit( getProfil(), point, null );
        final ProfilOperation operation = new ProfilOperation( "Wehrfeld erzeugen", getProfilEventManager(), changes, true );
        new ProfilOperationJob( operation ).schedule();
      }
    } );

    // Zeile 2
    new Label( m_deviderGroup, SWT.NONE );
    final Label l2 = new Label( m_deviderGroup, SWT.NONE );
    l2.setText( "Parameter" );

    final Label l3 = new Label( m_deviderGroup, SWT.NONE );
    l3.setText( "Position" );
    new Label( m_deviderGroup, SWT.NONE );

    // Wehrfeldsichtbar Check
    m_WehrfeldVisible = new Button( panel, SWT.CHECK );
    final GridData checkData = new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL );
    checkData.horizontalSpan = 2;
    m_WehrfeldVisible.setLayoutData( checkData );
    m_WehrfeldVisible.setText( "Feldtrenner anzeigen" );
    m_WehrfeldVisible.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        final ProfilOperation operation = new ProfilOperation( "Sichtbarkeit ändern:", getProfilEventManager(), true );
        operation.addChange( new VisibleMarkerEdit( getViewData(), ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_WEHR ), m_WehrfeldVisible.getSelection() ) );
        final IStatus status = operation.execute( new NullProgressMonitor(), null );
        operation.dispose();
        if( !status.isOK() )
          KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
      }
    } );

    updateControls();

    return panel;
  }

  protected void updateControls( )
  {
    if( m_Wehrart.getCombo().isDisposed() )
      return;

    // TODO IProfileObjects now returned as list from IProfile
    final IProfileObject[] profileObjects = getProfil().getProfileObjects();
    if( profileObjects.length < 1 || !(profileObjects[0] instanceof BuildingWehr) )
      return;

    final BuildingWehr building = (BuildingWehr) profileObjects[0];
    final String sWehrart = (String) building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART ) );
    final WEHRART wehrart = WEHRART.toWehrart( sWehrart );
    if( wehrart != null )
      m_Wehrart.setSelection( new StructuredSelection( wehrart ) );

    m_WehrfeldVisible.setSelection( getViewData().getMarkerVisibility( IWspmTuhhConstants.MARKER_TYP_WEHR ) );
    m_kronenParameter.setText( building.getValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ) ).toString() );
    final IProfilPointMarker[] deviders = null;//FIXME getProfil().getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_WEHR ) );
    final int deviders_length = deviders == null ? 0 : deviders.length;
    {
      while( m_deviderLines.size() < deviders_length )
      {
        m_deviderLines.add( new DeviderLine( m_deviderLines.size() + 1 ) );
      }
      while( m_deviderLines.size() > deviders_length )
      {
        m_deviderLines.getLast().dispose();
        m_deviderLines.removeLast();
      }
      int index = 0;
      for( final DeviderLine devl : m_deviderLines )
      {
        devl.refresh( deviders[index++] );
      }
    }
    m_deviderGroup.layout();
  }

  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    if( hint.isObjectDataChanged() || hint.isProfilPropertyChanged() || hint.isMarkerMoved() || hint.isMarkerDataChanged() )
    {
      final Control control = getControl();
      if( control != null && !control.isDisposed() )
        control.getDisplay().asyncExec( new Runnable()
        {
          public void run( )
          {
            updateControls();
          }
        } );
    }
  }
}