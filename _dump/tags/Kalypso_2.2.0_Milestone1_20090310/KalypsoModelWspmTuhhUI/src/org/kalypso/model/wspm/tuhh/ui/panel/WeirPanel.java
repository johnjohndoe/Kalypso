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

import java.util.HashMap;
import java.util.LinkedList;

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
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerEdit;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerSetPoint;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.changes.ProfileObjectEdit;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
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
  private class Wehrart
  {
    public final String m_id;

    public final String m_label;

    public final String m_text;

    public final String m_tooltip;

    public Wehrart( final String id, final String label, final String text, final String tooltip )
    {
      m_id = id;
      m_label = label;
      m_text = text;
      m_tooltip = tooltip;
    }
  }

  private class DeviderLine
  {
    protected final int position;

    protected Text m_beiwert;

    protected Text m_point;

    public DeviderLine( final Composite parent, final int index )
    {
      final Composite cmp = m_toolkit.createComposite( parent );
      position = index;
      GridLayout layout2 = new GridLayout( 4, true );
      layout2.marginWidth = 0;
      layout2.marginHeight = 0;
      cmp.setLayout( layout2 );
      final GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
      gridData.horizontalSpan = 2;
      cmp.setLayoutData( gridData );

      final Display display = parent.getDisplay();
      final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
      final Color badColor = display.getSystemColor( SWT.COLOR_RED );
      final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );

      m_toolkit.createLabel( cmp, Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.0") ); //$NON-NLS-1$
      m_point = m_toolkit.createText( cmp, "", SWT.TRAIL | SWT.SINGLE | SWT.BORDER ); //$NON-NLS-1$
      final GridData pointData = new GridData( SWT.FILL, SWT.CENTER, true, false );
      Label spacer = m_toolkit.createSeparator( cmp, SWT.SEPARATOR | SWT.HORIZONTAL );
      spacer.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false, 2, 1 ) );
      m_point.setLayoutData( pointData );

      m_point.addModifyListener( doubleModifyListener );
      m_point.addFocusListener( new FocusAdapter()
      {
        @Override
        public void focusGained( final FocusEvent e )
        {
          if( (m_point != null) && !m_point.isDisposed() )
            m_point.selectAll();
        }

        /**
         * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
         */
        @Override
        public void focusLost( final FocusEvent e )
        {
          if( (m_point == null) || m_point.isDisposed() )
            return;
          final double value = NumberUtils.parseQuietDouble( m_point.getText() );
          if( !Double.isNaN( value ) )
          {

            final IProfilPointMarker marker = getMarker();
            final IRecord point = ProfilUtil.findNearestPoint( getProfil(), value );
            final ProfilOperation operation = new ProfilOperation( Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.2"), getProfil(), true ); //$NON-NLS-1$
            operation.addChange( new PointMarkerSetPoint( marker, point ) );
            operation.addChange( new ActiveObjectEdit(  getProfil(), point, null ) );
            new ProfilOperationJob( operation ).schedule();
          }
        }
      } );

      if( position < Integer.MAX_VALUE )
      {
        final Composite btnGroup = m_toolkit.createComposite( cmp );
        GridLayout layout3 = new GridLayout( 2, true );
        layout3.marginWidth = 0;
        layout3.marginHeight = 0;

        btnGroup.setLayout( layout3 );
        btnGroup.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false, 2, 1 ) );

        final Button btnDel = m_toolkit.createButton( btnGroup, "", SWT.NONE ); //$NON-NLS-1$
        btnDel.setToolTipText( Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.4") ); //$NON-NLS-1$
        btnDel.setImage( m_deleteImg );
        btnDel.setEnabled( position > -1 );
        btnDel.addSelectionListener( new SelectionAdapter()
        {
          @Override
          public void widgetSelected( final SelectionEvent e )
          {

            final IProfilChange change = new PointMarkerEdit( getMarker(), null );
            final ProfilOperation operation = new ProfilOperation( Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.5"), getProfil(), change, true ); //$NON-NLS-1$
            new ProfilOperationJob( operation ).schedule();
          }
        } );
        final Button btnAdd = m_toolkit.createButton( btnGroup, "", SWT.NONE ); //$NON-NLS-1$
        btnAdd.setToolTipText( Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.7") ); //$NON-NLS-1$
        btnAdd.setImage( m_addImg );
        btnAdd.addSelectionListener( new SelectionAdapter()
        {
          @Override
          public void widgetSelected( final SelectionEvent e )
          {

            final IProfilPointMarker marker = getMarker();
            final IProfil profil = getProfil();
            final IRecord point = profil.getPoint( getProfil().indexOfPoint( marker.getPoint() ) + 1 );

            final ProfilOperation operation = new ProfilOperation( Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.8"), profil, true ); //$NON-NLS-1$
            final IProfilPointMarker trenner = profil.createPointMarker( IWspmTuhhConstants.MARKER_TYP_WEHR, point );

            if( trenner != null )
            {
              final Object objVal = marker.getValue();
              final IProfileObject[] buildings = getProfil().getProfileObjects();
              final Object dblVal = (objVal instanceof Double) ? objVal : ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT, buildings[0] );
              operation.addChange( new PointMarkerEdit( trenner, dblVal ) );
              operation.addChange( new ActiveObjectEdit( getProfil(), point, null ) );
              new ProfilOperationJob( operation ).schedule();
            }
          }
        } );

        m_toolkit.createLabel( cmp, Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.9") ); //$NON-NLS-1$
        m_beiwert = m_toolkit.createText( cmp, "", SWT.TRAIL | SWT.SINGLE | SWT.BORDER ); //$NON-NLS-1$
        m_beiwert.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
        m_beiwert.addModifyListener( doubleModifyListener );
        m_beiwert.addFocusListener( new FocusAdapter()
        {
          @Override
          public void focusGained( final FocusEvent e )
          {
            if( (m_beiwert == null) || m_beiwert.isDisposed() )
              return;
            m_beiwert.selectAll();
          }

          /**
           * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
           */
          @Override
          public void focusLost( final FocusEvent e )
          {
            if( (m_beiwert == null) || m_beiwert.isDisposed() )
              return;
            final double value = NumberUtils.parseQuietDouble( m_beiwert.getText() );
            if( !Double.isNaN( value ) )
            {
              if( position < 0 )
              {
                final IProfileObject[] profileObjects = getProfil().getProfileObjects();
                IProfileObject building = null;
                if( profileObjects.length > 0 )
                  building = profileObjects[0];

                final IProfilChange change = new ProfileObjectEdit( building, building.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ), value );
                final ProfilOperation operation = new ProfilOperation( Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.11"), getProfil(), change, true ); //$NON-NLS-1$
                new ProfilOperationJob( operation ).schedule();
              }
              else
              {

                final ProfilOperation operation = new ProfilOperation( Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.12"), getProfil(), true ); //$NON-NLS-1$
                operation.addChange( new PointMarkerEdit( getMarker(), value ) );
                new ProfilOperationJob( operation ).schedule();
              }
            }
          }
        } );
      }
    }

    protected final IProfilPointMarker getMarker( )
    {
      final String m_markerID = (position < 0 | position == Integer.MAX_VALUE) ? IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE : IWspmTuhhConstants.MARKER_TYP_WEHR;

      final IComponent cmpMarker = getProfil().hasPointProperty( m_markerID );
      final IProfilPointMarker[] markers = getProfil().getPointMarkerFor( cmpMarker );
      if( position < 0 )
        return markers[0];
      else if( position == Integer.MAX_VALUE )
        return markers[markers.length - 1];
      else
        return markers[position];
    }

    public void dispose( )
    {
      m_beiwert.getParent().dispose();
    }

    public void refresh( )
    {

      if( position < 0 )
      {
        final IProfileObject[] profileObjects = getProfil().getProfileObjects();
        IProfileObject building = null;
        if( profileObjects.length > 0 )
          building = profileObjects[0];
        final IComponent beiwert = building.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT );
        final String objValue = building.getValue( beiwert ).toString();
        m_beiwert.setText( objValue == null ? "" : NumberUtils.isDouble( objValue ) ? objValue : String.format( "%.4f", objValue ) ); //$NON-NLS-1$ //$NON-NLS-2$
        m_point.setText( String.format("%.4f", ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE, getMarker().getPoint() ) ) ); //$NON-NLS-1$
      }
      else if( position < Integer.MAX_VALUE )
      {
        final IProfilPointMarker marker = getMarker();
        final Object objValue = marker.getValue();
        final Double value = (objValue == null || !(objValue instanceof Double)) ? Double.NaN : (Double) objValue;
        m_beiwert.setText( value.toString() );
        m_point.setText( String.format( "%.4f", ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE, marker.getPoint() ) ) ); //$NON-NLS-1$
      }
      else
      {
        m_point.setText( String.format( "%.4f", ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE, getMarker().getPoint() ) ) ); //$NON-NLS-1$
      }
    }
  }

  protected ComboViewer m_Wehrart;

  protected Composite m_deviderGroup;

  private final LinkedList<DeviderLine> m_deviderLines;

  protected final Image m_deleteImg;

  protected final Image m_addImg;

  protected Label m_parameterLabel;

  private DeviderLine m_wehrStart;

  private DeviderLine m_wehrEnd;

  protected FormToolkit m_toolkit;

  private HashMap<String, Wehrart> m_wehrarten;

  public WeirPanel( final IProfil profile )
  {
    super( profile );
    m_deviderLines = new LinkedList<DeviderLine>();
    m_deleteImg = KalypsoModelWspmUIImages.ID_BUTTON_WEHR_DELETE.createImage();
    m_addImg = KalypsoModelWspmUIImages.ID_BUTTON_WEHR_ADD.createImage();
    m_wehrarten = new HashMap<String, Wehrart>();
    m_wehrarten.put( IWspmTuhhConstants.WEHR_TYP_SCHARFKANTIG, new Wehrart( IWspmTuhhConstants.WEHR_TYP_SCHARFKANTIG, Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.17"), Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.18"), Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.18") ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    m_wehrarten.put( IWspmTuhhConstants.WEHR_TYP_RUNDKRONIG, new Wehrart( IWspmTuhhConstants.WEHR_TYP_RUNDKRONIG, Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.21"), Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.22"), Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.23") ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    m_wehrarten.put( IWspmTuhhConstants.WEHR_TYP_BREITKRONIG, new Wehrart( IWspmTuhhConstants.WEHR_TYP_BREITKRONIG, Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.24"), Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.25"), Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.26") ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    m_wehrarten.put( IWspmTuhhConstants.WEHR_TYP_BEIWERT, new Wehrart( IWspmTuhhConstants.WEHR_TYP_BEIWERT, Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.27"), Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.28"), Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.29") ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
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
  protected Control doCreateControl( final Composite parent, FormToolkit toolkit, final int style )
  {
    m_toolkit = toolkit;
    final Composite panel = toolkit.createComposite( parent );
    final GridLayout gridLayout = new GridLayout( 2, false );
    panel.setLayout( gridLayout );

    // Wehrart ComboBox
    final String tooltip = "";/* TODO:Kim getLabelProvider; */ //$NON-NLS-1$
    final Label label = toolkit.createLabel( panel, Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.31"), style ); //$NON-NLS-1$
    label.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING ) );
    label.setToolTipText( tooltip );

    m_Wehrart = new ComboViewer( panel, SWT.DROP_DOWN | SWT.READ_ONLY | SWT.BORDER );
    m_Wehrart.getCombo().setLayoutData( new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
    m_Wehrart.setContentProvider( new ArrayContentProvider() );
    m_Wehrart.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        return ((Wehrart) element).m_label;
      }
    } );
    m_Wehrart.setInput( m_wehrarten.values() );
    m_Wehrart.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IProfileObject[] profileObjects = getProfil().getProfileObjects();
        if( profileObjects.length < 1 || !(profileObjects[0] instanceof BuildingWehr) )
          return;
        final BuildingWehr building = (BuildingWehr) profileObjects[0];
        final IStructuredSelection selection = (IStructuredSelection) m_Wehrart.getSelection();
        if( selection.isEmpty() )
          return;

        final Wehrart wehrart = (Wehrart) selection.getFirstElement();
        final String type = wehrart.m_id;
        final IComponent cWehr = building.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART );

        if( type.equals( building.getValue( cWehr ) ) )
          return;
        final IProfilChange change = new ProfileObjectEdit( building, cWehr, type );
        final ProfilOperation operation = new ProfilOperation( Messages.getString("org.kalypso.model.wspm.tuhh.ui.panel.WeirPanel.32"), getProfil(), change, true ); //$NON-NLS-1$
        new ProfilOperationJob( operation ).schedule();
      }
    } );
    toolkit.adapt( m_Wehrart.getCombo() );
    m_parameterLabel = toolkit.createLabel( panel, "", style ); //$NON-NLS-1$
    final GridData plGridData = new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL );
    plGridData.horizontalSpan = 2;
    m_parameterLabel.setLayoutData( plGridData );
    m_parameterLabel.setAlignment( SWT.RIGHT );

    final GridData psGridData = new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL );
    psGridData.horizontalSpan = 2;

    m_wehrStart = new DeviderLine( panel, -1 );
    // Wehrparameter Group
    m_deviderGroup = toolkit.createComposite( panel );
    final GridLayout layout4 = new GridLayout( 1, false );
    // layout4.marginHeight=0;
    m_deviderGroup.setLayout( layout4 );

    final GridData groupData = new GridData( SWT.FILL, SWT.TOP, true, false );

    groupData.horizontalSpan = 2;
    // groupData.exclude = true;
    m_deviderGroup.setLayoutData( groupData );

    m_wehrEnd = new DeviderLine( panel, Integer.MAX_VALUE );

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
    final IComponent objProp = building.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART );
    final String wehrart = (String) building.getValue( objProp );
    if( wehrart != null )
      m_Wehrart.setSelection( new StructuredSelection( m_wehrarten.get( wehrart ) ) );

    m_parameterLabel.setText( m_wehrarten.get( wehrart ).m_text );

    // m_WehrfeldVisible.setSelection( getViewData().getMarkerVisibility( IWspmTuhhConstants.MARKER_TYP_WEHR ) );
    m_wehrStart.refresh();
    m_wehrEnd.refresh();
    final IComponent cmpWehrTrenner = getProfil().hasPointProperty( IWspmTuhhConstants.MARKER_TYP_WEHR );

    final IProfilPointMarker[] deviders = getProfil().getPointMarkerFor( cmpWehrTrenner );

    while( m_deviderLines.size() < deviders.length )
    {
      m_deviderLines.add( new DeviderLine( m_deviderGroup, m_deviderLines.size() ) );
    }
    while( m_deviderLines.size() > deviders.length )
    {
      m_deviderLines.getLast().dispose();
      m_deviderLines.removeLast();
    }
    for( final DeviderLine devl : m_deviderLines )
    {
      devl.refresh();
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
          public void run( )
          {
            updateControls();
          }
        } );
    }
  }
}