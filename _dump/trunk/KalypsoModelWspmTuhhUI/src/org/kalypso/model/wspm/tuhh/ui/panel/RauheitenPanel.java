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

import java.util.HashMap;
import java.util.List;

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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyAdd;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author gernot
 */
public class RauheitenPanel extends AbstractProfilView
{
  protected Text m_VL;

  protected Text m_HF;

  protected Text m_VR;

  protected ComboViewer m_Rauheit;

  protected final HashMap<String, IComponent> m_RauheitTypes = new HashMap<String, IComponent>();

  protected String m_rauheitTyp;

  protected HashMap<String, Double> m_RauheitMap = new HashMap<String, Double>();

  protected Button m_updateOnDeviderMove;

  public RauheitenPanel( final IProfil profile, final ProfilViewData viewdata )
  {
    super( profile, viewdata );

    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( getProfil().getType() );
    final String[] components = provider.getPointProperties();
    for( final String component : components )
    {
      if( component.startsWith( IWspmTuhhConstants.POINT_PROPERTY + "RAUHEIT" ) )
      {
        m_RauheitTypes.put( component, provider.getPointProperty( component ) );
      }
    }
    m_RauheitMap.put( "manuelle Eingabe", null );

  }

  /**
   * @see com.bce.profil.ui.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite, int)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final int style )
  {
    // das panel
    final Composite panel = new Composite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout( 2, false );
    panel.setLayout( gridLayout );
    panel.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    // RauheitsTyp Combo
    m_Rauheit = new ComboViewer( panel, SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY );
    m_Rauheit.getCombo().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    m_Rauheit.setContentProvider( new ArrayContentProvider() );
    m_Rauheit.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        if( element instanceof IComponent )
        {
          final IComponent component = (IComponent) element;

          return component.getName();
        }
        return super.getText( element );
      }
    } );
    m_Rauheit.setInput( m_RauheitTypes.values() );

    m_Rauheit.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();

        final IComponent component = (IComponent) selection.getFirstElement();
        final IComponent old = m_RauheitTypes.get( m_rauheitTyp );

        if( component != null && old != null && !m_rauheitTyp.equals( component.getId() ) )
        {
          final ProfilOperation operation = new ProfilOperation( "Rauheitstyp ändern", getProfil(), true );

          final Object[] oldValues = ProfilUtil.getValuesFor( getProfil(), old );
          operation.addChange( new PointPropertyAdd( getProfil(), component, oldValues ) );
          operation.addChange( new PointPropertyRemove( getProfil(), old ) );
          new ProfilOperationJob( operation ).schedule();
        }
      }
    } );

    addLabel( panel, "Rauheitstyp", "Rauheitstyp" );

    // automatisches übernehmen wenn Marker geschoben werden
    final GridData checkData = new GridData( SWT.FILL, SWT.FILL, true, false );
    checkData.horizontalSpan = 2;
    m_updateOnDeviderMove = new Button( panel, SWT.CHECK );
    m_updateOnDeviderMove.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    ((GridData) m_updateOnDeviderMove.getLayoutData()).horizontalSpan = 2;
    m_updateOnDeviderMove.setText( "aktualisieren bei Trenneränderung" );
    m_updateOnDeviderMove.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( m_updateOnDeviderMove.getSelection() && !checkValues() )
        {
          // setBlockValues();
        }
      }
    } );

    // Rauheitswerte Vorland links
    final Group vlGroup = new Group( panel, SWT.None );
    vlGroup.setText( "Vorland links" );
    vlGroup.setLayout( new GridLayout( 2, false ) );
    vlGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    ((GridData) vlGroup.getLayoutData()).horizontalSpan = 2;
    addLabel( vlGroup, "Rauheit", "Rauheitswerte aus Datenbank übernehmen" );
    m_VL = addText( vlGroup );
    final Combo vl_Rauheit = new Combo( vlGroup, SWT.DROP_DOWN | SWT.READ_ONLY );
    vl_Rauheit.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    ((GridData) vl_Rauheit.getLayoutData()).horizontalSpan = 2;
    vl_Rauheit.setItems( m_RauheitMap.keySet().toArray( new String[0] ) );

    vl_Rauheit.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final Double value = m_RauheitMap.get( vl_Rauheit.getText() );
        if( value == null )
        {
          m_VL.setEditable( true );
        }
        else
        {
          m_VL.setText( value.toString() );
          m_VL.setEditable( false );
        }
      }
    } );
    vl_Rauheit.setText( vl_Rauheit.getItem( 0 ) );
    final Button vl_executeBtn = new Button( vlGroup, SWT.None );
    vl_executeBtn.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    ((GridData) vl_executeBtn.getLayoutData()).horizontalSpan = 2;
    vl_executeBtn.setText( "Rauheitswerte übernehmen" );
    vl_executeBtn.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        final IProfilPointMarker[] marker = getProfil().getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
        final ProfilOperation operation = new ProfilOperation( "Rauheiten bearbeiten", getProfil(), true );
        final Double value = NumberUtils.parseQuietDouble( m_VL.getText() );
        if( !value.isNaN() )
        {
          try
          {

            for( final IRecord point : getProfil().getPoints() )
            {
              if( point == marker[0].getPoint() )
                break;
              operation.addChange( new PointPropertyEdit( point, ProfilObsHelper.getPropertyFromId( point, m_rauheitTyp ), value ) );
            }
          }
          catch( final Exception exception )
          {
            throw new IllegalStateException();
          }
          new ProfilOperationJob( operation ).schedule();
        }
      }
    } );

    // Rauheitswerte Hauptöffnung
    final Group hfGroup = new Group( panel, SWT.None );
    hfGroup.setText( "Flußschlauch" );
    hfGroup.setLayout( new GridLayout( 2, false ) );
    hfGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    ((GridData) hfGroup.getLayoutData()).horizontalSpan = 2;
    addLabel( hfGroup, "Rauheit", "Rauheitswerte aus Datenbank übernehmen" );
    m_HF = addText( hfGroup );
    final Combo hf_Rauheit = new Combo( hfGroup, SWT.DROP_DOWN | SWT.READ_ONLY );
    hf_Rauheit.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    ((GridData) hf_Rauheit.getLayoutData()).horizontalSpan = 2;
    hf_Rauheit.setItems( m_RauheitMap.keySet().toArray( new String[0] ) );
    hf_Rauheit.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final Double value = m_RauheitMap.get( vl_Rauheit.getText() );
        if( value == null )
        {
          m_HF.setEditable( true );
        }
        else
        {
          m_HF.setText( value.toString() );
          m_HF.setEditable( false );
        }
      }
    } );
    hf_Rauheit.setText( hf_Rauheit.getItem( 0 ) );
    final Button hf_executeBtn = new Button( hfGroup, SWT.None );
    hf_executeBtn.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    ((GridData) hf_executeBtn.getLayoutData()).horizontalSpan = 2;
    hf_executeBtn.setText( "Rauheitswerte übernehmen" );
    hf_executeBtn.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        final IProfilPointMarker[] marker = getProfil().getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
        final List<IRecord> points = ProfilUtil.getInnerPoints( getProfil(), ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
        final ProfilOperation operation = new ProfilOperation( "Rauheiten bearbeiten", getProfil(), true );
        final Double value = NumberUtils.parseQuietDouble( m_HF.getText() );
        if( !value.isNaN() )
        {
          try
          {
            for( final IRecord point : points )
            {
              if( point == marker[marker.length - 1].getPoint() )
                break;
              operation.addChange( new PointPropertyEdit( point, ProfilObsHelper.getPropertyFromId( point, m_rauheitTyp ), value ) );
            }
          }
          catch( final Exception exception )
          {
            throw new IllegalStateException();
          }
          new ProfilOperationJob( operation ).schedule();
        }
      }
    } );

    // Rauheitswerte Vorland rechts
    final Group vrGroup = new Group( panel, SWT.None );
    vrGroup.setText( "Vorland rechts" );
    vrGroup.setLayout( new GridLayout( 2, false ) );
    vrGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    ((GridData) vrGroup.getLayoutData()).horizontalSpan = 2;
    addLabel( vrGroup, "Rauheit", "Rauheitswerte aus Datenbank übernehmen" );
    m_VR = addText( vrGroup );
    final Combo vr_Rauheit = new Combo( vrGroup, SWT.DROP_DOWN | SWT.READ_ONLY );
    vr_Rauheit.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    ((GridData) vr_Rauheit.getLayoutData()).horizontalSpan = 2;
    vr_Rauheit.setItems( m_RauheitMap.keySet().toArray( new String[0] ) );
    vr_Rauheit.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final Double value = m_RauheitMap.get( vr_Rauheit.getText() );
        if( value == null )
        {
          m_VR.setEditable( true );
        }
        else
        {
          m_VR.setText( value.toString() );
          m_VR.setEditable( false );
        }
      }
    } );
    vr_Rauheit.setText( vr_Rauheit.getItem( 0 ) );
    final Button vr_executeBtn = new Button( vrGroup, SWT.None );
    vr_executeBtn.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    ((GridData) vr_executeBtn.getLayoutData()).horizontalSpan = 2;
    vr_executeBtn.setText( "Rauheitswerte übernehmen" );
    vr_executeBtn.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        final IProfilPointMarker[] marker = getProfil().getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
        final ProfilOperation operation = new ProfilOperation( "Rauheiten bearbeiten", getProfil(), true );
        try
        {
          Double value = null;
          for( final IRecord point : getProfil().getPoints() )
          {
            if( point == marker[marker.length - 1].getPoint() )
            {
              value = NumberUtils.parseQuietDouble( m_VR.getText() );
              if( value.isNaN() )
                break;
            }
            if( value != null )
              operation.addChange( new PointPropertyEdit( point, ProfilObsHelper.getPropertyFromId( point, m_rauheitTyp ), value ) );
          }
        }
        catch( final Exception exception )
        {
          throw new IllegalStateException();
        }
        new ProfilOperationJob( operation ).schedule();
      }
    } );

    updateControls();
    return panel;
  }

  protected void setBlockValues( )
  {
    if( m_HF.isDisposed() || m_VL.isDisposed() || m_VR.isDisposed() )
      return;
    final Double value1 = NumberUtils.parseQuietDouble( m_VL.getText() );
    final Double value2 = NumberUtils.parseQuietDouble( m_HF.getText() );
    final Double value3 = NumberUtils.parseQuietDouble( m_VR.getText() );
    final IProfilPointMarker[] trennflaechen = getProfil().getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    // final IProfilPointMarker[] durchstroemte = getProfil().getPointMarkerFor(
    // IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    final ProfilOperation operation = new ProfilOperation( "Rauheiten bearbeiten", getProfil(), true );
    try
    {
      Double value = value1;// (Double) durchstroemte[0].getValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT );
      for( final IRecord point : getProfil().getPoints() )
      {
        if( point == trennflaechen[0].getPoint() )
          value = value2;// (Double) trennflaechen[0].getValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT );
        else if( point == trennflaechen[trennflaechen.length - 1].getPoint() )
          value = value3;// (Double) trennflaechen[trennflaechen.length - 1].getValueFor(
        // IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT );
        operation.addChange( new PointPropertyEdit( point, ProfilObsHelper.getPropertyFromId( point, m_rauheitTyp ), value ) );
      }
    }
    catch( final Exception e )
    {
      throw new IllegalStateException();
    }

    new ProfilOperationJob( operation ).schedule();
  }

  private Text addText( final Composite panel )
  {
    final Display display = panel.getDisplay();
    final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
    final Color badColor = display.getSystemColor( SWT.COLOR_RED );
    final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );
    final GridData data = new GridData();
    data.grabExcessHorizontalSpace = true;
    data.horizontalAlignment = GridData.FILL;
    final Text t = new Text( panel, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    t.setLayoutData( data );
    t.addModifyListener( doubleModifyListener );
    t.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( final FocusEvent e )
      {
        ((Text) e.widget).selectAll();
      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        final Double value = NumberUtils.parseQuietDouble( ((Text) e.widget).getText() );
        final String valueStr = String.format( "%.2f", value );
        if( !valueStr.equals( ((Text) e.widget).getText() ) )
          ((Text) e.widget).setText( valueStr );
      }
    } );
    return t;
  }

// protected void updateProperty( )
// {
// final IProfilPointMarker[] pmD = getProfil().getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
// final IProfilPointMarker[] pmT = getProfil().getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
// if( m_VL.isDisposed() || m_HF.isDisposed() || m_VR.isDisposed() || pmD.length != 2 || pmT.length != 2 )
// return;
// final Double value1 = NumberUtils.parseQuietDouble( m_VL.getText() );
// final Double value2 = NumberUtils.parseQuietDouble( m_HF.getText() );
// final Double value3 = NumberUtils.parseQuietDouble( m_VR.getText() );
// final ArrayList<IProfilChange> changes = new ArrayList<IProfilChange>();
// if( !value1.isNaN() && Math.abs( (Double) pmD[0].getValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT ) -
// value1 ) > 0 )
// {
// changes.add( new PointMarkerEdit( pmD[0], IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT, value1 ) );
// }
// if( !value2.isNaN() && Math.abs( (Double) pmT[0].getValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT ) -
// value2 ) > 0 )
// {
// changes.add( new PointMarkerEdit( pmT[0], IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT, value2 ) );
// }
// if( !value3.isNaN() && Math.abs( (Double) pmT[pmT.length - 1].getValueFor(
// IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT ) - value3 ) > 0 )
// {
// changes.add( new PointMarkerEdit( pmT[pmT.length - 1], IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT, value3 ) );
// }
// if( changes.size() > 0 )
// {
// final ProfilOperation operation = new ProfilOperation( "Rauheiten Blockweise setzen", getProfilEventManager(),
// changes.toArray( new IProfilChange[0] ), true );
// new ProfilOperationJob( operation ).schedule();
// }
// }

  private void addLabel( final Composite parent, final String text, final String toolTip )
  {
    final Label label = new Label( parent, SWT.CENTER );
    label.setText( text );
    label.setToolTipText( toolTip );
    final GridData data = new GridData();
    data.grabExcessHorizontalSpace = true;
    data.horizontalAlignment = GridData.CENTER;
    label.setLayoutData( data );
  }

  @SuppressWarnings("boxing")
  void updateControls( )
  {

    final IProfilPointMarker[] pmD = getProfil().getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
    final IProfilPointMarker[] pmT = getProfil().getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );

    for( final IComponent component : m_RauheitTypes.values() )
    {
      if( getProfil().hasPointProperty( component ) )
      {
        m_Rauheit.setSelection( new StructuredSelection( component ) );
        m_rauheitTyp = component.getId();
        break;
      }
    }

    if( !m_updateOnDeviderMove.isDisposed() )
      m_updateOnDeviderMove.setSelection( checkValues() );
    if( !m_VL.isDisposed() )
    {
      if( pmD.length > 0 )
        setEditText( m_VL, pmD[0] );
    }
    if( !m_HF.isDisposed() )
    {
      if( pmT.length > 0 )
        setEditText( m_HF, pmT[0] );
    }
    if( !m_VR.isDisposed() )
    {
      if( pmT.length > 1 )
        setEditText( m_VR, pmT[pmT.length - 1] );
    }

  }

  protected boolean checkValues( )
  {// TODO: FIXME
    try
    {
      final IProfilPointMarker[] durchstroemte = getProfil().getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
      final IProfilPointMarker[] trennflaechen = getProfil().getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );

      final IComponent rauheit = ProfilObsHelper.getPropertyFromId( getProfil(), m_rauheitTyp );
      final double precision = ProfilObsHelper.getPrecision( rauheit );
      Double value = (Double) durchstroemte[0].getPoint().getValue( ProfilObsHelper.getPropertyFromId( durchstroemte[0].getPoint(), m_rauheitTyp ) );

      for( final IRecord point : getProfil().getPoints() )
      {
        if( point.equals( trennflaechen[0].getPoint() ) )
          value = (Double) trennflaechen[0].getPoint().getValue( ProfilObsHelper.getPropertyFromId( trennflaechen[0].getPoint(), m_rauheitTyp ) );
        else if( point.equals( trennflaechen[trennflaechen.length - 1].getPoint() ) )
          value = (Double) trennflaechen[trennflaechen.length - 1].getPoint().getValue( ProfilObsHelper.getPropertyFromId( trennflaechen[trennflaechen.length - 1].getPoint(), m_rauheitTyp ) );
        else if( Math.abs( value - (Double) point.getValue( rauheit ) ) > precision )
          return false;
      }
    }
    catch( final Exception e )
    {
      return false;
    }
    return true;
  }

  private void setEditText( final Text text, final IProfilPointMarker devider )
  {
    text.setText( String.format( "%.2f", devider.getPoint().getValue( ProfilObsHelper.getPropertyFromId( devider.getPoint(), m_rauheitTyp ) ) ) );
    if( text.isFocusControl() )
      text.selectAll();
  }

  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    if( !getControl().isDisposed() )
    {
      getControl().getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          if( hint.isMarkerMoved() && m_updateOnDeviderMove.getSelection() )
            setBlockValues();
          if( hint.isPointPropertiesChanged() || hint.isPointValuesChanged() )
            updateControls();

        }
      } );
    }
  }
}