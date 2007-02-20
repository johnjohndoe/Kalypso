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

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointProperty;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerEdit;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;

/**
 * @author gernot
 */
public class RauheitenPanel extends AbstractProfilView
{
  Text m_VL;

  Text m_HF;

  Text m_VR;

  Button m_blockRauheit;

  boolean m_enablePanel;

  public RauheitenPanel( final IProfilEventManager pem, final ProfilViewData viewdata )
  {
    super( pem, viewdata );
  }

  /**
   * @see com.bce.profil.ui.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite, int)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final int style )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout( 3, true );
    panel.setLayout( gridLayout );
    panel.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    final IProfileObject building = getProfil().getProfileObject();
    final String[] pointProperties = (building == null) ? null : building.getPointProperties();
    m_enablePanel = !((building != null) && (pointProperties == null));
    m_blockRauheit = new Button( panel, SWT.CHECK );
    m_blockRauheit.setSelection( getViewData().useDeviderValue() );
    final IProfilPointMarker[] devs1 = (getProfil().getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE));
    final IProfilPointMarker[] devs2 = (getProfil().getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE  ));
    m_blockRauheit.setEnabled( m_enablePanel && (devs1.length == 2) && (devs2.length == 2));
    m_blockRauheit.setText( "einfache Rauheiten verwenden" );
    final GridData blockData = new GridData();
    blockData.horizontalSpan = 3;

    m_blockRauheit.setLayoutData( blockData );
    m_blockRauheit.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( org.eclipse.swt.events.SelectionEvent e )
      {

        if( getViewData().useDeviderValue() != m_blockRauheit.getSelection() )
        {
          getViewData().useDeviderValue( m_blockRauheit.getSelection() );
          if( m_blockRauheit.getSelection() )
            updateProperty( getProfil() );
        }
        updateControls();
        // final IProfilChange change = new PointPropertyHide( POINT_PROPERTY.RAUHEIT, !m_blockRauheit.getSelection() );
        // final ProfilOperation operation = new ProfilOperation( "Rauheiten Blockweise setzen",
        // getProfilEventManager(), change, true );
        // new ProfilOperationJob( operation ).schedule();
      }
    } );
    addLabel( panel, "VL", "Vorland links" );
    addLabel( panel, "HF", "Hauptöffnung" );
    addLabel( panel, "VR", "Vorland rechts" );

    m_VL = addText( panel );
    m_HF = addText( panel );
    m_VR = addText( panel );
    updateControls();
    return panel;
  }

  private Text addText( Composite panel )
  {
    final Display display = panel.getDisplay();
    final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
    final Color badColor = display.getSystemColor( SWT.COLOR_RED );
    final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );

    final IProfil profil = getProfil();
    final Text t = new Text( panel, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );

    final GridData data = new GridData();
    data.grabExcessHorizontalSpace = true;
    data.horizontalAlignment = GridData.FILL;
    t.setLayoutData( data );

    t.setEnabled( getViewData().useDeviderValue() );
    t.addModifyListener( doubleModifyListener );
    t.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( FocusEvent e )
      {
        ((Text) e.widget).selectAll();
      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        updateProperty( profil );
      }

    } );

    return t;
  }

  protected void updateProperty( final IProfil p )
  {
    final IProfilPointMarker[] pmD = p.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE);
    final IProfilPointMarker[] pmT = p.getPointMarkerFor(  IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    if( m_VL.isDisposed() || m_HF.isDisposed() || m_VR.isDisposed() || pmD.length != 2 || pmT.length != 2)
      return;
    final Double value1 = NumberUtils.parseQuietDouble( m_VL.getText() );
    final Double value2 = NumberUtils.parseQuietDouble( m_HF.getText() );
    final Double value3 = NumberUtils.parseQuietDouble( m_VR.getText() );
    final Double d1 = (Double) pmD[0].getValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT );
    final Double d2 = (Double) pmT[0].getValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT);
    final Double d3 = (Double) pmT[1].getValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT);
    final Double oldvalue1 = d1 == null ? 0.0 : d1;
    final Double oldvalue2 = d2 == null ? 0.0 : d2;
    final Double oldvalue3 = d3 == null ? 0.0 : d3;
    final ArrayList<IProfilChange> changes = new ArrayList<IProfilChange>();
    final IProfilPointProperty pp = p.getPointProperty( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT );
    final Double prec = pp.getPrecision();

    if( !value1.isNaN() && Math.abs( oldvalue1 - value1 ) > prec )
    {
      changes.add( new PointMarkerEdit( pmD[0], IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT, value1 ) );
    }
    if( !value2.isNaN() && Math.abs( oldvalue2 - value2 ) > prec )
    {
      changes.add( new PointMarkerEdit( pmT[0],  IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT, value2 ) );
    }
    if( !value3.isNaN() && Math.abs( oldvalue3 - value3 ) > prec )
    {
      changes.add( new PointMarkerEdit( pmT[1],  IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT, value3 ) );
    }
    Double currentValue = value1;
    for( IProfilPoint point : p.getPoints() )
    {
      if( pmT[0].getPoint() == point )
        currentValue = value2;
      else if( pmT[1].getPoint() == point )
        currentValue = value3;
      Double oldvalue;
      try
      {
        oldvalue = point.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT );
      }
      catch( Exception e )
      {
        oldvalue = 0.0;
      }
      if( !currentValue.isNaN() && Math.abs( oldvalue - currentValue ) > prec )
      {
        changes.add( new PointPropertyEdit( point, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT, currentValue ) );
      }

    }

    final ProfilOperation operation = new ProfilOperation( "Rauheiten Blockweise setzen", getProfilEventManager(), changes.toArray( new IProfilChange[0] ), true );
    new ProfilOperationJob( operation ).schedule();
  }

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

    final IProfilPointMarker[] pmD = getProfil().getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE);
    final IProfilPointMarker[] pmT = getProfil().getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE  );
    Boolean isBlockSetting = getViewData().useDeviderValue();
    if( !m_blockRauheit.isDisposed() )
    {
      m_blockRauheit.setSelection( isBlockSetting );
    }
    if( !m_VL.isDisposed() )
    {
      m_VL.setEnabled( isBlockSetting && m_enablePanel );
      if( pmD.length > 0 )
        setBlockValue( m_VL, pmD[0] );
    }
    if( !m_HF.isDisposed() )
    {
      m_HF.setEnabled( isBlockSetting && m_enablePanel );
      if( pmT.length > 0 )
        setBlockValue( m_HF, pmT[0] );
    }

    if( !m_VR.isDisposed() )
    {
      m_VR.setEnabled( isBlockSetting && m_enablePanel );
      if( pmT.length > 1 )
        setBlockValue( m_VR, pmT[1] );
    }
  }

  private void setBlockValue( final Text text, final IProfilPointMarker devider )
  {
    final Double value = (Double) devider.getValueFor(  IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT );
    try
    {
      text.setText( String.format( "%.2f", (value == null) ? devider.getPoint().getValueFor( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT ) : value ) );
    }
    catch( Exception e )
    {
      text.setText( "" );
    }
    if( text.isFocusControl() )
      text.selectAll();
  }

  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
    final Control control = getControl();

    if( hint.isMarkerMoved() && getViewData().useDeviderValue() )
    {
      if( control != null && !control.isDisposed() )
      {
        control.getDisplay().asyncExec( new Runnable()
        {
          public void run( )
          {
            updateProperty( getProfil() );
          }
        } );
      }
    }
    if( hint.isPointPropertiesChanged() )
    {
      if( control != null && !control.isDisposed() )
      {
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
}