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

import org.apache.commons.lang.ObjectUtils;
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
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyAdd;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * TODO: flieﬂzonen eingaben deaktivieren, wenn keine Flieﬂtzonen definiert
 * 
 * @author kimwerner
 */
public class RauheitenPanel extends AbstractProfilView
{
  protected Text m_li;

  protected Text m_hf;

  protected Text m_re;

  protected ComboViewer m_rauheitCombo;

  protected final HashMap<String, IComponent> m_RauheitTypes = new HashMap<String, IComponent>();

  protected HashMap<String, Double> m_RauheitMap = new HashMap<String, Double>();

  public RauheitenPanel( final IProfil profile )
  {
    super( profile );

    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( getProfil().getType() );
    final String[] components = provider.getPointProperties();
    for( final String componentID : components )
    {
      if( componentID.startsWith( IWspmTuhhConstants.POINT_PROPERTY + "RAUHEIT" ) ) //$NON-NLS-1$
      {
        final IComponent component = provider.getPointProperty( componentID );

        m_RauheitTypes.put( componentID, component );

      }
    }

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final FormToolkit toolkit )
  {
    // das panel
    final Composite panel = toolkit.createComposite( parent, SWT.NONE );
    panel.setLayout( new GridLayout( 2, false ) );

    // RauheitsTyp Combo
    m_rauheitCombo = new ComboViewer( panel, SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY );
    m_rauheitCombo.getCombo().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    m_rauheitCombo.setContentProvider( new ArrayContentProvider() );
    m_rauheitCombo.setInput( m_RauheitTypes.values() );

    m_rauheitCombo.setLabelProvider( new LabelProvider()
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

    m_rauheitCombo.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IComponent old = getRoughness();
        if( old == null )
          return;
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();

        final IComponent selected = (IComponent) selection.getFirstElement();

        if( !old.getId().equals( selected.getId() ) )
        {
          final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel.1" ), getProfil(), true ); //$NON-NLS-1$
          operation.addChange( new PointPropertyAdd( getProfil(), selected, old ) );
          operation.addChange( new PointPropertyRemove( getProfil(), old ) );
          new ProfilOperationJob( operation ).schedule();
        }
      }
    } );
    toolkit.adapt( m_rauheitCombo.getCombo() );

    final Group auto = new Group( panel, SWT.None );
    auto.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel.4" ) ); //$NON-NLS-1$
    auto.setLayout( new GridLayout( 2, false ) );
    auto.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    ((GridData) auto.getLayoutData()).horizontalSpan = 2;
    toolkit.adapt( auto );

    // Rauheitswerte Vorland links

    addLabel( toolkit, auto, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel.6" ), Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel.7" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    m_li = addText( toolkit, auto, null );

    m_li.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( final FocusEvent e )
      {
        m_li.selectAll();
      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        final Double value = NumberUtils.parseQuietDouble( m_li.getText() );
        if( value.isNaN() || Double.compare( value, 0.0 ) == 0 )
          return;

        final IProfil profil = getProfil();
        final IProfilPointMarker[] durchstroemte = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
        final IProfilPointMarker[] trennflaechen = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );

        if( durchstroemte.length < 2 || trennflaechen.length < 2 )
          return;

        final int i_left = 0;
        final int i_rechts = profil.indexOfPoint( trennflaechen[0].getPoint() );
        setValues( i_left, i_rechts, value );
      }
    } );

    // Rauheitswerte Hauptˆffnung
    addLabel( toolkit, auto, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel.8" ), Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel.9" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    m_hf = addText( toolkit, auto, null );
    m_hf.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( final FocusEvent e )
      {
        m_hf.selectAll();
      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        final Double value = NumberUtils.parseQuietDouble( m_hf.getText() );
        if( value.isNaN() || Double.compare( value, 0.0 ) == 0 )
          return;

        final IProfil profil = getProfil();

        final IProfilPointMarker[] durchstroemte = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
        final IProfilPointMarker[] trennflaechen = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );

        if( durchstroemte.length < 2 || trennflaechen.length < 2 )
          return;

        final int i_left = profil.indexOfPoint( trennflaechen[0].getPoint() );
        final int i_rechts = profil.indexOfPoint( trennflaechen[trennflaechen.length - 1].getPoint() );
        setValues( i_left, i_rechts, value );
      }
    } );
    
    // Rauheitswerte Vorland rechts
    addLabel( toolkit, auto, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel.10" ), Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel.11" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    m_re = addText( toolkit, auto, null );
    m_re.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( final FocusEvent e )
      {
        m_re.selectAll();
      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        final Double value = NumberUtils.parseQuietDouble( m_re.getText() );
        if( value.isNaN() || Double.compare( value, 0.0 ) == 0 )
          return;

        final IProfil profil = getProfil();

        final IProfilPointMarker[] durchstroemte = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
        final IProfilPointMarker[] trennflaechen = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );

        if( durchstroemte.length < 2 || trennflaechen.length < 2 )
          return;

        final int i_left = profil.indexOfPoint( trennflaechen[trennflaechen.length - 1].getPoint() );
        final int i_rechts = profil.getPoints().length;
        setValues( i_left, i_rechts, value );
      }
    } );
    updateControls();
    return panel;
  }

  protected void setValues( final int l, final int r, final Double value )
  {
    final IProfil profil = getProfil();
    final IComponent roughness = getRoughness();
    if( roughness == null )
      return;
    final int index = profil.indexOfProperty( roughness.getId() );
    for( int i = l; i < r; i++ )
    {
      final IRecord p = profil.getPoint( i );
      if( !ObjectUtils.equals( p.getValue( index ), value ) )
        p.setValue( index, value );
    }
  }

  private Text addText( final FormToolkit toolkit, final Composite panel, final Double value )
  {
    final Display display = panel.getDisplay();
    final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
    final Color badColor = display.getSystemColor( SWT.COLOR_RED );
    final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );
    final GridData data = new GridData();
    data.grabExcessHorizontalSpace = true;
    data.horizontalAlignment = GridData.FILL;

    final String text = value == null ? "<Not Set>" : String.format( "%.4f", value ); //$NON-NLS-1$ //$NON-NLS-2$
    final Text t = toolkit.createText( panel, text, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    t.setLayoutData( data );
    t.addModifyListener( doubleModifyListener );

    return t;
  }

  private void addLabel( final FormToolkit toolkit, final Composite parent, final String text, final String toolTip )
  {
    final Label label = toolkit.createLabel( parent, text, SWT.BEGINNING );
    label.setToolTipText( toolTip );
    final GridData data = new GridData( SWT.FILL, SWT.CENTER, false, false );
    label.setLayoutData( data );
  }

  private final void updateText( final Text t, final Double d )
  {
    if( t == null || t.isDisposed() )
      return;
    if( d == null || d.isNaN() )
      t.setText( "" ); //$NON-NLS-1$
    t.setText( String.format( "%.4f", d ) ); //$NON-NLS-1$
  }

  void updateControls( )
  {
    final IComponent roughness = getRoughness();
    if( roughness == null )
    {
      m_rauheitCombo.setSelection( null );
      updateText( m_re, null );
      updateText( m_li, null );
      updateText( m_hf, null );
      return;
    }
    m_rauheitCombo.setSelection( new StructuredSelection( m_RauheitTypes.get( roughness.getId() ) ) );
    final IProfil profile = getProfil();
    final IProfilPointMarker[] durchstroemte = profile.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    final IProfilPointMarker[] trennflaechen = profile.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );

    if( durchstroemte.length > 0 )
      updateText( m_li, ProfilUtil.getDoubleValueFor( roughness, durchstroemte[0].getPoint() ) );
    if( trennflaechen.length > 0 )
      updateText( m_hf, ProfilUtil.getDoubleValueFor( roughness, trennflaechen[0].getPoint() ) );
    if( trennflaechen.length > 1 )
      updateText( m_re, ProfilUtil.getDoubleValueFor( roughness, trennflaechen[trennflaechen.length - 1].getPoint() ) );

  }

  protected final IComponent getRoughness( )
  {
    final IComponent cmpKS = getProfil().hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
    final IComponent cmpKST = getProfil().hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST );
    if( cmpKS != null && cmpKST != null )
      return null;
    if( cmpKS != null )
      return cmpKS;
    if( cmpKST != null )
      return cmpKST;
    return null;
  }

  @Override
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    final Control control = getControl();

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