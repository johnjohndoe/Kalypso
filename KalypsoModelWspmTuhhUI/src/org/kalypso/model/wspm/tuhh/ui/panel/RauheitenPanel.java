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
import java.util.Collection;
import java.util.List;

import org.apache.commons.lang.ObjectUtils;
import org.apache.commons.lang.StringUtils;
import org.eclipse.jface.dialogs.IDialogSettings;
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
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyAdd;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */
public class RauheitenPanel extends AbstractProfilView
{
  private enum ZONE
  {
    left,
    channel,
    right;
  }

  private static final String SETTINGS_ROGUHNESS_COMPONENT = "roughnessComponent"; //$NON-NLS-1$

  private final Collection<IComponent> m_allKnownRoughnessTypes = new ArrayList<IComponent>();

  private final List<IComponent> m_availableRoughnessComponents = new ArrayList<IComponent>();

  private Text m_li;

  private Text m_hf;

  private Text m_re;

  private ComboViewer m_rauheitCombo;

  private IComponent m_currentRoughness;

  private RemoveRoughnessAction m_removeAction;

  private AddRoughnessAction m_addAction;

  public RauheitenPanel( final IProfil profile )
  {
    super( profile );

    // Find all possible roughness components
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( getProfil().getType() );
    final String[] components = provider.getPointProperties();
    for( final String componentID : components )
    {
      if( componentID.startsWith( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT ) ) //$NON-NLS-1$
      {
        final IComponent component = provider.getPointProperty( componentID );
        if( component != null )
          m_allKnownRoughnessTypes.add( component );
      }
    }

    initCurrentRoughness();
  }

  void initCurrentRoughness( )
  {
    final IProfil profile = getProfil();

    m_availableRoughnessComponents.clear();

    // Find available roughness components in profile
    final IComponent[] components = profile.getPointProperties();
    for( final IComponent component : components )
    {
      final String componentID = component.getId();
      if( componentID.startsWith( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT ) ) //$NON-NLS-1$
        m_availableRoughnessComponents.add( component );
    }

    m_currentRoughness = findCurrentRoughness();
  }

  /**
   * Choose, which is the current selected component.
   */
  private IComponent findCurrentRoughness( )
  {
    if( m_availableRoughnessComponents.size() == 0 )
      return null;

    final IDialogSettings settings = getDialogSettings();
    final String previousChoice = settings.get( SETTINGS_ROGUHNESS_COMPONENT );
    if( StringUtils.isBlank( previousChoice ) )
      return m_availableRoughnessComponents.get( 0 );

    /* Search if previous choice can be used */
    for( final IComponent component : m_availableRoughnessComponents )
    {
      if( component.getId().equals( previousChoice ) )
        return component;
    }

    /* Nothing found yet -> take first one */
    return m_availableRoughnessComponents.get( 0 );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final FormToolkit toolkit )
  {
    final Composite panel = toolkit.createComposite( parent, SWT.NONE );
    panel.setLayout( new GridLayout( 3, false ) );

    // RauheitsTyp Combo
    m_rauheitCombo = new ComboViewer( panel, SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY );
    m_rauheitCombo.getCombo().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    m_rauheitCombo.setContentProvider( new ArrayContentProvider() );
    m_rauheitCombo.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        return ComponentUtilities.getComponentLabel( (IComponent) element );
      }
    } );

    if( m_currentRoughness == null )
      m_rauheitCombo.setSelection( StructuredSelection.EMPTY );
    else
      m_rauheitCombo.setSelection( new StructuredSelection( m_currentRoughness ) );

    m_rauheitCombo.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        handleRoughnessSelectionChanged( selection );
      }
    } );
    toolkit.adapt( m_rauheitCombo.getCombo() );

    m_removeAction = new RemoveRoughnessAction();
    ActionButton.createButton( toolkit, panel, m_removeAction );

    m_addAction = new AddRoughnessAction( this );
    ActionButton.createButton( toolkit, panel, m_addAction );

    final Group fieldsGroup = new Group( panel, SWT.None );
    fieldsGroup.setText( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel.4" ) ); //$NON-NLS-1$
    fieldsGroup.setLayout( new GridLayout( 2, false ) );
    fieldsGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 3, 1 ) );
    toolkit.adapt( fieldsGroup );

    // Rauheitswerte Vorland links
    createLabel( toolkit, fieldsGroup, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel.6" ), Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel.7" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    m_li = createText( toolkit, fieldsGroup, ZONE.left );

    // Rauheitswerte Hauptöffnung
    createLabel( toolkit, fieldsGroup, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel.8" ), Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel.9" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    m_hf = createText( toolkit, fieldsGroup, ZONE.channel );

    // Rauheitswerte Vorland rechts
    createLabel( toolkit, fieldsGroup, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel.10" ), Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel.11" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    m_re = createText( toolkit, fieldsGroup, ZONE.right );

    updateControls();
    return panel;
  }

  protected void handleRoughnessSelectionChanged( final IStructuredSelection selection )
  {
    final Object element = selection.getFirstElement();
    if( element instanceof IComponent )
    {
      final IComponent selected = (IComponent) element;
      m_currentRoughness = selected;
    }
    else
      m_currentRoughness = null;

    /* Update settings */
    final IDialogSettings settings = getDialogSettings();
    if( m_currentRoughness == null )
      settings.put( SETTINGS_ROGUHNESS_COMPONENT, (String) null );
    else
      settings.put( SETTINGS_ROGUHNESS_COMPONENT, m_currentRoughness.getId() );

    final IComponent[] addableComponents = findAddableComponents();
    m_addAction.update( addableComponents );
    m_removeAction.update( getProfil(), m_currentRoughness );

    updateTextFields();
  }

  private IComponent[] findAddableComponents( )
  {
    final Collection<IComponent> addableComponents = new ArrayList<IComponent>();
    addableComponents.addAll( m_allKnownRoughnessTypes );
    addableComponents.removeAll( m_availableRoughnessComponents );
    return addableComponents.toArray( new IComponent[addableComponents.size()] );
  }

  private void createLabel( final FormToolkit toolkit, final Composite parent, final String text, final String toolTip )
  {
    final Label label = toolkit.createLabel( parent, text, SWT.BEGINNING );
    label.setToolTipText( toolTip );
    label.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );
  }

  private Text createText( final FormToolkit toolkit, final Composite panel, final ZONE zone )
  {
    final Display display = panel.getDisplay();
    final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
    final Color badColor = display.getSystemColor( SWT.COLOR_RED );
    final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );

    final Text field = toolkit.createText( panel, StringUtils.EMPTY, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    field.setToolTipText( Messages.getString("RauheitenPanel.0") ); //$NON-NLS-1$

    field.addModifyListener( doubleModifyListener );

    field.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( final FocusEvent e )
      {
        field.selectAll();
      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        final Double value = NumberUtils.parseQuietDouble( field.getText() );
        if( value.isNaN() )
        {
          updateText( field, zone );
          return;
        }

        // FIXME: check, if really something changed

        applyZoneValues( value, zone );
      }
    } );

    return field;
  }

  protected void applyZoneValues( final Double value, final ZONE zone )
  {
    final IProfil profil = getProfil();
    final int[] zoneLimits = getZoneLimits( profil, zone );
    if( zoneLimits == null )
      return;

    setValues( zoneLimits[0], zoneLimits[1], value );
  }

  private int[] getZoneLimits( final IProfil profil, final ZONE zone )
  {
    final IProfilPointMarker[] durchstroemte = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    final IProfilPointMarker[] trennflaechen = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    if( durchstroemte.length < 2 || trennflaechen.length < 2 )
      return null;

    switch( zone )
    {
      case left:
      {
        final int i_left = 0; // instead of DB-left we always start at the beginning of the profile
        final int i_rechts = profil.indexOfPoint( trennflaechen[0].getPoint() );
        return new int[] { i_left, i_rechts };
      }

      case channel:
      {
        final int i_left = profil.indexOfPoint( trennflaechen[0].getPoint() );
        final int i_rechts = profil.indexOfPoint( trennflaechen[trennflaechen.length - 1].getPoint() );
        return new int[] { i_left, i_rechts };
      }

      case right:
      {
        final int i_left = profil.indexOfPoint( trennflaechen[trennflaechen.length - 1].getPoint() );
        // instead of DB-right we always stop at the end of the profile
        final int i_rechts = profil.getPoints().length - 1; 
        return new int[] { i_left, i_rechts };
      }

      default:
        return null;
    }

  }

  private void setValues( final int l, final int r, final Double value )
  {
    final IProfil profil = getProfil();
    if( m_currentRoughness == null )
      return;

    final int index = profil.indexOfProperty( m_currentRoughness );
    if( index == -1 )
      return;

    final IRecord[] points = profil.getPoints( l, r - 1 );
    for( final IRecord p : points )
    {
      if( !ObjectUtils.equals( p.getValue( index ), value ) )
        p.setValue( index, value );
    }
  }

  private Double getValue( final ZONE zone )
  {
    final IProfil profile = getProfil();

    if( m_currentRoughness == null )
      return null;

    final int index = profile.indexOfProperty( m_currentRoughness );
    if( index == -1 )
      return null;

    final int[] zoneLimits = getZoneLimits( profile, zone );
    if( zoneLimits == null )
      return null;

    final int l = zoneLimits[0];
    final int r = zoneLimits[1];

    final IRecord[] points = profile.getPoints( l, r - 1 );
    if( points.length == 0 )
      return null;

    // Return value only if it is the same for all values in the zone, else return Double.NaN
    Number value = null;
    for( final IRecord p : points )
    {
      final Object currentValue = p.getValue( index );
      if( value == null )
        value = (Number) currentValue;
      else if( !value.equals( currentValue ) )
        return Double.NaN;
    }

    // Special case: all values are 'null': mark as Double.NaN i.e. not set, so user can set all values at once
    if( value == null )
      return Double.POSITIVE_INFINITY;

    return value.doubleValue();
  }

  void updateControls( )
  {
    m_rauheitCombo.setInput( m_availableRoughnessComponents );
    m_rauheitCombo.getCombo().setEnabled( m_availableRoughnessComponents.size() > 0 );

    if( m_currentRoughness == null )
      m_rauheitCombo.setSelection( StructuredSelection.EMPTY, true );
    else
      m_rauheitCombo.setSelection( new StructuredSelection( m_currentRoughness ), true );

    final IComponent[] addableComponents = findAddableComponents();
    m_addAction.update( addableComponents );
    m_removeAction.update( getProfil(), m_currentRoughness );
  }

  private void updateTextFields( )
  {
    updateText( m_li, ZONE.left );
    updateText( m_hf, ZONE.channel );
    updateText( m_re, ZONE.right );
  }

  void updateText( final Text field, final ZONE zone )
  {
    if( field == null || field.isDisposed() )
      return;

    final Double value = getValue( zone );

    field.setEnabled( value != null );

    if( value == null )
    {
      field.setText( StringUtils.EMPTY );
      field.setMessage( Messages.getString("RauheitenPanel.1") ); //$NON-NLS-1$
    }
    else if( value.isNaN() )
    {
      field.setText( StringUtils.EMPTY );
      field.setMessage( Messages.getString("RauheitenPanel.2") ); //$NON-NLS-1$
    }
    else if( value.isInfinite() )
    {
      field.setText( StringUtils.EMPTY );
      field.setMessage( Messages.getString("RauheitenPanel.3") ); //$NON-NLS-1$
    }
    else
    {
      final String text = String.format( "%.4f", value ); //$NON-NLS-1$
      field.setText( text );
      field.setMessage( StringUtils.EMPTY );
    }

    if( field.isFocusControl() )
      field.selectAll();
  }

  @Override
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    // TODO: protect against too many changes at once
    final Control control = getControl();

    final Runnable operation= new Runnable()
    {
      @Override
      public void run( )
      {
        initCurrentRoughness();

        updateControls();
      }
    };
    ControlUtils.asyncExec( control, operation );
  }

  public void addRoughness( final IComponent component )
  {
    // HACK: set settings so new component will be selected next
    getDialogSettings().put( SETTINGS_ROGUHNESS_COMPONENT, component.getId() );

    final IProfil profil = getProfil();

    final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.panel.RauheitenPanel.1" ), profil, true ); //$NON-NLS-1$
    operation.addChange( new PointPropertyAdd( getProfil(), component ) );
    new ProfilOperationJob( operation ).schedule();
  }
}