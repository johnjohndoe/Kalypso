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
package org.kalypso.model.wspm.ui.profil.wizard.propertyEdit;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
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
import org.kalypso.model.wspm.core.profil.filter.IProfilePointFilter;
import org.kalypso.model.wspm.core.util.pointpropertycalculator.IPointPropertyCalculator;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.i18n.Messages;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */
public class OperationChooserPage extends WizardPage
{
  private class PropertyCalculator
  {
    public final String m_id;

    public final String m_tooltip;

    public final IPointPropertyCalculator m_calculator;

    public PropertyCalculator( final String id, final String tooltip, final IPointPropertyCalculator calculator )
    {
      m_tooltip = tooltip;
      m_id = id;
      m_calculator = calculator;
    }
  }

  private final String SETTINGS_FILTER_IDS = "operationChooserPage.selectedfilters"; //$NON-NLS-1$

  private final String SETTINGS_CALCULATOR_ID = "operationChooserPage.selectedcalculator"; //$NON-NLS-1$

  private final String SETTINGS_CALCULATOR_VALUE = "operationChooserPage.calculatorvalue"; //$NON-NLS-1$

  private List<PropertyCalculator> m_calculators = null;

  private HashMap<String, IProfilePointFilter> m_filters = null;

  private Double m_value = Double.NaN;

  private final IStructuredSelection m_selectedpoints;

  public OperationChooserPage( final IStructuredSelection selection, final String title )
  {
    super( "operationChooserPage", title, null ); //$NON-NLS-1$
    m_selectedpoints = selection;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout() );

    final Set<String> selectedFilters = new HashSet<String>();
    String selectedCalculator = ""; //$NON-NLS-1$
    String doubleValue = ""; //$NON-NLS-1$
    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
    {
      // get selected filters
      final String[] idArray = dialogSettings.getArray( SETTINGS_FILTER_IDS );
      if( idArray != null )
        Collections.addAll( selectedFilters, idArray );

      // get selected calculator
      selectedCalculator = dialogSettings.get( SETTINGS_CALCULATOR_ID );

      // get doubleValue
      doubleValue = dialogSettings.get( SETTINGS_CALCULATOR_VALUE );
      if( doubleValue != null && doubleValue != "" ) //$NON-NLS-1$
      {
        m_value = Double.valueOf( doubleValue );
      }
      else
        m_value = Double.NaN;
    }
    createFilterGroup( panel, selectedFilters );
    createOperationGroup( panel, selectedCalculator, doubleValue );
    setControl( panel );

  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#canFlipToNextPage()
   */
  @Override
  public boolean canFlipToNextPage( )
  {
    return false;
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#isPageComplete()
   */
  @Override
  public boolean isPageComplete( )
  {
    try
    {
      return m_calculators.size() > 0 && m_filters.size() > 0 && !m_value.isNaN();
    }
    catch( final Exception e )
    {
      return false;
    }
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#setPageComplete(boolean)
   */

  private void createFilterGroup( final Composite composite, final Set<String> filterIds )
  {
    final Group group = new Group( composite, SWT.NONE );
    group.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );
    group.setLayout( new GridLayout( 1, false ) );
    group.setText( "Filter" );
    new Label( group, SWT.NONE ).setText( "Choose which profile points will be changed:" );

    if( m_filters == null )
    {
      m_filters = new HashMap<String, IProfilePointFilter>();
      final IProfilePointFilter[] filters = KalypsoModelWspmCoreExtensions.getProfilePointFilters();
      for( final IProfilePointFilter filter : filters )
      {
        m_filters.put( filter.getId(), filter );
        addFilterCheckbox( group, filter, filterIds.contains( filter.getId() ) );
      }
    }
  }

  private void createOperationGroup( final Composite composite, final String calculatorId, final String value )
  {
    final Group group = new Group( composite, SWT.NONE );
    group.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );
    group.setLayout( new GridLayout( 2, false ) );
    group.setText( Messages.getString( "org.kalypso.model.wspm.ui.profil.wizard.propertyEdit.OperationChooserPage.0" ) ); //$NON-NLS-1$
    final Label lbl = new Label( group, SWT.NONE );
    lbl.setText( Messages.getString( "org.kalypso.model.wspm.ui.profil.wizard.propertyEdit.OperationChooserPage.1" ) ); //$NON-NLS-1$
    final GridData labelData = new GridData();
    labelData.horizontalSpan = 2;
    lbl.setLayoutData( labelData );
    final Combo combo = new Combo( group, SWT.DROP_DOWN | SWT.READ_ONLY );
    combo.setLayoutData( new GridData() );
    final Display display = group.getDisplay();
    final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
    final Color badColor = display.getSystemColor( SWT.COLOR_RED );
    final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );

    final Label lbltip = new Label( group, SWT.NONE );
    final GridData gd = new GridData();
    gd.horizontalSpan = 2;
    lbltip.setLayoutData( gd );
    final Text bldText = new Text( group, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
    bldText.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, false, true ) );
    if( value != null )
      bldText.setText( value );

    if( m_calculators == null )
    {
      m_calculators = new ArrayList<PropertyCalculator>();
      final IExtensionRegistry registry = Platform.getExtensionRegistry();
      final IConfigurationElement[] elements = registry.getConfigurationElementsFor( org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.profil.wizard.propertyEdit.OperationChooserPage.2") ); //$NON-NLS-1$
      for( final IConfigurationElement element : elements )
      {
        final String id = element.getAttribute( "id" ); //$NON-NLS-1$
        final String label = element.getAttribute( "label" ); //$NON-NLS-1$
        final String tooltip = element.getAttribute( "tooltip" ); //$NON-NLS-1$
        try
        {
          final IPointPropertyCalculator calculator = (IPointPropertyCalculator) element.createExecutableExtension( "class" ); //$NON-NLS-1$
          final PropertyCalculator propCalc = new PropertyCalculator( id, tooltip, calculator );
          m_calculators.add( propCalc );
          combo.add( label );
          if( id.equals( calculatorId ) )
          {
            combo.select( combo.getItemCount() - 1 );
            lbltip.setText( tooltip );
          }
        }
        catch( final CoreException e )
        {
          KalypsoModelWspmUIPlugin.getDefault().getLog().log( e.getStatus() );
        }

      }
    }

    bldText.addModifyListener( doubleModifyListener );

    bldText.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( final FocusEvent e )
      {
        bldText.selectAll();
      }

      @Override
      public void focusLost( final FocusEvent e )
      {
        handleFocusLost( bldText.getText() );
      }
    } );

    combo.addSelectionListener( new SelectionAdapter()
    {

      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final IDialogSettings dialogSettings = getDialogSettings();
        if( dialogSettings != null )
        {
          final PropertyCalculator propertyCalculator = m_calculators.get( combo.getSelectionIndex() );
          dialogSettings.put( SETTINGS_CALCULATOR_ID, propertyCalculator.m_id );
          lbltip.setText( propertyCalculator.m_tooltip );
          lbltip.pack( true );
          group.changed( new Control[] { lbltip } );
        }
      }
    } );
  }

  private void addFilterCheckbox( final Group group, final IProfilePointFilter filter, final boolean selected )
  {
    final Button button = new Button( group, SWT.CHECK );
    button.setText( filter.getName() );
    button.setToolTipText( filter.getDescription() );
    if( "org.kalypso.model.wspm.tuhh.core.profile.SelectedProfilePointFilter".equals( filter.getId() ) ) //$NON-NLS-1$
      button.setEnabled( m_selectedpoints != null );
    button.setSelection( selected );
    button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final IDialogSettings dialogSettings = getDialogSettings();
        final Set<String> fs = new HashSet<String>();
        if( dialogSettings != null )
        {
          final String[] filterIds = dialogSettings.getArray( SETTINGS_FILTER_IDS );
          if( filterIds != null )
            Collections.addAll( fs, filterIds );
          if( button.getSelection() )
            fs.add( filter.getId() );
          else
            fs.remove( filter.getId() );
          dialogSettings.put( SETTINGS_FILTER_IDS, fs.toArray( new String[0] ) );
        }
      }
    } );
  }

  private final boolean isValid( final IProfil profil, final IRecord point, final Set<String> filterSet )
  {

    for( final String filterId : filterSet )
    {
      final IProfilePointFilter filter = m_filters.get( filterId );
      if( filter != null && filter.accept( profil, point ) )
        return true;
    }
    return false;
  }

  public IProfilChange[] changeProfile( final IProfil profil, final Object[] properties )
  {
    final IComponent[] propertyIds = new IComponent[properties.length];
    for( int i = 0; i < properties.length; i++ )
    {
      propertyIds[i] = (IComponent) properties[i];
    }
    final IDialogSettings dialogSettings = getDialogSettings();
    final Set<String> filterSet = new HashSet<String>();
    IPointPropertyCalculator calculator = null;
    if( dialogSettings != null )
    {
      final String[] filterIds = dialogSettings.getArray( SETTINGS_FILTER_IDS );
      if( filterIds != null )
      {
        Collections.addAll( filterSet, filterIds );
      }
      final String calculatorId = dialogSettings.get( SETTINGS_CALCULATOR_ID );
      if( calculatorId == null )
        return new IProfilChange[0];

      for( final PropertyCalculator pc : m_calculators )
      {
        if( pc.m_id.equals( calculatorId ) )
        {
          calculator = pc.m_calculator;
          break;
        }
      }
    }
    final List<IRecord> selectedPoints = new ArrayList<IRecord>();
    for( final IRecord point : profil.getPoints() )
    {

      if( isValid( profil, point, filterSet ) )
      {
        selectedPoints.add( point );
      }
    }
    if( m_selectedpoints != null && !m_selectedpoints.isEmpty() && filterSet.contains( "org.kalypso.model.wspm.tuhh.core.profile.SelectedProfilePointFilter" ) ) //$NON-NLS-1$
      return calculator.calculate( m_value, propertyIds, addSelection( selectedPoints ) );
    return calculator.calculate( m_value, propertyIds, selectedPoints );

  }

  final List<IRecord> addSelection( final List<IRecord> selected )
  {
    for( final Object obj : m_selectedpoints.toList() )
    {
      if( obj instanceof IRecord && !selected.contains( obj ) )
        selected.add( (IRecord) obj );
    }
    return selected;
  }

  protected void handleFocusLost( final String text )
  {
    final IDialogSettings dialogSettings = getDialogSettings();
    m_value = NumberUtils.parseQuietDouble( text );
    if( dialogSettings != null )
    {
      dialogSettings.put( SETTINGS_CALCULATOR_VALUE, m_value.isNaN() ? "" : m_value.toString() ); //$NON-NLS-1$
      setPageComplete( m_value.isNaN() );
    }
  }
}
