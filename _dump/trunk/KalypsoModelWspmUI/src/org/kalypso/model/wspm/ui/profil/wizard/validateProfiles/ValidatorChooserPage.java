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
package org.kalypso.model.wspm.ui.profil.wizard.validateProfiles;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.profil.validator.IValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.ValidatorRuleSet;
import org.kalypso.model.wspm.core.util.pointpropertycalculator.IPointPropertyCalculator;
import org.kalypso.model.wspm.ui.Messages;

/**
 * @author kimwerner
 */
public class ValidatorChooserPage extends WizardPage
{

  private class PropertyCalculator
  {
    private final String m_label;

    private final String m_id;

    private final String m_tooltip;

    private final IPointPropertyCalculator m_calculator;

    private PropertyCalculator( final String id, final String label, final String tooltip, final IPointPropertyCalculator calculator )
    {
      m_label = label;
      m_tooltip = tooltip;
      m_id = id;
      m_calculator = calculator;
    }
  }

  private final String SETTINGS_RULES_IDS = "validatorChooserPage.selectedrules"; //$NON-NLS-1$

  private final String SETTINGS_QUICKFIX_IDS = "reparatorChooserPage.selectedfixes"; //$NON-NLS-1$

  private HashMap<String, IValidatorRule> m_rules = null;

  private Double m_value = Double.NaN;

  public ValidatorChooserPage( )
  {
    super( "operationChooserPage", Messages.OperationChooserPage_4, null ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
// the panel
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout() );
    final Composite buttonpanel = new Composite( panel, SWT.RIGHT );
    buttonpanel.setLayout( new GridLayout( 3, true ) );
    final GridData buttonpaneldata = new GridData( GridData.HORIZONTAL_ALIGN_END | GridData.GRAB_HORIZONTAL );
    buttonpaneldata.grabExcessHorizontalSpace = true;
    buttonpanel.setData( buttonpaneldata );

    final Set<String> selectedRules = new HashSet<String>();
    final Set<String> selectedFixes = new HashSet<String>();
    
    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
    {
      // get selected rules
      final String[] ruleArray = dialogSettings.getArray( SETTINGS_RULES_IDS );
      if( ruleArray != null )
        Collections.addAll( selectedRules, ruleArray );

      // get selected fixes
      final String[] fixArray = dialogSettings.getArray( SETTINGS_QUICKFIX_IDS );
      if( fixArray != null )
        Collections.addAll( selectedFixes, fixArray );

      
    }
    createFilterGroup( panel, selectedRules, selectedFixes);
    setControl( panel );

  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#canFlipToNextPage()
   */
  @Override
  public boolean canFlipToNextPage( )
  {
    return true;
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#isPageComplete()
   */
  @Override
  public boolean isPageComplete( )
  {
    try
    {
      return  m_rules.size() > 0 ;
    }
    catch( final Exception e )
    {
      return false;
    }
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#setPageComplete(boolean)
   */

  private void createFilterGroup( final Composite composite, final Set<String> ruleIds, final Set<String> fixIds)
  {
    final Group group = new Group( composite, SWT.NONE );
    group.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );
    group.setLayout( new GridLayout( 1, false ) );
    group.setText( Messages.OperationChooserPage_8 );
    new Label( group, SWT.NONE ).setText( Messages.OperationChooserPage_9 );

    if( m_rules == null )
    {
      m_rules = new HashMap<String, IValidatorRule>();
      final ValidatorRuleSet ruleSet = KalypsoModelWspmCorePlugin.getValidatorSet( "" ); //$NON-NLS-1$
      final IValidatorRule[] rules = ruleSet.getRules();

      for( final IValidatorRule rule : rules )
      {
        m_rules.put( rule.getID(), rule );
        addRuleCheckbox( group, rule, ruleIds.contains( rule.getID() ) );
      }
    }
  }

 

  private void addRuleCheckbox( final Group group, final IValidatorRule rule, final boolean selected )
  {
    final Button button = new Button( group, SWT.CHECK );
    button.setText( rule.toString() );
    button.setToolTipText( rule.getDescription() );
    button.setSelection( selected );
    button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @SuppressWarnings("synthetic-access")//$NON-NLS-1$
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final IDialogSettings dialogSettings = getDialogSettings();
        final Set<String> fs = new HashSet<String>();
        if( dialogSettings != null )
        {
          final String[] ruleIds = dialogSettings.getArray( SETTINGS_RULES_IDS );
          if( ruleIds != null )
            Collections.addAll( fs, ruleIds );
          if( button.getSelection() )
            fs.add( rule.getID() );
          else
            fs.remove( rule.getID() );
          dialogSettings.put( SETTINGS_RULES_IDS, fs.toArray( new String[0] ) );
        }
      }
    } );
  }

 

//  public IProfilChange[] changeProfile( final IProfil profil, final Object[] properties )
//  {
//    final IComponent[] propertyIds = new IComponent[properties.length];
//    for( int i = 0; i < properties.length; i++ )
//    {
//      propertyIds[i] = (IComponent) properties[i];
//    }
//    final IDialogSettings dialogSettings = getDialogSettings();
//    final Set<String> filterSet = new HashSet<String>();
//    IPointPropertyCalculator calculator = null;
//    if( dialogSettings != null )
//    {
//      final String[] filterIds = dialogSettings.getArray( SETTINGS_RULE_IDS );
//      if( filterIds != null )
//      {
//        Collections.addAll( filterSet, filterIds );
//      }
//      final String calculatorId = dialogSettings.get( SETTINGS_CALCULATOR_ID );
//      if( calculatorId == null )
//        return new IProfilChange[0];
//
//      for( final PropertyCalculator pc : m_calculators )
//      {
//        if( pc.m_id.equals( calculatorId ) )
//        {
//          calculator = pc.m_calculator;
//          break;
//        }
//      }
//    }
//    final List<IRecord> selectedPoints = new ArrayList<IRecord>();
//    for( final IRecord point : profil.getPoints() )
//    {
//
//      if( isValid( profil, point, filterSet ) )
//      {
//        selectedPoints.add( point );
//      }
//    }
//    return calculator.calculate( m_value, propertyIds, selectedPoints );
//  }
}
