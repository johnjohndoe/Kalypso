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
package org.kalypso.model.wspm.tuhh.ui.imports;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Properties;
import java.util.Set;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class WProfPropertyPage extends WizardPage
{
  private static final String SETTINGS_SECTION = "wprofProperties"; //$NON-NLS-1$

  private final Collection<Text> m_valueTexts = new ArrayList<Text>();

  private final Properties m_defaultSpecification;

  private final Properties m_specification;

  public WProfPropertyPage( final String pageName, final Properties defaultSpecification )
  {
    super( pageName );

    m_defaultSpecification = defaultSpecification;
    m_specification = new Properties( defaultSpecification );

    setTitle( Messages.getString("WProfPropertyPage_0") ); //$NON-NLS-1$
    setDescription( Messages.getString("WProfPropertyPage_1") ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    initSpecification();

    final Group group = new Group( parent, SWT.NONE );
    group.setText( Messages.getString("WProfPropertyPage_2") ); //$NON-NLS-1$
    group.setLayout( new FillLayout() );

    final ScrolledForm scrolledForm = new ScrolledForm( group, SWT.V_SCROLL );
    scrolledForm.setExpandHorizontal( true );
    scrolledForm.setExpandVertical( true );

    final Composite body = scrolledForm.getBody();
    body.setLayout( new GridLayout( 3, false ) );

    /* Header */
    final Label keyLabel = new Label( body, SWT.NONE );
    keyLabel.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );
    keyLabel.setText( Messages.getString("WProfPropertyPage_3") ); //$NON-NLS-1$

    final Label defaultLabel = new Label( body, SWT.NONE );
    defaultLabel.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );
    defaultLabel.setText( Messages.getString("WProfPropertyPage_4") ); //$NON-NLS-1$

    final Label valueLabel = new Label( body, SWT.NONE );
    valueLabel.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );
    valueLabel.setText( Messages.getString("WProfPropertyPage_5") ); //$NON-NLS-1$

    final Set<Object> keySet = m_defaultSpecification.keySet();
    for( final Object key : keySet )
      createRow( body, (String) key );

    createResetButton( body );

    scrolledForm.reflow( true );

    setControl( group );
  }

  private void createResetButton( final Composite parent )
  {
    /* Label filler = */new Label( parent, SWT.NONE );
    final Button button = new Button( parent, SWT.PUSH );
    button.setText( Messages.getString("WProfPropertyPage_6") ); //$NON-NLS-1$
    button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleResetDefaults();
      }
    } );
  }

  protected void handleResetDefaults( )
  {
    m_specification.clear();
    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
    {
      // TODO: check: does this clear the section?
      dialogSettings.addNewSection( SETTINGS_SECTION );
    }

    for( final Text valueText : m_valueTexts )
      valueText.setText( "" ); //$NON-NLS-1$
  }

  private void createRow( final Composite parent, final String key )
  {
    final Label keyLabel = new Label( parent, SWT.NONE );
    keyLabel.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false ) );
    keyLabel.setText( key );

    final Text defaultText = new Text( parent, SWT.READ_ONLY | SWT.LEFT | SWT.BORDER );
    defaultText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    final String defaultValue = m_defaultSpecification.getProperty( key );
    defaultText.setText( defaultValue );

    final Object value = m_specification.get( key );
    final Text valueText = new Text( parent, SWT.LEFT | SWT.BORDER );
    valueText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    if( value instanceof String )
      valueText.setText( (String) value );

    valueText.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        final String text = valueText.getText();
        handlePropertyChanged( key, text );
      }
    } );

    m_valueTexts.add( valueText );
  }

  protected void handlePropertyChanged( final String key, final String value )
  {
    final String valueToSet = value.isEmpty() ? null : value;//$NON-NLS-1$

    if( valueToSet == null )
      m_specification.remove( key );
    else
      m_specification.setProperty( key, valueToSet );
    final IDialogSettings settings = getSettings();
    if( settings != null )
      settings.put( key, valueToSet );
  }

  private IDialogSettings getSettings( )
  {
    final IDialogSettings settings = getDialogSettings();
    if( settings == null )
      return null;

    final IDialogSettings section = settings.getSection( SETTINGS_SECTION );
    if( section == null )
      return settings.addNewSection( SETTINGS_SECTION );
    return section;
  }

  private void initSpecification( )
  {
    // read the specification from dialog settings
    final IDialogSettings settings = getSettings();
    if( settings == null )
      return;

    final Set<Object> keySet = m_defaultSpecification.keySet();
    for( final Object key : keySet )
    {
      final String value = settings.get( (String) key );
      if( value != null && !value.isEmpty() )
        m_specification.put( key, value.toUpperCase() );
    }
  }

  public Properties getSpecification( )
  {
    final Properties properties = new Properties();
    final Set<Object> keySet = m_defaultSpecification.keySet();
    for( final Object object : keySet )
    {
      final String key = (String) object;
      final String value = m_specification.getProperty( key );
      if( value != null && !value.isEmpty() )
        properties.setProperty( key, value );
    }

    return properties;
  }
}
