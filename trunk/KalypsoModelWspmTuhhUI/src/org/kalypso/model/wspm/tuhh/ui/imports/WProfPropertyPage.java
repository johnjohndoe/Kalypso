/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

    setTitle( "WProf Shape Properties" );
    setDescription( "Please choose how to read the properties from the shape file." );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    initSpecification();

    final Group group = new Group( parent, SWT.NONE );
    group.setText( "Property Assignment" );
    group.setLayout( new FillLayout() );

    final ScrolledForm scrolledForm = new ScrolledForm( group, SWT.V_SCROLL );
    scrolledForm.setExpandHorizontal( true );
    scrolledForm.setExpandVertical( true );

    final Composite body = scrolledForm.getBody();
    body.setLayout( new GridLayout( 2, false ) );

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
    button.setText( "Reset Defaults" );
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
      valueText.setText( "" );
  }

  private void createRow( final Composite parent, final String key )
  {
    final Text keyText = new Text( parent, SWT.READ_ONLY | SWT.LEFT | SWT.BORDER );
    keyText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    keyText.setText( key );

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
    final String valueToSet = value.isEmpty() ? null : value;

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
