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
package org.kalypso.risk.preferences;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class KalypsoRiskPreferencePage extends PreferencePage implements IWorkbenchPreferencePage
{
  public static final int DEFAULT_RISKTHEMEINFO_PRECISION = 2;

  public static final int MIN_RISKTHEMEINFO_PRECISION = 1;

  public static final int MAX_RISKTHEMEINFO_PRECISION = 12;

  public static final String KEY_RISKTHEMEINFO_PRECISION = "eclipse.preferences.kalypso.risk.themeinfo.precision"; //$NON-NLS-1$

  private ComboViewer m_cmbFormat;

  public KalypsoRiskPreferencePage( )
  {
    super();
    setTitle( Messages.getString( "org.kalypso.risk.preferences.title" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "org.kalypso.risk.preferences.description" ) ); //$NON-NLS-1$
    setPreferenceStore( KalypsoRiskPlugin.getDefault().getPreferenceStore() );
    checkStore();
  }

  protected void checkPage( )
  {
    setValid( false );

    final ISelection selection = m_cmbFormat.getSelection();
    if( selection == null )
    {
      setMessage( "" ); //$NON-NLS-1$
      setErrorMessage( Messages.getString( "org.kalypso.risk.preferences.comboerror" ) ); //$NON-NLS-1$
      return;
    }
    setMessage( null );
    setErrorMessage( null );
    setValid( true );
  }

  /**
   * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parent )
  {

    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );

    /*******************************************************************************************************************
     * SCREENSHOTS
     ******************************************************************************************************************/
    final Group grScreen = new Group( composite, SWT.NONE );
    grScreen.setText( Messages.getString( "org.kalypso.risk.preferences.formattitle" ) ); //$NON-NLS-1$
    grScreen.setLayout( new GridLayout() );
    grScreen.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final Composite cgrScreen = new Composite( grScreen, SWT.NONE );
    cgrScreen.setLayout( new GridLayout( 2, false ) );
    cgrScreen.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    /* screenshot format */
    final Label lFormat = new Label( cgrScreen, SWT.NONE );
    lFormat.setText( Messages.getString( "org.kalypso.risk.preferences.cmblabel" ) ); //$NON-NLS-1$

    final String[] formats = new String[MAX_RISKTHEMEINFO_PRECISION - MIN_RISKTHEMEINFO_PRECISION + 1];
    for( int i = MIN_RISKTHEMEINFO_PRECISION; i <= MAX_RISKTHEMEINFO_PRECISION; i++ )
      formats[i - MIN_RISKTHEMEINFO_PRECISION] = Integer.toString( i );

    m_cmbFormat = new ComboViewer( cgrScreen, SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE );
    m_cmbFormat.setContentProvider( new ArrayContentProvider() );
    m_cmbFormat.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        return super.getText( element );
      }
    } );
    m_cmbFormat.setInput( formats );
    m_cmbFormat.getCombo().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final String format = getPreferenceStore().getString( KEY_RISKTHEMEINFO_PRECISION );
    if( format == null || format.length() == 0 )
      m_cmbFormat.setSelection( new StructuredSelection( Integer.toString( DEFAULT_RISKTHEMEINFO_PRECISION ) ) );
    else
      m_cmbFormat.setSelection( new StructuredSelection( format ) );

    m_cmbFormat.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( final SelectionChangedEvent event )
      {
        checkPage();
      }
    } );

    checkPage();

    return composite;
  }

  private void checkStore( )
  {
    final IPreferenceStore store = getPreferenceStore();
    final String format = store.getString( KEY_RISKTHEMEINFO_PRECISION );
    if( format == null || format.length() == 0 )
      store.setValue( KEY_RISKTHEMEINFO_PRECISION, Integer.toString( DEFAULT_RISKTHEMEINFO_PRECISION ) );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
   */
  public void init( final IWorkbench workbench )
  {
  }

  /**
   * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
   */
  @Override
  protected void performDefaults( )
  {
    getPreferenceStore().setDefault( KEY_RISKTHEMEINFO_PRECISION, Integer.toString( DEFAULT_RISKTHEMEINFO_PRECISION ) );
    m_cmbFormat.setSelection( new StructuredSelection( Integer.toString( DEFAULT_RISKTHEMEINFO_PRECISION ) ) );
    super.performDefaults();
  }

  /**
   * @see org.eclipse.jface.preference.PreferencePage#performApply()
   */
  @Override
  protected void performApply( )
  {
    /* Get the preference store. */
    final IPreferenceStore store = getPreferenceStore();

    /* Set the new value */
    final ISelection selection = m_cmbFormat.getSelection();
    if( selection instanceof StructuredSelection )
    {
      final StructuredSelection strSelection = (StructuredSelection) selection;
      final Object element = strSelection.getFirstElement();

      store.setValue( KEY_RISKTHEMEINFO_PRECISION, element.toString() );
    }

    /* Save the plugin preferences. */
    KalypsoRiskPlugin.getDefault().savePluginPreferences();
    super.performApply();
  }

  /**
   * @see org.eclipse.jface.preference.PreferencePage#performOk()
   */
  @Override
  public boolean performOk( )
  {
    performApply();
    return super.performOk();
  }
}
