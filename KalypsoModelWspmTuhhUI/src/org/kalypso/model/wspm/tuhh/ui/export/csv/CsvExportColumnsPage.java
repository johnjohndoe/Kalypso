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
package org.kalypso.model.wspm.tuhh.ui.export.csv;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.dialogs.DialogSettings;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.kalypso.model.wspm.tuhh.core.profile.export.PatternReplacementColumn;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.action.ProfilesSelection;

/**
 * @author Gernot Belger
 */
public class CsvExportColumnsPage extends WizardPage
{
  enum OUTPUT_TYPE
  {
    point(Messages.getString( "CsvExportColumnsPage_0" )), //$NON-NLS-1$
    profiles(Messages.getString( "CsvExportColumnsPage_1" )); //$NON-NLS-1$

    private final String m_label;

    private OUTPUT_TYPE( final String label )
    {
      m_label = label;
    }

    /**
     * @see java.lang.Enum#toString()
     */
    @Override
    public String toString( )
    {
      return m_label;
    }
  }

  private static final String SETTINGS_TYPE = "outputType"; //$NON-NLS-1$

  private OUTPUT_TYPE m_type = OUTPUT_TYPE.point;

  private final ExportColumnsComposite m_columnsComposite;

  private final ExportConfigurationComposite m_configurationComposite;

  private final IDialogSettings m_settings = new DialogSettings( StringUtils.EMPTY );

  private ComboViewer m_typeCombo;

  public CsvExportColumnsPage( final ProfilesSelection profileSelection )
  {
    super( "csvColumns" ); //$NON-NLS-1$

    setTitle( Messages.getString( "CsvExportColumnsPage_2" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "CsvExportColumnsPage_3" ) ); //$NON-NLS-1$

    final PatternReplacementColumn[] defaultColumns = createDefaultColumns();
    m_columnsComposite = new ExportColumnsComposite( defaultColumns, profileSelection );

    final IPath stateLocation = KalypsoModelWspmTuhhUIPlugin.getDefault().getStateLocation();
    final File stateDir = stateLocation.toFile();
    final File configurationDir = new File( stateDir, "profileExportColumnConfigurations" ); //$NON-NLS-1$

    m_configurationComposite = new ExportConfigurationComposite( this, configurationDir );
  }

  public OUTPUT_TYPE getType( )
  {
    return m_type;
  }

  public PatternReplacementColumn[] getExportColumns( )
  {
    return m_columnsComposite.getColumns();
  }

  private PatternReplacementColumn[] createDefaultColumns( )
  {
    final Collection<PatternReplacementColumn> columns = new ArrayList<>();
    columns.add( new PatternReplacementColumn( Messages.getString( "CsvExportColumnsPage_5" ), "<Station>" ) ); //$NON-NLS-1$//$NON-NLS-2$
    columns.add( new PatternReplacementColumn( Messages.getString( "CsvExportColumnsPage_6" ), "<Name>" ) ); //$NON-NLS-1$//$NON-NLS-2$
    return columns.toArray( new PatternReplacementColumn[columns.size()] );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    m_columnsComposite.setDialogSettings( m_settings );

    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout() );
    setControl( panel );

    final Control typeControl = createTypeControl( panel );
    typeControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Control columnComposite = m_columnsComposite.createControl( panel );
    columnComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    final Control configurationControl = m_configurationComposite.createControl( panel );
    configurationControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
  }

  void applyDialogSettings( final IDialogSettings dialogSettings )
  {
    try
    {
      if( dialogSettings == null )
        return;

      final String typeName = dialogSettings.get( SETTINGS_TYPE );
      if( typeName != null )
      {
        m_type = OUTPUT_TYPE.valueOf( typeName );
      }

      if( m_typeCombo != null )
      {
        m_typeCombo.setSelection( new StructuredSelection( m_type ) );
      }

      m_columnsComposite.applyDialogSettings( dialogSettings );

    }
    catch( final IllegalArgumentException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * Writes my state into the dialog settings.
   */
  private void saveDialogSettings( )
  {
    m_settings.put( SETTINGS_TYPE, m_type.name() );
  }

  private Control createTypeControl( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setText( Messages.getString( "CsvExportColumnsPage_7" ) ); //$NON-NLS-1$

    group.setLayout( new GridLayout() );

    final ComboViewer typeCombo = new ComboViewer( group, SWT.READ_ONLY | SWT.DROP_DOWN );
    m_typeCombo = typeCombo;
    typeCombo.setContentProvider( new ArrayContentProvider() );
    typeCombo.setLabelProvider( new LabelProvider() );
    typeCombo.setInput( new OUTPUT_TYPE[] { OUTPUT_TYPE.point, OUTPUT_TYPE.profiles } );
    typeCombo.setSelection( new StructuredSelection( m_type ) );
    typeCombo.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        handleTypeChanged( (OUTPUT_TYPE) selection.getFirstElement() );
      }
    } );

    typeCombo.getControl().setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, true, false ) );

    return group;
  }

  protected void handleTypeChanged( final OUTPUT_TYPE type )
  {
    m_type = type;

    saveDialogSettings();
  }

  public void saveConfiguration( )
  {
    m_configurationComposite.saveConfiguration( m_settings );
  }
}
