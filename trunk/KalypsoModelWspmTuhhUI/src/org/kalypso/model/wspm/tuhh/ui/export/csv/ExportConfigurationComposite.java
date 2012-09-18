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
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.DialogSettings;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIImages;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class ExportConfigurationComposite
{
  private static final String STR_COMBO_DEFAULT_ENTRY = Messages.getString( "ExportConfigurationComposite_0" ); //$NON-NLS-1$

  private static final Object STR_COMBO_EMPTY_ENTRY = Messages.getString( "ExportConfigurationComposite_1" ); //$NON-NLS-1$

  private static final String SETTINGS_LABEL = "label"; //$NON-NLS-1$

  private final CsvExportColumnsPage m_csvExportColumnsPage;

  private String m_currentSaveName;

  private final Map<String, IDialogSettings> m_configurations = new HashMap<>();

  private final File m_settingsBaseDir;

  private ComboViewer m_savedConfigsCombo;

  private Text m_saveConfigNameField;

  public ExportConfigurationComposite( final CsvExportColumnsPage csvExportColumnsPage, final File settingsBaseDir )
  {
    m_csvExportColumnsPage = csvExportColumnsPage;
    m_settingsBaseDir = settingsBaseDir;

    m_settingsBaseDir.mkdirs();

    final File[] files = m_settingsBaseDir.listFiles();

    for( final File settingFile : files )
    {
      try
      {
        final DialogSettings configuration = new DialogSettings( StringUtils.EMPTY );
        configuration.load( settingFile.getAbsolutePath() );
        /*
         * We need to hash in order to preserve the filename (cannot use DialogSettgins#name as this is overwritten on
         * load.
         */
        m_configurations.put( settingFile.getName(), configuration );
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }
    }

  }

  public Control createControl( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setLayout( new GridLayout( 3, false ) );
    group.setText( Messages.getString( "ExportConfigurationComposite_3" ) ); //$NON-NLS-1$

    final Text saveConfigNameField = new Text( group, SWT.SINGLE | SWT.BORDER );
    m_saveConfigNameField = saveConfigNameField;
    saveConfigNameField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    saveConfigNameField.setMessage( Messages.getString( "ExportConfigurationComposite_4" ) ); //$NON-NLS-1$
    saveConfigNameField.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        handleNameFieldModified( saveConfigNameField.getText() );
      }
    } );

    final ComboViewer savedConfigsCombo = new ComboViewer( group, SWT.DROP_DOWN | SWT.READ_ONLY );
    m_savedConfigsCombo = savedConfigsCombo;
    savedConfigsCombo.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    savedConfigsCombo.setContentProvider( new ArrayContentProvider() );
    savedConfigsCombo.setLabelProvider( new LabelProvider()
    {
      @Override
      public String getText( final Object element )
      {
        if( element instanceof IDialogSettings )
          return ((IDialogSettings) element).get( SETTINGS_LABEL );

        return super.getText( element );
      }
    } );
    savedConfigsCombo.setSorter( new ViewerSorter() );

    updateControl();

    final ImageDescriptor removeImage = KalypsoModelWspmTuhhUIPlugin.getImageProvider().getImageDescriptor( KalypsoModelWspmTuhhUIImages.REMOVE_CSV_CONFIGURATION );
    final Action removeConfigAction = new Action( Messages.getString( "ExportConfigurationComposite_5" ), removeImage ) //$NON-NLS-1$
    {
      /**
       * @see org.eclipse.jface.action.Action#run()
       */
      @Override
      public void run( )
      {
        final IStructuredSelection selection = (IStructuredSelection) savedConfigsCombo.getSelection();
        final Object firstElement = selection.getFirstElement();
        if( firstElement instanceof IDialogSettings )
        {
          removeConfiguration( getText(), (IDialogSettings) firstElement );
        }
      }
    };
    ActionHyperlink.createHyperlink( null, group, SWT.PUSH, removeConfigAction );
    removeConfigAction.setEnabled( false );

    savedConfigsCombo.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        final Object selectedElement = selection.getFirstElement();

        if( selectedElement instanceof IDialogSettings )
        {
          final IDialogSettings settings = (IDialogSettings) selectedElement;

          removeConfigAction.setEnabled( true );

          final String label = settings.get( SETTINGS_LABEL );
          if( label != null )
          {
            saveConfigNameField.setText( label );
          }

          applySettings( settings );
        }
        else
        {
          removeConfigAction.setEnabled( false );
        }
      }
    } );

    return group;
  }

  private void updateControl( )
  {
    m_saveConfigNameField.setText( StringUtils.EMPTY );

    final Object[] input = getInput();
    m_savedConfigsCombo.setInput( input );
    m_savedConfigsCombo.getControl().setEnabled( input.length > 1 );
    m_savedConfigsCombo.setSelection( new StructuredSelection( input[0] ) );
  }

  protected void removeConfiguration( final String title, final IDialogSettings configuration )
  {
    final Shell shell = m_csvExportColumnsPage.getShell();

    final String configLabel = configuration.get( SETTINGS_LABEL );
    final String message = String.format( Messages.getString( "ExportConfigurationComposite_6" ), configLabel ); //$NON-NLS-1$
    if( !MessageDialog.openConfirm( shell, title, message ) )
      return;

    // delete file
    final String filename = findFilename( m_currentSaveName );
    final File file = new File( m_settingsBaseDir, filename );
    file.delete();

    m_configurations.remove( filename );

    updateControl();
  }

  private Object[] getInput( )
  {
    if( m_configurations.size() == 0 )
      return new Object[] { STR_COMBO_EMPTY_ENTRY };

    final IDialogSettings[] configurations = m_configurations.values().toArray( new IDialogSettings[m_configurations.size()] );
    final Object[] input = new Object[m_configurations.size() + 1];
    input[0] = STR_COMBO_DEFAULT_ENTRY;
    System.arraycopy( configurations, 0, input, 1, configurations.length );
    return input;
  }

  protected void applySettings( final IDialogSettings settings )
  {
    m_csvExportColumnsPage.applyDialogSettings( settings );
  }

  protected void handleNameFieldModified( final String text )
  {
    m_currentSaveName = text;
  }

  public void saveConfiguration( final IDialogSettings settings )
  {
    if( StringUtils.isBlank( m_currentSaveName ) || settings == null )
      return;

    try
    {
      settings.put( SETTINGS_LABEL, m_currentSaveName );

      final String filename = findFilename( m_currentSaveName );
      final File file = new File( m_settingsBaseDir, filename );
      settings.save( file.getAbsolutePath() );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
  }

  private String findFilename( final String saveName )
  {
    final Set<Entry<String, IDialogSettings>> entrySet = m_configurations.entrySet();
    for( final Entry<String, IDialogSettings> entry : entrySet )
    {
      final IDialogSettings configuration = entry.getValue();

      final String label = configuration.get( SETTINGS_LABEL );
      if( ObjectUtils.equals( label, saveName ) )
        return entry.getKey();
    }

    int counter = 0;
    while( true )
    {
      final String filename = "configuration_" + counter++; //$NON-NLS-1$
      final File configFile = new File( m_settingsBaseDir, filename );
      if( !configFile.exists() )
        return filename;
    }
  }

}
