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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.Hyperlink;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.tuhh.core.profile.export.PatternReplacementColumn;
import org.kalypso.model.wspm.tuhh.core.profile.pattern.ProfilePatternInputReplacer;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSectionColumn;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.action.ProfileSelection;

/**
 * @author Gernot Belger
 */
public class ExportColumnsComposite
{
  private static final String SETTINGS_SECTION = "columns"; //$NON-NLS-1$

  private static final String SETTINGS_HEADER = "header"; //$NON-NLS-1$

  private static final String SETTINGS_PATTERN = "pattern"; //$NON-NLS-1$

  private static final String SETTINGS_WIDTH = "width"; //$NON-NLS-1$

  private static final String SETTINGS_PRECISION = "precision"; //$NON-NLS-1$

  private final List<PatternReplacementColumn> m_columns;

  private ScrolledForm m_form;

  private IDialogSettings m_dialogSettings;

  private final ProfileSelection m_profileSelection;

  public ExportColumnsComposite( final PatternReplacementColumn[] columns, final ProfileSelection profileSelection )
  {
    m_profileSelection = profileSelection;
    m_columns = new ArrayList<PatternReplacementColumn>( Arrays.asList( columns ) );
  }

  public void setDialogSettings( final IDialogSettings dialogSettings )
  {
    m_dialogSettings = dialogSettings;
  }

  public Control createControl( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setText( Messages.getString("ExportColumnsComposite_5") ); //$NON-NLS-1$
    group.setLayout( new FillLayout() );

    m_form = new ScrolledForm( group, SWT.V_SCROLL );
    m_form.setExpandHorizontal( true );

    final Composite body = m_form.getBody();
    final GridLayout bodyLayout = new GridLayout( 6, false );
    bodyLayout.verticalSpacing = 1;
    body.setLayout( bodyLayout );

    updatePanel();

    return group;
  }

  private void updatePanel( )
  {
    if( m_form == null || m_form.isDisposed() )
      return;

    final Composite body = m_form.getBody();
    ControlUtils.disposeChildren( body );

    final ProfilePatternInputReplacer replacer = ProfilePatternInputReplacer.getINSTANCE();

    if( m_columns.size() == 0 )
    {
      final IStatus status = new Status( IStatus.INFO, KalypsoModelWspmTuhhUIPlugin.getID(), Messages.getString("ExportColumnsComposite_6") ); //$NON-NLS-1$
      new StatusComposite( body, SWT.NONE ).setStatus( status );
    }
    else
    {
      /* Header */
      final Label headerLabel = new Label( body, SWT.NONE );
      headerLabel.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );
      headerLabel.setText( Messages.getString("ExportColumnsComposite_7") ); //$NON-NLS-1$

      final Label widthLabel = new Label( body, SWT.NONE );
      widthLabel.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );
      widthLabel.setText( Messages.getString("ExportColumnsComposite_8") ); //$NON-NLS-1$

      final Label precisionLabel = new Label( body, SWT.NONE );
      precisionLabel.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );
      precisionLabel.setText( Messages.getString("ExportColumnsComposite_9") ); //$NON-NLS-1$

      final Label patternLabel = new Label( body, SWT.NONE );
      patternLabel.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );
      patternLabel.setText( Messages.getString("ExportColumnsComposite_10") ); //$NON-NLS-1$

      new Label( body, SWT.NONE ).setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false, 2, 1 ) );
    }

    for( int i = 0; i < m_columns.size(); i++ )
    {
      final PatternReplacementColumn column = m_columns.get( i );
      final String header = column.getHeader();
      final String pattern = column.getPattern();
      final int width = column.getFormatWidth();
      final int precision = column.getFormatPrecision();
      final Class< ? > type = column.getType();

      final boolean hasPrecision = hasPrecision( type );

      final Text headerText = new Text( body, SWT.SINGLE | SWT.BORDER );
      headerText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      headerText.setText( header );
      headerText.setMessage( Messages.getString("ExportColumnsComposite_11") ); //$NON-NLS-1$

      final Spinner widthSpinner = new Spinner( body, SWT.BORDER );
      widthSpinner.setValues( width, -1, 1000, 0, 1, 10 );

      final Spinner precisionSpinner = new Spinner( body, SWT.BORDER );
      precisionSpinner.setValues( precision, -1, 1000, 0, 1, 10 );
      precisionSpinner.setEnabled( hasPrecision );

      final Text patternText = new Text( body, SWT.SINGLE | SWT.BORDER );
      patternText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      patternText.setText( pattern );
      patternText.setMessage( Messages.getString("ExportColumnsComposite_12") ); //$NON-NLS-1$

      final Button patternMenuButton = replacer.createPatternButton( body, patternText );
      patternMenuButton.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );

      final RemoveColumnAction removeColumnAction = new RemoveColumnAction( column, this );
      final Hyperlink removeButton = ActionHyperlink.createHyperlink( null, body, SWT.PUSH, removeColumnAction );
      removeButton.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );

      final int columnIndex = i;
      final ModifyListener modifyListener = new ModifyListener()
      {
        @Override
        public void modifyText( final ModifyEvent e )
        {
          final String newHeader = headerText.getText();
          final String newPattern = patternText.getText();
          final int newWidth = widthSpinner.getSelection();
          final int newPrecision = precisionSpinner.getSelection();
          handleColumnChanged( columnIndex, newHeader, newPattern, newWidth, newPrecision );
        }
      };

      headerText.addModifyListener( modifyListener );
      patternText.addModifyListener( modifyListener );
      widthSpinner.addModifyListener( modifyListener );
      precisionSpinner.addModifyListener( modifyListener );
    }

    final AddColumnAction addColumnAction = new AddColumnAction( this );
    final ImageHyperlink addHyperlink = ActionHyperlink.createHyperlink( null, body, SWT.PUSH, addColumnAction );
    addHyperlink.setLayoutData( new GridData( SWT.RIGHT, SWT.CENTER, true, false, 6, 1 ) );

    final AddResultColumnsAction addResultColumnsAction = new AddResultColumnsAction( this, m_profileSelection );
    final ImageHyperlink addResultsHyperlink = ActionHyperlink.createHyperlink( null, body, SWT.PUSH, addResultColumnsAction );
    addResultsHyperlink.setLayoutData( new GridData( SWT.RIGHT, SWT.CENTER, true, false, 6, 1 ) );

    m_form.reflow( true );
  }

  private boolean hasPrecision( final Class< ? > type )
  {
    return type == Float.class || type == Double.class || type == BigDecimal.class;
  }

  protected void handleColumnChanged( final int columnIndex, final String header, final String pattern, final int width, final int precision )
  {
    if( columnIndex < 0 || columnIndex >= m_columns.size() )
      return;

    final PatternReplacementColumn patternReplacementColumn = new PatternReplacementColumn( header, pattern, width, precision );
    m_columns.set( columnIndex, patternReplacementColumn );

    saveDialogSettings();
  }

  public PatternReplacementColumn[] getColumns( )
  {
    return m_columns.toArray( new PatternReplacementColumn[m_columns.size()] );
  }

  public void addEmptyColumn( )
  {
    m_columns.add( new PatternReplacementColumn( StringUtils.EMPTY, StringUtils.EMPTY ) );

    updatePanel();
    saveDialogSettings();
  }

  public void removeColumn( final PatternReplacementColumn column )
  {
    m_columns.remove( column );

    updatePanel();
    saveDialogSettings();
  }

  private void saveDialogSettings( )
  {
    if( m_dialogSettings == null )
      return;

    /* Always create ne wsection in order to remove an existing one */
    final IDialogSettings subSection = m_dialogSettings.addNewSection( SETTINGS_SECTION );

    for( int i = 0; i < m_columns.size(); i++ )
    {
      final PatternReplacementColumn column = m_columns.get( i );
      final IDialogSettings columnSection = subSection.addNewSection( Integer.toString( i ) );
      columnSection.put( SETTINGS_HEADER, column.getHeader() );
      columnSection.put( SETTINGS_PATTERN, column.getPattern() );
      columnSection.put( SETTINGS_WIDTH, column.getFormatWidth() );
      columnSection.put( SETTINGS_PRECISION, column.getFormatPrecision() );
    }
  }

  public void applyDialogSettings( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    final IDialogSettings subSection = settings.getSection( SETTINGS_SECTION );
    if( subSection == null )
      return;

    m_columns.clear();

    final IDialogSettings[] columnSections = subSection.getSections();
    /* The settings class will not preserve the order of the sub-section, we need to sort it again */
    Arrays.sort( columnSections, new DialogSettingsByNameComparator() );

    for( final IDialogSettings columnSection : columnSections )
    {
      final String header = columnSection.get( SETTINGS_HEADER );
      final String pattern = columnSection.get( SETTINGS_PATTERN );
      final int width = NumberUtils.parseQuietInt( columnSection.get( SETTINGS_WIDTH ), PatternReplacementColumn.NOT_SET );
      final int precision = NumberUtils.parseQuietInt( columnSection.get( SETTINGS_PRECISION ), PatternReplacementColumn.NOT_SET );
      if( !StringUtils.isBlank( header ) && !StringUtils.isBlank( pattern ) )
        m_columns.add( new PatternReplacementColumn( header, pattern, width, precision ) );
    }

    updatePanel();

    saveDialogSettings();
  }

  public void addResultColumns( final WspmResultLengthSectionColumn[] selectedColumns )
  {
    if( ArrayUtils.isEmpty( selectedColumns ) )
      return;

    for( final WspmResultLengthSectionColumn column : selectedColumns )
    {
      final String label = column.getLabel();
      final String componentID = column.getComponentID();
      final String resultName = column.getResultName();

      final String component = shortenComponent( componentID );

      final String pattern = String.format( "<Result:%s:%s>", resultName, component ); //$NON-NLS-1$
      m_columns.add( new PatternReplacementColumn( label, pattern ) );
    }

    updatePanel();
    saveDialogSettings();
  }

  private String shortenComponent( final String componentID )
  {
    if( componentID.startsWith( IWspmConstants.LENGTH_SECTION_PROPERTY ) )
      return componentID.substring( IWspmConstants.LENGTH_SECTION_PROPERTY.length() );

    if( componentID.startsWith( IWspmConstants.URN_OGC_GML_DICT_KALYPSO_MODEL_WSPM_COMPONENTS ) )
      return componentID.substring( IWspmConstants.URN_OGC_GML_DICT_KALYPSO_MODEL_WSPM_COMPONENTS.length() );

    return componentID;
  }

}
