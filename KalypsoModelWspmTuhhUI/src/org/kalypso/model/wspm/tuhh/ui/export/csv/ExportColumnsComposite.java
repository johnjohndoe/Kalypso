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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

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
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.model.wspm.tuhh.core.profile.export.PatternReplacementColumn;
import org.kalypso.model.wspm.tuhh.core.profile.pattern.ProfilePatternInputReplacer;

/**
 * @author Gernot Belger
 */
public class ExportColumnsComposite
{
  private final List<PatternReplacementColumn> m_columns;

  private ScrolledForm m_form;

  public ExportColumnsComposite( final PatternReplacementColumn[] columns )
  {
    m_columns = new ArrayList<PatternReplacementColumn>( Arrays.asList( columns ) );
  }

  public Control createControl( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setText( "Export Columns" );
    group.setLayout( new FillLayout() );

    m_form = new ScrolledForm( group );
    m_form.setExpandHorizontal( true );

    final Composite body = m_form.getBody();
    body.setLayout( new GridLayout( 4, false ) );

    updatePanel();

    return group;
  }

  private void updatePanel( )
  {
    final Composite body = m_form.getBody();
    ControlUtils.disposeChildren( body );

    final ProfilePatternInputReplacer replacer = ProfilePatternInputReplacer.getINSTANCE();

    /* Header */
    final Label headerLabel = new Label( body, SWT.NONE );
    headerLabel.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );
    headerLabel.setText( "Column Name" );

    final Label patternLabel = new Label( body, SWT.NONE );
    patternLabel.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );
    patternLabel.setText( "Column Value" );

    new Label( body, SWT.NONE );
    new Label( body, SWT.NONE );

    for( int i = 0; i < m_columns.size(); i++ )
    {
      final PatternReplacementColumn column = m_columns.get( i );

      final Text headerText = new Text( body, SWT.SINGLE | SWT.BORDER );
      headerText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      headerText.setText( column.getHeader() );
      headerText.setMessage( "<Please enter column name>" );

      final Text patternText = new Text( body, SWT.SINGLE | SWT.BORDER );
      patternText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      patternText.setText( column.getPattern() );
      patternText.setMessage( "<Please enter column value or pattern>" );

      final Button patternMenuButton = replacer.createPatternButton( body, patternText );
      patternMenuButton.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );

      final RemoveColumnAction removeColumnAction = new RemoveColumnAction( column, this );
      final Button removeButton = ActionButton.createButton( null, body, removeColumnAction );
      removeButton.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );

      final int columnIndex = i;
      final ModifyListener modifyListener = new ModifyListener()
      {
        @Override
        public void modifyText( final ModifyEvent e )
        {
          final String header = headerText.getText();
          final String pattern = patternText.getText();
          handleColumnChanged( header, pattern, columnIndex );
        }
      };

      headerText.addModifyListener( modifyListener );
      patternText.addModifyListener( modifyListener );
    }

    final AddColumnAction addColumnAction = new AddColumnAction( this );
    final ImageHyperlink addHyperlink = ActionHyperlink.createHyperlink( null, body, SWT.PUSH, addColumnAction );
    addHyperlink.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 4, 1 ) );

    m_form.reflow( true );
  }

  protected void handleColumnChanged( final String header, final String pattern, final int columnIndex )
  {
    if( columnIndex < 0 || columnIndex >= m_columns.size() )
      return;

    final PatternReplacementColumn patternReplacementColumn = new PatternReplacementColumn( header, pattern );
    m_columns.set( columnIndex, patternReplacementColumn );
  }

  public PatternReplacementColumn[] getColumns( )
  {
    return m_columns.toArray( new PatternReplacementColumn[m_columns.size()] );
  }

  public void addColumn( )
  {
    m_columns.add( new PatternReplacementColumn( "", "" ) );

    updatePanel();
  }

  public void removeColumn( final PatternReplacementColumn column )
  {
    m_columns.remove( column );

    updatePanel();
  }

}
