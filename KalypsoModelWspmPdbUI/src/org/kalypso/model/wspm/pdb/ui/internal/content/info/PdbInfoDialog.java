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
package org.kalypso.model.wspm.pdb.ui.internal.content.info;

import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.ViewerColumn;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.viewers.ColumnViewerUtil;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerColumnItem;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.contribs.eclipse.ui.forms.ToolkitUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.connect.IPdbSettingsControl;
import org.kalypso.model.wspm.pdb.connect.PDBRole;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class PdbInfoDialog extends TitleAreaDialog
{
  private final IDialogSettings m_settings;

  private FormToolkit m_toolkit;

  private final IPdbConnection m_connection;

  private DataBindingContext m_binding;

  public PdbInfoDialog( final Shell shell, final IPdbConnection connection )
  {
    super( shell );

    m_connection = connection;

    m_settings = DialogSettingsUtils.getDialogSettings( WspmPdbUiPlugin.getDefault(), getClass().getName() );
  }

  @Override
  protected IDialogSettings getDialogBoundsSettings( )
  {
    return DialogSettingsUtils.getSection( m_settings, "bounds" ); //$NON-NLS-1$
  }

  @Override
  protected boolean isResizable( )
  {
    return true;
  }

  @Override
  protected void configureShell( final Shell newShell )
  {
    super.configureShell( newShell );

    newShell.setText( Messages.getString( "PdbInfoDialog.0" ) ); //$NON-NLS-1$
  }

  @Override
  protected Control createDialogArea( final Composite parent )
  {
    final Composite area = (Composite) super.createDialogArea( parent );
    m_toolkit = ToolkitUtils.createToolkit( area );
    m_toolkit.adapt( area );

    m_binding = new DataBindingContext();

    createConnectionGroup( area );
    createRoleInfo( area );
    createInfoGroup( area );

    final String name = m_connection.getLabel();
    setTitle( String.format( Messages.getString( "PdbInfoDialog.1" ), name ) ); //$NON-NLS-1$
    setMessage( Messages.getString( "PdbInfoDialog.2" ) ); //$NON-NLS-1$

    return area;
  }

  private void createConnectionGroup( final Composite area )
  {
    final Section group = m_toolkit.createSection( area, Section.TITLE_BAR );
    group.setText( Messages.getString( "PdbInfoDialog.3" ) ); //$NON-NLS-1$
    group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    final IPdbSettings settings = m_connection.getSettings();

    final IPdbSettingsControl editor = settings.createEditControl( m_binding, group );
    editor.setEditable( false );
    final Control editorControl = editor.getControl();
    m_toolkit.adapt( editorControl, true, true );
    group.setClient( editorControl );
    final ImageDescriptor pageImage = editor.getPageImage();

    final Image titleImage = pageImage.createImage();
    setTitleImage( titleImage );
    group.addDisposeListener( new DisposeListener()
    {
      @Override
      public void widgetDisposed( final DisposeEvent e )
      {
        titleImage.dispose();
      }
    } );
  }

  private void createRoleInfo( final Composite parent )
  {
    final Section group = m_toolkit.createSection( parent, Section.TITLE_BAR );
    group.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    group.setText( Messages.getString( "PdbInfoDialog.4" ) ); //$NON-NLS-1$

    final Composite panel = m_toolkit.createComposite( group );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( panel );
    group.setClient( panel );

    m_toolkit.createLabel( panel, Messages.getString( "PdbInfoDialog.5" ) ); //$NON-NLS-1$

    final PDBRole role = m_connection.getRole();
    final Text field = m_toolkit.createText( panel, role.toString(), SWT.SINGLE );
    field.setEditable( false );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
  }

  private void createInfoGroup( final Composite area )
  {
    final Section group = m_toolkit.createSection( area, Section.TITLE_BAR );
    group.setText( Messages.getString( "PdbInfoDialog.6" ) ); //$NON-NLS-1$
    group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    final Control infoControl = createInfoTable( group );
    group.setClient( infoControl );
  }

  private Control createInfoTable( final Composite parent )
  {
    final TableViewer viewer = new TableViewer( parent, SWT.BORDER | SWT.FULL_SELECTION );
    final Table table = viewer.getTable();
    table.setHeaderVisible( true );
    table.addControlListener( new ColumnsResizeControlListener() );

    viewer.setContentProvider( new ArrayContentProvider() );

    final ViewerColumn keyColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    final ViewerColumnItem keyItem = new ViewerColumnItem( keyColumn );
    keyItem.setText( Messages.getString( "PdbInfoDialog.7" ) ); //$NON-NLS-1$
    keyItem.setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( keyItem.getColumn() );
    keyColumn.setLabelProvider( new PdbInfoKeyProvider() );
    ColumnViewerSorter.registerSorter( keyColumn, new ViewerComparator() );
    ColumnViewerSorter.setSortState( keyColumn, Boolean.FALSE );

    final ViewerColumn valueColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    final ViewerColumnItem valueItem = new ViewerColumnItem( valueColumn );
    valueItem.setText( Messages.getString( "PdbInfoDialog.8" ) ); //$NON-NLS-1$
    valueItem.setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( valueItem.getColumn() );
    valueColumn.setLabelProvider( new PdbInfoValueProvider() );

    final PdbInfo info = m_connection.getInfo();
    viewer.setInput( info.getEntries() );

    return table;
  }

  @Override
  protected void createButtonsForButtonBar( final Composite parent )
  {
    createButton( parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true );
  }
}