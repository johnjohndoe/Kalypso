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
package org.kalypso.model.wspm.ui.profil.view.table.swt;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.IBaseLabelProvider;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.kalypso.contribs.eclipse.jface.viewers.ExcelClipboardAdapter;
import org.kalypso.contribs.eclipse.jface.viewers.ITooltipProvider;
import org.kalypso.contribs.eclipse.jface.viewers.NoMouseDownTableViewer;
import org.kalypso.contribs.eclipse.jface.viewers.TableViewerTooltipListener;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.contribs.eclipse.swt.custom.ExcelTableCursor3_1;
import org.kalypso.contribs.eclipse.swt.custom.TableCursor;
import org.kalypso.contribs.eclipse.swt.custom.ExcelTableCursor3_1.ADVANCE_MODE;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.PointAdd;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;
import org.kalypso.model.wspm.core.profil.changes.PointRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIImages;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.profil.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;

/**
 * <p>
 * Zeigt die Daten des Profils in tabellarischer Form.>/p>
 * <p>
 * Features:
 * </p>
 * <ul>
 * <li>Anzeige der Daten des Profils als Tabelle. Spalten sind die Datensätze (Höhe, Rauheit, ...), Zeilen sind die
 * Profilpunkte. OK</li>
 * <li>Per Kontextmenu lassen sich Spalten ein und ausblenden. Reihenfolge der Spalten lässt sich ändern-</li>
 * <li>Markieren mehrerer Zeilen möglich. OK</li>
 * <li>Markierte Zeilen können gelöscht werden.</li>
 * <li>Einfügen eines Punktes unter der Zeile mit dem Fokus. Der neue Punkt entsteht in der geometrischen Mitte
 * zwischen markiertem Punkt und nächstem Punkt. Alle Werte werden interpoliert.</li>
 * <li>Editieren eines einzelnen Wertes.</li>
 * <li>Problemwerte (Rücksprünge, ungültige Werte) werden markiert.</li>
 * </ul>
 * 
 * @author belger
 */
public class ProfilSWTTableView extends AbstractProfilView
{
  private static final String INSERT_POINT_NAME = "Punkt einfügen";

  private static final String DELETE_POINTS_NAME = "Punkte löschen";

  public static final String SORT_KEY = "sortViewer";

  public static final String ACTION_DELETEPOINTS = "profiltable.action.deletepoints";

  public static final String ACTION_INSERTPOINT = "profiltable.action.insertpoint";

  public static final String ACTION_COPY = "profiltable.action.copy";

  public static final String ACTION_PASTE = "profiltable.action.paste";

  public static final String ACTION_SELECTALL = "protfiltable.action.selectall";

  public static final String ACTION_FILLVALUES = "protfiltable.action.fillvalues";

  protected final static class ColumnStruct
  {
    private final Collection<CellEditor> m_editors = new LinkedList<CellEditor>();

    private final Collection<String> m_properties = new LinkedList<String>();

    public CellEditor[] getEditors( )
    {
      return m_editors.toArray( new CellEditor[m_editors.size()] );
    }

    protected String[] getProperties( )
    {
      return m_properties.toArray( new String[m_properties.size()] );
    }

    public void addColumn( final CellEditor editor, final String id )
    {
      m_editors.add( editor );
      m_properties.add( id );
    }
  }

  private final Map<String, ProfilTableAction> m_actions = new HashMap<String, ProfilTableAction>();

  protected TableViewer m_viewer;

  private Composite m_tableContainer;

  private ExcelTableCursor3_1 m_cursor;

  private final MenuManager m_menuManager = new MenuManager();

  private Image m_sortDownImage = KalypsoModelWspmUIImages.ID_SORT_DOWN.createImage();

  private Image m_sortUpImage = KalypsoModelWspmUIImages.ID_SORT_UP.createImage();

  private Image m_emptyImage = KalypsoModelWspmUIImages.ID_EMPTY.createImage();

  /** The associated resource to the given profile */
  private final IFile m_file;

  public ProfilSWTTableView( final IProfilEventManager pem, final ProfilViewData viewdata, final IFile file )
  {
    super( pem, viewdata, null );
    m_file = file;

    createActions();
    m_menuManager.add( new GroupMarker( IWorkbenchActionConstants.MB_ADDITIONS ) );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.IProfilView#dispose()
   */
  @Override
  public void dispose( )
  {
    m_menuManager.dispose();

    if( m_sortDownImage != null )
      m_sortDownImage.dispose();

    if( m_sortUpImage != null )
      m_sortUpImage.dispose();

    if( m_emptyImage != null )
      m_emptyImage.dispose();

    if( m_viewer != null )
    {

      final IContentProvider contentProvider = m_viewer.getContentProvider();
      if( contentProvider != null )
        contentProvider.dispose();

      final IBaseLabelProvider labelProvider = m_viewer.getLabelProvider();
      if( labelProvider != null )
        labelProvider.dispose();
      m_viewer.getTable().dispose();
      m_viewer = null;
    }

    super.dispose();
  }

  protected TableViewer getViewer( )
  {
    return m_viewer;
  }

  protected TableCursor getCursor( )
  {
    return m_cursor;
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite,
   *      int)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final int style )
  {
    m_tableContainer = new Composite( parent, style );
    final GridLayout containerLayout = new GridLayout();
    containerLayout.marginHeight = 0;
    containerLayout.marginWidth = 0;
    m_tableContainer.setLayout( containerLayout );

    final GridData tableLayoutData = new GridData( GridData.FILL_BOTH );
    tableLayoutData.horizontalIndent = 0;
    tableLayoutData.verticalIndent = 0;

    m_tableContainer.setLayoutData( tableLayoutData );

    final Control table = doCreateTable();
    table.setLayoutData( tableLayoutData );

    return table;
  }

  private Control doCreateTable( )
  {
    m_viewer = new NoMouseDownTableViewer( m_tableContainer, SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION );

    final Table table = m_viewer.getTable();
    table.setFont( table.getParent().getFont() );
    table.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    table.setLinesVisible( true );
    table.setToolTipText( "Klick auf Tabellenkopf verändert die Sortierung.\nDurch Ziehen des Tabellenkopfs kann die Reihenfolge der Spalten verändert werden." );

    m_viewer.setContentProvider( new ProfilContentProvider( m_file ) );
    m_viewer.setLabelProvider( new ProfilLabelProvider( m_viewer ) );
    m_viewer.setCellModifier( new ProfilCellModifier( m_viewer ) );
    m_viewer.setUseHashlookup( true );
    TableViewerTooltipListener.hookViewer( m_viewer, false );

    final ExcelTableCursor3_1 cursor = new ExcelTableCursor3_1( m_viewer, SWT.NONE, ExcelTableCursor3_1.ADVANCE_MODE.RIGHT, true );
    m_cursor = cursor;

    final Menu menu = m_menuManager.createContextMenu( table );
    table.setMenu( menu );
    m_cursor.setMenu( m_menuManager.createContextMenu( table ) );

    cursor.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final TableItem row = cursor.getRow();

        // change the active point, but dont put it into the undo queue
        final IProfilPoint point = (IProfilPoint) row.getData();
        final ProfilOperation operation = new ProfilOperation( "", getProfilEventManager(), new ActiveObjectEdit( getProfil(), point, null ), true );
        final IStatus status = operation.execute( new NullProgressMonitor(), null );
        operation.dispose();
        if( !status.isOK() )
          KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );

        final ITooltipProvider labelProvider = (ITooltipProvider) m_viewer.getLabelProvider();
        cursor.setToolTipText( labelProvider.getTooltip( row.getData() ) );
      }
    } );

    final IProfil profil = getProfil();
    refreshColumns( profil );

    return table;
  }

  public MenuManager getContextMenuManager( )
  {
    return m_menuManager;
  }

  public IAction getAction( final String id )
  {
    return m_actions.get( id );
  }

  private void packColumns( )
  {
    final Table table = m_viewer.getTable();
    final TableColumn[] columns = table.getColumns();
    for( final TableColumn tc : columns )
    {
      if( tc.getData( ProfilLabelProvider.COLUMN_KEY ) == null )
        tc.setWidth( 16 );
      else
      {
        tc.pack();
        // ein bisserl drauf, damit man auch die Spaltentexte lesen kann
        tc.setWidth( tc.getWidth() + m_emptyImage.getBounds().width );
      }
    }
  }

  protected void refreshColumns( final IProfil profil )
  {
    final Table table = m_viewer.getTable();

    final TableColumn[] columns = table.getColumns();
    for( int i = 0; i < columns.length; i++ )
    {
      final TableColumn column = columns[i];
      column.dispose();
    }

    table.setHeaderVisible( profil != null );

    final ColumnStruct struct = new ColumnStruct();

    if( profil != null )
    {
      addColumn( struct, null ).setResizable( false );

      final List<POINT_PROPERTY> tableDataKeys = profil.getPointProperties( true );
      for( final POINT_PROPERTY key : tableDataKeys )
        addColumn( struct, key );
    }

    m_viewer.setInput( getProfilEventManager() );
    m_viewer.setCellEditors( struct.getEditors() );
    m_viewer.setColumnProperties( struct.getProperties() );

    packColumns();
  }

  public TableColumn addColumn( final ColumnStruct struct, final POINT_PROPERTY columnKey )
  {
    final String text = columnKey == null ? "" : columnKey.toString();
    final String id = columnKey == null ? "" : columnKey.toString();

    final Table table = m_viewer.getTable();
    final TableColumn column = new TableColumn( table, SWT.CENTER );
    column.setText( text );
    column.setAlignment( SWT.RIGHT );
    column.setData( ProfilLabelProvider.COLUMN_KEY, columnKey );
    column.setMoveable( true );

    // causes columns to resize and warning images to be stretched; but
    // what to to else?
    if( columnKey != null )
      column.setImage( m_emptyImage );

    column.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        changeSortColumn( column );
      }
    } );

    struct.addColumn( new TextCellEditor( table, SWT.BORDER ), id );

    return column;
  }

  protected void changeSortColumn( final TableColumn column )
  {
    final Boolean oldstate = (Boolean) column.getData( SORT_KEY );

    // clear columns
    final Table table = m_viewer.getTable();
    if( table.indexOf( column ) == 0 )
      return;

    for( final TableColumn col : table.getColumns() )
    {
      col.setData( SORT_KEY, null );
      col.setImage( m_emptyImage );
    }

    final POINT_PROPERTY key = (POINT_PROPERTY) column.getData( ProfilLabelProvider.COLUMN_KEY );

    final ProfilViewerSorter sorter;
    final Boolean newstate;
    final Image img;
    if( oldstate == null )
    {
      sorter = new ProfilViewerSorter( key, false );
      img = m_sortDownImage;
      newstate = Boolean.FALSE;
    }
    else if( !oldstate.booleanValue() )
    {
      sorter = new ProfilViewerSorter( key, true );
      img = m_sortUpImage;
      newstate = Boolean.TRUE;
    }
    else
    {
      sorter = null;
      img = m_emptyImage;
      newstate = null;
    }

    column.setData( SORT_KEY, newstate );
    column.setImage( img );

    m_viewer.setSorter( sorter );
  }

  public void setAdvanceMode( final String string )
  {
    m_cursor.setAdvanceMode( ExcelTableCursor3_1.ADVANCE_MODE.valueOf( string ) );
  }

  public static String[][] getAdvanceModes( )
  {
    final ADVANCE_MODE[] advance_modes = ExcelTableCursor3_1.ADVANCE_MODE.values();
    final String[][] descriptions = new String[advance_modes.length][];
    for( int i = 0; i < descriptions.length; i++ )
      descriptions[i] = new String[] { advance_modes[i].toString(), advance_modes[i].name() };

    return descriptions;
  }

  private void createActions( )
  {
    m_actions.put( ACTION_DELETEPOINTS, new ProfilTableAction( this, DELETE_POINTS_NAME )
    {
      @Override
      public void run( )
      {
        final TableViewer viewer = getViewer();
        if( viewer == null )
          return;

        final IStructuredSelection selection = (IStructuredSelection) viewer.getSelection();

        @SuppressWarnings("unchecked")
        final IProfilPoint[] pointsToDelete = (IProfilPoint[]) selection.toList().toArray( new IProfilPoint[selection.size()] );
        final PointRemove[] changes = new PointRemove[pointsToDelete.length];
        for( int i = 0; i < pointsToDelete.length; i++ )
        {
          final IProfilPoint point = pointsToDelete[i];
          changes[i] = new PointRemove( getProfil(), point );
        }

        final ProfilOperation operation = new ProfilOperation( "", getProfilEventManager(), changes, false );
        new ProfilOperationJob( operation ).schedule();
      }
    } );

    m_actions.put( ACTION_INSERTPOINT, new ProfilTableAction( this, INSERT_POINT_NAME )
    {
      @Override
      public void run( )
      {
        if( getViewer().getSorter() != null )
        {
          MessageDialog.openWarning( getViewer().getControl().getShell(), INSERT_POINT_NAME, "Punkte können nur in unsortierte Tabelle eingefügt werden.\nKlicken Sie auf die markierte Spalte, um die Sortierung aufzuheben." );
          return;
        }

        final TableItem row = getCursor().getRow();
        // if( row == null )
        // return;

        final IProfilPoint thePointBefore = (row == null)?null:(IProfilPoint) row.getData();

        // final PointInsert change = new PointInsert( getProfil(), thePointBefore );
        final PointAdd change = new PointAdd( getProfil(), thePointBefore, null );
        final ProfilOperation operation = new ProfilOperation( "", getProfilEventManager(), change, true );
        new ProfilOperationJob( operation ).schedule();
      }
    } );

    m_actions.put( ACTION_SELECTALL, new ProfilTableAction( this )
    {
      @Override
      public void run( )
      {
        getViewer().getTable().selectAll();
      }
    } );

    m_actions.put( ACTION_COPY, new ProfilTableAction( this )
    {
      @Override
      public void run( )
      {
        new ExcelClipboardAdapter( getViewer() ).doCopy();
      }
    } );

    m_actions.put( ACTION_PASTE, new ProfilTableAction( this )
    {
      @Override
      public void run( )
      {
        final ExcelClipboardAdapter adapter = new ExcelClipboardAdapter( getViewer() );

        final Table table = getViewer().getTable();

        int row = table.indexOf( getCursor().getRow() );
        final int column = getCursor().getColumn();

        // wird wissen, dass das einfügen einen refresh auslöst, deswegen: false
        adapter.doPaste( row, column, false );
      }
    } );
    m_actions.put( ACTION_FILLVALUES, new ProfilTableAction( this )
    {
      @Override
      public void run( )
      {
        final TableCursor cursor = getCursor();
        final TableItem row = cursor.getRow();
        final int column = cursor.getColumn();

        final TableViewer viewer = getViewer();
        final String property = "" + viewer.getColumnProperties()[column];

        final ProfilCellModifier cellModifier = (ProfilCellModifier) viewer.getCellModifier();
        final Object aktiveElement = row.getData();
        final double value = cellModifier.getValueAsDouble( aktiveElement, property );

        final IStructuredSelection selection = (IStructuredSelection) viewer.getSelection();

        final List<PointPropertyEdit> changes = new ArrayList<PointPropertyEdit>( selection.size() );

        for( final Iterator iter = selection.iterator(); iter.hasNext(); )
        {
          @SuppressWarnings("unchecked")
          final Object element = iter.next();
          if( element != aktiveElement )
            changes.add( new PointPropertyEdit( (IProfilPoint) element, ProfilCellModifier.propertyForID( getProfil(), property ), value ) );
        }

        final PointPropertyEdit[] profilChanges = changes.toArray( new PointPropertyEdit[changes.size()] );
        final ProfilOperation operation = new ProfilOperation( "Werte setzen", getProfilEventManager(), profilChanges, true );
        new ProfilOperationJob( operation ).schedule();
      }
    } );
  }

  public ISelectionProvider getSelectionProvider( )
  {
    return m_viewer;
  }

  /**
   * Refreshed den Viewer und den Cursor
   */
  protected void refresh( )
  {
    ViewerUtilities.refresh( m_viewer, true );

    final TableCursor cursor = m_cursor;
    if( cursor != null && !cursor.isDisposed() )
    {
      cursor.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          if( !cursor.isDisposed() )
            cursor.redraw();
        }
      } );
    }
  }

  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    m_tableContainer.getDisplay().syncExec( new Runnable()
    {
      public void run( )
      {
        if( hint.isPointPropertiesChanged() )
        {
          refreshColumns( getProfil() );
          return;
        }

        if( hint.isPointPropertiesChanged() || hint.isPointsChanged() || hint.isPointValuesChanged() || hint.isDeviderMoved() )
          refresh();

        if( hint.isActivePointChanged() )
          refreshActivePoint();
      }
    } );

  }

  protected void refreshActivePoint( )
  {
    final IProfil profil = getProfil();
    if( profil == null )
      return;

    final IProfilPoint activePoint = profil.getActivePoint();
    if( activePoint == null )
      return;

    final POINT_PROPERTY activeProperty = profil.getActiveProperty();
    final String propertyName = activeProperty == null ? null : activeProperty.toString();

    final Table table = m_viewer.getTable();
    if( table == null || table.isDisposed() )
      return;

    final TableItem[] items = table.getItems();
    for( final TableItem item : items )
    {
      if( item.getData() == activePoint )
      {
        final Object[] columnProperties = m_viewer.getColumnProperties();

        // default, the current column
        int column = m_cursor.getColumn();
        // set to active column
        for( int i = 0; i < columnProperties.length; i++ )
        {
          if( columnProperties[i].equals( propertyName ) )
          {
            column = i;
            break;
          }
        }

        m_cursor.setSelection( item, column );
        table.setSelection( new TableItem[] { item } );
      }
    }
  }
}
