package org.kalypso.editor.tableeditor.layerTable;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.kalypso.eclipse.jface.viewers.ICellEditorFactory;
import org.kalypso.eclipse.swt.custom.ExcelLikeTableCursor;
import org.kalypso.editor.tableeditor.actions.ColumnAction;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.event.ModellEventListener;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.PoolableKalypsoFeatureTheme;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.template.gistableview.GistableviewType.LayerType;
import org.kalypso.template.gistableview.GistableviewType.LayerType.ColumnType;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.util.factory.FactoryException;

/**
 * @author Belger
 */
public class LayerTableViewer extends TableViewer implements ISelectionProvider,
    ModellEventListener, ICommandTarget
{
  private static Logger LOGGER = Logger.getLogger( LayerTableViewer.class.getName() );

  public static final String COLUMN_PROP_NAME = "columnName";

  public static final String COLUMN_PROP_EDITABLE = "columnEditable";

  private IMenuManager m_menu;

  private IMenuManager m_spaltenMenu;

  private Runnable m_refreshRunner = new Runnable()
  {
    public void run()
    {
      refresh( /* row */);
    }
  };

  private final ICellEditorFactory m_cellEditorFactory;

  /** typeName : String -> CellEditor */
  private Map m_editorMap = new HashMap();

  private ICommandTarget m_commandTarget = new JobExclusiveCommandTarget( null );

  public LayerTableViewer( final Composite parent, final ICellEditorFactory cellEditorFactory )
  {
    super( parent, SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION );

    m_cellEditorFactory = cellEditorFactory;

    setContentProvider( new LayerTableContentProvider() );
    setLabelProvider( new LayerTableLabelProvider( this ) );
    setCellModifier( new LayerTableCellModifier( this ) );

    // init table
    final Table table = getTable();
    table.setHeaderVisible( true );
    table.setLinesVisible( true );

    new ExcelLikeTableCursor( this, SWT.NONE );

    // TODO: anderer Konstruktor?
    //    setTableTemplate( null );
    
    final MenuManager menuMgr = new MenuManager( "#contextMenu" );
    menuMgr.setRemoveAllWhenShown( true );
    menuMgr.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( final IMenuManager manager )
      {
        appendSpaltenActions( manager );
      }
    } );
    final Menu menu = menuMgr.createContextMenu( table );
    table.setMenu( menu );
  }

  public void appendSpaltenActions( final IMenuManager manager )
  {
    final PoolableKalypsoFeatureTheme theme = getTheme();
    if( theme == null )
      return;

    final KalypsoFeatureLayer layer = theme.getLayer();
    if( layer == null )
      return;

    final FeatureTypeProperty[] ftps = layer.getFeatureType().getProperties();
    for( int i = 0; i < ftps.length; i++ )
      manager.add( new ColumnAction( this, ftps[i].getName() ) );
  }

  public void dispose()
  {
    applyTableTemplate( null, null );

    if( m_menu != null )
      m_menu.dispose();
    if( m_spaltenMenu != null )
      m_menu.dispose();
  }

  public void applyTableTemplate( final Gistableview tableView, final IProject project )
  {
    clearColumns();
    setTheme( null );

    if( tableView == null )
      return;

    final LayerType layer = tableView.getLayer();
    setTheme( new PoolableKalypsoFeatureTheme( layer, project ) );

    final List columnList = layer.getColumn();
    for( final Iterator iter = columnList.iterator(); iter.hasNext(); )
    {
      final ColumnType ct = (ColumnType)iter.next();
      addColumn( ct.getName(), ct.getWidth(), ct.isEditable() );
    }
  }

  public PoolableKalypsoFeatureTheme getTheme()
  {
    return (PoolableKalypsoFeatureTheme)getInput();
  }

  private void setTheme( final PoolableKalypsoFeatureTheme theme )
  {
    final IKalypsoTheme oldTheme = (IKalypsoTheme)getInput();

    if( oldTheme != null )
    {
      oldTheme.removeModellListener( this );
      oldTheme.dispose();
    }

    if( theme != null )
      theme.addModellListener( this );

    setInput( theme );
  }

  //  public void setModel( final LayerTableModel model )
  //  {
  //    clearColumns();
  //
  //    if( m_model != null )
  //    {
  //      m_model.removeModelListener( this );
  //      m_model.getTheme().removeModellListener( this );
  //    }
  //
  //    m_model = model;
  //
  //    if( model != null )
  //    {
  //      m_model.addModelListener( this );
  //      m_model.getTheme().addModellListener( this );
  //    }
  //
  //    final IAction[] actions = createActions();
  //
  //    m_menu = refillMenu( m_menu, actions );
  //    m_spaltenMenu = refillMenu( m_spaltenMenu, actions );
  //
  //    m_viewer.getTable().setMenu( ( (MenuManager)m_menu ).createContextMenu(
  // m_viewer.getTable() ) );
  //
  //    createColumns();
  //  }

  //  private IAction[] createActions()
  //  {
  //    if( m_model != null )
  //    {
  //      final FeatureTypeProperty[] ftps =
  // m_model.getFeatureType().getProperties();
  //
  //      final IAction[] actions = new IAction[ftps.length];
  //
  //      for( int i = 0; i < ftps.length; i++ )
  //        actions[i] = new ColumnAction( m_columnCommandTarget, this, ftps[i],
  // m_model
  //            .getInitialWidth( ftps[i] ) != 0 );
  //
  //      return actions;
  //    }
  //
  //    return new IAction[] {};
  //  }

  //  private IMenuManager refillMenu( final IMenuManager oldMenu, final
  // IAction[] actions )
  //  {
  //    if( oldMenu != null )
  //    {
  //      oldMenu.removeAll();
  //      oldMenu.dispose();
  //    }
  //
  //    final IMenuManager menu = new MenuManager( "Spalten" );
  //
  //    // create context menu
  //    for( int i = 0; i < actions.length; i++ )
  //      menu.add( actions[i] );
  //
  //    return menu;
  //  }

  protected void clearColumns()
  {
    final Table table = getTable();
    if( table.isDisposed() )
      return;

    final TableColumn[] columns = table.getColumns();
    for( int i = 0; i < columns.length; i++ )
      columns[i].dispose();
  }

  public void addColumn( final String propertyName, final int width, final boolean isEditable )
  {
    // TODO
    final Table table = getTable();

    final TableColumn tc = new TableColumn( table, SWT.CENTER );
    tc.setWidth( width );
    tc.setData( COLUMN_PROP_NAME, propertyName );
    tc.setData( COLUMN_PROP_EDITABLE, Boolean.valueOf( isEditable ) );
    tc.setText( propertyName );

    refresh();
  }

  public void removeColumn( final String name )
  {
    final TableColumn column = getColumn( name );
    if( column != null )
      column.dispose();

    refresh();
  }

  /**
   * @see org.eclipse.jface.viewers.StructuredViewer#refresh()
   */
  public void refresh()
  {
    // zuerst alle celleditoren neu berechnen
    // hack, weil man getCellEditors nicht vernünftig überschreiben kann
    setCellEditors( createCellEditors() );
    setColumnProperties( createColumnProperties() );

    if( m_spaltenMenu != null )
      m_spaltenMenu.dispose();

    super.refresh();
  }

  /**
   * @see org.eclipse.jface.viewers.TableViewer#getCellEditors()
   */
  private CellEditor[] createCellEditors()
  {
    final Table table = getTable();
    final TableColumn[] columns = table.getColumns();
    final CellEditor[] editors = new CellEditor[columns.length];

    final IKalypsoTheme theme = (IKalypsoTheme)getInput();
    if( theme == null || theme.getLayer() == null )
      return editors;

    final FeatureType featureType = theme.getLayer().getFeatureType();
    for( int i = 0; i < editors.length; i++ )
    {
      final String propName = columns[i].getData( COLUMN_PROP_NAME ).toString();
      final FeatureTypeProperty ftp = featureType.getProperty( propName );
      if( ftp != null )
      {
        final String type = ftp.getType();
        CellEditor editor = (CellEditor)m_editorMap.get( type );
        if( editor == null )
        {
          try
          {
            editor = m_cellEditorFactory.createEditor( type, table, SWT.NONE );
            m_editorMap.put( type, editor );
          }
          catch( final FactoryException e )
          {
            // ignore: Type not supported
            LOGGER.warning( "CellEditor not found for type: " + ftp.getType() );
          }
        }

        editors[i] = editor;
      }
    }

    return editors;
  }

  /**
   * @see org.eclipse.jface.viewers.TableViewer#getColumnProperties()
   */
  private String[] createColumnProperties()
  {
    final Table table = getTable();
    final TableColumn[] columns = table.getColumns();
    final String[] properties = new String[columns.length];

    for( int i = 0; i < properties.length; i++ )
      properties[i] = columns[i].getData( COLUMN_PROP_NAME ).toString();

    return properties;
  }

  //  protected void createColumns()
  //  {
  //    if( m_model == null )
  //      return;
  //
  //    final Table table = m_viewer.getTable();
  //
  //    final FeatureType featureType =
  // m_model.getTheme().getLayer().getFeatureType();
  //    final LayerTableModel.Column[] columns = m_model.getColumns();
  //
  //    final String[] colProperties = new String[columns.length];
  //    final CellEditor[] cellEditors = new CellEditor[columns.length];
  //    for( int i = 0; i < columns.length; i++ )
  //    {
  //      final FeatureTypeProperty ftp = columns[i].ftp;
  //      if( ftp == null )
  //      {
  //        LOGGER.warning( "Column doesnt exist: " + i );
  //        continue;
  //      }
  //
  //      final TableColumn tc = new TableColumn( table, SWT.CENTER );
  //      tc.setWidth( 100 );
  //
  //      final String columnName = ftp.getName();
  //      if( columnName != null )
  //        tc.setText( columnName );
  //      tc.setData( ftp );
  //      tc.setWidth( m_model.getInitialWidth( ftp ) );
  //
  //      colProperties[i] = columnName;
  //
  //      m_ftp2ColumnMap.put( ftp, tc );
  //
  //      try
  //      {
  //        if( m_model.isEditable( ftp ) )
  //          cellEditors[i] = m_cellEditorFactory.createEditor( ftp.getType(), table,
  // SWT.NONE );
  //      }
  //      catch( final FactoryException e )
  //      {
  //        // ignore: Type not supported
  //        LOGGER.warning( "CellEditor not found for type: " + ftp.getType() );
  //      }
  //    }
  //
  //    m_viewer.setColumnProperties( colProperties );
  //    m_viewer.setCellEditors( cellEditors );
  //    m_viewer.setInput( m_model );
  //    m_viewer.setCellModifier( new LayerTableCellModifier( m_model, featureType
  // ) );
  //  }

  public void selectRow( final KalypsoFeature feature )
  {
    getControl().getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        setSelection( new StructuredSelection( feature ) );
      }
    } );
  }

  /**
   * @see org.kalypso.ogc.event.ModellEventListener#onModellChange(org.kalypso.ogc.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( !isDisposed() )
      getControl().getDisplay().asyncExec( m_refreshRunner );
  }

  public IMenuManager getSpaltenMenu()
  {
    return m_spaltenMenu;
  }

  public boolean isDisposed()
  {
    return getTable().isDisposed();
  }

  /**
   * @see org.kalypso.util.command.ICommandTarget#postCommand(org.kalypso.util.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( final ICommand command, final Runnable runnable )
  {
    m_commandTarget.postCommand( command, runnable );
  }

  public String getPropertyName( final int columnIndex )
  {
    final TableColumn column = getTable().getColumn( columnIndex );
    return column.getData( COLUMN_PROP_NAME ).toString();
  }

  public boolean isEditable( final String property )
  {
    final TableColumn column = getColumn( property );
    return column == null ? false : ( (Boolean)column.getData( COLUMN_PROP_EDITABLE ) )
        .booleanValue();
  }

  private TableColumn getColumn( final String property )
  {
    final TableColumn[] columns = getTable().getColumns();
    for( int i = 0; i < columns.length; i++ )
    {
      final String name = columns[i].getData( COLUMN_PROP_NAME ).toString();
      if( property.equals( name ) )
        return columns[i];
    }

    return null;
  }

  public int getWidth( final String propertyName )
  {
    final TableColumn column = getColumn( propertyName );
    if( column != null )
      return column.getWidth();

    return 0;
  }

  public boolean hasColumn( final String propertyName )
  {
    return getColumn( propertyName ) != null;
  }
}