package org.kalypso.ogc.gml.table;

import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.bind.JAXBException;

import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableCursor;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.eclipse.swt.custom.ExcelLikeTableCursor;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.PoolableKalypsoFeatureTheme;
import org.kalypso.ogc.gml.event.ModellEvent;
import org.kalypso.ogc.gml.event.ModellEventListener;
import org.kalypso.ogc.gml.table.celleditors.ICellEditorFactory;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.template.gistableview.ObjectFactory;
import org.kalypso.template.gistableview.GistableviewType.LayerType;
import org.kalypso.template.gistableview.GistableviewType.LayerType.ColumnType;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.pool.PoolableObjectType;

/**
 * @author Belger
 */
public class LayerTableViewer extends TableViewer implements ISelectionProvider,
    ModellEventListener, ICommandTarget
{
  private static Logger LOGGER = Logger.getLogger( LayerTableViewer.class.getName() );

  public static final String COLUMN_PROP_NAME = "columnName";

  public static final String COLUMN_PROP_EDITABLE = "columnEditable";

  private final ObjectFactory m_gistableviewFactory = new ObjectFactory();

  private Runnable m_refreshRunner = new Runnable()
  {
    public void run()
    {
      refresh( /* row */);
    }
  };

  private final ICellEditorFactory m_cellEditorFactory;

  private ICommandTarget m_commandTarget = new JobExclusiveCommandTarget( null );

  private final int m_selectionID;

  private final Color m_selectColor;

  private final Color m_unselectColor;

  private final IProject m_project;

  private boolean m_bSynchronizeSelect = false;

  private final TableCursor m_tableCursor;

  public LayerTableViewer( final Composite parent, final IProject project,
      final ICellEditorFactory cellEditorFactory, final int selectionID )
  {
    super( parent, SWT.BORDER | SWT.HIDE_SELECTION );

    m_cellEditorFactory = cellEditorFactory;
    m_selectionID = selectionID;
    m_project = project;

    setContentProvider( new LayerTableContentProvider() );
    setLabelProvider( new LayerTableLabelProvider( this ) );
    setCellModifier( new LayerTableCellModifier( this ) );

    // init table
    final Table table = getTable();
    table.setHeaderVisible( true );
    table.setLinesVisible( true );

    final TableCursor tc = new ExcelLikeTableCursor( this, SWT.NONE ); 
    m_tableCursor = tc;
    m_tableCursor.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( final SelectionEvent e )
      {
        selectFeature( (KalypsoFeature)tc.getRow().getData() );
      }
    } );

    m_unselectColor = table.getBackground();
    m_selectColor = table.getDisplay().getSystemColor( SWT.COLOR_YELLOW );
  }

  public void dispose()
  {
    applyTableTemplate( null, null );

    m_tableCursor.dispose();
  }

  /**
   * @see org.eclipse.jface.viewers.TableViewer#hookControl(org.eclipse.swt.widgets.Control)
   */
  protected void hookControl( final Control control )
  {
    // wir wollen nicht die Hooks von TableViewer und StrukturedViewer
    // TODO: geht das auch anders?
    control.addDisposeListener( new DisposeListener()
    {
      public void widgetDisposed( DisposeEvent event )
      {
        handleDispose( event );
      }
    } );
  }

  protected void handleDispose( final DisposeEvent event )
  {
    super.handleDispose( event );
  }

  /**
   * Falls true, wird die Selektion in der Tabelle (=Cursor-Position) auf die
   * selektion im Gis-Modell übertragen
   */
  public void setSynchronizeSelect( final boolean synchronize )
  {
    m_bSynchronizeSelect = synchronize;
  }

  public boolean isSelectSynchronized()
  {
    return m_bSynchronizeSelect;
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

    if( !isDisposed() )
      setInput( theme );
  }

  public void clearColumns()
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
    // TODO: Spezialbehandlung für letzte Spalte?

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
    refreshCellEditors();
    setColumnProperties( createColumnProperties() );

    super.refresh();

    // und die tableitems einfärben??
    final TableItem[] items = getTable().getItems();
    for( int i = 0; i < items.length; i++ )
    {
      final TableItem item = items[i];
      final KalypsoFeature kf = (KalypsoFeature)item.getData();
      if( kf.isSelected( m_selectionID ) )
        item.setBackground( m_selectColor );
      else
        item.setBackground( m_unselectColor );
    }

    if( m_tableCursor != null )
      m_tableCursor.redraw();
  }

  /**
   * @see org.eclipse.jface.viewers.TableViewer#getCellEditors()
   */
  private void refreshCellEditors()
  {
    final CellEditor[] oldEditors = getCellEditors();
    if( oldEditors != null )
    {
      for( int i = 0; i < oldEditors.length; i++ )
      {
        if( oldEditors[i] != null )
          oldEditors[i].dispose();
      }
    }

    final Table table = getTable();
    if( table.isDisposed() )
      return;
    
    final TableColumn[] columns = table.getColumns();
    final CellEditor[] editors = new CellEditor[columns.length];

    final IKalypsoTheme theme = (IKalypsoTheme)getInput();
    if( theme == null || theme.getLayer() == null )
    {
      setCellEditors( editors );
      return;
    }

    final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)theme.getLayer();
    final FeatureType featureType = layer.getFeatureType();
    for( int i = 0; i < editors.length; i++ )
    {
      final String propName = columns[i].getData( COLUMN_PROP_NAME ).toString();
      final FeatureTypeProperty ftp = featureType.getProperty( propName );
      if( ftp != null )
      {
        try
        {
          editors[i] = m_cellEditorFactory.createEditor( ftp, m_project, table, SWT.NONE );
        }
        catch( final FactoryException e )
        {
          // ignore: Type not supported
          LOGGER.warning( "CellEditor not found for type: " + ftp.getType() );
        }
      }
    }

    setCellEditors( editors );
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

  public void selectFeature( final KalypsoFeature feature )
  {
    final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)getTheme().getLayer();
    final KalypsoFeature[] features = layer.getAllFeatures();
    for( int i = 0; i < features.length; i++ )
      features[i].unselect( m_selectionID );
    
    feature.select( m_selectionID );

    layer.fireModellEvent( null );
  }
  
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
   * @see org.kalypso.ogc.gml.event.ModellEventListener#onModellChange(org.kalypso.ogc.gml.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( !isDisposed() )
      getControl().getDisplay().asyncExec( m_refreshRunner );
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

  public int getColumnCount()
  {
    return getTable().getColumnCount();
  }

  public Gistableview createTableTemplate() throws JAXBException
  {
    final Gistableview tableTemplate = m_gistableviewFactory.createGistableview();
    final LayerType layer = m_gistableviewFactory.createGistableviewTypeLayerType();

    final PoolableObjectType key = getTheme().getLayerKey();
    layer.setId( "1" );
    layer.setHref( key.getSourceAsString() );
    layer.setLinktype( key.getType() );
    layer.setActuate( "onRequest" );
    layer.setType( "simple" );

    tableTemplate.setLayer( layer );

    final List columns = layer.getColumn();

    final TableColumn[] tableColumns = getTable().getColumns();
    for( int i = 0; i < tableColumns.length; i++ )
    {
      final TableColumn tc = tableColumns[i];

      final ColumnType columnType = m_gistableviewFactory
          .createGistableviewTypeLayerTypeColumnType();

      columnType.setName( tc.getData( COLUMN_PROP_NAME ).toString() );
      columnType.setEditable( ( (Boolean)tc.getData( COLUMN_PROP_EDITABLE ) ).booleanValue() );
      columnType.setWidth( tc.getWidth() );

      columns.add( columnType );
    }

    return tableTemplate;
  }

  public void saveData()
  {
    getTheme().saveFeatures();
  }
}