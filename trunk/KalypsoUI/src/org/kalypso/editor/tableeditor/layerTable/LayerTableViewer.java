package org.kalypso.editor.tableeditor.layerTable;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
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
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.eclipse.jface.viewers.ICellEditorFactory;
import org.kalypso.eclipse.swt.custom.ExcelLikeTableCursor;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.event.ModellEventListener;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.PoolableKalypsoFeatureTheme;
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

  /** typeName : String -> CellEditor */
  private Map m_editorMap = new HashMap();

  private ICommandTarget m_commandTarget = new JobExclusiveCommandTarget( null );

  private final int m_selectionID;

  private final Color m_selectColor = new Color( null, 250, 0, 0 );

  private Color m_unselectColor = new Color( null, 255, 255, 255 );

  public LayerTableViewer( final Composite parent, final ICellEditorFactory cellEditorFactory, final int selectionID )
  {
    //super( parent, SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION );
    super( parent, SWT.HIDE_SELECTION );

    m_cellEditorFactory = cellEditorFactory;
    m_selectionID = selectionID;

    setContentProvider( new LayerTableContentProvider() );
    setLabelProvider( new LayerTableLabelProvider( this ) );
    setCellModifier( new LayerTableCellModifier( this ) );

    // init table
    final Table table = getTable();
    table.setHeaderVisible( true );
    table.setLinesVisible( true );

    new ExcelLikeTableCursor( this, SWT.NONE );
  }

  public void dispose()
  {
    applyTableTemplate( null, null );
    
    m_selectColor.dispose();
    m_unselectColor.dispose();
  }
  
  /**
   * @see org.eclipse.jface.viewers.TableViewer#hookControl(org.eclipse.swt.widgets.Control)
   */
  protected void hookControl( final Control control )
  {
    // wir wollen nicht die Hooks von TableViewer und StrukturedViewer
    // TODO: geht das auch anders?
    control.addDisposeListener(new DisposeListener() {
      public void widgetDisposed(DisposeEvent event) {
        handleDispose(event);
      }
    });
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
    // TODO: Spezialbehandlung f�r letzte Spalte?

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
    // hack, weil man getCellEditors nicht vern�nftig �berschreiben kann
    setCellEditors( createCellEditors() );
    setColumnProperties( createColumnProperties() );

    super.refresh();
    
    // und die tableitems einf�rben??
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
    
    final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)theme.getLayer();
    final FeatureType featureType = layer.getFeatureType();
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
}