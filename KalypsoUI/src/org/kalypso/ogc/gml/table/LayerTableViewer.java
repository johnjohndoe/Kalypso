package org.kalypso.ogc.gml.table;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.bind.JAXBException;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureList;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree.model.feature.event.ModellEventProvider;
import org.deegree.model.feature.event.ModellEventProviderAdapter;
import org.deegree_impl.model.feature.visitors.UnselectFeatureVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableCursor;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.eclipse.swt.custom.ExcelLikeTableCursor;
import org.kalypso.ogc.gml.GisTemplateFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.table.celleditors.IFeatureModifierFactory;
import org.kalypso.ogc.gml.table.command.ChangeSortingCommand;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.template.gistableview.ObjectFactory;
import org.kalypso.template.gistableview.GistableviewType.LayerType;
import org.kalypso.template.gistableview.GistableviewType.LayerType.ColumnType;
import org.kalypso.template.gistableview.GistableviewType.LayerType.SortType;
import org.kalypso.util.command.DefaultCommandManager;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;
import org.kalypso.util.command.InvisibleCommand;
import org.kalypso.util.command.JobExclusiveCommandTarget;

/**
 * @todo TableCursor soll sich auch bewegen, wenn die Sortierung sich ändert
 * 
 * @author Belger
 */
public class LayerTableViewer extends TableViewer implements ISelectionProvider,
    ModellEventListener, ICommandTarget, ModellEventProvider, SelectionListener, ICellModifier
{
  protected Logger LOGGER = Logger.getLogger( LayerTableViewer.class.getName() );

  public static final String COLUMN_PROP_NAME = "columnName";

  public static final String COLUMN_PROP_EDITABLE = "columnEditable";

  public static final String COLUMN_PROP_WIDTH = "columnWidth";

  private final ObjectFactory m_gistableviewFactory = new ObjectFactory();

  private final IFeatureModifierFactory m_featureControlFactory;

  private ICommandTarget m_commandTarget = new JobExclusiveCommandTarget(
      new DefaultCommandManager(), null );

  private ModellEventProvider m_modellEventProvider = new ModellEventProviderAdapter();

  private final int m_selectionID;

  private final Color m_selectColor;

  private final Color m_unselectColor;

  private final TableCursor m_tableCursor;

  private IFeatureModifier[] m_modifier;

  private final boolean m_bCursorSelects;

  private final LayerTableSorter m_sorter = new LayerTableSorter();

  protected final ICommandTarget m_templateTarget;

  protected boolean m_isApplyTemplate = false;

  /**
   * This class handles selections of the column headers. Selection of the
   * column header will cause resorting of the shown tasks using that column's
   * sorter. Repeated selection of the header will toggle sorting order
   * (ascending versus descending).
   */
  private final SelectionListener m_headerListener = new SelectionAdapter()
  {
    /**
     * Handles the case of user selecting the header area.
     * <p>
     * If the column has not been selected previously, it will set the sorter of
     * that column to be the current tasklist sorter. Repeated presses on the
     * same column header will toggle sorting order (ascending/descending).
     */
    public void widgetSelected( final SelectionEvent e )
    {
      // column selected - need to sort
      final TableColumn tableColumn = (TableColumn)e.widget;

      m_templateTarget.postCommand( new ChangeSortingCommand( LayerTableViewer.this, tableColumn ),
          null );
    }
  };

  private ControlListener m_headerControlListener = new ControlAdapter()
  {

    /**
     * @see org.eclipse.swt.events.ControlAdapter#controlResized(org.eclipse.swt.events.ControlEvent)
     */
    public void controlResized( final ControlEvent e )
    {
      if( m_isApplyTemplate == true )
        return;

      final TableColumn tc = (TableColumn)e.widget;

      // kann nicht rückgüngig gemacht werden, sorgt aber dafür, dass der Editor
      // dirty ist
      final int width = tc.getWidth();
      if( width != ( (Integer)tc.getData( COLUMN_PROP_WIDTH ) ).intValue() )
      {
        m_templateTarget.postCommand( new InvisibleCommand(), null );
        tc.setData( COLUMN_PROP_WIDTH, new Integer( width ) );
      }
    }
  };

  /**
   * @param bCursorSelects
   *          falls true, wird immer die unter dem Cursor liegende Zeile
   *          selektiert
   */
  public LayerTableViewer( final Composite parent, final ICommandTarget templateTarget,
      final IFeatureModifierFactory featureControlFactory, final int selectionID,
      final boolean bCursorSelects )
  {
    super( parent, SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION );

    m_featureControlFactory = featureControlFactory;
    m_selectionID = selectionID;
    m_bCursorSelects = bCursorSelects;
    m_templateTarget = templateTarget;

    setContentProvider( new LayerTableContentProvider() );
    setLabelProvider( new LayerTableLabelProvider( this ) );
    setCellModifier( this );
    setSorter( m_sorter );

    // init table
    final Table table = getTable();
    table.setHeaderVisible( true );
    table.setLinesVisible( true );

    final TableCursor tc = new ExcelLikeTableCursor( this, SWT.NONE );
    m_tableCursor = tc;

    m_unselectColor = table.getBackground();
    m_selectColor = table.getDisplay().getSystemColor( SWT.COLOR_YELLOW );

    table.addSelectionListener( this );
  }

  public void assignSelectionToFeatures()
  {
    final IKalypsoFeatureTheme theme = getTheme();

    // clear selection
    final UnselectFeatureVisitor unselectvisitor = new UnselectFeatureVisitor( m_selectionID );
    theme.getFeatureList().accept( unselectvisitor );

    final TableItem[] selection = getTable().getSelection();
    for( int i = 0; i < selection.length; i++ )
      ( (Feature)selection[i].getData() ).select( m_selectionID );

    getTheme().getWorkspace().fireModellEvent(
        new ModellEvent( getTheme().getWorkspace(), ModellEvent.SELECTION_CHANGED ) );
  }

  public boolean isCursorSelects()
  {
    return m_bCursorSelects;
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
    // geht das auch anders?
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

  public void applyTableTemplate( final Gistableview tableView, final URL context )
  {
    m_isApplyTemplate = true;

    clearColumns();
    setTheme( null );

    if( tableView != null )
    {
      final LayerType layer = tableView.getLayer();
      setTheme( new GisTemplateFeatureTheme( layer, context, -1 ) );

      final SortType sort = layer.getSort();
      if( sort != null )
      {
        m_sorter.setPropertyName( sort.getPropertyName() );
        m_sorter.setInverse( sort.isInverse() );
      }

      final List columnList = layer.getColumn();
      for( final Iterator iter = columnList.iterator(); iter.hasNext(); )
      {
        final ColumnType ct = (ColumnType)iter.next();
        addColumn( ct.getName(), ct.getWidth(), ct.isEditable(), false );
      }
    }

    refreshCellEditors();
    refreshColumnProperties();
    refresh();

    m_isApplyTemplate = false;
  }

  public IKalypsoFeatureTheme getTheme()
  {
    return (IKalypsoFeatureTheme)getInput();
  }

  private void setTheme( final IKalypsoFeatureTheme theme )
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

  public void addColumn( final String propertyName, final int width, final boolean isEditable,
      final boolean bRefreshColumns )
  {
    final Table table = getTable();

    final TableColumn tc = new TableColumn( table, SWT.CENTER );
    tc.setData( COLUMN_PROP_NAME, propertyName );
    tc.setData( COLUMN_PROP_EDITABLE, Boolean.valueOf( isEditable ) );
    // die Breite noch mal extra speichern, damit das Redo beim Resizen geht
    tc.setData( COLUMN_PROP_WIDTH, new Integer( width ) );
    tc.setWidth( width );

    setColumnText( tc );

    tc.addSelectionListener( m_headerListener );
    tc.addControlListener( m_headerControlListener );

    if( bRefreshColumns )
    {
      refreshCellEditors();
      refreshColumnProperties();

      refresh();
    }
  }

  protected void setColumnText( final TableColumn tc )
  {
    final String propertyName = (String)tc.getData( COLUMN_PROP_NAME );
    final String sortPropertyName = m_sorter.getPropertyName();

    String text = propertyName;
    if( propertyName.equals( sortPropertyName ) )
      text += " " + ( m_sorter.isInverse() ? "\u00ab" : "\u00bb" );

    tc.setText( text );
  }

  public void removeColumn( final String name )
  {
    final TableColumn column = getColumn( name );
    if( column != null )
      column.dispose();

    refreshCellEditors();
    refreshColumnProperties();
    refresh();
  }

  /**
   * @see org.eclipse.jface.viewers.StructuredViewer#refresh()
   */
  public void refresh()
  {
    if( isDisposed() )
      return;

    // die Namen der Spalten auffrisch, wegen der Sortierungs-Markierung
    final TableColumn[] columns = getTable().getColumns();
    for( int i = 0; i < columns.length; i++ )
      setColumnText( columns[i] );

    super.refresh();

    // und die tableitems einfärben
    final TableItem[] items = getTable().getItems();
    for( int i = 0; i < items.length; i++ )
    {
      final TableItem item = items[i];
      final Feature kf = (Feature)item.getData();
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
    m_modifier = null;
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
    setCellEditors( editors );

    final IKalypsoTheme theme = (IKalypsoTheme)getInput();
    final FeatureType featureType = theme == null ? null : getTheme().getFeatureType();
    if( featureType == null )
      return;

    m_modifier = new IFeatureModifier[columns.length];
    for( int i = 0; i < editors.length; i++ )
    {
      final String propName = columns[i].getData( COLUMN_PROP_NAME ).toString();
      final FeatureTypeProperty ftp = featureType.getProperty( propName );
      if( ftp != null )
      {
        m_modifier[i] = m_featureControlFactory.createFeatureModifier( ftp );
        editors[i] = m_modifier[i].createCellEditor( table );

        editors[i].setValidator( m_modifier[i] );
      }
    }

    setCellEditors( editors );
  }

  /**
   * @see org.eclipse.jface.viewers.TableViewer#getColumnProperties()
   */
  private void refreshColumnProperties()
  {
    final Table table = getTable();
    if( table.isDisposed() )
      return;

    final TableColumn[] columns = table.getColumns();
    final String[] properties = new String[columns.length];

    for( int i = 0; i < properties.length; i++ )
      properties[i] = columns[i].getData( COLUMN_PROP_NAME ).toString();

    setColumnProperties( properties );
  }

  public void selectRow( final Feature feature )
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
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    final List selFeatures = new ArrayList();

    // Feature-Selection auf Selection übertragen
    final FeatureList featureList = getTheme().getFeatureList();
    if( featureList != null )
    {
      for( final Iterator iter = featureList.iterator(); iter.hasNext(); )
      {
        final Feature f = (Feature)iter.next();
        if( f.isSelected( m_selectionID ) )
          selFeatures.add( f );
      }
    }

    if( !isDisposed() )
      getControl().getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          handleModelChanged( modellEvent, selFeatures );
        }
      } );

    fireModellEvent( modellEvent );
  }

  protected void handleModelChanged( final ModellEvent event, final List newSelection )
  {
    if( event != null && event.getEventSource() == getTheme()
        && event.getType() == ModellEvent.THEME_ADDED )
    {
      refreshCellEditors();
      refreshColumnProperties();
    }

    refresh();

    final IStructuredSelection selection = (IStructuredSelection)getSelection();
    if( !newSelection.equals( selection.toList() ) )
      setSelection( new StructuredSelection( newSelection ), false );
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

    ( (GisTemplateFeatureTheme)getTheme() ).fillLayerType( layer, "id", true );

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

    final LayerTableSorter sorter = (LayerTableSorter)getSorter();
    final String propertyName = sorter.getPropertyName();
    if( propertyName != null )
    {
      final SortType sort = m_gistableviewFactory.createGistableviewTypeLayerTypeSortType();
      sort.setPropertyName( propertyName );
      sort.setInverse( sorter.isInverse() );
      layer.setSort( sort );
    }

    return tableTemplate;
  }

  public void saveData( final IProgressMonitor monitor ) throws CoreException
  {
    ( (GisTemplateFeatureTheme)getTheme() ).saveFeatures( monitor );
  }

  public String[][] exportTable( final boolean onlySelected )
  {
    Object[] features;

    if( onlySelected )
    {
      final IStructuredSelection sel = (IStructuredSelection)getSelection();
      features = sel.toArray();
    }
    else
      features = getTheme().getFeatureList().toFeatures();

    final Collection lines = new ArrayList();

    final ITableLabelProvider labelProvider = (ITableLabelProvider)getLabelProvider();

    final Table table = getTable();
    final TableColumn[] columns = table.getColumns();

    final String[] firstLine = new String[columns.length];
    for( int j = 0; j < columns.length; j++ )
      firstLine[j] = (String)columns[j].getData( COLUMN_PROP_NAME );
    lines.add( firstLine );

    for( int i = 0; i < features.length; i++ )
    {
      final String[] line = new String[columns.length];

      for( int j = 0; j < columns.length; j++ )
        line[j] = labelProvider.getColumnText( features[i], j );

      lines.add( line );
    }

    return (String[][])lines.toArray( new String[features.length][] );
  }

  public void addModellListener( ModellEventListener listener )
  {
    m_modellEventProvider.addModellListener( listener );
  }

  public void fireModellEvent( ModellEvent event )
  {
    m_modellEventProvider.fireModellEvent( event );
  }

  public void removeModellListener( ModellEventListener listener )
  {
    m_modellEventProvider.removeModellListener( listener );
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetSelected( SelectionEvent e )
  {
    assignSelectionToFeatures();
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetDefaultSelected( SelectionEvent e )
  {
  // nix tun
  }

  public IFeatureModifier getModifier( final int columnIndex )
  {
    return m_modifier == null ? null : m_modifier[columnIndex];
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#canModify(java.lang.Object,
   *      java.lang.String)
   */
  public boolean canModify( final Object element, final String property )
  {
    return isEditable( property );
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#getValue(java.lang.Object,
   *      java.lang.String)
   */
  public Object getValue( final Object element, final String property )
  {
    final IFeatureModifier modifier = getModifier( property );

    if( modifier != null )
      return modifier.getValue( (Feature)element );

    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#modify(java.lang.Object,
   *      java.lang.String, java.lang.Object)
   */
  public void modify( final Object element, final String property, final Object value )
  {
    final IFeatureModifier modifier = getModifier( property );

    if( modifier != null )
    {
      final TableItem ti = (TableItem)element;
      final Feature feature = (Feature)ti.getData();
      final Object object = modifier.parseInput( feature, value );
      
      final IKalypsoFeatureTheme theme = getTheme();
      
      final FeatureChange fc = new FeatureChange( feature, property, object );
      
      final ICommand command = new ChangeFeaturesCommand( theme.getWorkspace(), new FeatureChange[] { fc } );
      theme.postCommand( command, null );

    }
  }

  public IFeatureModifier getModifier( final String name )
  {
    if( m_modifier != null )
    {
      for( int i = 0; i < m_modifier.length; i++ )
      {
        final IFeatureModifier fm = m_modifier[i];
        final FeatureTypeProperty ftp = fm.getFeatureTypeProperty();
        if( ftp.getName().equals( name ) )
          return fm;
      }
    }

    return null;
  }

}