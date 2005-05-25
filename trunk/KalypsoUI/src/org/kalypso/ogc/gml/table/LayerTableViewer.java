/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.table;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.bind.JAXBException;

import org.bce.eclipse.swt.widgets.TableColumnTooltipListener;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableCursor;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
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
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.table.celleditors.IFeatureModifierFactory;
import org.kalypso.ogc.gml.table.command.ChangeSortingCommand;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.template.gistableview.ObjectFactory;
import org.kalypso.template.gistableview.GistableviewType.LayerType;
import org.kalypso.template.gistableview.GistableviewType.LayerType.ColumnType;
import org.kalypso.template.gistableview.GistableviewType.LayerType.SortType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.actions.CommandableFeatureSelection;
import org.kalypso.ui.preferences.IKalypsoPreferences;
import org.kalypso.util.command.DefaultCommandManager;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;
import org.kalypso.util.command.InvisibleCommand;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.util.swt.SWTUtilities;
import org.kalypsodeegree.model.feature.Annotation;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.feature.event.IGMLWorkspaceModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;
import org.kalypsodeegree.model.feature.event.ModellEventProviderAdapter;

/**
 * @todo TableCursor soll sich auch bewegen, wenn die Sortierung sich �ndert
 * 
 * @author Belger
 */
public class LayerTableViewer extends TableViewer implements ModellEventListener, ICommandTarget, ModellEventProvider, ICellModifier
{
  protected Logger LOGGER = Logger.getLogger( LayerTableViewer.class.getName() );

  public static final String COLUMN_PROP_NAME = "columnName";

  public static final String COLUMN_PROP_EDITABLE = "columnEditable";

  public static final String COLUMN_PROP_WIDTH = "columnWidth";

  public static final String COLUMN_PROP_FORMAT = "columnFormat";

  private final ObjectFactory m_gistableviewFactory = new ObjectFactory();

  private final IFeatureModifierFactory m_featureControlFactory;

  private ICommandTarget m_commandTarget = new JobExclusiveCommandTarget( new DefaultCommandManager(), null );

  private ModellEventProvider m_modellEventProvider = new ModellEventProviderAdapter();

  private final int m_selectionID;

  //  final Color m_selectColor;

  //  final Color m_unselectColor;

  final TableCursor m_tableCursor;

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
     * same column header will toggle sorting order
     * (ascending/descending/original).
     */
    public void widgetSelected( final SelectionEvent e )
    {
      // column selected - need to sort
      final TableColumn tableColumn = (TableColumn)e.widget;

      m_templateTarget.postCommand( new ChangeSortingCommand( LayerTableViewer.this, tableColumn ), null );
    }
  };

  ControlListener m_headerControlListener = new ControlAdapter()
  {

    /**
     * @see org.eclipse.swt.events.ControlAdapter#controlResized(org.eclipse.swt.events.ControlEvent)
     */
    public void controlResized( final ControlEvent e )
    {
      if( m_isApplyTemplate == true )
        return;

      final TableColumn tc = (TableColumn)e.widget;

      // kann nicht r�ckg�ngig gemacht werden, sorgt aber daf�r, dass der Editor
      // dirty ist
      final int width = tc.getWidth();
      if( width != ( (Integer)tc.getData( COLUMN_PROP_WIDTH ) ).intValue() )
      {
        m_templateTarget.postCommand( new InvisibleCommand(), null );
        // removeListener and again add listener may reduce some flickering
        // effects ?? (doemming)
        tc.removeControlListener( m_headerControlListener );
        tc.setData( COLUMN_PROP_WIDTH, new Integer( width ) );
        tc.addControlListener( m_headerControlListener );
      }
    }
  };

  /**
   * @param parent
   * @param templateTarget
   * @param featureControlFactory
   * @param selectionID
   * @param bCursorSelects falls true, wird immer die unter dem Cursor liegende
   *          Zeile selektiert
   */
  public LayerTableViewer( final Composite parent, final int style, final ICommandTarget templateTarget,
      final IFeatureModifierFactory featureControlFactory, final int selectionID, final boolean bCursorSelects )
  {
    super( parent, style | SWT.MULTI | SWT.FULL_SELECTION );

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
    // disable capture to let selection of table and tableviewer in sync
    table.setCapture( false );

    final TableCursor tc = new ExcelLikeTableCursor( this, SWT.NONE );
    //    final TableCursor tc = new TableCursor(getTable(),SWT.NONE);
    m_tableCursor = tc;
    addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( SelectionChangedEvent event )
      {
        // TODO what to do with the selection ?
        //        IStructuredSelection selection =
        // (IStructuredSelection)event.getSelection();
        //        System.out.println( "LayerTableViewer.selectionChanged: " +
        // selection.toList().size() + " elements have changed" );
        //        System.out.println( " LayerTableViewer.getSelection() " + (
        // (IStructuredSelection)getSelection() ).toList().size() + " features"
        // );
        //        System.out.println( " LayerTableViewer.getTable.getSelection() " +
        // getTable().getSelection().length + " items" );
      }
    } );
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
    // ??
    //    control.addDisposeListener( new DisposeListener()
    //    {
    //      public void widgetDisposed( DisposeEvent event )
    //      {
    //        handleDispose( event );
    //      }
    //    } );
    super.hookControl( control );
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
      setTheme( new GisTemplateFeatureTheme( layer, context ) );

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
        addColumn( ct.getName(), ct.isEditable(), ct.getWidth(), ct.getAlignment(), ct.getFormat(), false );
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

  public void addColumn( final String propertyName, final boolean isEditable, final int width, final String alignment, String format,
      final boolean bRefreshColumns )
  {
    final Table table = getTable();

    final int alignmentInt = SWTUtilities.createStyleFromString( alignment );
    final TableColumn tc = new TableColumn( table, alignmentInt );
    tc.setAlignment( alignmentInt );
    tc.setData( COLUMN_PROP_NAME, propertyName );
    tc.setData( COLUMN_PROP_EDITABLE, Boolean.valueOf( isEditable ) );
    // die Breite noch mal extra speichern, damit das Redo beim Resizen geht
    tc.setData( COLUMN_PROP_WIDTH, new Integer( width ) );
    tc.setData( COLUMN_PROP_FORMAT, format );
    tc.setWidth( width );

    setColumnText( tc );

    TableColumnTooltipListener.hookControl( tc );

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
    String text;
    String tooltip;
    try
    {
      final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme)getInput();
      FeatureType featureType = theme.getFeatureType();
      FeatureTypeProperty property = featureType.getProperty( propertyName );

      final String lang = KalypsoGisPlugin.getDefault().getPluginPreferences().getString( IKalypsoPreferences.LANGUAGE );
      final Annotation annotation = property.getAnnotation( lang );
      text = annotation.getLabel();
      tooltip = annotation.getTooltip();
    }
    catch( Exception e )
    {
      // if data is not loaded yet, we provide the propertyname
      text = propertyName;
      tooltip = null;
    }
    if( propertyName.equals( sortPropertyName ) )
      text += " " + ( m_sorter.isInverse() ? "\u00ab" : "\u00bb" );

    tc.setText( text );
    tc.setData( TableColumnTooltipListener.TOOLTIP_PROPERTY, tooltip );
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
    // die Namen der Spalten auffrischen, wegen der Sortierungs-Markierung
    final TableColumn[] columns = getTable().getColumns();
    for( int i = 0; i < columns.length; i++ )
      setColumnText( columns[i] );
    super.refresh();
  }

  /**
   * @see org.eclipse.jface.viewers.TableViewer#getCellEditors()
   */
  private void refreshCellEditors()
  {
    m_modifier = null;
    // dispose old modifiers
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
    // set new modifiers, new celleditors and new cellvalidators
    m_modifier = new IFeatureModifier[columns.length];
    for( int i = 0; i < editors.length; i++ )
    {
      final String propName = columns[i].getData( COLUMN_PROP_NAME ).toString();
      final String format = (String)columns[i].getData( COLUMN_PROP_FORMAT );
      final FeatureTypeProperty ftp = featureType.getProperty( propName );
      if( ftp != null )
      {
        m_modifier[i] = m_featureControlFactory.createFeatureModifier( ftp, format );
        editors[i] = m_modifier[i].createCellEditor( table );
        editors[i].setValidator( m_modifier[i] );
      }
    }
    setCellEditors( editors );
  }

  /**
   *  
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

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if(getTheme()==null)
      return;
    // TODO hanlde here the highlighting stuff
    // Feature-Selection auf Selection �bertragen
    if( ( modellEvent instanceof IGMLWorkspaceModellEvent && ( (IGMLWorkspaceModellEvent)modellEvent ).getGMLWorkspace() == getTheme().getWorkspace() )
        ||

        ( modellEvent != null && modellEvent.getEventSource() == getTheme() && modellEvent.isType( ModellEvent.THEME_ADDED ) ) )
    {
      if( !isDisposed() )
        getControl().getDisplay().asyncExec( new Runnable()
        {
          public void run()
          {
            handleModelChanged( modellEvent );
          }
        } );
    }
    fireModellEvent( modellEvent );
  }

  protected void handleModelChanged( final ModellEvent event )
  {
    // TODO theme add
    if( event != null && event.getEventSource() == getTheme() && event.isType( ModellEvent.THEME_ADDED ) )
    {
      refreshCellEditors();
      refreshColumnProperties();
      refresh();
    }
    final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme)getInput();
    if( theme == null )
      return;
    if( !( event instanceof IGMLWorkspaceModellEvent ) )
      return;
    if( ( (IGMLWorkspaceModellEvent)event ).getGMLWorkspace() != theme.getWorkspace() )
      return;

    if( event instanceof FeaturesChangedModellEvent )
    {
      List features = ( (FeaturesChangedModellEvent)event ).getFeatures();
      update( features.toArray(), null );
      if( m_tableCursor != null && !m_tableCursor.isDisposed() )
      {
        m_tableCursor.update();
        m_tableCursor.redraw();
      }
    }
    if( event instanceof FeatureStructureChangeModellEvent )
    {
      if( ( (FeatureStructureChangeModellEvent)event ).getParentFeature() == theme.getFeatureList().getParentFeature() )
        refresh();
    }
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

  public String getColumnAlignment( final int columnIndex )
  {
    if( columnIndex == -1 )
      return "SWT.LEAD";

    final TableColumn column = getTable().getColumn( columnIndex );
    return "" + column.getStyle();
  }

  public String getColumnFormat( final int columnIndex )
  {
    if( columnIndex == -1 )
      return null;

    final TableColumn column = getTable().getColumn( columnIndex );
    return (String)column.getData( COLUMN_PROP_FORMAT );
  }

  public boolean isEditable( final String property )
  {
    final TableColumn column = getColumn( property );
    return column == null ? false : ( (Boolean)column.getData( COLUMN_PROP_EDITABLE ) ).booleanValue();
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

  public int getColumnID( final String property )
  {
    final TableColumn[] columns = getTable().getColumns();
    for( int i = 0; i < columns.length; i++ )
    {
      final String name = columns[i].getData( COLUMN_PROP_NAME ).toString();
      if( property.equals( name ) )
        return i;
    }

    return -1;
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

      final ColumnType columnType = m_gistableviewFactory.createGistableviewTypeLayerTypeColumnType();

      columnType.setName( tc.getData( COLUMN_PROP_NAME ).toString() );
      columnType.setEditable( ( (Boolean)tc.getData( COLUMN_PROP_EDITABLE ) ).booleanValue() );
      columnType.setWidth( tc.getWidth() );
      columnType.setAlignment( "" + tc.getStyle() );
      columnType.setFormat( (String)tc.getData( COLUMN_PROP_FORMAT ) );

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
      // as result==null does not explicitly mean that
      // the value is invalid, we have to ask the celleditor for invalidity
      int columnID = getColumnID( property );
      if( !getCellEditors()[columnID].isValueValid() )
        return;

      final Object object = modifier.parseInput( feature, value );

      final IKalypsoFeatureTheme theme = getTheme();

      final FeatureChange fc = new FeatureChange( feature, property, object );

      final ICommand command = new ChangeFeaturesCommand( theme.getWorkspace(), new FeatureChange[]
      { fc } );
      theme.postCommand( command, new Runnable()
      {
        public void run()
        {
          refresh();
        }
      } );
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

  /**
   * @see org.eclipse.jface.viewers.StructuredViewer#getSelection()
   */
  public ISelection getSelection()
  {
    final IStructuredSelection selection = (IStructuredSelection)super.getSelection();
    final IKalypsoFeatureTheme theme = getTheme();
    if( theme == null )
      return selection;
    final CommandableWorkspace workspace = theme.getWorkspace();
    if( workspace == null )
      return selection;
    return new CommandableFeatureSelection( theme, selection );
  }
}