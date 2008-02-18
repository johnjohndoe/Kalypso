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
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColumnViewerEditor;
import org.eclipse.jface.viewers.FocusCellHighlighter;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerEditor;
import org.eclipse.jface.viewers.TableViewerFocusCellManager;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.commons.command.DefaultCommandManager;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.command.InvisibleCommand;
import org.kalypso.contribs.eclipse.jface.viewers.ColumnViewerEditorActivationStrategy;
import org.kalypso.contribs.eclipse.jface.viewers.FocusCellOwnerDrawHighlighter;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.contribs.eclipse.swt.widgets.TableColumnTooltipListener;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.annotation.AnnotationUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.GisTemplateFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeListener;
import org.kalypso.ogc.gml.KalypsoFeatureThemeSelection;
import org.kalypso.ogc.gml.KalypsoThemeAdapter;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.MapModell;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionListener;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.table.celleditors.IFeatureModifierFactory;
import org.kalypso.ogc.gml.table.command.ChangeSortingCommand;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.template.gistableview.ObjectFactory;
import org.kalypso.template.gistableview.Gistableview.Layer;
import org.kalypso.template.gistableview.Gistableview.Layer.Column;
import org.kalypso.template.gistableview.Gistableview.Layer.Sort;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.util.swt.SWTUtilities;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.feature.event.IGMLWorkspaceModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;
import org.kalypsodeegree.model.feature.event.ModellEventProviderAdapter;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @todo TableCursor soll sich auch bewegen, wenn die Sortierung sich �ndert
 * @author Belger
 */
public class LayerTableViewer extends TableViewer implements ModellEventListener, ModellEventProvider, ICommandTarget, ICellModifier
{
  protected Logger LOGGER = Logger.getLogger( LayerTableViewer.class.getName() );

  public static final String COLUMN_PROP_NAME = "columnName";

  /**
   * Label Property. Feature-Annotation style format string. The context-feature in this case is the paretn feature of
   * the shown list.
   */
  public static final String COLUMN_PROP_LABEL = "columnLabel";

  /**
   * Tooltip Property. Feature-Annotation style format string. The context-feature in this case is the paretn feature of
   * the shown list.
   */
  public static final String COLUMN_PROP_TOOLTIP = "columnTooltip";

  public static final String COLUMN_PROP_EDITABLE = "columnEditable";

  public static final String COLUMN_PROP_WIDTH = "columnWidth";

  public static final String COLUMN_PROP_FORMAT = "columnFormat";

  private final IFeatureModifierFactory m_featureControlFactory;

  private final ICommandTarget m_commandTarget = new JobExclusiveCommandTarget( new DefaultCommandManager(), null );

  private final ModellEventProviderAdapter m_modellEventProvider = new ModellEventProviderAdapter();

  private IFeatureModifier[] m_modifier;

  private final LayerTableSorter m_sorter = new LayerTableSorter();

  protected final ICommandTarget m_templateTarget;

  protected boolean m_isApplyTemplate = false;

  private final IFeatureSelectionListener m_globalSelectionListener = new IFeatureSelectionListener()
  {
    public void selectionChanged( final IFeatureSelection selection )
    {
      final Feature[] features = FeatureSelectionHelper.getFeatures( selection );
      final List<Feature> globalFeatureList = new ArrayList<Feature>( Arrays.asList( features ) );

      // filter ths which are in my list
      final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) getInput();
      if( theme == null )
        return;
      final FeatureList featureList = theme.getFeatureList();
      final List themeFeatures = featureList == null ? new ArrayList() : (List) featureList;
      globalFeatureList.retainAll( themeFeatures );
      final Feature[] globalFeatures = globalFeatureList.toArray( new Feature[globalFeatureList.size()] );

      final Control control = getControl();
      if( control.isDisposed() )
        return;

      control.getDisplay().syncExec( new Runnable()
      {
        public void run( )
        {
          if( !getTable().isDisposed() )
          {
            final ISelection tableSelection = getSelection();
            if( tableSelection instanceof IFeatureSelection )
            {
              final IFeatureSelection currentSelection = (IFeatureSelection) tableSelection;
              final Feature[] currentFeatures = FeatureSelectionHelper.getFeatures( currentSelection );
              if( !org.kalypso.contribs.java.util.Arrays.equalsUnordered( globalFeatures, currentFeatures ) )
              {
                // setting table selection to:
                setSelection( selection );
              }
            }
          }
        }
      } );
    }
  };

  /**
   * This class handles selections of the column headers. Selection of the column header will cause resorting of the
   * shown tasks using that column's sorter. Repeated selection of the header will toggle sorting order (ascending
   * versus descending).
   */
  private final SelectionListener m_headerListener = new SelectionAdapter()
  {
    /**
     * Handles the case of user selecting the header area.
     * <p>
     * If the column has not been selected previously, it will set the sorter of that column to be the current tasklist
     * sorter. Repeated presses on the same column header will toggle sorting order (ascending/descending/original).
     */
    @Override
    public void widgetSelected( final SelectionEvent e )
    {
      // column selected - need to sort
      final TableColumn tableColumn = (TableColumn) e.widget;

      m_templateTarget.postCommand( new ChangeSortingCommand( LayerTableViewer.this, tableColumn ), null );
    }
  };

  private final ControlListener m_headerControlListener = new ControlAdapter()
  {
    /**
     * @see org.eclipse.swt.events.ControlAdapter#controlResized(org.eclipse.swt.events.ControlEvent)
     */
    @Override
    public void controlResized( final ControlEvent e )
    {
      if( m_isApplyTemplate == true )
        return;

      final TableColumn tc = (TableColumn) e.widget;

      // kann nicht r�ckg�ngig gemacht werden, sorgt aber daf�r, dass der Editor
      // dirty ist
      final int width = tc.getWidth();
      if( width != ((Integer) tc.getData( COLUMN_PROP_WIDTH )).intValue() )
      {
        m_templateTarget.postCommand( new InvisibleCommand(), null );
        // removeListener and again add listener may reduce some flickering
        // effects ?? (doemming)
        tc.removeControlListener( this );
        tc.setData( COLUMN_PROP_WIDTH, new Integer( width ) );
        tc.addControlListener( this );
      }
    }
  };

  private final IKalypsoThemeListener m_themeListener = new KalypsoThemeAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.KalypsoThemeAdapter#statusChanged(org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void statusChanged( final IKalypsoTheme source )
    {
      handleStatusChanged( source );
    }
  };

  private final IFeatureSelectionManager m_selectionManager;

  private final IFeatureChangeListener m_fcl;

  private final TableViewerFocusCellManager m_focusCellManager;

  /**
   * @param parent
   * @param templateTarget
   * @param featureControlFactory
   */
  public LayerTableViewer( final Composite parent, final int style, final ICommandTarget templateTarget, final IFeatureModifierFactory featureControlFactory, final IFeatureSelectionManager selectionManager, final IFeatureChangeListener fcl )
  {
    super( parent, style | SWT.MULTI | SWT.FULL_SELECTION );

    m_featureControlFactory = featureControlFactory;
    m_templateTarget = templateTarget;
    m_selectionManager = selectionManager;
    m_fcl = fcl;
    if( m_selectionManager != null )
      m_selectionManager.addSelectionListener( m_globalSelectionListener );

    setContentProvider( new LayerTableContentProvider( selectionManager ) );
    setLabelProvider( new LayerTableLabelProvider( this ) );
    setCellModifier( this );
    setSorter( m_sorter );

    // init table
    final Table table = getTable();
    table.setHeaderVisible( true );
    table.setLinesVisible( true );
    // disable capture to let selection of table and tableviewer in sync
    table.setCapture( false );

    // Override the default TableViewerEditor:
    // - multi-selection is enabled
    // - there is a focused cell
    // - right-click opens context-menu everywhere
    // - single-click starts editing
    final FocusCellHighlighter focusHighlighter = new FocusCellOwnerDrawHighlighter( this );
    m_focusCellManager = new TableViewerFocusCellManager( this, focusHighlighter );
    final ColumnViewerEditorActivationStrategy editorActivationStrategy = new ColumnViewerEditorActivationStrategy( this );
    TableViewerEditor.create( this, m_focusCellManager, editorActivationStrategy, ColumnViewerEditor.KEYBOARD_ACTIVATION | ColumnViewerEditor.TABBING_HORIZONTAL
        | ColumnViewerEditor.TABBING_MOVE_TO_ROW_NEIGHBOR );
  }

  public void dispose( )
  {
    applyTableTemplate( null, null );

    if( m_selectionManager != null )
      m_selectionManager.removeSelectionListener( m_globalSelectionListener );
  }

  public void applyTableTemplate( final Gistableview tableView, final URL context, @SuppressWarnings("unused")
  final boolean dummy )
  {
    m_isApplyTemplate = true;

    if( tableView != null )
    {
      final Layer layer = tableView.getLayer();
      if( layer.getHref() != null )
      {
        // Only dispose theme if we really replace it
        // TODO: check this: sometimes we get a theme from outside... what to do in that case?
        disposeTheme( getInput() );

        final MapModell pseudoModell = new MapModell( KalypsoCorePlugin.getDefault().getCoordinatesSystem(), null );

        final GisTemplateFeatureTheme theme = new GisTemplateFeatureTheme( layer, context, m_selectionManager, pseudoModell, null, true );
        setInput( theme );
      }
      final Sort sort = layer.getSort();
      final List<Column> columnList = layer.getColumn();
      setSortAndColumns( sort, columnList );
    }

    refreshAll();
    m_isApplyTemplate = false;
  }

  public void applyTableTemplate( final Gistableview tableView, final URL context )
  {
    m_isApplyTemplate = true;
    clearColumns();

    if( getContentProvider() != null )
      setInput( null );

    if( tableView != null )
    {
      // Only dispose theme if we really replace it
      disposeTheme( getInput() );

      final Layer layer = tableView.getLayer();

      final MapModell pseudoModell = new MapModell( KalypsoCorePlugin.getDefault().getCoordinatesSystem(), null );

      final GisTemplateFeatureTheme theme = new GisTemplateFeatureTheme( layer, context, m_selectionManager, pseudoModell, null, true );
      setInput( theme );

      final Sort sort = layer.getSort();
      final List<Column> columnList = layer.getColumn();
      setSortAndColumns( sort, columnList );
    }

    refreshAll();
    checkColumns();
    m_isApplyTemplate = false;
  }

  private void setSortAndColumns( final Sort sort, final List<Column> columnList )
  {
    if( sort != null )
    {
      m_sorter.setPropertyName( sort.getPropertyName() );
      m_sorter.setInverse( sort.isInverse() );
    }

    for( final Column ct : columnList )
      addColumn( ct.getName(), ct.getLabel(), ct.getTooltip(), ct.isEditable(), ct.getWidth(), ct.getAlignment(), ct.getFormat(), false );
  }

  public IKalypsoFeatureTheme getTheme( )
  {
    return (IKalypsoFeatureTheme) getInput();
  }

  public void clearColumns( )
  {
    final Table table = getTable();
    if( table.isDisposed() )
      return;

    final TableColumn[] columns = table.getColumns();
    for( final TableColumn element : columns )
      element.dispose();
  }

  public void addColumn( final String propertyName, final String label, final String tooltip, final boolean isEditable, final int width, final String alignment, final String format, final boolean bRefreshColumns )
  {
    final Table table = getTable();

    final int alignmentInt = SWTUtilities.createStyleFromString( alignment );
    final TableColumn tc = new TableColumn( table, alignmentInt );
    tc.setAlignment( alignmentInt );

    tc.setData( COLUMN_PROP_NAME, propertyName );
    tc.setData( COLUMN_PROP_LABEL, label );
    tc.setData( COLUMN_PROP_TOOLTIP, tooltip );
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
      refreshAll();
    }
  }

  protected void setColumnText( final TableColumn tc )
  {
// System.out.println("");
    final String propertyName = (String) tc.getData( COLUMN_PROP_NAME );

    final String label = (String) tc.getData( COLUMN_PROP_LABEL );
    final String tooltip = (String) tc.getData( COLUMN_PROP_TOOLTIP );

    final String sortPropertyName = m_sorter.getPropertyName();

    final String[] textAndTooltip = getLabelAndTooltip( label, tooltip, propertyName );

    final String text;
    if( propertyName.equals( sortPropertyName ) )
      text = textAndTooltip[0] + " " + (m_sorter.isInverse() ? "\u00ab" : "\u00bb");
    else
      text = textAndTooltip[0];

    final String tooltipText = textAndTooltip[1];

    tc.setText( text );
    tc.setData( TableColumnTooltipListener.TOOLTIP_PROPERTY, tooltipText );
  }

  private String[] getLabelAndTooltip( final String label, final String tooltip, final String propertyName )
  {
    final String[] result = new String[2];

    result[0] = propertyName; // prepare for exception

    try
    {
      final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) getInput();

      final IFeatureType featureType = theme.getFeatureType();

      if( featureType != null )
      {
        final IPropertyType property = featureType.getProperty( propertyName );

        final IAnnotation annotation = AnnotationUtilities.getAnnotation( property );
        result[0] = annotation.getLabel();
        result[1] = annotation.getTooltip();
      }

      if( label != null )
        result[0] = label;

      if( tooltip != null )
        result[1] = tooltip;
    }
    catch( final Exception e )
    {
      // if data is not loaded yet, we provide the propertyname
      e.printStackTrace();

      result[1] = e.toString();
    }

    return result;
  }

  private void checkColumns( )
  {
    final Object input = getInput();
    if( input == null || !(input instanceof GisTemplateFeatureTheme) )
      return;
    final IFeatureType featureType = ((GisTemplateFeatureTheme) input).getFeatureType();
    final Table table = getTable();

    if( featureType == null || table == null || table.isDisposed() )
      return;

    final TableColumn[] columns = table.getColumns();
    boolean changed = false;

    for( final TableColumn column : columns )
    {
      if( column != null )
      {
        final String propName = column.getData( COLUMN_PROP_NAME ).toString();
        if( featureType.getProperty( propName ) == null )
        {
          column.dispose();
          changed = true;
        }
      }
    }

    if( changed )
      refreshAll();
  }

  public void refreshAll( )
  {
    refreshCellEditors();
    refreshColumnProperties();
    refresh();
  }

  public void removeColumn( final String name )
  {
    final TableColumn column = getColumn( name );
    if( column != null )
      column.dispose();

    refreshAll();
  }

  /**
   * @see org.eclipse.jface.viewers.StructuredViewer#refresh()
   */
  @Override
  public void refresh( )
  {
    if( isDisposed() )
      return;
    checkColumns();
    // die Namen der Spalten auffrischen, wegen der Sortierungs-Markierung
    final TableColumn[] columns = getTable().getColumns();
    for( final TableColumn element : columns )
      setColumnText( element );
    super.refresh();
  }

  /**
   * @see org.eclipse.jface.viewers.TableViewer#getCellEditors()
   */
  private void refreshCellEditors( )
  {
    m_modifier = null;
    // dispose old modifiers
    final CellEditor[] oldEditors = getCellEditors();
    if( oldEditors != null )
    {
      for( final CellEditor element : oldEditors )
      {
        if( element != null )
          element.dispose();
      }
    }

    final Table table = getTable();
    if( table.isDisposed() )
      return;

    final TableColumn[] columns = table.getColumns();
    final CellEditor[] editors = new CellEditor[columns.length];
    setCellEditors( editors );

    final IKalypsoTheme theme = (IKalypsoTheme) getInput();
    final IFeatureType featureType = theme == null ? null : getTheme().getFeatureType();
    if( featureType == null )
      return;
    // set new modifiers, new celleditors and new cellvalidators
    m_modifier = new IFeatureModifier[columns.length];
    for( int i = 0; i < editors.length; i++ )
    {
      final String propName = columns[i].getData( COLUMN_PROP_NAME ).toString();
      final String format = (String) columns[i].getData( COLUMN_PROP_FORMAT );
      final IPropertyType ftp = featureType.getProperty( propName );
      if( ftp != null )
      {
        m_modifier[i] = m_featureControlFactory.createFeatureModifier( ftp, format, m_selectionManager, m_fcl );
        editors[i] = m_modifier[i].createCellEditor( table );
        editors[i].setValidator( m_modifier[i] );
      }
    }
    setCellEditors( editors );
  }

  /**
   *
   */
  private void refreshColumnProperties( )
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
    if( getTheme() == null )
      return;
    if( (modellEvent instanceof IGMLWorkspaceModellEvent && ((IGMLWorkspaceModellEvent) modellEvent).getGMLWorkspace() == getTheme().getWorkspace()) )
    {
      if( !isDisposed() )
        getControl().getDisplay().asyncExec( new Runnable()
        {
          public void run( )
          {
            handleModelChanged( modellEvent );
          }
        } );
    }
    else
    {
      if( !isDisposed() )
      {
        getControl().getDisplay().asyncExec( new Runnable()
        {
          public void run( )
          {
            refresh();
          }
        } );
      }
    }

    fireModellEvent( modellEvent );
  }

  protected void handleModelChanged( final ModellEvent event )
  {
    final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) getInput();
    if( theme == null )
      return;
    if( !(event instanceof IGMLWorkspaceModellEvent) )
      return;
    if( ((IGMLWorkspaceModellEvent) event).getGMLWorkspace() != theme.getWorkspace() )
      return;

    if( event instanceof FeaturesChangedModellEvent )
    {
      final Feature[] features = ((FeaturesChangedModellEvent) event).getFeatures();
      update( features, null );
    }
    if( event instanceof FeatureStructureChangeModellEvent )
    {
      final Feature[] features = ((FeatureStructureChangeModellEvent) event).getParentFeatures();
      for( final Feature feature : features )
      {
        if( feature == theme.getFeatureList().getParentFeature() )
        {
          refresh();
          break;
        }
      }
    }
  }

  public boolean isDisposed( )
  {
    return getTable().isDisposed();
  }

  /**
   * @see org.kalypso.commons.command.ICommandTarget#postCommand(org.kalypso.commons.command.ICommand,
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
    return (String) column.getData( COLUMN_PROP_FORMAT );
  }

  public boolean isEditable( final String property )
  {
    final TableColumn column = getColumn( property );
    return column == null ? false : ((Boolean) column.getData( COLUMN_PROP_EDITABLE )).booleanValue();
  }

  private TableColumn getColumn( final String property )
  {
    final TableColumn[] columns = getTable().getColumns();
    for( final TableColumn element : columns )
    {
      final String name = element.getData( COLUMN_PROP_NAME ).toString();
      if( property.equals( name ) )
        return element;
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

  public int getColumnCount( )
  {
    return getTable().getColumnCount();
  }

  public Gistableview createTableTemplate( )
  {
    final ObjectFactory m_gistableviewFactory = new ObjectFactory();
    final Gistableview tableTemplate = m_gistableviewFactory.createGistableview();
    final Layer layer = m_gistableviewFactory.createGistableviewLayer();

    ((GisTemplateFeatureTheme) getTheme()).fillLayerType( layer, "id", true );

    tableTemplate.setLayer( layer );

    final List<Column> columns = layer.getColumn();

    final TableColumn[] tableColumns = getTable().getColumns();
    for( final TableColumn tc : tableColumns )
    {
      final Column columnType = m_gistableviewFactory.createGistableviewLayerColumn();

      columnType.setName( tc.getData( COLUMN_PROP_NAME ).toString() );
      columnType.setLabel( (String) tc.getData( COLUMN_PROP_LABEL ) );
      columnType.setTooltip( (String) tc.getData( COLUMN_PROP_TOOLTIP ) );
      columnType.setEditable( ((Boolean) tc.getData( COLUMN_PROP_EDITABLE )).booleanValue() );
      columnType.setWidth( tc.getWidth() );
      columnType.setAlignment( "" + tc.getStyle() );
      columnType.setFormat( (String) tc.getData( COLUMN_PROP_FORMAT ) );

      columns.add( columnType );
    }

    final LayerTableSorter sorter = (LayerTableSorter) getSorter();
    final String propertyName = sorter.getPropertyName();
    if( propertyName != null )
    {
      final Sort sort = m_gistableviewFactory.createGistableviewLayerSort();
      sort.setPropertyName( propertyName );
      sort.setInverse( sorter.isInverse() );
      layer.setSort( sort );
    }

    return tableTemplate;
  }

  public void saveData( final IProgressMonitor monitor ) throws CoreException
  {
    // TODO inserted this test against null because got a NullPointerException, ok?
    final GisTemplateFeatureTheme theme = (GisTemplateFeatureTheme) getTheme();
    if( theme != null )
      theme.saveFeatures( monitor );
  }

  public String[][] exportTable( final boolean onlySelected )
  {
    Object[] features;

    if( onlySelected )
    {
      final IStructuredSelection sel = (IStructuredSelection) getSelection();
      features = sel.toArray();
    }
    else
      features = getTheme().getFeatureList().toFeatures();

    final Collection<String[]> lines = new ArrayList<String[]>();

    final ITableLabelProvider labelProvider = (ITableLabelProvider) getLabelProvider();

    final Table table = getTable();
    final TableColumn[] columns = table.getColumns();

    // TODO: exports the property name, not the current label; change this
    final String[] firstLine = new String[columns.length];
    for( int j = 0; j < columns.length; j++ )
      firstLine[j] = (String) columns[j].getData( COLUMN_PROP_NAME );
    lines.add( firstLine );

    for( final Object element : features )
    {
      final String[] line = new String[columns.length];

      for( int j = 0; j < columns.length; j++ )
        line[j] = labelProvider.getColumnText( element, j );

      lines.add( line );
    }

    return lines.toArray( new String[features.length][] );
  }

  public void addModellListener( final ModellEventListener listener )
  {
    m_modellEventProvider.addModellListener( listener );
  }

  public void fireModellEvent( final ModellEvent event )
  {
    m_modellEventProvider.fireModellEvent( event );
  }

  public void removeModellListener( final ModellEventListener listener )
  {
    m_modellEventProvider.removeModellListener( listener );
  }

  public IFeatureModifier getModifier( final int columnIndex )
  {
    return m_modifier == null ? null : m_modifier[columnIndex];
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#canModify(java.lang.Object, java.lang.String)
   */
  public boolean canModify( final Object element, final String property )
  {
    // TODO ask modifier also, as for some types editor may not be implemented
    return isEditable( property );
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#getValue(java.lang.Object, java.lang.String)
   */
  public Object getValue( final Object element, final String property )
  {
    final IFeatureModifier modifier = getModifier( property );

    if( modifier != null )
      return modifier.getValue( (Feature) element );

    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#modify(java.lang.Object, java.lang.String, java.lang.Object)
   */
  public void modify( final Object element, final String property, final Object value )
  {
    final IFeatureModifier modifier = getModifier( property );

    if( modifier != null )
    {
      final TableItem ti = (TableItem) element;
      final Feature feature = (Feature) ti.getData();
      // as result==null does not explicitly mean that
      // the value is invalid, we have to ask the celleditor for invalidity
      final int columnID = getColumnID( property );
      if( !getCellEditors()[columnID].isValueValid() )
        return;

      final Object object = modifier.parseInput( feature, value );
      final Object oldValue = modifier.getValue( feature );
      if( oldValue != null && oldValue.equals( value ) )
        return;

      // dialogs may return FeatureChange objects (doemming)
      final FeatureChange fc;
      final IKalypsoFeatureTheme theme = getTheme();
      if( object instanceof FeatureChange )
        fc = (FeatureChange) object;
      else
      {
        final IPropertyType pt = FeatureHelper.getPT( feature, property );
        fc = new FeatureChange( feature, pt, object );
      }
      final ICommand command = new ChangeFeaturesCommand( theme.getWorkspace(), new FeatureChange[] { fc } );
      theme.postCommand( command, new Runnable()
      {
        public void run( )
        {
          ViewerUtilities.refresh( LayerTableViewer.this, true );
        }
      } );
    }
  }

  public IFeatureModifier getModifier( final String name )
  {
    if( m_modifier != null )
    {
      for( final IFeatureModifier fm : m_modifier )
      {
        if( fm != null )
        {
          final IPropertyType ftp = fm.getFeatureTypeProperty();
          if( ftp.getName().equals( name ) )
            return fm;
        }
      }
    }
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.StructuredViewer#getSelection()
   */
  @Override
  public ISelection getSelection( )
  {
    final IKalypsoFeatureTheme theme = getTheme();
    final IStructuredSelection selection = (IStructuredSelection) super.getSelection();
    if( theme == null )
      return selection;

    final ViewerCell focusCell = m_focusCellManager.getFocusCell();

    final Feature focusedFeature = focusCell == null ? null : (Feature) focusCell.getElement();
    final int column = focusCell == null ? -1 : focusCell.getColumnIndex();

    final IPropertyType focusedProperty = (column < 0 || m_modifier == null || column > m_modifier.length - 1) ? null : m_modifier[column].getFeatureTypeProperty();

    return new KalypsoFeatureThemeSelection( selection.toList(), theme, m_selectionManager, focusedFeature, focusedProperty );
  }

  @Override
  protected void inputChanged( final Object input, final Object oldInput )
  {
    if( !isDisposed() )
      super.inputChanged( input, oldInput );

    disposeTheme( oldInput );

    clearColumns();

    setTheme( input );
  }

  private void setTheme( final Object input )
  {
    final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) input;
    if( theme != null )
    {
      theme.addKalypsoThemeListener( m_themeListener );

      final CommandableWorkspace workspace = theme.getWorkspace();
      if( workspace != null )
        workspace.addModellListener( this );
    }
  }

  /*
   * HACK: we let the content provider dispose the theme, because when our dispose method is called nowadays, the input
   * is already set to null. <p>TODO: move everything into the content provider.
   */
  public void disposeTheme( final Object oldInput )
  {
    final IKalypsoFeatureTheme oldTheme = (IKalypsoFeatureTheme) oldInput;

    if( oldTheme != null )
    {
      oldTheme.removeKalypsoThemeListener( m_themeListener );

      final CommandableWorkspace workspace = oldTheme.getWorkspace();
      if( workspace != null )
        workspace.removeModellListener( this );
      oldTheme.dispose();
    }
  }

  /** Registers this MenuManager es context menu on table and table cursor */
  public void setMenu( final MenuManager menuManager )
  {
    final Table table = getTable();
    final Menu tablemenu = menuManager.createContextMenu( table );
    table.setMenu( tablemenu );
  }

  protected void handleStatusChanged( final IKalypsoTheme source )
  {
    if( source instanceof IKalypsoFeatureTheme )
    {
      final CommandableWorkspace workspace = ((IKalypsoFeatureTheme) source).getWorkspace();
      if( workspace != null )
        workspace.addModellListener( this );
    }
  }
}