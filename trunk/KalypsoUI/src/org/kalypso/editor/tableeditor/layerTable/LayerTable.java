package org.kalypso.editor.tableeditor.layerTable;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.util.command.ICommandManager;

/**
 * @author bce
 */
public class LayerTable implements ILayerTableModelListener
{
  protected final TableViewer m_viewer;

  private LayerTableModel m_model = null;

  private final ICommandManager m_commandManager;

  public LayerTable( final Composite parent, final ICommandManager commandManager )
  {
    m_commandManager = commandManager;

    m_viewer = new TableViewer( parent, SWT.MULTI | SWT.FULL_SELECTION );

    m_viewer.setContentProvider( new LayerTableContentProvider() );
    m_viewer.setLabelProvider( new LayerTableLabelProvider() );

    m_viewer.getTable().setHeaderVisible( true );
    m_viewer.getTable().setLinesVisible( true );
  }

  public void dispose()
  {
    if( m_model != null )
      m_model.removeModelListener( this );
  }

  public LayerTableModel getModel()
  {
    return m_model;
  }

  public void setModel( final LayerTableModel model )
  {
    final Table table = m_viewer.getTable();
    final TableColumn[] columns = table.getColumns();
    for( int i = 0; i < columns.length; i++ )
      columns[i].dispose();

    if( m_model != null )
      m_model.removeModelListener( this );

    m_model = model;

    if( model == null )
      return;

    m_model.addModelListener( this );

    final MenuManager mm = new MenuManager( "Kontext", "context" );

    final KalypsoFeatureLayer layer = model.getLayer();
    final FeatureType featureType = layer.getFeatureType();
    final FeatureTypeProperty[] featureTypeProperties = featureType.getProperties();
    for( int i = 0; i < featureTypeProperties.length; i++ )
    {
      final TableColumn tc = new TableColumn( table, SWT.CENTER );
      tc.setWidth( 100 );

      final FeatureTypeProperty ftp = featureTypeProperties[i];
      tc.setText( ftp.getName() );
      tc.setData( ftp );

      handleColumn( tc );

      mm.add( new ColumnAction( m_commandManager, this, ftp, getModel().isVisible( ftp ) ) );
    }

    table.setMenu( mm.createContextMenu( table ) );

    m_viewer.setInput( model );
  }

  protected void handleColumn( final TableColumn tc )
  {
    final boolean bVisible = m_model.isVisible( (FeatureTypeProperty)tc.getData() );
    tc.setResizable( bVisible );
    tc.setWidth( bVisible ? 100 : 0 );
  }

  private final static class ColumnAction extends Action
  {
    private final FeatureTypeProperty m_ftp;

    private final ICommandManager m_commandManager;

    private final LayerTable m_layerTable;

    public ColumnAction( final ICommandManager commandManager, final LayerTable layerTable,
        final FeatureTypeProperty ftp, final boolean bVisible )
    {
      super( ftp.getName() );

      m_ftp = ftp;
      m_commandManager = commandManager;
      m_layerTable = layerTable;

      setChecked( bVisible );
    }

    /**
     * @see org.eclipse.jface.action.IAction#run()
     */
    public void run()
    {
      m_commandManager.postCommand( new SetColumnVisibleCommand( m_layerTable.getModel(), m_ftp,
          isChecked() ), null );
    }
  }

  /**
   * @see org.kalypso.editor.tableeditor.layerTable.ILayerTableModelListener#onColumnChanged(org.deegree.model.feature.FeatureTypeProperty)
   */
  public void onColumnChanged( final FeatureTypeProperty ftp )
  {
    m_viewer.getControl().getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        final TableColumn[] columns = m_viewer.getTable().getColumns();
        for( int i = 0; i < columns.length; i++ )
        {
          final TableColumn column = columns[i];
          if( column.getData() == ftp )
          {
            handleColumn( column );
            break;
          }
        }
      }
    } );
  }

  /**
   * 
   * @see org.kalypso.editor.tableeditor.layerTable.ILayerTableModelListener#onRowsChanged(org.deegree.model.feature.Feature)
   */
  public void onRowsChanged( final Feature row )
  {
    m_viewer.getControl().getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        m_viewer.refresh( /* row */ );
      }
    } );
  }

  public void selectRow( final Feature feature )
  {
    m_viewer.getControl().getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        m_viewer.setSelection( new StructuredSelection( feature ) );
      }
    } );

  }
}