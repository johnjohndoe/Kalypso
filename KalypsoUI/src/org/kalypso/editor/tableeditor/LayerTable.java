package org.kalypso.editor.tableeditor;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.deegree.graphics.Layer;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TableColumn;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.xml.tableview.Tableview;
import org.kalypso.xml.types.TableviewLayerType;
import org.kalypso.xml.types.TableviewLayerType.ColumnType;

/**
 * @author bce
 */
public class LayerTable extends TableViewer
{
  public LayerTable( Composite parent )
  {
    super( parent );
    setContentProvider(new LayerTableContentProvider());
    setLabelProvider(new LayerTableLabelProvider());
    
    getTable().setHeaderVisible(true );
    getTable().setLinesVisible(true);
  }

  public LayerTable( Composite parent, int style )
  {
    super( parent, style );
  }

  public void setTableview( final Tableview tableview, final IProject project )
  {
    setModel( null );
    
    if( tableview == null )
      return;
    
    final KalypsoGisPlugin plugin = KalypsoGisPlugin.getDefault();

    final TableviewLayerType layerType = tableview.getLayer();

    try
    {
      final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)plugin.getPool( Layer.class ).borrowObject(
          new PoolableObjectType( layerType.getType(), layerType.getSource(), project ) );

      final List cols = new ArrayList();
      for( final Iterator iter = layerType.getColumn().iterator(); iter.hasNext(); )
        cols.add( ((ColumnType)iter.next()).getName() );
      
      setModel( new LayerTableModel( layer, (String[])cols.toArray( new String[cols.size()] ) ) );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }
  
  public void setModel( final LayerTableModel model )
  {
    final TableColumn[] columns = getTable().getColumns();
    for( int i = 0; i < columns.length; i++ )
      columns[i].dispose();
    
    if( model == null )
      return;
    
    final KalypsoFeatureLayer layer = model.getLayer();
    final FeatureType featureType = layer.getFeatureType();
    final FeatureTypeProperty[] featureTypeProperties = featureType.getProperties();
    for( int i = 0; i < featureTypeProperties.length; i++ )
    {
      final TableColumn tc = new TableColumn( getTable(), SWT.CENTER );
      tc.setWidth( 100 );
      tc.setText( featureTypeProperties[i].getName() );
    }

    setInput( model );
  }

  public TableviewLayerType createLayerType()
  {
    // TODO
    return null;
  }
}
