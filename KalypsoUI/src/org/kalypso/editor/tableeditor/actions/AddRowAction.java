package org.kalypso.editor.tableeditor.actions;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree_impl.model.feature.FeatureFactory;
import org.eclipse.jface.action.Action;
import org.eclipse.ui.IEditorPart;
import org.kalypso.editor.tableeditor.GisTableEditor;
import org.kalypso.editor.tableeditor.GisTableEditorActionBarContributor;
import org.kalypso.editor.tableeditor.layerTable.LayerTable;
import org.kalypso.editor.tableeditor.layerTable.LayerTableModel;

/**
 * @author bce
 */
public class AddRowAction extends Action
{
  private final GisTableEditorActionBarContributor m_contributor;

  public AddRowAction( final GisTableEditorActionBarContributor contributor )
  {
    super( "Zeile hinzufügen" );

    setToolTipText( "Fügt eine neue Zeile zur Tabelle hinzu" );
    
    m_contributor = contributor;
  }

  
  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    final IEditorPart activeEditor = m_contributor.getPage().getActiveEditor();
    if( activeEditor instanceof GisTableEditor )
    {
      final GisTableEditor gisTableEditor = ((GisTableEditor)activeEditor);
      final LayerTable layerTable = gisTableEditor.getLayerTable();
      final LayerTableModel model = layerTable.getModel();

      final FeatureType featureType = model.getFeatureType();
      final Object[] properties = new Object[featureType.getProperties().length];
      
      final Feature feature = FeatureFactory.createFeature( "x", featureType, properties ); 
      
      final Runnable r = new Runnable() {
        public void run()
        {
            layerTable.selectRow( feature );
        }};
      
      
      gisTableEditor.postCommand( new AddRowCommand( model, feature ), r );
    }
  }
}
