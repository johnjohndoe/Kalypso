package org.kalypso.ogc.gml.table.celleditors;

import java.util.HashMap;
import java.util.Map;

import org.deegree.model.feature.Feature;
import org.eclipse.jface.viewers.TextCellEditor;

/**
 * @author Belger
 */
public class TextFeatureCellEditor extends AbstractFeatureCellEditor
{
  public TextFeatureCellEditor(  )
  {
    setCellEditor( new TextCellEditor() );
  }

  /**
   * @see org.kalypso.ogc.gml.table.celleditors.AbstractFeatureCellEditor#doGetValues()
   */
  protected Map doGetValues()
  {
    final Object value = getEditor().getValue();

    final Map map = new HashMap();
    map.put( getPropertyName(), value == null ? "" : value.toString() );

    return map;
  }

  /**
   * @see org.kalypso.ogc.gml.table.celleditors.AbstractFeatureCellEditor#doSetFeature(org.kalypso.ogc.gml.Feature)
   */
  protected void doSetFeature( final Feature feature )
  {
    final Object value = feature.getProperty( getPropertyName() );
    getEditor().setValue( value == null ? "" : value.toString() );
  }

  /**
   * @see org.kalypso.ogc.gml.table.celleditors.AbstractFeatureCellEditor#renderLabel(org.kalypso.ogc.gml.Feature)
   */
  public String renderLabel( Feature feature )
  {
    final Object property = feature.getProperty( getPropertyName() );
    return property == null ? "<kein Wert>" :property.toString();
  }
}
