package org.kalypso.ogc.gml.table.celleditors;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.viewers.TextCellEditor;
import org.kalypso.ogc.gml.KalypsoFeature;

/**
 * @author Belger
 */
public class IntegerFeatureCellEditor extends AbstractFeatureCellEditor
{
  public IntegerFeatureCellEditor()
  {
    setCellEditor( new TextCellEditor() );
    setValidator( DefaultCellValidators.INTEGER_VALIDATOR );
  }

  /**
   * @see org.kalypso.ogc.gml.table.celleditors.AbstractFeatureCellEditor#doGetValues()
   */
  protected Map doGetValues()
  {
    final Object value = getEditor().getValue();

    final Map map = new HashMap();
    map.put( getPropertyName(), value == null ? null : new Integer( value.toString() ) );

    return map;
  }

  /**
   * @see org.kalypso.ogc.gml.table.celleditors.AbstractFeatureCellEditor#doSetFeature(org.kalypso.ogc.gml.KalypsoFeature)
   */
  protected void doSetFeature( final KalypsoFeature feature )
  {
    final Object value = feature.getProperty( getPropertyName() );
    getEditor().setValue( value == null ? "" : value.toString() );
  }

  /**
   * @see org.kalypso.ogc.gml.table.celleditors.AbstractFeatureCellEditor#renderLabel(org.kalypso.ogc.gml.KalypsoFeature)
   */
  public String renderLabel( final KalypsoFeature feature )
  {
    final Object property = feature.getProperty( getPropertyName() );
    return property == null ? "<kein Wert>" :property.toString();
  }
}
