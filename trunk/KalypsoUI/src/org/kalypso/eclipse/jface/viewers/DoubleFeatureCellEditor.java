package org.kalypso.eclipse.jface.viewers;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.viewers.TextCellEditor;
import org.kalypso.ogc.gml.KalypsoFeature;

/**
 * @author Belger
 */
public class DoubleFeatureCellEditor extends AbstractFeatureCellEditor
{
  public DoubleFeatureCellEditor( final String propertyName )
  {
    super( new TextCellEditor(), propertyName );
    
    setValidator( DefaultCellValidators.DOUBLE_VALIDATOR );
  }

  /**
   * @see org.kalypso.eclipse.jface.viewers.AbstractFeatureCellEditor#doGetValues()
   */
  protected Map doGetValues()
  {
    final Object value = getEditor().getValue();

    final Map map = new HashMap();
    map.put( getPropertyName(),new Double( value == null ? "0.0" : value.toString() ) );

    return map;
  }

  /**
   * @see org.kalypso.eclipse.jface.viewers.AbstractFeatureCellEditor#doSetFeature(org.kalypso.ogc.gml.KalypsoFeature)
   */
  protected void doSetFeature( final KalypsoFeature feature )
  {
    final Object value = feature.getProperty( getPropertyName() );
    getEditor().setValue( value == null ? "" : value.toString() );
  }
}
