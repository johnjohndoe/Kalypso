package org.kalypso.eclipse.jface.viewers;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.viewers.TextCellEditor;
import org.kalypso.ogc.gml.KalypsoFeature;

/**
 * @author belger
 */
public class TextFeatureCellEditor extends AbstractFeatureCellEditor
{
  public TextFeatureCellEditor( final String propertyName )
  {
    super( new TextCellEditor(), propertyName );
  }

  /**
   * @see org.kalypso.eclipse.jface.viewers.AbstractFeatureCellEditor#doGetValues()
   */
  protected Map doGetValues()
  {
    final Object value = getEditor().getValue();

    final Map map = new HashMap();
    map.put( getPropertyName(), value == null ? "" : value.toString() );

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
