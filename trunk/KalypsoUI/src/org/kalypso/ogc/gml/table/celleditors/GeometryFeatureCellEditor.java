package org.kalypso.ogc.gml.table.celleditors;

import java.util.Map;

import org.deegree.model.feature.Feature;

/**
 * @author gernot
 */
public class GeometryFeatureCellEditor extends AbstractFeatureCellEditor
{

  /**
   * @see org.kalypso.ogc.gml.table.celleditors.AbstractFeatureCellEditor#doGetValues()
   */
  protected Map doGetValues()
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.table.celleditors.AbstractFeatureCellEditor#doSetFeature(org.kalypso.ogc.gml.Feature)
   */
  protected void doSetFeature( Feature feature )
  {
  // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.table.celleditors.AbstractFeatureCellEditor#renderLabel(org.kalypso.ogc.gml.Feature)
   */
  public String renderLabel( final Feature feature )
  {
    final Object property = feature.getProperty( getPropertyName() );
    return property == null ? "<null>" : property.getClass().getName();
  }

}
