package org.kalypso.ogc.gml.table.celleditors;

import java.util.Map;

import org.kalypso.ogc.gml.KalypsoFeature;

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
   * @see org.kalypso.ogc.gml.table.celleditors.AbstractFeatureCellEditor#doSetFeature(org.kalypso.ogc.gml.KalypsoFeature)
   */
  protected void doSetFeature( KalypsoFeature feature )
  {
  // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.table.celleditors.AbstractFeatureCellEditor#renderLabel(org.kalypso.ogc.gml.KalypsoFeature)
   */
  public String renderLabel( final KalypsoFeature feature )
  {
    final Object property = feature.getProperty( getPropertyName() );
    return property == null ? "<null>" : property.getClass().getName();
  }

}
