package org.kalypso.eclipse.jface.viewers;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Control;
import org.kalypso.ogc.gml.KalypsoFeature;


/**
 * @author Belger
 */
public class ObservationFeatureCellEditor extends AbstractFeatureCellEditor
{
  public ObservationFeatureCellEditor( final String propertyName )
  {
    super( new DialogCellEditor(), propertyName );
    
    setValidator( DefaultCellValidators.DOUBLE_VALIDATOR );
  }

  /**
   * @see org.kalypso.eclipse.jface.viewers.AbstractFeatureCellEditor#doGetValues()
   */
  protected Map doGetValues()
  {
    final Object value = getEditor().getValue();

    final Map map = new HashMap();
    map.put( getPropertyName(), value );

    return map;
  }

  /**
   * @see org.kalypso.eclipse.jface.viewers.AbstractFeatureCellEditor#doSetFeature(org.kalypso.ogc.gml.KalypsoFeature)
   */
  protected void doSetFeature( final KalypsoFeature feature )
  {
    final Object value = feature.getProperty( getPropertyName() );
    getEditor().setValue( value );
  }

}
