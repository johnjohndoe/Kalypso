package org.kalypso.eclipse.jface.viewers;

import java.util.Map;

import org.eclipse.jface.viewers.CellEditor;
import org.kalypso.ogc.gml.KalypsoFeature;

/**
 * @author belger
 */
public abstract class AbstractFeatureCellEditor extends AbstractDecoratorFeatureCellEditor
{
  private final String m_propertyName;

  public AbstractFeatureCellEditor( final CellEditor cellEditor, final String propertyName )
  {
    super( cellEditor );
    
    m_propertyName = propertyName;
  }
  
  public String getPropertyName()
  {
    return m_propertyName;
  }

  /**
   * Gibt eine map: propertyname -> neuer Wert   zurück
   * 
   * @see org.eclipse.jface.viewers.CellEditor#doGetValue()
   */
  protected final Object doGetValue()
  {
    return doGetValues();
  }
  
  protected abstract Map doGetValues();

  /**
   * value muss ein Feature sein
   * 
   * @see org.eclipse.jface.viewers.CellEditor#doSetValue(java.lang.Object)
   */
  protected final void doSetValue( final Object kalypsoFeature )
  {
      doSetFeature( (KalypsoFeature)kalypsoFeature ); 
  }

  protected abstract void doSetFeature( final KalypsoFeature feature );
}
