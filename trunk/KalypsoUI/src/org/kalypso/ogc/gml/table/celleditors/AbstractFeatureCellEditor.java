package org.kalypso.ogc.gml.table.celleditors;

import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.kalypso.ogc.gml.KalypsoFeature;

/**
 * @author Belger
 */
public abstract class AbstractFeatureCellEditor extends AbstractDecoratorFeatureCellEditor
{
  private String m_propertyName = null;

  private IProject m_project = null;

  public void setPropertyName( final String name )
  {
    m_propertyName = name;
  }

  public String getPropertyName()
  {
    return m_propertyName;
  }

  public void setProject( final IProject project )
  {
    m_project = project;
  }

  public IProject getProject()
  {
    return m_project;
  }

  /**
   * Gibt eine map: propertyname -> neuer Wert zurück
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

  public abstract String renderLabel( final KalypsoFeature feature );
}