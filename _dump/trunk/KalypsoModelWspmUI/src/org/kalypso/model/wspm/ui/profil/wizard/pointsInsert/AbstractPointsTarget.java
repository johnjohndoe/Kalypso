/**
 * 
 */
package org.kalypso.model.wspm.ui.profil.wizard.pointsInsert;

import org.eclipse.core.runtime.IConfigurationElement;

/**
 * @author Belger
 */
public abstract class AbstractPointsTarget implements IPointsTarget
{
  private String m_id;

  private String m_label;

  private String m_description;

  /**
   * @see org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.IPointsSource#getLabel()
   */
  public final String getLabel( )
  {
    return m_label;
  }

  public final String getDescription( )
  {
    return m_description;
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.IPointsSource#getID()
   */
  public final String getID( )
  {
    return m_id;
  }

  @Override
  public String toString( )
  {
    return getLabel();
  }

  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  public final void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
    m_id = config.getAttribute( "id" );
    m_label = config.getAttribute( "label" );
    m_description = config.getAttribute( "description" );
  }

}
