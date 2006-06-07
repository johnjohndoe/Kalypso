/**
 * 
 */
package org.kalypso.model.wspm.ui.profil.wizard.pointsInsert;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * @author Belger
 */
public abstract class AbstractPointsSource implements IPointsSource
{
  private String m_id;

  private String m_label;

  private String m_description;

  private Control m_control;

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

  public final void createControl( final Composite parent )
  {
    m_control = doCreateControl( parent );
  }
protected abstract void loadState( final IDialogSettings settings );
  public final Control getControl( final IDialogSettings settings  )
  {
    loadState(settings);
    return m_control;
  }

  protected abstract Control doCreateControl( final Composite parent );

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
