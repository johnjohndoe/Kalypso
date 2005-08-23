package com.bce.eind.core.profil.reparator;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;

/**
 * Handles the standard implementation details. Should be derived by most implementors of
 * {@link com.bce.eind.core.profil.reparator.IProfilReparator}.
 */
public abstract class AbstractProfilReparator implements IProfilReparator
{
  private String m_id;

  private String m_name;

  private String m_description;

  public final void setInitializationData( final IConfigurationElement config, final String propertyName,
      final Object data ) throws CoreException
  {
    m_id = config.getAttribute( "id" );
    m_name = config.getAttribute( "name" );
    m_description = config.getAttribute( "description" );
  }

  public final String getID( )
  {
    return m_id;
  }

  public final String getName( )
  {
    return m_name;
  }

  public final String getDescription( )
  {
    return m_description;
  }
}
