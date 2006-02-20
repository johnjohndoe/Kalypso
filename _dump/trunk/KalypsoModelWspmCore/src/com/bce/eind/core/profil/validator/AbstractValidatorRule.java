package com.bce.eind.core.profil.validator;

import org.eclipse.core.runtime.IConfigurationElement;

public abstract class AbstractValidatorRule implements IValidatorRule
{
  private String m_id;
  private String m_description;
  private String m_type;

  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
    m_id = config.getAttribute( "id" );
    m_type = config.getAttribute( "type" );
    m_description = config.getAttribute( "description" );
  }

  public String getID( )
  {
    return m_id;
  }

  public String getDescription( )
  {
    return m_description;
  }
  
  public String getType( )
  {
    return m_type;
  }
  
  @Override
  public String toString( )
  {
    return m_description;
  }
}
