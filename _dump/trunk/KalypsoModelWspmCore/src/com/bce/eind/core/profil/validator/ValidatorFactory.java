package com.bce.eind.core.profil.validator;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

public class ValidatorFactory
{
  private IConfigurationElement[] m_ruleElements;

  public ValidatorFactory( )
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    m_ruleElements = registry.getConfigurationElementsFor( "com.bce.eind.core.validatorrule" );
  }

  public IValidatorRule[] createValidatorRules( final String type ) throws CoreException
  {
    final Collection<IValidatorRule> rules = new ArrayList<IValidatorRule>();
    
    for( final IConfigurationElement element : m_ruleElements )
    {
      if( element.getAttribute( "type" ).equals( type ) )
      {
        final IValidatorRule rule = (IValidatorRule)element.createExecutableExtension( "class" );
        rules.add( rule );
      }
    }
    
    return rules.toArray( new IValidatorRule[rules.size()] );
  }
}
