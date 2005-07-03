package com.bce.eind.core.profil.validator;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.bce.eind.core.ProfilCorePlugin;

public class ValidatorFactory
{
  private IConfigurationElement[] m_ruleElements;

  public ValidatorFactory( )
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    m_ruleElements = registry.getConfigurationElementsFor( "com.bce.eind.core.validatorrule" );
  }

  public IValidatorRule[] createValidatorRules( final String type )
  {
    final Collection<IValidatorRule> rules = new ArrayList<IValidatorRule>();
    
    for( final IConfigurationElement element : m_ruleElements )
    {
      if( element.getAttribute( "type" ).equals( type ) )
      {
        try
        {
          final Object protoRule = element.createExecutableExtension( "class" );
          if( protoRule instanceof IValidatorRule )
            rules.add( (IValidatorRule)protoRule );
        }
        catch( final CoreException e )
        {
          ProfilCorePlugin.getDefault().getLog().log( e.getStatus() );
        }
      }
    }
    
    return rules.toArray( new IValidatorRule[rules.size()] );
  }
}
