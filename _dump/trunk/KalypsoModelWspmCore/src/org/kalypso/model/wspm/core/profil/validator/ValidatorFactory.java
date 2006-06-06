package org.kalypso.model.wspm.core.profil.validator;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;


public class ValidatorFactory
{
  private IConfigurationElement[] m_ruleElements;
  private IConfigurationElement[] m_typeElements;
  private final String[] m_types;
  private final IValidatorRule[] m_rules;

  public ValidatorFactory( )
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    m_ruleElements = registry.getConfigurationElementsFor( "org.kalypso.model.wspm.core.validatorrule" );
    m_typeElements = registry.getConfigurationElementsFor( "org.kalypso.model.wspm.core.validatortype" );

    final Collection<String> types = new ArrayList<String>();
    
    for( final IConfigurationElement element : m_typeElements )
    {
        final String type = element.getAttribute( "name" );
        if( type != null )
          types.add( type );
    }
    
    m_types = new String[types.size()];

    // create all rules
    final Collection<IValidatorRule> rules = new ArrayList<IValidatorRule>();
    for( final IConfigurationElement element : m_ruleElements )
    {
        try
        {
          final Object protoRule = element.createExecutableExtension( "class" );
          if( protoRule instanceof IValidatorRule )
            rules.add( (IValidatorRule)protoRule );
        }
        catch( final CoreException e )
        {
          KalypsoModelWspmCorePlugin.getDefault().getLog().log( e.getStatus() );
        }
    }
    
    m_rules = rules.toArray( new IValidatorRule[rules.size()] );
  }
  
  public IValidatorRule[] getAllRules()
  {
    return m_rules;
  }

  public String[] getValidatorTypes()
  {
    return m_types;
  }
  
  public IValidatorRule[] createValidatorRules( final String type )
  {
    final Collection<IValidatorRule> rules = new ArrayList<IValidatorRule>();
    
    for( final IValidatorRule rule : m_rules )
    {
      if( rule.getType().equals( type ) )
        rules.add( rule );
    }
    
    return rules.toArray( new IValidatorRule[rules.size()] );
  }
}
