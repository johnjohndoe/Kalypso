package com.bce.eind.core;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

import com.bce.eind.core.profil.validator.IValidatorRule;
import com.bce.eind.core.profil.validator.ValidatorFactory;
import com.bce.eind.core.profil.validator.ValidatorRuleSet;

public class ProfilCorePlugin extends Plugin
{
  /** The shared instance. */
  private static ProfilCorePlugin plugin;

  public static ProfilCorePlugin getDefault( )
  {
    return plugin;
  }

  public static String getID( )
  {
    return getDefault().getBundle().getSymbolicName();
  }
  
  private ValidatorFactory m_validatorFactory = null;

  @Override
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );

    plugin = this;
  }

  @Override
  public void stop( BundleContext context ) throws Exception
  {
    super.stop( context );

    plugin = null;
  }

  public ValidatorRuleSet getValidatorFactory( final String type )
  {
    if( m_validatorFactory == null )
      m_validatorFactory = new ValidatorFactory();
    
    final IValidatorRule[] rules = m_validatorFactory.createValidatorRules( type );
    return new ValidatorRuleSet( rules );
  }
}
