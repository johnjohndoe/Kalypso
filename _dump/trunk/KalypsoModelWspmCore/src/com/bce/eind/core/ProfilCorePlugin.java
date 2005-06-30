package com.bce.eind.core;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

import com.bce.eind.core.profil.validator.ValidatorFactory;

public class ProfilCorePlugin extends Plugin
{
  /** The shared instance. */
  private static ProfilCorePlugin plugin;

  public static ProfilCorePlugin getDefault( )
  {
    return plugin;
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

  public ValidatorFactory getValidatorFactory( )
  {
    if( m_validatorFactory == null )
      m_validatorFactory = new ValidatorFactory();
    
    return m_validatorFactory;
  }
}
