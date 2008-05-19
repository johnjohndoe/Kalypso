package org.kalypso.model.wspm.core;

import org.eclipse.core.runtime.Plugin;
import org.kalypso.model.wspm.core.profil.validator.IValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.ValidatorFactory;
import org.kalypso.model.wspm.core.profil.validator.ValidatorRuleSet;
import org.osgi.framework.BundleContext;

public class KalypsoModelWspmCorePlugin extends Plugin
{
  /** The shared instance. */
  private static KalypsoModelWspmCorePlugin plugin;

  public static KalypsoModelWspmCorePlugin getDefault( )
  {
    return plugin;
  }

  public static String getID( )
  {
    return getDefault().getBundle().getSymbolicName();
  }

  /** The rules will will created (laziliy) only once and used in every rule set. */
 
  private ValidatorFactory m_validatorFactory = null;

  @Override
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );

    plugin = this;
  }

  @Override
  public void stop( final BundleContext context ) throws Exception
  {
    super.stop( context );

    plugin = null;
  }

  public static ValidatorRuleSet getValidatorSet( final String type )
  {
    final ValidatorFactory vf = getDefault().getValidatorFactory();

    final IValidatorRule[] rules = vf.createValidatorRules( type );
    return new ValidatorRuleSet( rules );
  }

  public ValidatorFactory getValidatorFactory( )
  {
    if( m_validatorFactory == null )
      m_validatorFactory = new ValidatorFactory();

    return m_validatorFactory;
  }
}
