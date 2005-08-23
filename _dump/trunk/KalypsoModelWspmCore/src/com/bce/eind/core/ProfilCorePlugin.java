package com.bce.eind.core;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

import com.bce.eind.core.profil.reparator.IProfilReparator;
import com.bce.eind.core.profil.reparator.ReparatorRuleSet;
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
  
  /** The rules will will created (laziliy) only once and used in every rule set. */
  private IProfilReparator[] m_reparatorRules;

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
  
  public final ReparatorRuleSet createReparatorRuleSet( )
  {
    if( m_reparatorRules == null )
      m_reparatorRules = ProfilCoreExtensions.createReaparatorRules();

    return new ReparatorRuleSet( m_reparatorRules );
  }

}
