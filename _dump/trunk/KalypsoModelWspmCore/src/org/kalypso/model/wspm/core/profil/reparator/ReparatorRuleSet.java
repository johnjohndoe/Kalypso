package org.kalypso.model.wspm.core.profil.reparator;

import org.kalypso.model.wspm.core.profil.IProfil;

public class ReparatorRuleSet
{
  private IProfilReparator[] m_rules;

  public ReparatorRuleSet( IProfilReparator[] rules )
  {
    m_rules = rules;
  }

  /**
   * Applies one(!) change to the given profile in order to repair it. Typically called in the
   * profilChanged event loop.
   * 
   * @param false,
   *          if no rule was triggered, else true.
   */
  public boolean repairProfile( final IProfil profil )
  {
    // find first rule which wants to apply changes
    for( final IProfilReparator rule : m_rules )
    {
      if( rule.hasChanges( profil ) )
      {
        rule.doChanges( profil );
        return true;
      }
    }

    return false;
  }
}
