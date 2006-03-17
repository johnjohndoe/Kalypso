package com.bce.eind.core.profil.validator;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;

import com.bce.eind.core.ProfilCorePlugin;
import com.bce.eind.core.profil.IProfil;

/**
 * A set of validation rules and the means to use them.
 * 
 * @author belger
 */
public class ValidatorRuleSet
{
  private final IValidatorRule[] m_rules;

  public ValidatorRuleSet( final IValidatorRule[] rules )
  {
    m_rules = rules;
  }

  public IStatus validateProfile( final IProfil profil, final IValidatorMarkerCollector collector,
      final boolean validate, final String[] excludeIDs )
  {
    final IValidatorRule[] rules = m_rules;
    final List<IStatus> stati = new ArrayList<IStatus>( rules.length );
    final List<String> excludeRules = java.util.Arrays.asList( excludeIDs );

    if( validate && rules != null )
    {
      for( final IValidatorRule r : rules )
      {
        if( !excludeRules.contains( r.getID() ) )
        {
          try
          {
            r.validate( profil, collector );
          }
          catch( final CoreException e )
          {
            stati.add( e.getStatus() );
          }
        }
      }
    }

    if( stati.size() == 0 )
      return Status.OK_STATUS;

    return new MultiStatus( ProfilCorePlugin.getID(), 0,
        stati.toArray( new IStatus[stati.size()] ),
        "Ein oder mehr Fehler sind bei der Validierung des Profils aufgetreten.", null );
  }

  public IValidatorRule[] getRules( )
  {
    return m_rules;
  }
}
