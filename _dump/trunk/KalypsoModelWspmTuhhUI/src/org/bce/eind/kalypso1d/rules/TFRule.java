package org.bce.eind.kalypso1d.rules;

import org.eclipse.core.runtime.CoreException;

import com.bce.eind.core.profil.DeviderKey;
import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilConstants;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.ProfilPointProperty;
import com.bce.eind.core.profil.validator.AbstractValidatorRule;
import com.bce.eind.core.profil.validator.IValidatorMarkerCollector;

/**
 * Trennflächen und Bordvollpunkte dürfen nur innerhalb der durchströmten Bereiche liegen
 * 
 * @author belger
 */
public class TFRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    final IProfilPoint dbL = profil.getDevider( DeviderKey.DURCHSTROEMTE_L );
    final IProfilPoint dbR = profil.getDevider( DeviderKey.DURCHSTROEMTE_R );

    final int dblPos = profil.indexOf( dbL );
    final int dbrPos = profil.indexOf( dbR );

    validate( collector, profil, dblPos, dbrPos, DeviderKey.TRENNFLAECHE_L );
    validate( collector, profil, dblPos, dbrPos, DeviderKey.TRENNFLAECHE_R );
    validate( collector, profil, dblPos, dbrPos, DeviderKey.BORDVOLL_L );
    validate( collector, profil, dblPos, dbrPos, DeviderKey.BORDVOLL_R );
  }
  
  private void validate( final IValidatorMarkerCollector collector, final IProfil profil, final int dblPos, final int dbrPos, final DeviderKey key ) throws CoreException
  {
    final IProfilPoint point = profil.getDevider( key );
    if( point == null )
      return;
    
    final int tfPos = profil.indexOf( point );
    if( tfPos < dblPos || tfPos > dbrPos )
    {
      final String name = key.getValue() == 1 ? "rechts" : "links";
      final String message = "'" + key.getProfilPointProperty().getLabel() + " " + name + "' ausserhalb des durchströmten Bereichs.";
      String location = "";
      try
      {
        final double valueFor = point.getValueFor( ProfilPointProperty.BREITE );
        location = String.format( "Breite = " + IProfilConstants.FMT_STATION , valueFor );
      }
      catch( final ProfilDataException e )
      {
        e.printStackTrace();
        
        // should never happen, ignore
      }
      
      collector.createProfilMarker( true, message, location, new Integer( tfPos ) );
    }
  }
}
