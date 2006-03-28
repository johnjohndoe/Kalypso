package org.kalypso.eind.tuhh.stationaer.rules;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.bce.eind.core.ProfilCorePlugin;
import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.validator.AbstractValidatorRule;
import com.bce.eind.core.profil.validator.IValidatorMarkerCollector;

/**
 * Brückenkanten dürfen nicht unterhalb des Geländeniveaus liegen Oberkante darf nicht unter Unterkante
 * 
 * @author belger
 */
public class BewuchsRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if(( profil == null )||(!profil.getProfilPoints().propertyExists(POINT_PROPERTY.BEWUCHS_AX)))
      return;
    try
    {
      final double[] aX = profil.getValuesFor( POINT_PROPERTY.BEWUCHS_AX );
      final double[] aY = profil.getValuesFor( POINT_PROPERTY.BEWUCHS_AY );
      final double[] dP = profil.getValuesFor( POINT_PROPERTY.BEWUCHS_DP );
      {
        for( int i = 0; i < aX.length; i++ )
        {
          if( (aX[i] + aY[i] + dP[i] != 0) && (aX[i] * aY[i] * dP[i] == 0) )
          {
            collector.createProfilMarker( true, "Ist ein Wert für Bewuchs Null, werden die übrigen ignoriert.", "", i, POINT_PROPERTY.BEWUCHS_AX.toString() );
          }
        }
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, ProfilCorePlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }

}
