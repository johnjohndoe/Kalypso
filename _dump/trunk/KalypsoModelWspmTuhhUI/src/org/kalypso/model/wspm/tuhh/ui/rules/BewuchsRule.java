package org.kalypso.model.wspm.tuhh.ui.rules;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;


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
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmCorePlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }

}
