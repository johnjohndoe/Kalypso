package org.kalypso.eind.tuhh.stationaer.rules;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.bce.eind.core.ProfilCorePlugin;
import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilConstants;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.util.ProfilUtil;
import com.bce.eind.core.profil.validator.AbstractValidatorRule;
import com.bce.eind.core.profil.validator.IValidatorMarkerCollector;

public class DoppelterPunktRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector )
      throws CoreException
  {
    if( profil == null )
      return;

    try
    {
      final List<IProfilPoint> points = profil.getPoints();
      IProfilPoint prevPoint = null;
      for( final IProfilPoint point : points )
      {
        if( prevPoint != null )
        {
           if( ProfilUtil.comparePoints(new POINT_PROPERTY[]{POINT_PROPERTY.BREITE ,POINT_PROPERTY.HOEHE},prevPoint,point))
               {
            collector.createProfilMarker( false, "Doppelter Punkt bei Breite = "
                + String.format( IProfilConstants.FMT_STATION, point.getValueFor(POINT_PROPERTY.BREITE ) ), "", profil.getPoints()
                .indexOf( point ), POINT_PROPERTY.BREITE.toString() );
        }
        }
        prevPoint = point;
      }
    }
    catch( final ProfilDataException e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, ProfilCorePlugin.getDefault().getBundle()
          .getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }

}
