package org.bce.eind.kalypso1d.rules;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.bce.eind.core.ProfilCorePlugin;
import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilConstants;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.PARAMETER;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.validator.AbstractValidatorRule;
import com.bce.eind.core.profil.validator.IValidatorMarkerCollector;

public class RuecksprungRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil,
      final IValidatorMarkerCollector collector ) throws CoreException
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
          final double x1 = prevPoint.getValueFor( POINT_PROPERTY.BREITE );
          final double x2 = point.getValueFor( POINT_PROPERTY.BREITE );
          final double y1 = prevPoint.getValueFor( POINT_PROPERTY.HOEHE );
          final double y2 = point.getValueFor( POINT_PROPERTY.HOEHE );
          if( x2 - x1 < 0.0 )
            collector.createProfilMarker( true,
                "Gauss-R�cksprung bei Breite = "
                    + String.format( IProfilConstants.FMT_STATION, x2 ), "",
                profil.getPoints().indexOf( point ), POINT_PROPERTY.BREITE
                    .toString() );
          else if( (x2 - x1 < (Double) POINT_PROPERTY.BREITE
              .getParameter( PARAMETER.PRECISION ))
              && (y1 != y2) )
            collector.createProfilMarker( false,
                "Senkrechte Wand bei Breite = "
                    + String.format( IProfilConstants.FMT_STATION, x2 ), "",
                profil.getPoints().indexOf( point ), POINT_PROPERTY.BREITE
                    .toString() );
        }

        prevPoint = point;
      }
    }
    catch( final ProfilDataException e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, ProfilCorePlugin
          .getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }

}
