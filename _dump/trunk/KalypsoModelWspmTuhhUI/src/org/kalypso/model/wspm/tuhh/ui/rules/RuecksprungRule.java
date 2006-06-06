package org.kalypso.model.wspm.tuhh.ui.rules;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.PARAMETER;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;


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
                "Gauss-Rücksprung bei Breite = "
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
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin
          .getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }

}
