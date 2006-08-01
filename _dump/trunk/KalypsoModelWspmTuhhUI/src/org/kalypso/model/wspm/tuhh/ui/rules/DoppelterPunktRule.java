package org.kalypso.model.wspm.tuhh.ui.rules;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;

public class DoppelterPunktRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
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
          if( ProfilUtil.comparePoints( new POINT_PROPERTY[] { POINT_PROPERTY.BREITE, POINT_PROPERTY.HOEHE }, prevPoint, point ) )
          {
            collector.createProfilMarker( false, "Doppelter Punkt bei Breite = " + String.format( IProfilConstants.FMT_STATION, point.getValueFor( POINT_PROPERTY.BREITE ) ), "", profil.getPoints().indexOf( point ), POINT_PROPERTY.BREITE.toString(), null );
          }
        }
        prevPoint = point;
      }
    }
    catch( final ProfilDataException e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }

}
