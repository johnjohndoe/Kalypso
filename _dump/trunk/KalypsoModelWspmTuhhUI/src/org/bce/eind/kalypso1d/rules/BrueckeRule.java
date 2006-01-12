package org.bce.eind.kalypso1d.rules;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.bce.eind.core.ProfilCorePlugin;
import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilConstants;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_TYP;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.validator.AbstractValidatorRule;
import com.bce.eind.core.profil.validator.IValidatorMarkerCollector;

/**
 * Brückenkanten dürfen nicht unterhalb des Geländeniveaus liegen Oberkante darf
 * nicht unter Unterkante
 * 
 * @author belger
 */
public class BrueckeRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil,
      final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( (profil == null)
        || (profil.getBuilding() == null)
        || (profil.getBuilding().getTyp() != IProfilBuilding.BUILDING_TYP.BRUECKE) )
      return;

    try
    {
      validateProfilLines( profil, collector );
      validateDevider( profil, collector );

    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, ProfilCorePlugin
          .getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }

  private void validateDevider( final IProfil profil,
      final IValidatorMarkerCollector collector ) throws Exception
  {
    final LinkedList<IProfilPoint> points = profil.getPoints();
    final IProfilDevider[] devider = profil
        .getDevider( DEVIDER_TYP.DURCHSTROEMTE );
    if( devider.length != 2 )
      return;
    if( (devider[0].getPoint() != points.getFirst())
        || (devider[1].getPoint() != points.getLast()) )
    {
      collector
          .createProfilMarker(
              true,
              "Die Grenzen der durchströmten Bereiche müssen auf der ersten und der letzten Geländekoordinate liegen.",
              "", 0, POINT_PROPERTY.BREITE.toString() );
    }
  }

  private void validateProfilLines( final IProfil profil,
      final IValidatorMarkerCollector collector ) throws Exception
  {
    final List<IProfilPoint> points = profil.getPoints();
    for( final IProfilPoint point : points )
    {

      final double h = point.getValueFor( POINT_PROPERTY.HOEHE );
      final double b = point.getValueFor( POINT_PROPERTY.BREITE );
      final double ok = point.getValueFor( POINT_PROPERTY.OBERKANTEBRUECKE );
      final double uk = point.getValueFor( POINT_PROPERTY.UNTERKANTEBRUECKE );

      if( ok < h )
      {
        collector.createProfilMarker( true, "Oberkante Brücke ["
            + String.format( IProfilConstants.FMT_STATION, b )
            + "]unter Geländehöhe", "", profil.getPoints().indexOf( point ),
            POINT_PROPERTY.BREITE.toString() );
      }
      if( uk < h )
      {
        collector.createProfilMarker( true, "Unterkante Brücke ["
            + String.format( IProfilConstants.FMT_STATION, b )
            + "]unter Geländehöhe", "", profil.getPoints().indexOf( point ),
            POINT_PROPERTY.BREITE.toString() );
      }
      if( ok < uk )
      {
        collector.createProfilMarker( true, "Oberkante Brücke ["
            + String.format( IProfilConstants.FMT_STATION, b )
            + "]unter Unterkante Brücke", "", profil.getPoints()
            .indexOf( point ), POINT_PROPERTY.BREITE.toString() );
      }
    }
  }
}
