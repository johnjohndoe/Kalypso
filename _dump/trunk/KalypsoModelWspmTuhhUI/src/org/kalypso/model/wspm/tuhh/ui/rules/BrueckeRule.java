package org.kalypso.model.wspm.tuhh.ui.rules;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.validator.AbstractValidatorRule;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;


/**
 * Br�ckenkanten d�rfen nicht unterhalb des Gel�ndeniveaus liegen Oberkante darf
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
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmCorePlugin
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
              "Die Grenzen der durchstr�mten Bereiche m�ssen auf der ersten und der letzten Gel�ndekoordinate liegen.",
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
        collector.createProfilMarker( true, "Oberkante Br�cke ["
            + String.format( IProfilConstants.FMT_STATION, b )
            + "]unter Gel�ndeh�he", "", profil.getPoints().indexOf( point ),
            POINT_PROPERTY.BREITE.toString() );
      }
      if( uk < h )
      {
        collector.createProfilMarker( true, "Unterkante Br�cke ["
            + String.format( IProfilConstants.FMT_STATION, b )
            + "]unter Gel�ndeh�he", "", profil.getPoints().indexOf( point ),
            POINT_PROPERTY.BREITE.toString() );
      }
      if( ok < uk )
      {
        collector.createProfilMarker( true, "Oberkante Br�cke ["
            + String.format( IProfilConstants.FMT_STATION, b )
            + "]unter Unterkante Br�cke", "", profil.getPoints()
            .indexOf( point ), POINT_PROPERTY.BREITE.toString() );
      }
    }
  }
}
