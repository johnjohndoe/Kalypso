package org.bce.eind.kalypso1d.rules;

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
import com.bce.eind.core.profil.util.ProfilUtil;
import com.bce.eind.core.profil.validator.AbstractValidatorRule;
import com.bce.eind.core.profil.validator.IValidatorMarkerCollector;

/**
 * Br�ckenkanten d�rfen nicht unterhalb des Gel�ndeniveaus liegen Oberkante darf nicht unter Unterkante
 * 
 * @author belger
 */
public class WehrRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil, final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( (profil == null) || (profil.getBuilding() == null) || (profil.getBuilding().getTyp() != IProfilBuilding.BUILDING_TYP.WEHR) )
      return;

    try
    {
      validateLimits( profil, collector );
      validateProfilLines( profil, collector );
      validateDevider( profil, collector );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, ProfilCorePlugin.getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }
  }

  private void validateDevider( final IProfil profil, final IValidatorMarkerCollector collector ) throws Exception
  {
    final IProfilDevider[] deviders = profil.getDevider( new DEVIDER_TYP[] { DEVIDER_TYP.TRENNFLAECHE, DEVIDER_TYP.WEHR } );
    if (deviders.length < 3) return;
    if( deviders[0].getTyp()==DEVIDER_TYP.WEHR  )
    {
      final IProfilPoint point =  deviders[0].getPoint();
      collector.createProfilMarker( true, "Wehrfeldtrenner [" + String.format( IProfilConstants.FMT_STATION, point.getValueFor(POINT_PROPERTY.BREITE ) ) + "]au�erhalb der Trennfl�chen", "", profil.getPoints().indexOf(point), POINT_PROPERTY.BREITE.toString() ); 
    }
    if( deviders[deviders.length -1].getTyp()==DEVIDER_TYP.WEHR  )
    {
      final IProfilPoint point =  deviders[deviders.length -1].getPoint();
      collector.createProfilMarker( true, "Wehrfeldtrenner [" + String.format( IProfilConstants.FMT_STATION, point.getValueFor(POINT_PROPERTY.BREITE ) ) + "]au�erhalb der Trennfl�chen", "", profil.getPoints().indexOf(point), POINT_PROPERTY.BREITE.toString() ); 
    }
  }

  private void validateProfilLines( final IProfil profil, final IValidatorMarkerCollector collector ) throws Exception
  {
    final List<IProfilPoint> points = profil.getPoints();
    for( final IProfilPoint point : points )
    {

      final double h = point.getValueFor( POINT_PROPERTY.HOEHE );
      final double b = point.getValueFor( POINT_PROPERTY.BREITE );
      final double wk = point.getValueFor( POINT_PROPERTY.OBERKANTEWEHR );

      if( wk < h )
      {
        collector.createProfilMarker( true, "Oberkante Wehr [" + String.format( IProfilConstants.FMT_STATION, b ) + "]unter Gel�ndeh�he", "", profil.getPoints().indexOf( point ), POINT_PROPERTY.BREITE.toString() );
      }

    }
  }

  private void validateLimits( final IProfil profil, final IValidatorMarkerCollector collector ) throws Exception
  {
    final List<IProfilPoint> points = ProfilUtil.getInnerPoints( profil, DEVIDER_TYP.TRENNFLAECHE );

    if( points.size() < 2 )
      return;
    final IProfilPoint firstPoint = points.get( 0 );
    final IProfilPoint lastPoint = points.get( points.size() - 1 );

    if( Math.abs( firstPoint.getValueFor( POINT_PROPERTY.HOEHE ) - firstPoint.getValueFor( POINT_PROPERTY.OBERKANTEWEHR ) ) > 0.001 )
    {
      collector.createProfilMarker( true, "Der erste Punkt[" + String.format( IProfilConstants.FMT_STATION, firstPoint.getValueFor( POINT_PROPERTY.BREITE ) )
          + "]der OK-Wehr muss auf Gel�ndeh�he liegen", "", profil.getPoints().indexOf( firstPoint ), POINT_PROPERTY.BREITE.toString() );
    }
    if( Math.abs( lastPoint.getValueFor( POINT_PROPERTY.HOEHE ) - lastPoint.getValueFor( POINT_PROPERTY.OBERKANTEWEHR ) ) > 0.001 )
    {
      collector.createProfilMarker( true, "Der letzte Punkt[" + String.format( IProfilConstants.FMT_STATION, lastPoint.getValueFor( POINT_PROPERTY.BREITE ) )
          + "]der OK-Wehr muss auf Gel�ndeh�he liegen", "", profil.getPoints().indexOf( lastPoint ), POINT_PROPERTY.BREITE.toString() );
    }
  }
}
