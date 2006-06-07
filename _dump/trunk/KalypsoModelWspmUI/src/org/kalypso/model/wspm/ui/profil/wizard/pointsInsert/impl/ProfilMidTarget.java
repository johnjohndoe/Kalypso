/**
 * 
 */
package org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.impl;

import java.util.Collection;

import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPoints;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.PointAdd;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.AbstractPointsTarget;


/**
 * @author Belger
 */
public class ProfilMidTarget extends AbstractPointsTarget
{
  /**
   * @see org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.IPointsTarget#insertPoints(org.kalypso.model.wspm.core.profil.impl.ProfilEventManager,
   *      IProfilPoints)
   */
  public void insertPoints( final IProfilEventManager pem, final IProfilPoints points )
  {
    final int pointsCount = points.getPoints().size();
    final Collection<POINT_PROPERTY> existingProps = pem.getProfil().getPointProperties( false );
    
    final IProfilChange[] changes = new IProfilChange[pointsCount];
    try
    {
      final IProfilPoint activePkt = pem.getProfil().getActivePoint();
      final IProfilPoint targetPkt = (activePkt != null) ? activePkt : pem.getProfil().getPoints().getLast();
      final double deltaX = points.getPoints().getFirst().getValueFor( POINT_PROPERTY.BREITE ) - targetPkt.getValueFor( POINT_PROPERTY.BREITE );
      final double deltaY = points.getPoints().getFirst().getValueFor( POINT_PROPERTY.HOEHE ) - targetPkt.getValueFor( POINT_PROPERTY.HOEHE );
      int i = changes.length - 1;
      for( IProfilPoint point : points.getPoints() )
      {
        final IProfilPoint newPoint = targetPkt.clonePoint();
        newPoint.setValueFor(POINT_PROPERTY.BREITE , point.getValueFor( POINT_PROPERTY.BREITE ) - deltaX);
        newPoint.setValueFor(POINT_PROPERTY.HOEHE , point.getValueFor( POINT_PROPERTY.HOEHE ) - deltaY );
        for( POINT_PROPERTY prop : existingProps )
        {
          if( points.propertyExists( prop ) && (prop != POINT_PROPERTY.BREITE)&& (prop != POINT_PROPERTY.HOEHE))
           {
            newPoint.setValueFor( prop, point.getValueFor( prop ) );
          }
        }
        changes[i--] = new PointAdd( pem.getProfil(), targetPkt,  newPoint );
      }
    }
    catch( ProfilDataException e )
    {
      // should never happen, stops operation and raise NullPointerException in ProfilOperation.doChange
      changes[0] = null;
    }
    final ProfilOperation operation = new ProfilOperation( "Punkte einfügen", pem, changes, false );
    new ProfilOperationJob( operation ).schedule();
  }
}
