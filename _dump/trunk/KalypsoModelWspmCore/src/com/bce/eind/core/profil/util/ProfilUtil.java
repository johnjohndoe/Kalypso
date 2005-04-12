/*
 * Created on 14.03.2005
 */
package com.bce.eind.core.profil.util;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.impl.points.ProfilPoint;

/**
 * @author kimwerner
 */
public class ProfilUtil
{
  public static final IProfilPoint splitSegment( IProfilPoint startPoint, IProfilPoint endPoint )
      throws ProfilDataException
  {//TODO: Interpolation eigenschaft an Spalte anhängen und hier nur noch iteriren
    if( (startPoint == null) | (endPoint == null) )
      throw new ProfilDataException( "Profilpunkt existiert nicht" );
    final ProfilPoint point = (ProfilPoint)startPoint.clonePoint();

    final double m_x = (startPoint.getValueFor( IProfil.BREITE ) + endPoint
        .getValueFor( IProfil.BREITE )) / 2.0;
    point.setValueFor( IProfil.BREITE, m_x );

    final double m_y = (startPoint.getValueFor( IProfil.HOEHE ) + endPoint
        .getValueFor( IProfil.HOEHE )) / 2.0;
    point.setValueFor( IProfil.HOEHE, m_y );

    return point;
  }
}
