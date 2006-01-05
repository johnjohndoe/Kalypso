package com.bce.eind.core.profil.impl.points.operations;

import java.util.LinkedList;

import com.bce.eind.core.profil.IPointOperation;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

public class AddOperation implements IPointOperation
{

  /**
   * @see com.bce.eind.core.profil.IPointOperation#doOperation(com.bce.eind.core.profil.IProfilPoint[],
   *      java.lang.Object[]) values = Double[] {Breite,Hoehe} return null if operation failed
   */

  public Object[] doOperation( LinkedList<IProfilPoint> points, final Object[] values )
  {
    if( values.length != 2 )
      return null;
    try
    {
      final Double deltaB = (Double)values[0];
      final Double deltaH = (Double)values[1];
      for( IProfilPoint point : points )
      {
        point.setValueFor( POINT_PROPERTY.BREITE, point.getValueFor( POINT_PROPERTY.BREITE )
            + deltaB );
        point
            .setValueFor( POINT_PROPERTY.HOEHE, point.getValueFor( POINT_PROPERTY.HOEHE ) + deltaH );
      }
      return new Double[]
      { -deltaB, -deltaH };

    }
    catch( Exception e )
    {
      return null;
    }
  }

}