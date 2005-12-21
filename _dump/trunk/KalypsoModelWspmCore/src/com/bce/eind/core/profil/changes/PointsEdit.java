package com.bce.eind.core.profil.changes;

import java.util.LinkedList;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.POINT_OPERATION;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.impl.PlainProfil;

public final class PointsEdit implements IProfilChange
{
  private final LinkedList<IProfilPoint> m_points;

  private final POINT_PROPERTY m_property;

  private final Double m_value;
  
  private final IProfil m_profil;
  
  private final POINT_OPERATION m_operation;

  public PointsEdit(final IProfil profil, final LinkedList<IProfilPoint> points, final POINT_PROPERTY property,final POINT_OPERATION operation, final double value )
  {
    m_points = points;
    m_property = property;
    m_value = value;
    m_profil = profil;
    m_operation = operation;
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    hint.setPointValuesChanged();
    m_profil.editPoints(m_points,m_operation,m_property,m_value);
    switch(m_operation)
    {
      case ADD: return new PointsEdit(m_profil,m_points,m_property,m_operation, -m_value);
      default: return null;
    }
    
  
  }

 
}