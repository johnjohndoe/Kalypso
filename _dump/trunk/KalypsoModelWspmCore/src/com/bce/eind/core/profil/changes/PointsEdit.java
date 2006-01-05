package com.bce.eind.core.profil.changes;

import java.util.LinkedList;

import com.bce.eind.core.profil.IPointOperation;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.impl.PlainProfil;

public final class PointsEdit implements IProfilChange
{
  private final LinkedList<IProfilPoint> m_points;

  private final Object[] m_values;

  private final IPointOperation m_operation;

  public PointsEdit( final LinkedList<IProfilPoint> points, final IPointOperation operation,
      final Object[] values )
  {
    m_points = points;
    m_values = values;
    m_operation = operation;
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    hint.setPointValuesChanged();
    final Object[] reverseValues = m_operation.doOperation( m_points, m_values );

    return new PointsEdit( m_points, m_operation, reverseValues );

  }

}