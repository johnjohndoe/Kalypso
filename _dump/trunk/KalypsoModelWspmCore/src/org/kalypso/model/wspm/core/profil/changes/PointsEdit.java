package org.kalypso.model.wspm.core.profil.changes;

import java.util.LinkedList;

import org.kalypso.model.wspm.core.profil.IPointOperation;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;


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
   * @see org.kalypso.model.wspm.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  public IProfilChange doChange( final ProfilChangeHint hint )
  {
    if (hint!=null) hint.setPointValuesChanged();
    final Object[] reverseValues = m_operation.doOperation( m_points, m_values );

    return new PointsEdit( m_points, m_operation, reverseValues );

  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getObject()
   */
  public Object getObject( )
  {
      return m_points;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getPointProperty()
   */
  public POINT_PROPERTY getPointProperty( )
  {
     return null;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getValue()
   */
  public Double getValue( )
  {
     return null;
  }

}