package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.impl.PlainProfil;

public class BuildingEdit implements IProfilChange
{
  private final IProfilBuilding m_building;

  private final BUILDING_PROPERTY m_property;

  private final Object m_newValue;

  public BuildingEdit( final IProfilBuilding building, final BUILDING_PROPERTY property,
      final Object newValue )
  {
    m_building = building;
    m_property = property;
    m_newValue = newValue;
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    hint.setBuildingDataChanged();
    
    final Object oldValue = m_building.getValueFor( m_property );
    m_building.setValue( m_property, m_newValue );
    return new BuildingEdit( m_building, m_property, oldValue );
  }

  /* (non-Javadoc)
   * @see com.bce.eind.core.profil.IProfilChange#getChangedPoint()
   */
  public IProfilPoint getChangedPoint( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /* (non-Javadoc)
   * @see com.bce.eind.core.profil.IProfilChange#getChangedProperty()
   */
  public POINT_PROPERTY getChangedProperty( )
  {
    // TODO Auto-generated method stub
    return null;
  }
}
